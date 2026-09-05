open Mir
open Buildmir
open Analysis


(* Extract all SSA IDs defined by an operation *)
let extract_op_defs = function
  | Func (res, _, _) | Pack (res, _, _) | CallClosure (res, _)
  | CallDirect (res, _, _) | Immi32 (res, _) | Immi8 (res, _)
  | ImmUnit res | Uopi32 (res, _, _) | Uopi8 (res, _, _)
  | Bopi32 (res, _, _, _) | Bopi8 (res, _, _, _)
  | Tupwrp (res, _) | Veclit (res, _) | Vecinit (res, _, _)
  | Veclen (res, _) | Vecread (res, _, _) | Vecwrite (res, _, _, _)
  | Vecinsert (res, _, _, _) | Vecslice (res, _, _, _)
  | Vecextend (res, _, _, _) | LoadGlobal (res, _) -> [res]
  | Tupuwrp (res_list, _) -> res_list
  | Copy (res, _) -> [res]
  | StoreGlobal _ | Drop _ | DropGlobal _ -> []

(* Extract all SSA IDs used by an operation *)
let extract_op_uses = function
  | Pack (_, oldclos, args) -> oldclos.ssaid :: List.map (fun sc -> sc.ssaid) args
  | CallClosure (_, clos) -> [clos.ssaid]
  | CallDirect (_, _, args) -> List.map (fun sc -> sc.ssaid) args
  | Copy (_, a) -> [a]
  | Drop mems -> mems
  | StoreGlobal (_, v) -> [v.ssaid]
  | Uopi32 (_, _, a) | Uopi8 (_, _, a) -> [a]
  | Bopi32 (_, _, a, b) | Bopi8 (_, _, a, b) -> [a; b]
  | Tupwrp (_, elms) | Veclit (_, elms) -> List.map (fun sc -> sc.ssaid) elms
  | Tupuwrp (_, tup) -> [tup.ssaid]
  | Vecinit (_, defval, dims) -> defval :: dims
  | Veclen (_, vec) -> [vec]
  | Vecread (_, vec, idxs) -> vec :: idxs
  | Vecwrite (_, vec, val_, idxs) -> vec.ssaid :: val_ :: idxs
  | Vecinsert (_, vec, vecins, idxs) -> vec.ssaid :: vecins.ssaid :: idxs
  | Vecslice (_, vec, start, len) -> [vec; start; len]
  | Vecextend (_, vec, lit, off) -> [vec; lit; off]
  | Func _ | LoadGlobal _ | DropGlobal _ | Immi32 _ | Immi8 _ | ImmUnit _ -> []

(* Operations that cannot be deleted even if their results are ignored *)
let is_critical_op = function
  | CallClosure _ | CallDirect _ | StoreGlobal _ | Drop _ | DropGlobal _ -> true
  | Func _ | Pack _ | Tupuwrp _ | Vecread _ | Vecslice _
  | Copy _ | LoadGlobal _
  | Immi32 _ | Immi8 _ | ImmUnit _ | Uopi32 _
  | Uopi8 _ | Bopi32 _ | Bopi8 _ | Tupwrp _
  | Veclit _ | Vecinit _ 
  | Veclen _ | Vecwrite _ | Vecinsert _
  | Vecextend _  -> false

(* Helper to filter a list using a boolean mask *)
let rec filter_mask mask lst =
  match mask, lst with
  | [], [] -> []
  | m::ms, x::xs -> if m then x :: filter_mask ms xs else filter_mask ms xs
  | _ -> invalid_arg "mask length mismatch"

let dce_opt_func (aly : analysis_info) (fn : func) =

  let def_to_uses = Hashtbl.create 256 in
  let worklist = Queue.create () in
  let live_ssas = ref SsaSet.empty in

  let bbs = fn.bbs in

  let mark_live ssaid =
    if not (SsaSet.mem ssaid !live_ssas) then begin
      live_ssas := SsaSet.add ssaid !live_ssas;
      Queue.add ssaid worklist
    end
  in

  let add_dependency def use =
    let existing = match Hashtbl.find_opt def_to_uses def with
      | Some xs -> xs
      | None -> []
    in
    Hashtbl.replace def_to_uses def (use :: existing)
  in

  (* =========================================================
     PHASE 1: Build Graph and Mark Roots
     ========================================================= *)
  BBMap.iter (fun _ bb ->
    (* 1a. Process Operations *)
    List.iter (fun op ->
      let uses = extract_op_uses op in
      if is_critical_op op then
        List.iter mark_live uses (* Critical ops are roots *)
      else
        let defs = extract_op_defs op in
        List.iter (fun def -> 
          List.iter (add_dependency def) uses
        ) defs
    ) bb.ops;

    (* 1b. Process Terminators & Map BB Arguments *)
    let map_branch_args (brbbid : bbid) (brargs : ssaconsume list) =
      let target_bb = BBMap.find brbbid bbs in
      (* target_bb parameter depends on incoming branch argument *)
      List.iter2 (fun param arg -> 
        add_dependency param arg.ssaid
      ) target_bb.args brargs
    in

    match bb.term with
    | Some (Ret ret) -> 
        mark_live ret (* Return is a root *)
    | Some (Br (brbbid, brargs)) -> 
        map_branch_args brbbid brargs (* Arguments are NOT roots, just data flow! *)
    | Some (Cbr (cond, _, _)) -> 
        mark_live cond;    (* Branch condition IS a root *)
    | None -> ()
  ) bbs;

  (* =========================================================
     PHASE 2: Propagate Liveness (Worklist)
     ========================================================= *)
  while not (Queue.is_empty worklist) do
    let needed_ssaid = Queue.pop worklist in
    match Hashtbl.find_opt def_to_uses needed_ssaid with
    | Some uses -> List.iter mark_live uses
    | None -> ()
  done;

  (* =========================================================
     PHASE 3: Sweep & Prune
     ========================================================= *)
  
  (* Determine which parameters survive in each Basic Block *)
  let bb_arg_masks = Hashtbl.create (fn.next_bbid) in
  BBMap.iter (fun _ (bb : bb) ->
    let mask = List.map (fun p -> SsaSet.mem p !live_ssas) bb.args in
    Hashtbl.add bb_arg_masks bb.bbid mask
  ) bbs;

  (* Helper to filter branch arguments based on the target block's surviving parameters *)
  let prune_branch (brbbid : bbid) (brargs : ssaconsume list) =
    let mask = Hashtbl.find bb_arg_masks brbbid in
    filter_mask mask brargs
  in

  (* Mutate the bbs in place to strip dead code *)
  BBMap.iter (fun _ bb ->
    (* 3a. Strip dead operations *)
    bb.ops <- List.filter (fun op ->
      if is_critical_op op then true
      else
        let defs = extract_op_defs op in
        List.exists (fun def -> SsaSet.mem def !live_ssas) defs
    ) bb.ops;

    (* 3b. Strip dead parameters *)
    bb.args <- List.filter (fun p -> SsaSet.mem p !live_ssas) bb.args;

    (* 3c. Strip dead arguments in terminators *)
    bb.term <- match bb.term with
      | Some (Br (brbbid, brargs)) -> Some (Br (brbbid, prune_branch brbbid brargs))
      | term_other -> term_other
  ) bbs;
  
  invalidate_all_analysis aly fn.funcid


let dce_opt (b : builder) (aly : analysis_info) : unit =
  FuncMap.iter (fun _fid fn -> 
    match fn.extern_name with
    | Some _ -> ()
    | None -> dce_opt_func aly fn
  ) b.program.funcs