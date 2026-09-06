(*

  A collection of Analysis for the MIR

  - Predecessors
  - Reverse Post Order
  - Liveliness
  - Borrowers and Lenders
  - Domination

  The analysis_info record collects all in one place. The intention is of course
  to use an analysis_info for one program only but in and of itself the analysis_info
  record is not bound / does not contain to a MIR Program.

*)


open Mir

(* ========================================================================= *)
(* MIR Analysis                                                              *)
(* ========================================================================= *)

type preds_info = {
  preds: bbid list array; (* preds[bbid] = list of predecessor bbids *)
}

type rpo_info = {
  rpo_lst: bbid list;      (* reverse post order of basic blocks *)
  rpo_idx: int array;    (* rpo_index[bbid] = index of bbid in rpo *)
}

type live_info = {
  live_in  : SsaSet.t array;
  live_out : SsaSet.t array;
  block_defs: SsaSet.t array;
  block_uses: SsaSet.t array;
  (* op_index counts form bottom to top, so lower op_index mean later in the block *)
  def: (bbid * int) array; (* def[ssaid] = (bbid, op_index) definition of ssaid, if any *)
  last_use : ((bbid * int) list ) array; (* last_use[ssaid] = (bbid, op_index) of last use of ssaid*)
}

type borrow_info = {
  (* Given ssaid, find all DIRECT borrowers *)
  lender_to_borrowers : ssaid list array;
  (* Given ssaid, find all DIRECT lenders *)
  borrower_to_lenders : ssaid list array;
}

type dom_info = {
  (* idom[bbid] = immediate parent in dominator tree *)
  idom : bbid option array;
  (* dom tree *)
  dom_tree : bbid list array;
}

type analysis_info = {
  preds_arr : preds_info option Dynarray.t;
  rpo_arr : rpo_info option Dynarray.t;
  live_arr : live_info option Dynarray.t;
  borrow_arr: borrow_info option Dynarray.t;
  dom_arr : dom_info option Dynarray.t;
}

let create_analysis_info () : analysis_info = {
  preds_arr = Dynarray.create ();
  rpo_arr = Dynarray.create ();
  live_arr = Dynarray.create ();
  borrow_arr = Dynarray.create ();
  dom_arr = Dynarray.create ();
}

let dynarray_len_check (arr : 'a option Dynarray.t) (funcid : funcid) : unit =
  while funcid >= Dynarray.length arr do
    Dynarray.add_last arr None
  done

let invalidate_all_analysis (aly : analysis_info) (funcid : funcid) : unit =
  dynarray_len_check aly.preds_arr funcid;
  Dynarray.set aly.preds_arr funcid None;
  dynarray_len_check aly.rpo_arr funcid;
  Dynarray.set aly.rpo_arr funcid None;
  dynarray_len_check aly.live_arr funcid;
  Dynarray.set aly.live_arr funcid None;
  dynarray_len_check aly.borrow_arr funcid;
  Dynarray.set aly.borrow_arr funcid None;
  dynarray_len_check aly.dom_arr funcid;
  Dynarray.set aly.dom_arr funcid None

(* ========================================================================= *)
(* Predecessors                                                              *)
(* ========================================================================= *)

let get_preds predsarr bbid = Array.get predsarr bbid

let add_pred predsarr pred_bbid succ_bbid =
  let old_pred = Array.get predsarr succ_bbid in 
  Array.set predsarr succ_bbid (pred_bbid :: old_pred)

let rem_pred predsarr pred_bbid succ_bbid =
  let old_pred = Array.get predsarr succ_bbid in 
  let new_pred = List.filter (fun x -> x <> pred_bbid) old_pred in
  Array.set predsarr succ_bbid new_pred

let compute_preds fn =
  let predsarr = Array.make (fn.next_bbid) [] in

  (* Accumulate Predecessor Info *)
  BBMap.iter (fun _ bb ->
    match bb.term with
    | Some (Br (brbbid, _)) -> (
        add_pred predsarr bb.bbid brbbid
    )
    | Some (Cbr (_, ibr, ebr)) -> (
        add_pred predsarr bb.bbid ibr;
        add_pred predsarr bb.bbid ebr
    )
    | Some _ -> ()
    | None -> failwith ("preds: func: " ^ fn.name ^ " bb " ^ string_of_int bb.bbid ^ " has no term")
  ) fn.bbs;
  { preds = predsarr }

let get_preds_info (aly : analysis_info) (func : func) : preds_info =
  dynarray_len_check aly.preds_arr func.funcid;
  match Dynarray.get aly.preds_arr func.funcid with
  | Some preds_info -> preds_info
  | None -> 
    let preds_info = compute_preds func in
    Dynarray.set aly.preds_arr func.funcid (Some preds_info);
    preds_info


(* ========================================================================= *)
(* Reverse Post Order                                                        *)
(* ========================================================================= *)

let compute_rpo (fn : func) =
  let visited = Hashtbl.create (BBMap.cardinal fn.bbs) in
  let rpo = ref [] in

  let rec dfs bbid =
    if not (Hashtbl.mem visited bbid) then begin
      Hashtbl.add visited bbid ();
      match BBMap.find_opt bbid fn.bbs with
      | Some bb ->
          (* 1. Extract successors *)
          let succs = match bb.term with
            | Some (Br (brbbid, _)) -> [brbbid]
            | Some (Cbr (_, br_t, br_f)) -> [br_f; br_t]
            | Some (Ret _) | None -> []
          in
          (* 2. Visit children (Post-Order step) *)
          List.iter dfs succs;
          (* 3. Prepend to list (implicitly Reverses the Post-Order) *)
          rpo := bb.bbid :: !rpo
      | None -> ()
    end;
  in

  (match fn.entry_bb with
  | Some entry_bb -> dfs entry_bb
  | None -> failwith ("compute_rpo: function " ^ fn.name ^ " has no entry basic block"));
  let rpo_idx = Array.make fn.next_bbid (-1) in
  List.iteri (fun idx bbid -> rpo_idx.(bbid) <- idx) !rpo;
  { rpo_lst = !rpo; rpo_idx = rpo_idx }

let get_rpo_info (aly : analysis_info) (func : func) : rpo_info =
  dynarray_len_check aly.rpo_arr func.funcid;
  match Dynarray.get aly.rpo_arr func.funcid with
  | Some rpo_info -> rpo_info
  | None -> 
    let rpo_info = compute_rpo func in
    Dynarray.set aly.rpo_arr func.funcid (Some rpo_info);
    rpo_info


(* ========================================================================= *)
(* Liveliness                                                                *)
(* ========================================================================= *)


let compute_live (aly : analysis_info) (f : func) =
  let n = f.next_bbid in
  let live_in = Array.make n SsaSet.empty in
  let live_out = Array.make n SsaSet.empty in

  (* --- STEP 1: Precompute local Use and Def for each block --- *)
  let block_uses = Array.make n SsaSet.empty in
  let block_defs = Array.make n SsaSet.empty in

  BBMap.iter (fun bbid bb ->
    let uses = ref SsaSet.empty in
    let defs = ref SsaSet.empty in

    let apply_use u = uses := SsaSet.add u !uses in
    let apply_uses us = List.iter apply_use us in
    let apply_sc_uses scs = List.iter (fun sc -> apply_use sc.ssaid) scs in
    let apply_def d = defs := SsaSet.add d !defs in
    let apply_defs ds = List.iter apply_def ds in

    (* 1. Accumulate Terminator uses *)
    (match bb.term with
     | Some (Br (_, brargs)) -> 
        apply_sc_uses brargs
     | Some (Cbr (cond, tbbid, fbbid)) -> 
        apply_use cond;
     | Some (Ret ret) -> 
        apply_use ret
     | None -> failwith ("live_analysis: function " ^ string_of_int f.funcid ^ " block " ^ string_of_int bbid ^ " has no terminator")
    );

    (* 2. Accumulate Op uses/defs (processed in reverse order) *)
    List.iter (fun op ->
      match op with
      | Func (res, _, _) -> apply_def res
      | Pack (res, sc, scs) -> apply_def res; apply_use sc.ssaid; apply_sc_uses scs
      | CallClosure (res, sc) -> apply_def res; apply_use sc.ssaid
      | CallDirect (res, _, scs) -> apply_def res; apply_sc_uses scs
      | Copy (res, a) -> apply_def res; apply_use a
      | Drop mems -> apply_uses mems
      | StoreGlobal (_, sc) -> apply_use sc.ssaid
      | LoadGlobal (res, _) -> apply_def res
      | DropGlobal _ -> ()
      | Immi32 (res, _) | Immi8 (res, _) | ImmUnit res -> apply_def res
      | Uopi32 (res, _, a) | Uopi8 (res, _, a) -> apply_def res; apply_use a
      | Bopi32 (res, _, a, b) | Bopi8 (res, _, a, b) -> apply_def res; apply_uses [a; b]
      | Tupwrp (res, scs) -> apply_def res; apply_sc_uses scs
      | Tupuwrp (res_list, sc) -> apply_defs res_list; apply_use sc.ssaid
      | Veclit (res, scs) -> apply_def res; apply_sc_uses scs
      | Vecinit (res, defval, dims) -> apply_def res; apply_use defval; apply_uses dims
      | Veclen (res, vec) -> apply_def res; apply_use vec
      | Vecread (res, vec, idxs) -> apply_def res; apply_use vec; apply_uses idxs
      | Vecwrite (res, val_sc, vec, idxs) -> apply_def res; apply_use vec; apply_use val_sc.ssaid; apply_uses idxs
      | Vecinsert (res, vec_sc, vecins_sc, idxs) -> apply_def res; apply_use vec_sc.ssaid; apply_use vecins_sc.ssaid; apply_uses idxs
      | Vecslice (res, vec, start, len) -> apply_def res; apply_use vec; apply_use start; apply_use len
      | Vecextend (res, vec, lit, off) -> apply_def res; apply_use vec; apply_use lit; apply_use off
    ) bb.ops;

    (* 3. Block arguments act as definitions at the entry *)
    apply_defs bb.args;

    block_uses.(bbid) <- SsaSet.diff !uses !defs;
    block_defs.(bbid) <- !defs; 
  ) f.bbs;

  (* --- STEP 2: Fast Fixed-Point Loop using Precomputed Sets --- *)
  let changed = ref true in
  let rpo = get_rpo_info aly f in
  let po_bbid = List.rev rpo.rpo_lst in

  while !changed do
    changed := false;

    List.iter (fun bbid ->

      let bb = BBMap.find bbid f.bbs in
      let old_live_in = live_in.(bbid) in

      (* live_out = Union of live_in of all successors *)
      let out_set = match bb.term with
        | Some (Br (brbbid, _)) -> live_in.(brbbid)
        | Some (Cbr (_, tbbid, fbbid)) -> SsaSet.union live_in.(tbbid) live_in.(fbbid)
        | Some (Ret _) | None -> SsaSet.empty
      in
      live_out.(bbid) <- out_set;

      (* live_in = use U (live_out \ def) *)
      let u = block_uses.(bbid) in
      let d = block_defs.(bbid) in
      let new_live_in = SsaSet.union u (SsaSet.diff out_set d) in

      live_in.(bbid) <- new_live_in;

      if not (SsaSet.equal old_live_in new_live_in) then
        changed := true;
    ) po_bbid;
  done;

  (* --- STEP 3: Compute Last Use Information --- *)
  let def = Array.make f.next_ssaid (-1, -1) in
  let last_use = Array.make f.next_ssaid [] in

  BBMap.iter (fun bbid bb ->
    let used_later = ref live_out.(bbid) in
    let add_def ssaid op_index = def.(ssaid) <- (bbid, op_index) in
    let add_use ssaid op_index =
      if not (SsaSet.mem ssaid !used_later) then
        (last_use.(ssaid) <- (bbid, op_index) :: last_use.(ssaid);
        used_later := SsaSet.add ssaid !used_later)
    in
    let add_defs ssaids op_index = List.iter (fun ssaid -> add_def ssaid op_index) ssaids in
    let add_uses ssaids op_index = List.iter (fun ssaid -> add_use ssaid op_index) ssaids in
    let add_sc_uses scs op_index = List.iter (fun sc -> add_use sc.ssaid op_index) scs in

    List.iteri (fun op_index op ->
      match op with
      | Func (res, _, _) -> add_def res op_index
      | Pack (res, sc, scs) -> add_def res op_index; 
                               add_use sc.ssaid op_index; 
                               add_sc_uses scs op_index
      | CallClosure (res, sc) -> add_def res op_index; 
                                 add_use sc.ssaid op_index
      | CallDirect (res, _, scs) -> add_def res op_index;
                                    add_sc_uses scs op_index
      | Copy (res, a) -> add_def res op_index; add_use a op_index
      | Drop mems -> add_uses mems op_index
      | StoreGlobal (_, sc) -> add_use sc.ssaid op_index
      | LoadGlobal (res, _) -> add_def res op_index
      | DropGlobal _ -> ()
      | Immi32 (res, _) | Immi8 (res, _) | ImmUnit res -> add_def res op_index
      | Uopi32 (res, _, a) | Uopi8 (res, _, a) -> add_def res op_index; add_use a op_index
      | Bopi32 (res, _, a, b) | Bopi8 (res, _, a, b) -> add_def res op_index; add_uses [a; b] op_index
      | Tupwrp (res, scs) -> add_def res op_index; add_sc_uses scs op_index
      | Tupuwrp (res_list, sc) -> add_defs res_list op_index; add_use sc.ssaid op_index
      | Veclit (res, scs) -> add_def res op_index; add_sc_uses scs op_index
      | Vecinit (res, defval, dims) -> add_def res op_index; add_use defval op_index; add_uses dims op_index
      | Veclen (res, vec) -> add_def res op_index; add_use vec op_index
      | Vecread (res, vec, idxs) -> add_def res op_index; add_use vec op_index; add_uses idxs op_index
      | Vecwrite (res, val_sc, vec, idxs) -> add_def res op_index;
                                              add_use vec op_index;
                                              add_use val_sc.ssaid op_index;
                                              add_uses idxs op_index
      | Vecinsert (res, vec_sc, vecins_sc, idxs) -> add_def res op_index;
                                                  add_use vec_sc.ssaid op_index;
                                                  add_use vecins_sc.ssaid op_index;
                                                  add_uses idxs op_index
      | Vecslice (res, vec, start, len) -> add_def res op_index; add_use vec op_index; add_use start op_index; add_use len op_index
      | Vecextend (res, vec, lit, off) -> add_def res op_index; add_use vec op_index; add_use lit op_index; add_use off op_index
    )  bb.ops
  ) f.bbs;

  { live_in; live_out; block_defs; block_uses; def; last_use }

let get_live_info (aly : analysis_info) (func : func) : live_info =
  dynarray_len_check aly.live_arr func.funcid;
  match Dynarray.get aly.live_arr func.funcid with
  | Some live_info -> live_info
  | None -> 
    let live_info = compute_live aly func in
    Dynarray.set aly.live_arr func.funcid (Some live_info);
    live_info

(* ========================================================================= *)
(* Borrowing                                                                 *)
(* ========================================================================= *)

let compute_borrow (fn : func) =

  let lender_to_borrowers = Array.make fn.next_ssaid [] in
  let borrower_to_lenders = Array.make fn.next_ssaid [] in

  (* a borrows from b*)
  let borrow borrower lender =
    lender_to_borrowers.(lender) <- borrower :: lender_to_borrowers.(lender);
    borrower_to_lenders.(borrower) <- lender :: borrower_to_lenders.(borrower)
  in

  (*helper for the terms*)
  let bbargs_borrow brbbid brargs =
    let targbb = BBMap.find brbbid fn.bbs in
    List.iter2 (fun brarg bbarg ->
    (*
      I put all the potential borrows in the graph while this makes
      the resolvers a bit more complex it saves a massive headache 
      when answering questions like: is this bb arg borrow legal and/or
      does it extend the live range of the owners
    *)
    if Mir.is_memtyp (Mir.get_mirtyp_func fn bbarg)
    then borrow bbarg brarg.ssaid  
    ) brargs targbb.args
  in

  BBMap.iter (fun _ bb ->

    (*ops borrows*)
    List.iter (fun op ->
      match op with
      | Tupuwrp (deflst, tup) -> (
        List.iter (fun def ->
           if Mir.is_memtyp (Mir.get_mirtyp_func fn def)
           then borrow def tup.ssaid
        ) deflst
      )
      | Vecread (def, vec, idxlst) -> (
        match Mir.get_mirtyp_func fn vec with
        | TMIRVec (vecdim, _) when List.length idxlst < vecdim -> borrow def vec
        | _ -> ()
      ) 
      | Vecslice (def, vec, _, _) -> borrow def vec
      (* when at some point a mir op is added and does borrow 
         I need to remember that here I check for this. With | _ -> the compiler does not tell me
         like this it does so thats why I put this here*)
      | Func _ | Pack _ | CallClosure _ | CallDirect _
      | Copy _ | Drop _ | StoreGlobal _ | LoadGlobal _
      | Immi32 _ | Immi8 _ | ImmUnit _ | Uopi32 _
      | Uopi8 _ | Bopi32 _ | Bopi8 _ | Tupwrp _
      | Veclit _ | Vecinit _ | DropGlobal _
      | Veclen _ | Vecwrite _ | Vecinsert _
      | Vecextend _ -> ()
    ) bb.ops;

    (*term borrows*)
    match bb.term with
    | Some ( Br (brbbid, brargs) ) -> bbargs_borrow brbbid brargs
    | _ -> ();

  ) fn.bbs;

  { lender_to_borrowers; borrower_to_lenders}

let get_borrow_info (aly : analysis_info) (func : func) : borrow_info =
  dynarray_len_check aly.borrow_arr func.funcid;
  match Dynarray.get aly.borrow_arr func.funcid with
  | Some borrow_info -> borrow_info
  | None -> 
    let borrow_info = compute_borrow func in
    Dynarray.set aly.borrow_arr func.funcid (Some borrow_info);
    borrow_info

(* Find all borrowers of a given lender not just the direct ones (can contain duplicates)*)
  let find_borrowers_excludelist (excludelist: ssaid list) (aly : analysis_info) (fn : func) (lender : ssaid) : ssaid list =
  let borrow_info = get_borrow_info aly fn in
  let rec transacc acc lender =
    let borrowers = borrow_info.lender_to_borrowers.(lender) in
    List.fold_left (fun acc borrower -> 
      match get_ownership_func fn borrower with
      | Borrowed when List.mem borrower excludelist -> acc
      | Borrowed -> transacc (borrower :: acc) borrower 
      | Owned -> acc
      | NoMem -> failwith (Printf.sprintf "find_borrowers: ssaid %d is not a memory type but in borrowers" borrower)
     ) acc borrowers
  in
  transacc [] lender

let find_borrowers = find_borrowers_excludelist []


(* 
  Find all function local owners of a given borrower
  DOES NOT CHECK IF PASSED BORROWER IS A BORROWER
  this is usefull in the case where we would mb
  want to put it owned but atm its borrowed
*)
let find_funclocal_owners (aly : analysis_info) (fn : func) (borrower : ssaid) : ssaid list =
  let borrow_info = get_borrow_info aly fn in
  let rec transacc acc borrower =
    let lenders = borrow_info.borrower_to_lenders.(borrower) in
    (* Function args that are passed borrowed are ignored *)
    List.fold_left (fun acc lender ->
      match get_ownership_func fn lender with
      | Borrowed -> transacc acc lender
      | Owned -> (lender :: acc)
      | NoMem -> failwith (Printf.sprintf "find_owners: ssaid %d is not a memory type but in lenders" borrower)
    ) acc lenders
  in
  transacc [] borrower

(* ========================================================================= *)
(* Dominance                                                                 *)
(* ========================================================================= *)

let compute_dom (aly : analysis_info) (fn : func) =
  
  let rpo_list = (get_rpo_info aly fn).rpo_lst in
  let rpo_idx = (get_rpo_info aly fn).rpo_idx in
  
  let get_preds = get_preds (get_preds_info aly fn).preds in

  (* --- Cooper-Harvey-Kennedy Algorithm Setup --- *)
  let idom = Array.make fn.next_bbid None in

  let entry_id = Option.get fn.entry_bb in
  idom.(entry_id) <- fn.entry_bb;

  (* Intersect function: walks up the dominator tree until two paths meet *)
  let intersect b1 b2 =
    let f1 = ref b1 in
    let f2 = ref b2 in
    while !f1 <> !f2 do
      while rpo_idx.(!f1) > rpo_idx.(!f2) do
        match idom.(!f1) with
        | Some parent -> f1 := parent
        | None -> failwith "dom_analysis: malformed idom during intersect"
      done;
      while rpo_idx.(!f2) > rpo_idx.(!f1) do
        match idom.(!f2) with
        | Some parent -> f2 := parent
        | None -> failwith "dom_analysis: malformed idom during intersect"
      done;
    done;
    !f1
  in

  (* --- STEP 4: Fixed-Point Iteration over RPO --- *)
  let rpo_nodes_except_entry = List.filter (fun b -> b <> entry_id) rpo_list in
  let changed = ref true in

  while !changed do
    changed := false;
    List.iter (fun b ->
      let b_preds = get_preds b in
      (* Filter predecessors that already have an idom assigned *)
      let processed_preds = List.filter (fun p -> idom.(p) <> None) b_preds in

      match processed_preds with
      | [] -> ()
      | first_p :: rest_p ->
          let new_idom = List.fold_left (fun acc p ->
            intersect p acc
          ) first_p rest_p in

          if idom.(b) <> Some new_idom then begin
            idom.(b) <- Some new_idom;
            changed := true
          end
    ) rpo_nodes_except_entry;
  done;

  (* Entry block has no strict dominator *)
  idom.(entry_id) <- None;

  (* --- STEP 5: Build Dominator Tree --- *)
  let dom_tree = Array.make fn.next_bbid [] in
  Array.iteri (fun b_id opt_idom ->
    match opt_idom with
    | Some parent_id -> dom_tree.(parent_id) <- b_id :: dom_tree.(parent_id)
    | None -> ()
  ) idom;

  { idom = idom; dom_tree = dom_tree }


let get_dom_info (aly : analysis_info) (func : func) : dom_info =
  dynarray_len_check aly.dom_arr func.funcid;
  match Dynarray.get aly.dom_arr func.funcid with
  | Some dom_info -> dom_info
  | None -> 
    let dom_info = compute_dom aly func in
    Dynarray.set aly.dom_arr func.funcid (Some dom_info);
    dom_info



let does_strictly_dominate aly fn dominator dominated =
  let dom_info = get_dom_info aly fn in
  let rec check_dom b =
    if b = dominator then true
    else match dom_info.idom.(b) with
      | Some parent -> check_dom parent
      | None when fn.entry_bb = Some b -> false
      | _ -> failwith "does_strictly_dominate: reached a block with no idom that is not the entry"
  in
  if dominator = dominated 
  then false 
  else check_dom dominated
