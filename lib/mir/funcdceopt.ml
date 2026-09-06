
(*
  
  MIR Dead Function Elimination

  Scans all func and calldirect ops in the program,
  all functions that are never used get removed.

*)

open Mir
open Buildmir
open Analysis

module FuncSet = Set.Make(Int)
let funcdce_opt (b : builder) (_ : analysis_info) : unit =
  match b.program.main_funcid with
  | None -> ()
  | Some main_funcid -> (

  let marked = ref FuncSet.empty in
  let wl = ref [] in

  let mark (funcid : funcid) = 
    if not @@ FuncSet.mem funcid !marked then
      (marked := FuncSet.add funcid !marked;
      wl := funcid :: !wl)
  in

  let is_marked (funcid : funcid) (_ : func) = 
    FuncSet.mem funcid !marked 
  in

  let wl_pop () = 
    match !wl with
    | h :: tl -> (wl := tl; h)
    | [] -> failwith "funcdce_opt pop called on empty wl"
  in

  let wl_nonempty () = [] <> !wl in

  let op_mark = function
  | Func (_, funcid1_ref, funcid2_opt_ref) -> (
    mark !funcid1_ref;
    match !funcid2_opt_ref with
    | Some funcid2 -> mark funcid2
    | None -> ()
  )
  | CallDirect (_, funcid_ref, _) -> mark !funcid_ref
  | Pack _ | CallClosure _ | Tupuwrp _ | DropGlobal _
  | Copy _ | Drop _ | StoreGlobal _ | LoadGlobal _
  | Immi32 _ | Immi8 _ | ImmUnit _ | Uopi32 _
  | Uopi8 _ | Bopi32 _ | Bopi8 _ | Tupwrp _
  | Veclit _ | Vecinit _ | Vecread _
  | Veclen _ | Vecwrite _ | Vecinsert _
  | Vecextend _ | Vecslice _ -> ()
  in

  (*
    Roots
  *)
  mark main_funcid;
  (match b.program.init_globals_funcid with | Some igfuncid -> mark igfuncid | None -> ());
  (match b.program.uninit_globals_funcid with | Some uigfuncid -> mark uigfuncid | None -> ());

  (*
    Mark Phase
  *)
  while wl_nonempty () do
    let funcid = wl_pop () in
    let func = FuncMap.find funcid b.program.funcs in
    BBMap.iter ( fun _ bb ->
      List.iter op_mark bb.ops
    ) func.bbs
  done;

  (*
    Prune Phase
  *)
  b.program.funcs <- FuncMap.filter is_marked b.program.funcs

  )