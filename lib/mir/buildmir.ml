open Errors
open Mir
open Printmir

(* ========================================================================= *)
(* Builder State Context                                                     *)
(* ========================================================================= *)

type cursor = func option * bb option

type builder = {
  program : program;
  mutable next_funcid : int;
  mutable next_globalid : int;
  mutable cursor : cursor;
}

let create_builder () : builder = {
  program = { 
        globals = GlobalMap.empty; 
        funcs = FuncMap.empty; 
        init_globals_funcid = None; 
        main_funcid = None; 
        uninit_globals_funcid = None };
  next_funcid = 0;
  next_globalid = 0;
  cursor = (None, None);
}

let get_program (b : builder) : program =
  b.program

(* ========================================================================= *)
(* Moving the Cursor                                                         *)
(* ========================================================================= *)

let cp_set (b : builder) : cursor =
  b.cursor

let cp_ret (b : builder) (cp : cursor) : unit =
  b.cursor <- cp

let switch_func (b : builder) (target_fn : func) : unit =
  b.cursor <- (Some target_fn, None)

let switch_bb (b : builder) (target_bb : bb) : unit =
  (*does not check if the bb is in the function !!!!*)
  match b.cursor with
  | (None, _) -> failwith "Builder Error: Cannot switch basic block without an active function!"
  | (Some fn, _) when fn.next_bbid > target_bb.bbid -> b.cursor <- (Some fn, Some target_bb)
  | _ -> failwith "Builder Error: Cannot switch to non-existent basic block!"

(* ========================================================================= *)
(* MIR Helpers (Finding Things)                                              *)
(* ========================================================================= *)

let get_mirtyp (b : builder) (ssaid : ssaid) : mirtyp =
  match b.cursor with
  | (None, _) -> raise (Errors.MirError "No active function in builder")
  | (Some func, _) -> Mir.get_mirtyp_func func ssaid

let get_ownership (b : builder) (ssaid : ssaid) : ownership =
  match b.cursor with
  | (None, _) -> raise (Errors.MirError "No active function in builder")
  | (Some func, _) -> Mir.get_ownership_func func ssaid

let find_func_opt (b : builder) (fid : funcid) : func option =
  FuncMap.find_opt fid b.program.funcs

let find_func (b : builder) (fid : funcid) : func =
  match find_func_opt b fid with
  | Some fn -> fn
  | None -> raise (Errors.MirError (Printf.sprintf "Function with id %d not found" fid))

let find_bb_opt (b : builder) (fid : funcid) (bbid : bbid) : bb option =
  match find_func_opt b fid with
  | None -> None
  | Some fn -> BBMap.find_opt bbid fn.bbs

let find_bb (b : builder) (fid : funcid) (bbid : bbid) : bb =
  match find_bb_opt b fid bbid with
  | Some bb -> bb
  | None -> raise (Errors.MirError (Printf.sprintf "Basic block with id %d not found in function %d" bbid fid))

let find_global_opt (b : builder) (gid : globalid) : global option =
  GlobalMap.find_opt gid b.program.globals

let find_global (b : builder) (gid : globalid) : global =
  match find_global_opt b gid with
  | Some g -> g
  | None -> raise (Errors.MirError (Printf.sprintf "Global with id %d not found" gid))


(* ========================================================================= *)
(* MIR Type Inference                                                        *)
(* ========================================================================= *)

let func_get_clos_mirtyp (b : builder) (fn : func) : mirtyp =
  TMIRClos (List.map (fun (ssaid, _) -> get_mirtyp_func fn ssaid) fn.args, fn.rettyp)

(* 
  given some op, where all used ssaids funcids and globalids are known
  infer the type of the ssaids defined by the op, additionaly checks
  if the op is used right ie. are i32s used for indexs and so on
  In this senses this is the core of a small typechecker for the mir
*)
let infer_mirtyp_from_op (b : builder) (op : op) (mirtyp_hint : mirtyp option) : (ssaid * mirtyp) list =
  match op with
  | Func (ssaid, funcid_ref, funcidopt_ref) -> (
    let ftyp1 = func_get_clos_mirtyp b (find_func b !funcid_ref) in
    match !funcidopt_ref with
    | Some fid2 -> 
        let ftyp2 = func_get_clos_mirtyp b (find_func b fid2) in
        if ftyp1 = ftyp2 then
          [(ssaid, ftyp1)]
        else
          raise (Errors.MirError (Printf.sprintf "Function types do not match for funcid %d and allownedfuncid %d" !funcid_ref fid2))
    | None -> [(ssaid, ftyp1)]
  )
  | Pack (ssaid, clos , args) -> (
      match get_mirtyp b clos.ssaid with
      | TMIRClos (typs_avail, ret) ->
          let typs_clos = List.take (List.length args) typs_avail in
          let typs_args = List.map (fun arg -> get_mirtyp b arg.ssaid) args in
          if List.for_all2 (fun t1 t2 -> t1 = t2) typs_clos typs_args then
            [(ssaid, TMIRClos ( List.drop (List.length args) typs_avail, ret))]
          else
            raise (Errors.MirError (Printf.sprintf "Argument types do not match closure type in Pack operation\n ClosTyp: %s\n ArgsTyps: %s\n"
              (Printmir.string_of_typ (get_mirtyp b clos.ssaid))
              (String.concat ", " (List.map (Printmir.string_of_typ) typs_args))
            ))
      | _ -> raise (Errors.MirError "Expected a closure type for the old closure in Pack operation")
  )
  | CallClosure (ssaid, clos) -> (
      match get_mirtyp b clos.ssaid with
      | TMIRClos ([], ret) -> [(ssaid, ret)]
      | _ -> raise (Errors.MirError "Expected a closure with no remaining arguments for CallClosure operation")
  )
  | CallDirect (ssaid, funcid_ref, args) -> (
      let fn = find_func b !funcid_ref in
      let typs_args = List.map (fun arg -> get_mirtyp b arg.ssaid) args in
      let typs_fn_args = List.map (fun (arg_ssaid,_  ) -> get_mirtyp b arg_ssaid) fn.args in
      if List.for_all2 (fun t1 t2 -> t1 = t2) typs_args typs_fn_args then
        [(ssaid, fn.rettyp)]
      else
        raise (Errors.MirError "Argument types do not match function type in CallDirect operation")
  )
  | Copy (ssaid, a) -> (
        let typ_a = get_mirtyp b a in
        if not (is_memtyp typ_a) then
          raise (Errors.MirError "Copy operation requires the source to be a memory type")
        else
          [(ssaid, typ_a)]
  )
  | Drop gclst -> (
      if
      List.for_all (fun gcssaid -> 
        match get_mirtyp b gcssaid with
        | TMIRUnit | TMIRI32 | TMIRI8 -> false
        | _ -> true
        ) gclst; 
      then
        []
      else
        raise (Errors.MirError "Drop operation requires all arguments to be memory objects")
  )
  | StoreGlobal (gid, loc) -> (
        let global = find_global b gid in
        if get_mirtyp b loc.ssaid = global.typ then
          []
        else
          raise (Errors.MirError "Value type does not match global type in StoreGlobal operation")
  )
  | LoadGlobal (ssaid, gid) -> 
      let global = find_global b gid in
      [(ssaid, global.typ)]
  | Immi32 (ssaid, _) -> [(ssaid, TMIRI32)]
  | Immi8 (ssaid, _) -> [(ssaid, TMIRI8)]
  | ImmUnit (ssaid) -> [(ssaid, TMIRUnit)]
  | Uopi32 (ssaid, _, i) -> (
    match get_mirtyp b i with
    | TMIRI32 -> [(ssaid, TMIRI32)]
    | _ -> raise (Errors.MirError "Expected a 32-bit integer type for the operand in Uopi32 operation")
  )
  | Uopi8 (ssaid, _, c) -> (
    match get_mirtyp b c with
    | TMIRI8 -> [(ssaid, TMIRI8)]
    | _ -> raise (Errors.MirError "Expected an 8-bit integer type for the operand in Uopi8 operation")
  )
  | Bopi32 (ssaid, _, l, r) -> (
    match get_mirtyp b l, get_mirtyp b r with
    | TMIRI32, TMIRI32 -> [(ssaid, TMIRI32)]
    | _ -> raise (Errors.MirError "Expected 32-bit integer types for both operands in Bopi32 operation")
  )
  | Bopi8 (ssaid, op, l, r) -> (
    match get_mirtyp b l, get_mirtyp b r with
    | TMIRI8, TMIRI8 -> (
        match op with
        | Eqi8 | Neqi8 | Lti8 | Gti8 | LtEqi8 | GtEqi8 -> [(ssaid, TMIRI32)]
        | _ -> [(ssaid, TMIRI8)]
    )
    | _ -> raise (Errors.MirError "Expected 8-bit integer types for both operands in Bopi8 operation")
  )
  | Tupwrp (ssaid, elems) -> (
      let elem_typs = List.map (fun elem -> get_mirtyp b elem.ssaid) elems in
      [(ssaid, TMIRTup elem_typs)]
  )
  | Tupuwrp (elms, tup) -> (
    match get_mirtyp b tup.ssaid with
    | TMIRTup elem_typs when List.length elms = List.length elem_typs -> List.map2 (fun ssaid typ -> (ssaid, typ)) elms elem_typs
    | TMIRTup _ -> raise (Errors.MirError "Number of elements to extract does not match the tuple type in Tupuwrp operation")
    | _ -> raise (Errors.MirError "Expected a tuple type for the tuple operand in Tupuwrp operation")
  )
  | Veclit (ssaid, lits) -> (
    match lits with
    | h :: tl -> (
      let inner_type = get_mirtyp b h.ssaid in
      if List.for_all (fun lit -> get_mirtyp b lit.ssaid = inner_type) tl then
        let vec_typ = match inner_type with
          | TMIRI32 -> TMIRVec (1, TMIRVECI32)
          | TMIRI8 -> TMIRVec (1, TMIRVECI8)
          | TMIRVec (dim, datatyp) -> TMIRVec (dim + 1, datatyp)
          | _ -> raise (Errors.MirError "Veclit operation requires all literal elements to be of type vector, i32 or i8")
        in
        (if (Some vec_typ) = mirtyp_hint 
        then [(ssaid, vec_typ)]
        else raise (Errors.MirError "Veclit operation mirtype hint and infered type dont match")  )
      else
        raise (Errors.MirError "Veclit operation requires all literal elements to be of the same type")
    )
    | [] -> (
        match mirtyp_hint with
        | Some hint -> [(ssaid, hint)]
        | None -> raise (Errors.MirError "Veclit operation requires at least one literal element")
    )
  )
  | Vecinit (ssaid, lit, szlst) -> (
    if List.for_all (fun sz -> get_mirtyp b sz = TMIRI32) szlst then
      let dim = List.length szlst in
      let vec_typ = match get_mirtyp b lit with
        | TMIRI32 -> TMIRVec (dim, TMIRVECI32)
        | TMIRI8 -> TMIRVec (dim, TMIRVECI8)
        | TMIRVec (diminner, datatyp) -> TMIRVec (dim + diminner, datatyp)
        | _ -> raise (Errors.MirError "Vecinit operation requires the default value to be of type i32 or i8")
      in
      [(ssaid, vec_typ)]
    else
      raise (Errors.MirError "Vecinit operation requires all size arguments to be of type i32")
  )
  | Veclen (ssaid, vec) -> (
    match get_mirtyp b vec with
    | TMIRVec _ -> [(ssaid, TMIRI32)]
    | _ -> raise (Errors.MirError "Veclen operation requires the vector argument to be of type vector")
  )
  | Vecread (ssaid, vec, idxlst) -> (
    if List.for_all (fun sz -> get_mirtyp b sz = TMIRI32) idxlst then
      let idx_depth = List.length idxlst in
      let vec_typ = 
        match get_mirtyp b vec with
        | TMIRVec (dim, datatyp) when idx_depth = dim -> (match datatyp with | TMIRVECI32 -> TMIRI32 | TMIRVECI8 -> TMIRI8)
        | TMIRVec (dim, datatyp) when idx_depth < dim -> (TMIRVec (dim - idx_depth, datatyp))
        | TMIRVec _ -> raise (Errors.MirError "Vecread operation index depth exceeds vector dimension")
        | _ -> raise (Errors.MirError "Vecread operation requires the vector argument to be of type vector")
      in
      [(ssaid, vec_typ)]
    else
      raise (Errors.MirError "Vecread operation requires all index arguments to be of type i32")
  )
  | Vecwrite (ssaid, vec, ic, idxlst) -> (
    if List.for_all (fun sz -> get_mirtyp b sz = TMIRI32) idxlst then
      let idx_depth = List.length idxlst in
      let vec_typ = 
        match get_mirtyp b vec.ssaid with
        | TMIRVec (dim, datatyp) when idx_depth = dim -> (
          let ic_expected_typ = match datatyp with | TMIRVECI32 -> TMIRI32 | TMIRVECI8 -> TMIRI8 in
          if get_mirtyp b ic = ic_expected_typ then
            TMIRVec (dim, datatyp)
          else
            raise (Errors.MirError "Vecwrite operation value type does not match vector inner type")
        )
        | TMIRVec _ -> raise (Errors.MirError "Vecwrite operation index depth does not match vector dimension")
        | _ -> raise (Errors.MirError "Vecwrite operation requires the vector argument to be of type vector")
      in
      [(ssaid, vec_typ)]
    else
      raise (Errors.MirError "Vecwrite operation requires all index arguments to be of type i32")
  )
  | Vecinsert (ssaid, vec, vecins, idxlst) -> (
    if List.for_all (fun sz -> get_mirtyp b sz = TMIRI32) idxlst then
      let idx_depth = List.length idxlst in
      let vec_typ = (
        match get_mirtyp b vec.ssaid, get_mirtyp b vecins.ssaid with
        | TMIRVec (dim_vec, datatyp_vec) , TMIRVec (dim_ins, datatyp_ins) 
            when idx_depth + dim_ins = dim_vec && datatyp_vec = datatyp_ins -> 
            TMIRVec (dim_vec, datatyp_vec)
        | TMIRVec _ , TMIRVec _ -> raise (Errors.MirError "Vecinsert operation inner values dont match or idx depth + vecins dimension does not match vector dimension")
        | _, _ -> raise (Errors.MirError "Vecinsert operation requires vec and vecins arguments to be of type vector")
      )
      in
      [(ssaid, vec_typ)]
    else
      raise (Errors.MirError "Vecinsert operation requires all index arguments to be of type i32")
  )
  | Vecslice (ssaid, vec, start, len) -> (
      match get_mirtyp b vec, get_mirtyp b start, get_mirtyp b len with
      | TMIRVec (dim, datatyp), TMIRI32, TMIRI32 -> [ (ssaid, TMIRVec (dim, datatyp)) ]
      | _, _, _ -> raise (Errors.MirError "Vecslice operation requires the vec argument to be of type vector and start and len arguments to be of type i32")    
  )
  | Vecextend (ssaid, vec, lit, extsz) -> (
      match get_mirtyp b vec, get_mirtyp b extsz with
      | TMIRVec (dim, datatyp), TMIRI32 -> (
        let lit_typ = get_mirtyp b lit in
        if dim = 1 then
          match datatyp, lit_typ with
          | TMIRVECI32, TMIRI32 
          | TMIRVECI8, TMIRI8 -> [(ssaid, TMIRVec (dim, datatyp))]
          | _ -> raise (Errors.MirError "Vecextend operation requires the lit argument to match the inner type of the vec argument")
        else
          match lit_typ with
          | TMIRVec (dim_lit, datatyp_lit) when dim_lit + 1 = dim && datatyp_lit = datatyp -> [(ssaid, TMIRVec (dim, datatyp))]
          | _ -> raise (Errors.MirError "Vecextend operation requires the lit argument to be a vector of the same inner type as the vec argument and of dimension one less than the vec argument")
      )
      | _ -> raise (Errors.MirError "Vecextend operation requires the vec argument to be of type vector and extsz arguments to be of type i32")    
  )

(*
  given some op, where all used ssaids funcids and globalids are known
  infer the ownership of the ssaids defined by the op
*)
let infer_ownership_from_op (b : builder) (op : op) : (ssaid * ownership) list =
  match op with
  | Func (ssaid, _, _) -> [(ssaid, Owned)]
  | Pack (ssaid, _, _) -> [(ssaid, Owned)]
  | CallClosure (ssaid, _) -> [(ssaid, Owned)]
  | CallDirect (ssaid, _, _) -> [(ssaid, Owned)]
  | Copy (ssaid, _) -> [(ssaid, Owned)]
  | Drop _ -> []
  | StoreGlobal _ -> []
  | LoadGlobal (ssaid, _) -> [(ssaid, Borrowed)]
  | Immi32 (ssaid, _) -> [(ssaid, NoMem)]
  | Immi8 (ssaid, _) -> [(ssaid, NoMem)]
  | ImmUnit ssaid -> [(ssaid, NoMem)]
  | Uopi32 (ssaid, _, _) -> [(ssaid, NoMem)]
  | Uopi8 (ssaid, _, _) -> [(ssaid, NoMem)]
  | Bopi32 (ssaid, _, _, _) -> [(ssaid, NoMem)]
  | Bopi8 (ssaid, _, _, _) -> [(ssaid, NoMem)]
  | Tupwrp (ssaid, _) -> [(ssaid, Owned)]
  (* Tup extract and view triggers a redundant call on the type inference function but I feel to not enforce an order in which
     infer_ownership_from_op and infer_mirtyp_from_op are called is worth it*)
  | Tupuwrp (elms , tup) -> (
    let elm_own = if tup.consume then Owned else Borrowed in
    List.map (fun (ssaid, mirtyp) ->
              match mirtyp with
              | TMIRUnit | TMIRI32 | TMIRI8 -> (ssaid, NoMem)
              | _ -> (ssaid, elm_own)
              ) (infer_mirtyp_from_op b op None)
  )
  | Veclit (ssaid, _) -> [(ssaid, Owned)]
  | Vecinit (ssaid, _, _) -> [(ssaid, Owned)]
  | Veclen (ssaid, _) -> [(ssaid, NoMem)]
  | Vecread (ssaid, _, _) -> (
      match (infer_mirtyp_from_op b op None) with
      | [(ssaid, mirtyp)] -> (
        if is_memtyp mirtyp then
          [(ssaid, Borrowed)]
        else
          [(ssaid, NoMem)]
      )
      | _ -> raise (Errors.MirError "Vecread operation should infer exactly one type for the result SSA ID")
  )
  | Vecwrite (ssaid, _, _, _) -> [(ssaid, Owned)]
  | Vecinsert (ssaid, _, _, _) -> [(ssaid, Owned)]
  | Vecslice (ssaid, _, _, _) -> [(ssaid, Borrowed)]
  | Vecextend (ssaid, _, _, _) -> [(ssaid, Owned)]

(*
let check_branch (b : builder) (branch : branch) : unit =
  match b.cursor with
  | (Some fn, _) -> (
      match BBMap.find_opt branch.bbid fn.bbs with
      | None -> raise (Errors.MirError "Branch target basic block not found in current function")
      | Some target_bb -> (
          if List.length branch.args <> List.length target_bb.args then
            raise (Errors.MirError "Branch argument count does not match target basic block argument count")
          else
            List.iter2 (fun arg_ssa target_arg_ssa -> 
              if get_mirtyp b arg_ssa.ssaid <> get_mirtyp b target_arg_ssa then
                raise (Errors.MirError "Branch argument type does not match target basic block argument type")
            ) branch.args target_bb.args
      )
  )
  | _ -> raise (Errors.MirError "No active function in builder")

let check_term (b : builder) (term : term) : unit =
  match term with
  | Br branch -> check_branch b branch
  | Cbr (cond_ssaid, true_branch, false_branch) -> (
      if get_mirtyp b cond_ssaid <> TMIRI32 then
        raise (Errors.MirError "Condition SSA ID for conditional branch must be of type i32");
      check_branch b true_branch;
      check_branch b false_branch
  )
  | Ret ret_ssa -> (
      match b.cursor with
      | (Some fn, _) -> (
          if get_mirtyp b ret_ssa.ssaid <> fn.rettyp then
            raise (Errors.MirError "Return SSA ID type does not match function return type"))
      | _ -> raise (Errors.MirError "No active function in builder")
  )*)

(* ========================================================================= *)
(* Create Functions & Basic Blocks & Globals                                 *)
(* ========================================================================= *)

let create_func (b : builder) 
                (name : string)
                (args_w_mirtyp : (ssaid * (string option) * mirtyp) list)
                (rettyp : mirtyp)
                (extern_name : string option)
                : func =
  let fid = b.next_funcid in
  b.next_funcid <- b.next_funcid + 1;
  let args = List.map (fun (arg_ssaid, arg_name, _) -> (arg_ssaid, arg_name)) args_w_mirtyp in
  let max_ssaid = List.fold_left (fun acc (arg_ssaid, _, _) -> max acc arg_ssaid) 0 args_w_mirtyp in
  let mirtyps = Dynarray.make (max_ssaid + 1) TMIRUnit in
  let memowns = Dynarray.make (max_ssaid + 1) NoMem in
  List.iter (fun (arg_ssaid, _, mirtyp) -> 
                Dynarray.set mirtyps arg_ssaid mirtyp;
                match mirtyp with
                | TMIRUnit | TMIRI32 | TMIRI8 -> ()
                | _ -> Dynarray.set memowns arg_ssaid Borrowed
            ) args_w_mirtyp;
  let fn = {funcid = fid;
            name = name;
            args = args;
            rettyp = rettyp;
            extern_name = extern_name;
            next_ssaid = max_ssaid + 1;
            next_bbid = 0;
            entry_bb = None;
            bbs = BBMap.empty;
            ssatyps = mirtyps;
            memown = memowns;
            } in
  let p = b.program in
  p.funcs <- FuncMap.add fid fn p.funcs;
  fn

let delete_func (b : builder) (func : func) : unit =
  b.program.funcs <- FuncMap.remove func.funcid b.program.funcs

let create_bb (b : builder) 
              (name : string) 
              (args : (ssaid * mirtyp) list) : bb =
  match b.cursor with
  | (None, _) -> failwith "Builder Error: Cannot create basic block without an active function!"
  | (Some fn, _) ->
      let id = fn.next_bbid in
      fn.next_bbid <- fn.next_bbid + 1;
      List.iter (fun (arg_ssaid, mirtyp) -> 
        match mirtyp with
        | TMIRUnit | TMIRI32 | TMIRI8 -> set_mirtyp_ownership_func fn arg_ssaid mirtyp NoMem
        | _ -> set_mirtyp_ownership_func fn arg_ssaid mirtyp Owned (*hmmm have to think about this*)
      ) args;
      let new_bb = { bbid = id; 
                     name; 
                     args = List.map fst args; 
                     ops = []; 
                     term = None } in
      fn.bbs <- BBMap.add id new_bb fn.bbs;
      new_bb

let set_entry_bb (b : builder) (bbid : bbid) : unit =
  match b.cursor with
  | (None, _) -> failwith "Builder Error: Cannot set entry basic block without an active function!"
  | (Some fn, _) -> (
    match BBMap.find_opt bbid fn.bbs with
    | None -> failwith (Printf.sprintf "Builder Error: Basic block with id %d not found in function %s" bbid fn.name)
    | Some _ -> fn.entry_bb <- Some bbid
  )

let create_global (b : builder) (typ : mirtyp) : global =
  let globalid = b.next_globalid in
  b.next_globalid <- b.next_globalid + 1;
  let global = { globalid; typ } in
  b.program.globals <- GlobalMap.add globalid global b.program.globals;
  global

(* ========================================================================= *)
(* Copy Things                                                               *)
(* ========================================================================= *)

let copy_ssaconsume (sc : ssaconsume) = 
  { ssaid = sc.ssaid; consume = sc.consume }

let rec copy_op (o : op) : op =
  match o with
  | Func (res, funcid1, funcid2_opt) -> Func (res, ref !funcid1, ref !funcid2_opt)
  | Pack (res, sc, scs) -> Pack (res, copy_ssaconsume sc, List.map copy_ssaconsume scs)
  | CallClosure (res, sc) -> CallClosure (res, copy_ssaconsume sc)
  | CallDirect (res, fid_ref, scs) -> CallDirect (res, ref !fid_ref, List.map copy_ssaconsume scs)
  | StoreGlobal (gid, sc) -> StoreGlobal (gid, copy_ssaconsume sc)
  | Tupwrp (res, scs) -> Tupwrp (res, List.map copy_ssaconsume scs)
  | Tupuwrp (res, sc) -> Tupuwrp (res, copy_ssaconsume sc)
  | Veclit (res, scs) -> Veclit (res, List.map copy_ssaconsume scs)
  | Vecwrite (res, val_sc, vec, idxs) -> Vecwrite (res, copy_ssaconsume val_sc, vec, idxs)
  | Vecinsert (res, vec_sc, ins_sc, idxs) -> Vecinsert (res, copy_ssaconsume vec_sc, copy_ssaconsume ins_sc, idxs)
  | Copy _ | Drop _ | LoadGlobal _
  | Immi32 _ | Immi8 _ | ImmUnit _ | Uopi32 _
  | Uopi8 _ | Bopi32 _ | Bopi8 _ 
  | Vecinit _ 
  | Veclen _ | Vecread _ 
  | Vecslice _ | Vecextend _ -> o

let copy_term (t : term option) : term option =
  match t with
  | None -> None
  | Some (Br (brbbid, brargs)) -> Some (Br (brbbid, List.map copy_ssaconsume brargs))
  | Some (Cbr _) -> t
  | Some (Ret _) -> t

let copy_bb (b : bb) : bb =
  {
    bbid = b.bbid;
    name = b.name;
    args = b.args;
    ops = List.map copy_op b.ops;
    term = copy_term b.term;
  }

let copy_func (b : builder) (fid : funcid) : func =
  let new_funcid = b.next_funcid in
  b.next_funcid <- b.next_funcid + 1;
  let fn = find_func b fid in
  let newfn = {
    funcid = new_funcid;
    name = fn.name ^ "_copy" ^ string_of_int new_funcid;
    args = fn.args;
    rettyp = fn.rettyp;
    extern_name = fn.extern_name;
    next_ssaid = fn.next_ssaid;
    next_bbid = fn.next_bbid;
    entry_bb = fn.entry_bb;
    bbs = BBMap.map copy_bb fn.bbs; (* Deep copy of basic blocks *)
    ssatyps = Dynarray.copy fn.ssatyps; (* Copy the type array *)
    memown = Dynarray.copy fn.memown; (* Copy the ownership array *)
  } in
  b.program.funcs <- FuncMap.add new_funcid newfn b.program.funcs;
  newfn


(* ========================================================================= *)
(* Emitting Instructions & Terminators                                       *)
(* ========================================================================= *)

let emit_op_hint (b : builder) (op : op) (mirtyp_hint : mirtyp option) : unit =
  match b.cursor with
  | (Some fn, Some bb) -> (
    let mirtyp_defs = infer_mirtyp_from_op b op mirtyp_hint in
    let ownership_defs = infer_ownership_from_op b op in
    List.iter2 (fun (ssaid_typ, typ) (ssaid_own, own) -> 
      assert (ssaid_typ = ssaid_own);  (* Ensure the SSA IDs match *)
      set_mirtyp_ownership_func fn ssaid_typ typ own
    ) mirtyp_defs ownership_defs;
    bb.ops <- op :: bb.ops
  )
  | (None, _) -> failwith "Builder Error: Cannot emit op without an active function!"
  | (_, None) -> failwith "Builder Error: Cannot emit op without an active basic block!"

let emit_op (b : builder) (op : op) : unit =
  emit_op_hint b op None


let emit_term (b : builder) (term : term) : unit =
  match b.cursor with
  | (_, None) -> failwith "Builder Error: Cannot emit terminator without an active basic block!"
  | (_, Some bb) ->
      match bb.term with
      | Some _ -> failwith (Printf.sprintf "Builder Error: Basic block '%s' already has a terminator!" bb.name)
      | None -> (*check_term b term;*) bb.term <- Some term

(* ========================================================================= *)
(* Generating SSA IDs                                                        *)
(* ========================================================================= *)

let fresh_ssaid (b : builder) : ssaid =
  match b.cursor with
  | (None, _) -> failwith "Builder Error: Cannot generate SSA ID without an active function!"
  | (Some fn, _) ->
      let id = fn.next_ssaid in
      fn.next_ssaid <- fn.next_ssaid + 1;
      Dynarray.add_last fn.ssatyps TMIRUnit;
      Dynarray.add_last fn.memown NoMem;
      if fn.next_ssaid <> Dynarray.length fn.ssatyps || fn.next_ssaid <> Dynarray.length fn.memown then
        raise (Errors.MirError "Internal Error: SSA ID counter is out of sync with type and ownership arrays");
      id

(* ========================================================================= *)
(* Other Helpers                                                             *)
(* ========================================================================= *)

  let sub_id submap id = 
    match List.assoc_opt id submap with
    | Some new_id -> new_id
    | None -> id 
  
  let sub_id_list submap ids = List.map (sub_id submap) ids 

  let sub_sc submap sc = 
    { ssaid = sub_id submap sc.ssaid; consume = sc.consume } 
  
  let sub_sc_list submap scs = List.map (sub_sc submap) scs 

let sub_ops_uses submap ops =

  (*make helpers less redundant*)
  let sub_id = sub_id submap in
  let sub_id_list = sub_id_list submap in
  let sub_sc = sub_sc submap in
  let sub_sc_list = sub_sc_list submap in

  List.map (fun op ->
    match op with
    | Func _ 
    | LoadGlobal _ 
    | Immi32 _ 
    | Immi8 _ 
    | ImmUnit _ -> op
    
    | Pack (dst, oldclos, args) -> 
        Pack (dst, sub_sc oldclos, sub_sc_list args)
    
    | CallClosure (dst, clos) -> 
        CallClosure (dst, sub_sc clos)
    
    | CallDirect (dst, fid, args) -> 
        CallDirect (dst, fid, sub_sc_list args)

    | Copy (dst, a) -> 
        Copy (dst, sub_id a)

    | Drop mems -> 
        Drop (sub_id_list mems)  (* Only uses! *)
    
    | StoreGlobal (gid, v) -> 
        StoreGlobal (gid, sub_sc v)
    
    | Uopi32 (dst, uop, a) -> 
        Uopi32 (dst, uop, sub_id a)
    
    | Uopi8 (dst, uop, a) -> 
        Uopi8 (dst, uop, sub_id a)
    
    | Bopi32 (dst, bop, a, b) -> 
        Bopi32 (dst, bop, sub_id a, sub_id b)
    
    | Bopi8 (dst, bop, a, b) -> 
        Bopi8 (dst, bop, sub_id a, sub_id b)
    
    | Tupwrp (dst, elms) -> 
        Tupwrp (dst, sub_sc_list elms)
    
    | Tupuwrp (elms, tup) -> 
        Tupuwrp (elms, sub_sc tup)
    
    | Veclit (dst, elms) -> 
        Veclit (dst, sub_sc_list elms)
    
    | Vecinit (dst, defval, dims) -> 
        Vecinit (dst, sub_id defval, sub_id_list dims)
    
    | Veclen (dst, vec) -> 
        Veclen (dst, sub_id vec)
    
    | Vecread (dst, vec, idxs) -> 
        Vecread (dst, sub_id vec, sub_id_list idxs)
    
    | Vecwrite (dst, vec, v, idxs) -> 
        Vecwrite (dst, sub_sc vec, sub_id v, sub_id_list idxs)
    
    | Vecinsert (dst, vec, vecins, idxs) -> 
        Vecinsert (dst, sub_sc vec, sub_sc vecins, sub_id_list idxs)
    
    | Vecslice (dst, vec, start, len) -> 
        Vecslice (dst, sub_id vec, sub_id start, sub_id len)
    
    | Vecextend (dst, vec, lit, off) -> 
        Vecextend (dst, sub_id vec, sub_id lit, sub_id off)
  ) ops