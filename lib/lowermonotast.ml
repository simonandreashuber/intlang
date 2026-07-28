open Ast
open Mir
open Errors

let convert_uopi32 (op : Ast.uopi32) : Mir.uopi32arg =
  match op with
  | Ast.Negi32 -> Mir.Negi32
  | Ast.Noti32 -> Mir.Noti32

let convert_bopi32 (op : Ast.bopi32) : Mir.bopi32arg =
  match op with
  | Ast.Eqi32 -> Mir.Eqi32
  | Ast.Neqi32 -> Mir.Neqi32
  | Ast.Lti32 -> Mir.Lti32
  | Ast.Gti32 -> Mir.Gti32
  | Ast.LtEqi32 -> Mir.LtEqi32
  | Ast.GtEqi32 -> Mir.GtEqi32
  | Ast.ULti32 -> Mir.ULti32
  | Ast.UGti32 -> Mir.UGti32
  | Ast.ULtEqi32 -> Mir.ULtEqi32
  | Ast.UGtEqi32 -> Mir.UGtEqi32
  | Ast.Muli32 -> Mir.Muli32
  | Ast.Subi32 -> Mir.Subi32
  | Ast.Addi32 -> Mir.Addi32
  | Ast.Divi32 -> Mir.Divi32
  | Ast.Modi32 -> Mir.Modi32
  | Ast.UDivi32 -> Mir.UDivi32
  | Ast.UModi32 -> Mir.UModi32
  | Ast.Andi32 -> Mir.Andi32
  | Ast.Ori32 -> Mir.Ori32
  | Ast.Xori32 -> Mir.Xori32
  | Ast.Shli32 -> Mir.Shli32
  | Ast.Shri32 -> Mir.Shri32
  | Ast.UShri32 -> Mir.UShri32

let res_mirtyp_of_bopi8 (op : Ast.bopi8) : Mir.typmir =
  match op with
    | Ast.Eqi8 | Ast.Neqi8 | Ast.Lti8 | Ast.Gti8 | Ast.LtEqi8 | Ast.GtEqi8 -> TMIRI32
    | _ -> TMIRI8


let convert_uopi8 (op : Ast.uopi8) : Mir.uopi8arg =
  match op with
  | Ast.Negi8 -> Mir.Negi8
  | Ast.Noti8 -> Mir.Noti8

let convert_bopi8 (op : Ast.bopi8) : Mir.bopi8arg =
  match op with
  | Ast.Eqi8 -> Mir.Eqi8
  | Ast.Neqi8 -> Mir.Neqi8
  | Ast.Lti8 -> Mir.Lti8
  | Ast.Gti8 -> Mir.Gti8
  | Ast.LtEqi8 -> Mir.LtEqi8
  | Ast.GtEqi8 -> Mir.GtEqi8
  | Ast.Addi8 -> Mir.Addi8
  | Ast.Subi8 -> Mir.Subi8
  | Ast.Andi8 -> Mir.Andi8
  | Ast.Ori8 -> Mir.Ori8
  | Ast.Xori8 -> Mir.Xori8

let trunc_n_mirfunctyp (n : int) (typ : typmir) : typmir =
  match typ with
  | TMIRFun (args, ret) -> TMIRFun (List.filteri (fun j _ -> j >= n) args, ret)
  | _ -> raise (Errors.LowerMonoTASTError "Expected function type in trunc_n_mirfunctyp")

let trunc_n_mirfunctyp = trunc_n_mirfunctyp 1

let rec ast_type_to_mir_type (typ_ast : Ast.typ) : Mir.mir =

  (* create fully saturated version *)
  let rec funcaux argacc arg_typ ret_typ =
    let arg_typmir = ast_type_to_mir_type arg_typ in
    match repr ret_typ with
    | TFun (arg_typ_inner, ret_typ_inner) ->  (funcaux (arg_typmir :: argacc) arg_typ_inner ret_typ_inner)
    | _ -> TMIRFun (List.rev (arg_typmir :: argacc), ast_type_to_mir_type ret_typ)
  in

  (* find the compressed version for the restricted vectors *)
  let rec vecaux typ_inner depth =
    match repr typ_inner with
    | TVec typ_inner_inner -> (vecaux typ_inner_inner (depth + 1))
    | TI32 -> TMIRVec (depth, TMIRVECI32)
    | TI8 -> TMIRVec (depth, TMIRVECI8)
    | _ -> raise (Errors.LowerMonoTASTError "Vector that is not of i32 or i8, veccheck seems to fail")
  in

  match repr typ_ast with
  | TUnit -> TMIRUnit
  | TI32 -> TMIRI32
  | TI8 -> TMIRI8
  | TFun (arg_typ, ret_typ) -> funcaux [] arg_typ ret_typ
  | TTup typ_list -> TMIRTup (List.map ast_type_to_mir_type typ_list)
  | TVec typ_inner -> vecaux typ_inner 1
  | Tvar _ -> raise (Errors.LowerMonoTASTError "Polymorphic Type in MonoTAST, Monomorphizer seems to fail")

module UuidSet = Set.Make(struct type t = int let compare = compare end)
module UuidMap = Map.Make(struct type t = int let compare = compare end)

let find_capt_vars (bound : UuidSet.t) (l : tlexp) : (string * uuid * Ast.typ) list =
  let rec walk (bound : UuidSet.t) (acc : UuidMap.t) (expr : tlexp) =
    match expr with
    | VarT (name_ref, uuid_ref, typ) ->
        let u = !uuid_ref in
        if UuidSet.mem u bound then acc
        else UuidMap.add u (!name_ref, typ) acc
    | LamT (n, u, body, _) ->
        let bound' = UuidSet.add u bound in
        walk bound' acc body
    | LamUnitT (body, _) ->
        walk bound acc body
    | AppT (e1, e2, _)
    | SeqT (e1, e2, _) ->
        let acc1 = walk bound acc e1 in
        walk bound acc1 e2
    | IfT (cond, t_branch, e_branch, _) ->
        let acc1 = walk bound acc cond in
        let acc2 = walk bound acc1 t_branch in
        walk bound acc2 e_branch
    | LetinT (n, u, e, body, _) ->
        let acc1 = walk bound acc e in
        let bound' = UuidSet.add u bound in
        walk bound' acc1 body
    | LetrecinT (n, u, e, body, _) ->
        let bound' = UuidSet.add u bound in
        let acc1 = walk bound' acc e in
        walk bound' acc1 body
    | LetinTupleT (binders, e, body, _) ->
        let acc1 = walk bound acc e in
        let bound' = 
          List.fold_left (fun bd -> function
            | Some (n, u) -> UuidSet.add u bd
            | None -> bd
          ) bound binders 
        in
        walk bound' acc1 body
    | TupleT (el, _) ->
        List.fold_left (walk bound) acc el
    | I32LitT _ | I8LitT _ | UnitLitT _ -> 
        acc
    | UopI32T (_, e, _) | UopI8T (_, e, _) | VeclenT (e, _) ->
        walk bound acc e
    | BopI32T (_, e1, e2, _) | BopI8T (_, e1, e2, _) ->
        let acc1 = walk bound acc e1 in
        walk bound acc1 e2
    | VecLitT (el, _) ->
        List.fold_left (walk bound) acc el
    | VecmkT (defval, sizes, _) ->
        let acc1 = walk bound acc defval in
        List.fold_left (walk bound) acc1 sizes
    | VecgetT (vec, idxs, _) ->
        let acc1 = walk bound acc vec in
        List.fold_left (walk bound) acc1 idxs
    | VecsetT (vec, v, idxs, _) ->
        let acc1 = walk bound acc vec in
        let acc2 = walk bound acc1 v in
        List.fold_left (walk bound) acc2 idxs
    | VecsliceT (vec, start, len, _) ->
        let acc1 = walk bound acc vec in
        let acc2 = walk bound acc1 start in
        walk bound acc2 len
    | VecextendT (vec, lit, off, _) ->
        let acc1 = walk bound acc vec in
        let acc2 = walk bound acc1 lit in
        walk bound acc2 off
  in
  let captured_map = walk bound UuidMap.empty l in
  UuidMap.fold (fun u (n, typ) lst -> (n, Some u, typ) :: lst) captured_map []

type mirval = 
  | MIRSsaid of ssaid
  | MIRFuncid of funcid

let env_put (env : UuidMap.t) (uuid : uuid) (mirval : mirval) : UuidMap.t =
  UuidMap.add !uuid mirval env

let env_get (env : UuidMap.t) (uuid : uuid) : mirval option =
  UuidMap.find_opt !uuid env

let eta_expansion (b : Mir.builder) (unsat_ssaid : ssaid) : ssaid =
  (*idea if dublication observed in real code: creat a cache with eta func signatures for reuse*)
  
  let unsat_arg_ssaid = fresh_ssaid b in
  let unsat_mirtyp = find_mirtyp b unsat_ssaid in
  let unsat_func_arg = (fresh_ssaid b, unsat_mirtyp) in
  let fresh_ssaid_for_args args = List.map (fun (_, argtyp) -> (fresh_ssaid b, argtyp)) args in
  (*extract all args and return type of the fully saturated version, the inner list indicates the points where calls are needed*)
  let rec extract_sat_args (acc : ((ssaid * typmir) list) list) (ftyp : mirtyp) : (((ssaid * typmir) list) list) * typmir =
    match ftyp with
    | TMIRFun (args, rettyp) -> extract_ret_args ((fresh_ssaid_for_args args) :: acc) rettyp
    | _ -> (List.rev acc, ftyp)
  in
  let sat_args_lstlst, sat_rettyp = extract_ret_args [] unsat_mirtyp in

  let cp = funcbb_checkpoint b in

  let eta_func = create_func b (Some ("eta_expansion_for_ssaid_" ^ string_of_int ssaid)) sat_rettyp ((unsat_arg_ssaid, unsat_mirtyp) :: (List.flatten sat_args_lstlst)) in
  switch_to_func b func;
  let bbentry = create_bb b "entry" in
  switch_to_bb b bbentry;
  let res_ssaid = List.fold_left ( fun closure_ssaid args_lst ->
      let closure_mirtyp = find_mirtyp b closure_ssaid in
      match closure_mirtyp with
      | TMIRFun (_, ret_mirtyp) -> (
        let pack_ssaid = fresh_ssaid b in
        emit_op b ( Pack (pack_ssaid, TMIR ([], ret_mirtyp), closure_ssaid, List.map (fst) args_lst ) );
        let res_ssaid = fresh_ssaid b in
        emit_op b ( CallClosure (res_ssaid, ret_mirtyp, pack_ssaid) );
        res_ssaid
      )
      | _ -> raise (Errors.LowerMonoTASTError "Eta expansion some precomputaion list of list problem")
    ) unsat_arg_ssaid sat_args_lstlst in
  emit_term b res_ssaid;

  funcbb_restore b cp;
  
  let eta_rawfunc_ssaid = fresh_ssaid b in
  let eta_rawfunc_mirtyp = func_get_mirtyp eta_func.funcid in
  emit_op b ( Func (eta_rawfunc_ssaid, eta_rawfunc_mirtyp, eta_func.funcid) );
  let eta_func_ssaid = fresh_ssaid b in
  emit_op b ( Pack (eta_func_ssaid, trunc_n_mirfunctyp 1 eta_rawfunc_mirtyp, eta_rawfunc_ssaid, [unsat_ssaid]) );
  eta_func_ssaid

  
let func_to_closure (b : Mir.builder) (env : UuidMap.t) (func : func) (cap_uuids : uuid list) : ssaid =
    let func_ssaid = fresh_ssaid b in
    let func_mirtyp = func_get_mirtyp b func.funcid in
    emit_op b (Func (func_ssaid, func_mirtyp, func.funcid));
    if cap_uuids = [] then
      func_ssaid
    else (
      let closure_ssaid = fresh_ssaid b in
      let closure_mirtyp = trunc_n_mirfunctyp (List.length cap_uuids) func_mirtyp in
      let pack_ssaids = List.map (fun uuid -> match UuidMap.find_opt !uuid env with
        | Some (MIRSsaid ssaid) -> ssaid
        | Some (MIRFuncid _) -> raise (Errors.LowerMonoTASTError "passing function with no captured vars as a captured var, this should not happen as this function does not need to be caputured")
        | None -> raise (Errors.LowerMonoTASTError "captured variables ssaid not found in environment, or mb some weird thing with unit")
      ) cap_uuids in
      emit_op b (Pack (closure_ssaid, closure_mirtyp, func_ssaid, pack_ssaids));
      closure_ssaid
    )



(* Lowers a non delayed expression, returns the ssaid with the expression result *)
let rec lower_body (b : Mir.builder) (env : UuidMap.t) (l : tlexp) : ssaid =
  match l with
  | VarT (_, uuid, _) -> (
    match env_get env !uuid with
    | Some (MIRSsaid ssaid) -> ssaid
    | Some (MIRFuncid funcid) -> 
        (*function id in the env means this is a function that is 
          compiletime known and does not does not have any lambda lifted catpured vars
          so we can just instanciate it directly*)
        let ssaid = fresh_ssaid b in
        let func_mirtyp = func_retmirtyp b funcid in
        let op = Func (ssaid, func_mirtyp, funcid) in
        emit_op b op;
        ssaid
    | None -> raise (Errors.LowerMonoTASTError "Variable not found in environment")
  )
  | LamT _ 
  | LamUnitT _ -> (
    (*anonymous lambda*)
    let func, cap_uuids = lower_func b env None l in
    func_to_closure b env func cap_uuids
  )
  | AppT (e1, e2, _) -> (
    let ssaid_func = lower_body b env e1 in
    let func_mirtyp = find_mirtyp b ssaid_func in
    let arg_ssaid = lower_body b env e2 in
    let arg_mirtyp = find_mirtyp b arg_ssaid in
    let sat_arg_ssaid = 
      match arg_mirtyp with
      | TMIRFun (_, TMIRFun _) -> 
          (*Functions that return functions are not fully saturated, but all functions
            are lowered such that arguments are expected to have function is fully saturated form.
            Hence I put a wrapper around the unsatureted functions or in other words an eta expansion.*)
          eta_expansion b arg_ssaid
      | _ -> arg_ssaid
    in
    let pack_ssaid = fresh_ssaid b in
    let pack_mirtyp = trunc_n_mirfunctyp 1 func_mirtyp in
    emit_op b (Pack (pack_ssaid, pack_mirtyp, [sat_arg_ssaid]));
    match pack_mirtyp with
    | TMIRFun ( [], ret_mirtyp) -> (
        let res_ssaid = fresh_ssaid b in
        emit_op b (CallClosure (res_ssaid, ret_mirtyp, pack_ssaid));
        res_ssaid
      )
    | TMIRFun ( _, _) -> pack_ssaid
    | _ -> raise (Errors.LowerMonoTASTError "Expected function type after packing in AppT lowering")
  )
  | SeqT (e1, e2, _) -> (
    let _ = lower_body b env e1 in
    lower_body b env e2
  )
  | IfT (cond, t_branch, e_branch, _) -> (
    let bb_then = create_bb b "then" [] in
    let bb_else = create_bb b "else" [] in
    let ssaid_cond = lower_body b env cond in
    let cbr_term = Cbr (ssaid_cond, (bb_then.bbid, []), (bb_else.bbid, [])) in
    emit_term b cbr_term;
    switch_bb b bb_then;
    let ssaid_then = lower_body b env t_branch in
    let res_mirtyp = find_mirtyp b ssaid_then in
    let merge_res_ssaid = fresh_ssaid b in
    let bb_merge = create_bb b "merge" [(merge_res_ssaid, res_mirtyp)] in
    let ifbb_term = Br (bb_merge.bbid, [ssaid_then]) in
    emit_term b ifbb_term;
    switch_bb b bb_else;
    let ssaid_else = lower_body b env e_branch in
    let elsebb_term = Br (bb_merge.bbid, [ssaid_else]) in
    emit_term b elsebb_term;
    switch_bb b bb_merge;
    merge_res_ssaid
  )
  | LetinT (n, u, e, body, _) -> (
      match body with
      | LamT _ | LamUnitT _ -> (
          let func, cap_uuids = lower_func b env (Some n) None e in
          let env' = 
            (*this allows local helpers that dont lamlift to be used 
              directly when captured somewhere else and not passed as a lamlifted arg*)
            if cap_uuids = [] then (
              env_put env u (MIRFuncid func.funcid)
            ) else (
              let closure_ssaid = func_to_closure b env func cap_uuids in
              env_put env u (MIRSsaid closure_ssaid) in
            )
          in
          lower_body b env' e
        )
      | _ -> (
        let ssaid_e = lower_body b env e in
        let env' = env_put env u (MIRSsaid ssaid_e) in
        lower_body b env' body
        ) 
    )
  | LetrecinT (n, u, e, body, _) -> (
      let func, cap_uuids = 
        match e with
        | LamT _ | LamUnitT _ -> lower_func b env (Some n) (Some u) e 
        | _ -> raise (Errors.LowerMonoTASTError "Letrec bound to non-lambda expression")
      in
      let env' = 
        (*this allows local helpers that dont lamlift to be used 
          directly when captured somewhere else and not passed as a lamlifted arg*)
        if cap_uuids = [] then (
          env_put env u (MIRFuncid func.funcid)
        ) else (
          let closure_ssaid = func_to_closure b env func cap_uuids in
          env_put env u (MIRSsaid closure_ssaid) in
        )
      in
      lower_body b env' e
    )
  | LetinTupleT (elms, e, body, _) -> (
    let tup_ssaid = lower_body b env e in
    let tup_mirtyplst = 
      match find_mirtyp b tup_ssaid with 
      | TMIRTup mirtyplst -> mirtyplst
      | _ -> raise (Errors.LowerMonoTASTError "Expected Tuple Mir type in LetinTuple lowering")
    in
    let idx = ref 0 in
    let env' = List.fold_left2 (fun env_acc elm_opt elm_mirtyp ->
        match elm_opt with
        | Some (name, uuid) -> (
          let elm_ssaid = fresh_ssaid b in
          emit_op b (Tupget (elm_ssaid, elm_mirtyp, tup_ssaid, !idx));
          idx := !idx + 1;
          env_put env_acc uuid (MIRSsaid elm_ssaid)
        )
        | None -> (idx := !idx + 1; env_acc)
        ) env elms tup_mirtyplst in
    lower_body b env' body
  )
  | TupleT (explst, _) -> (
      let ssaid_lst = List.map 
        (fun elm -> 
          let elm_ssaid = lower_body b env elm in
          let elm_mirtyp = find_mirtyp b elm_ssaid in
          match elm_mirtyp with
          | TMIRFun (_, TMIRFun _) -> 
              (*Functions that return functions are not fully saturated, to avoid ever having a tuple 
                with an unsaturated function I just do an eta expansion at creation if needed*)
              eta_expansion b elm_ssaid
          | _ -> elm_ssaid
        ) explst 
      in
      let mirtyplst = List.map (find_mirtyp b) ssaid_lst in
      let tup_ssaid = fresh_ssaid b in
      emit_op b (Tupinit (tup_ssaid, TMIRTup mirtyplst, ssaid_list));
      tup_ssaid
    )
  | I32LitT i -> (
      let ssaid = fresh_ssaid b in
      emit_op b (Immi32 (ssaid, TMIRI32, i));
      ssaid
    )
  | I8LitT i -> (
      let ssaid = fresh_ssaid b in
      emit_op b (Immi8 (ssaid, TMIRI8, i));
      ssaid
    )
  | UnitLitT _ -> (
      let ssaid = fresh_ssaid b in
      emit_op b (ImmUnit (ssaid, TMIRUnit));
      ssaid
    )
  | UopI32T (op, e, _) -> (
      let ssaid_e = lower_body b env e in
      let mirtyp_e = find_mirtyp b ssaid_e in
      let res_ssaid = fresh_ssaid b in
      emit_op b (Uopi32 (res_ssaid, TMIRI32, convert_uopi32 op, ssaid_e));
      res_ssaid
    )
  | UopI8T (op, e, _) -> (
      let ssaid_e = lower_body b env e in
      let mirtyp_e = find_mirtyp b ssaid_e in
      let res_ssaid = fresh_ssaid b in
      emit_op b (Uopi8 (res_ssaid, TMIRI8, convert_uopi8 op, ssaid_e));
      res_ssaid
    )
  | BopI32T (op, e1, e2, _) -> (
      let ssaid_e1 = lower_body b env e1 in
      let ssaid_e2 = lower_body b env e2 in
      let mirtyp_e1 = find_mirtyp b ssaid_e1 in
      let mirtyp_e2 = find_mirtyp b ssaid_e2 in
      let res_ssaid = fresh_ssaid b in
      emit_op b (Bopi32 (res_ssaid, TMIRI32, convert_bopi32 op, ssaid_e1, ssaid_e2));
      res_ssaid
    )
  | BopI8T (op, e1, e2, _) -> (
      let ssaid_e1 = lower_body b env e1 in
      let ssaid_e2 = lower_body b env e2 in
      let mirtyp_e1 = find_mirtyp b ssaid_e1 in
      let mirtyp_e2 = find_mirtyp b ssaid_e2 in
      let res_ssaid = fresh_ssaid b in
      emit_op b (Bopi8 (res_ssaid, res_mirtyp_of_bopi8 op, convert_bopi8 op, ssaid_e1, ssaid_e2));
      res_ssaid
    )
  | VecLit (elst, asttyp) -> (
      let ssaid_lst = List.map (lower_body b env) elst in
      let vec_ssaid = fresh_ssaid b in
      let vec_mirtyp = ast_type_to_mir_type asttyp in
      emit_op b (Veclit (vec_ssaid, vec_mirtyp, ssaid_lst));
      vec_ssaid
    )
  | VecmkT (defval, sizes, asttyp) -> (
      let ssaid_defval = lower_body b env defval in
      let ssaid_sizes = List.map (lower_body b env) sizes in
      let vec_ssaid = fresh_ssaid b in
      let vec_mirtyp = ast_type_to_mir_type asttyp in
      emit_op b (Vecmk (vec_ssaid, vec_mirtyp, ssaid_defval, ssaid_sizes));
      vec_ssaid
    )
  | VecgetT (vec, idxs, asttyp) -> (
      let ssaid_vec = lower_body b env vec in
      let ssaid_idxs = List.map (lower_body b env) idxs in
      let res_ssaid = fresh_ssaid b in
      let res_mirtyp = ast_type_to_mir_type asttyp in
      emit_op b (Vecget (res_ssaid, res_mirtyp, ssaid_vec, ssaid_idxs));
      res_ssaid
  )
  | VecsetT (vec, v, idxs, asttyp) -> (
      let ssaid_vec = lower_body b env vec in
      let ssaid_v = lower_body b env v in
      let ssaid_idxs = List.map (lower_body b env) idxs in
      let res_ssaid = fresh_ssaid b in
      let res_mirtyp = ast_type_to_mir_type asttyp in
      emit_op b (Vecset (res_ssaid, res_mirtyp, ssaid_vec, ssaid_v, ssaid_idxs));
      res_ssaid
  )
  | VecsliceT (vec, start, len, asttyp) -> (
      let ssaid_vec = lower_body b env vec in
      let ssaid_start = lower_body b env start in
      let ssaid_len = lower_body b env len in
      let res_ssaid = fresh_ssaid b in
      let res_mirtyp = ast_type_to_mir_type asttyp in
      emit_op b (Vecslice (res_ssaid, res_mirtyp, ssaid_vec, ssaid_start, ssaid_len));
      res_ssaid
  )
  | VecextendT (vec, lit, off, asttyp) -> (
      let ssaid_vec = lower_body b env vec in
      let ssaid_lit = lower_body b env lit in
      let ssaid_off = lower_body b env off in
      let res_ssaid = fresh_ssaid b in
      let res_mirtyp = ast_type_to_mir_type asttyp in
      emit_op b (Vecextend (res_ssaid, res_mirtyp, ssaid_vec, ssaid_lit, ssaid_off));
      res_ssaid
  )

    
  
   
(*really this only lowers non recursive functions form the top level like the main function
  all the top level stuff needs a bit of special handling since all the explicit rec structure is gone
  since my tast is just a flat expression list. So I first need to create all the top level functions and then
  add them to the env and then lower each body*)
and lower_func (b : Mir.builder) (env : UuidMap.t) (name_opt : string option) (rec_u : uuid option) (l : tlexp) : func =
  let rec lower_func_aux (arg_acc : string * (uuid option) * Ast.typ list) (l : tlexp) : func * uuid list =
    match l with
    | LamT (name, uuid, body, typ) -> (
      let arg_typ = match repr typ with
        | TFun (arg_typ, _) -> arg_typ
        | _ -> raise (Errors.LowerMonoTASTError "Expected function type in LamT")
      in
      lower_func_aux ((name, Some uuid, arg_typ) :: arg_acc) body
    )
    | LamUnitT (body, _) -> (
      lower_func_aux (( "unit", None, Ast.TUnit) :: arg_acc) body
    )
    | _ -> (
      (*if there the function is recursive the uuid of the function might be used in its body 
        but it should not be caputred as it will be reconstructed as the first thing in the function*)
      let capture_banned_uuids = match rec_u with
        | Some u -> UuidSet.singleton u
        | None -> UuidSet.empty
      in
      (*uuids that are in the env stored as a MIRFuncid are functions and dont caputre anything, 
        hence we dont need to pass them as they are just globaly known*)
      let capture_banned_uuids = UuidMap.fold (fun u mirval acc ->
                                                  match mirval with
                                                  | MIRSaid _ -> acc
                                                  | MIRFuncid _ -> UuidSet.add u acc) env capture_banned_uuids in
      let ast_args_lamlift = find_capt_vars capture_banned_uuids l in      

      let ast_args_lam = List.rev arg_acc in
      
      let ast_args_to_mir_args env arg_uuids = List.rev @@ List.fold_left 
              (fun (acc_mir_args, acc_env) (name, uuid_opt, typ) ->  
                let ssaid = fresh_ssaid b in
                let acc_env' = match uuid_opt with
                  | Some uuid -> env_put acc_env uuid (MIRSsaid ssaid)
                  | None -> acc_env
                in
              ((ssaid, ast_type_to_mir_type typ) :: acc_mir_args, acc_env' )
              ) ([], env) arg_uuids in
      let mir_args_lamlift, env' = ast_args_to_mir_args env ast_args_lamlift in
      let mir_args_lam, env'' = ast_args_to_mir_args env' ast_args_lam in

      let ret_mir_typ = ast_type_to_mir_type (tlexp_get_type l) in

      let func = create_func b name_opt ret_mir_typ (mir_args_lamlift @ mir_args_lam) in
      switch_to_func b func;
      let bbentry = create_bb b "entry" in
      switch_to_bb b bbentry;      switch_to_func b func;
      let bbentry = create_bb b "entry" in
      switch_to_bb b bbentry;

      let env'' = 
        match rec_u with
        (*no captured vars so we can keep it as a global function in the env*)
        | Some u when mir_args_lamlift = [] -> env_put env' u (MIRFuncid func.funcid)
        (*captured vars so we need to recreate the closure*)
        | Some u -> (
          let func_mirtyp = func_get_mirtyp b func.funcid in
          let func_ssaid = fresh_ssaid b in
          emit_op b (Func (func_ssaid, func_mirtyp, func.funcid));
          let closure_ssaid = fresh_ssaid b in
          let closure_mirtyp = trunc_n_mirfunctyp (List.length mir_args_lamlift) func_mirtyp in
          emit_op b (Pack (closure_ssaid, closure_mirtyp, func_ssaid, List.map (fun (ssaid, _) -> ssaid) mir_args_lamlift));
          env_put env' u (MIRSsaid closure_ssaid)
        )
        (*not a recursive function so no ahead of time knowledge needed*)
        | None -> env'
      in

      let res_ssaid = lower_body b env'' l in
      emit_term b (Ret res_ssaid);
      (func, ast_args_lamlift)
    )
  in
  let cp = funcbb_checkpoint b in
  let res = lower_func_aux [] l in
  funcbb_restore b cp;
  res
  

let lower_monotast (monotast : Ast.monotast) : Mir.program = 
  let b = create_builder () in
  List.iter (fun (uuid, name, l) -> lower_func b uuid name l) monotast;
  get_program b