open Ast
open Mir
open Errors

let rec ast_type_to_mir_type (typ_ast : Ast.typ) : Mir.mir =
  let rec funcaux arg_typ ret_typ =
    let arg_typmir = ast_type_to_mir_type arg_typ in
    match repr ret_typ with
    | TFun (arg_typ_inner, ret_typ_inner) -> arg_typmir :: (funcaux arg_typ_inner ret_typ_inner)
    | _ -> [ast_type_to_mir_type ret_typ]
  in

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
  | TFun (arg_typ, ret_typ) -> TMIRFun (funcaux arg_typ ret_typ)
  | TTup typ_list -> TMIRTup (List.map ast_type_to_mir_type typ_list)
  | TVec typ_inner -> vecaux typ_inner 1
  | Tvar _ -> raise (Errors.LowerMonoTASTError "Polymorphic Type in MonoTAST, Monomorphizer seems to fail")

module UuidSet = Set.Make(struct type t = int let compare = compare end)
module UuidMap = Map.Make(struct type t = int let compare = compare end)

let find_capt_vars (l : tlexp) : (string * uuid * Ast.typ) list =
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
  let captured_map = walk UuidSet.empty UuidMap.empty l in
  UuidMap.fold (fun u (n, typ) lst -> (n, Some u, typ) :: lst) captured_map []

type mirval = 
  | MIRSsaid of ssaid
  | MIRFuncid of funcid

let env_put (env : UuidMap.t) (uuid : uuid) (mirval : mirval) : UuidMap.t =
  UuidMap.add !uuid mirval env

let env_get (env : UuidMap.t) (uuid : uuid) : mirval option =
  UuidMap.find_opt !uuid env

(* Lowers a non delayed expression, returns the ssaid with the expression result *)
let lower_body (b : Mir.builder) (env : UuidMap.t) (l : tlexp) : ssaid =
  match l with
  | VarT (_, uuid, _) -> (
      match env_get env !uuid with
      | Some (MIRSsaid ssaid) -> ssaid
      | Some (MIRFuncid funcid) -> 
          let ssaid = fresh_ssaid b in
          let func_mirtyp = func_retmirtyp b funcid in
          let op = Func (ssaid, Mir.MIRFunc (funcid, 0), funcid) in
          emit_op b op;
          ssaid
      | None -> raise (Errors.LowerMonoTASTError "Variable not found in environment")
    )
  | LamT _ 
  | LamUnitT _ -> (
        let func = lower_func b env None l in
        let ssaid = fresh_ssaid b in
        let funcid = func.funcid in
        let op = Func (ssaid, Mir.MIRFunc (funcid, 0), funcid) in
        emit_op b op;
        ssaid
    )
  | AppT (e1, e2, _) -> (
        (*TODO: fix ary ness mismatch with eta conversion or mono pass*)
        let ssaid_func = lower_body b env e1 in
        let func_mirtyp = find_mirtyp b ssaid_func in
        let ssaid_arg = lower_body b env e2 in
        let ssaid_pack = fresh_ssaid b in
        let funcid, cntp1 = match func_mirtyp with
          | Some (TMIRFun (funcid, cnt)) -> (funcid, cnt + 1)
          | _ -> raise (Errors.LowerMonoTASTError "Expected function type for application")
        in 
        let pack_mirtyp = TMIRFun (funcid, cntp1) in
        let op = Pack (ssaid_pack, pack_mirtyp, [ssaid_arg]) in
        emit_op b op;
        if cntp1 = (func_aryness b funcid) then (
          let ssaid_res = fresh_ssaid b in
          let res_mirtyp = func_retmirtyp b funcid in
          let op_call = Call (ssaid_res, res_mirtyp, ssaid_pack) in
          emit_op b op_call;
          ssaid_res
        ) else (
          ssaid_pack
        )
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
      let ssaid_e = lower_body b env e in
      let env' = env_put env u (MIRSsaid ssaid_e) in
      lower_body b env' body
    )
  | LetrecinT (n, u, e, body, _) -> (
      let func = 
        match e with
        | LamT _ | LamUnitT _ -> lower_func b env (Some n) (Some u) e 
        | _ -> raise (Errors.LowerMonoTASTError "Letrec bound to non-lambda expression")
      in
      let ssaid = fresh_ssaid b in
      let funcid = func.funcid in
      let op = Func (ssaid, Mir.MIRFunc (funcid, 0), funcid) in
      emit_op b op;
      let env' = env_put env u (MIRSsaid ssaid) in
      lower_body b env' body
    )
  | LetinTupleT (elms, e, body, _) -> (
    let tup_ssaid = lower_body b env e in
    let tup_mirtyplst = 
      match find_mirtyp b tup_ssaid with 
      | Some (TMIRTup mirtyplst) -> mirtyplst
      | None -> raise (Errors.LowerMonoTASTError "Expected Tuple Mir type in LetinTuple lowering")
    in
    let idx = ref 0 in
    let env' = List.fold_left2 (fun env_acc elm_opt elm_mirtyp ->
        match elm_opt with
        | Some (name, uuid) -> (
          let elm_ssaid = fresh_ssaid b in
          let op = Tupget (elm_ssaid, elm_mirtyp, tup_ssaid, !idx) in
          emit_op b op;
          idx := !idx + 1;
          env_put env_acc uuid (MIRSsaid elm_ssaid)
        )
        | None -> (idx := !idx + 1; env_acc)
        ) env elms tup_mirtyplst in
    lower_body b env' body
  )
  | TupleT (el, _) -> (
      let ssaid_list = List.map (lower_body b env) el in
      let mirtyplst = List.map (fun ssaid -> match find_mirtyp b ssaid with
        | Some typ -> typ
        | None -> raise (Errors.LowerMonoTASTError "Expected type for tuple element")
      ) ssaid_list in
      let tup_ssaid = fresh_ssaid b in
      let op = Tupinit (tup_ssaid, TMIRTup mirtyplst, ssaid_list) in
      emit_op b op;
      tup_ssaid
    )

    
  
   

let lower_func (b : Mir.builder) (env : UuidMap.t) (name_opt : string option) (rec_u : uuid option) (l : tlexp) : func =
  let rec lower_func_aux (arg_acc : string * (uuid option) * Ast.typ list) (l : tlexp) : unit =
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
      let args_func = List.rev arg_acc in
      let args_capt = find_capt_vars l in
      let ret_mir_typ = ast_type_to_mir_type (tlexp_get_type l) in
      let mir_args, env' = List.fold_left (fun (acc_mir_args, acc_env) (name, uuid_opt, typ) -> 
          let ssaid = fresh_ssaid b in
          let acc_env' = match uuid_opt with
            | Some uuid -> env_put acc_env uuid (MIRSsaid ssaid)
            | None -> acc_env
          in
         ((ssaid, ast_type_to_mir_type typ) :: acc_mir_args, acc_env' )
         ) ([], env) ( args_capt @ args_func ) in
      let func = create_func b name_opt ret_mir_typ (List.rev mir_args) in
      let env'' = match rec_u with
        | Some u -> env_put env' u (MIRFuncid func.funcid)
        | None -> env'
      switch_to_func b func;
      let bbentry = create_bb b "entry" in
      switch_to_bb b bbentry;
      lower_body b env'' l;
      func
    )
  in
  let cp = funcbb_checkpoint b in
  let func = lower_func_aux [] l in
  funcbb_restore b cp;
  func
  

let lower_monotast (monotast : Ast.monotast) : Mir.program = 
  let b = create_builder () in
  List.iter (fun (uuid, name, l) -> lower_func b uuid name l) monotast;
  get_program b