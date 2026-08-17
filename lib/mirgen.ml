open Errors
open Ast
open PrintIntlang
open Mir
open Printmir
open Buildmir

module UuidSet = Set.Make(Int)
module UuidMap = Map.Make(Int)

(* ========================================================================= *)
(* Uop and Bop Conversion                                                    *)
(* ========================================================================= *)

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

(* ========================================================================= *)
(* Mir Type Helpers                                                          *)
(* ========================================================================= *)

let clos_mirtyp_trunc_n (n : int) (mirtyp : mirtyp) : mirtyp =
  match mirtyp with
  | TMIRClos (args, ret) -> TMIRClos (List.filteri (fun j _ -> j >= n) args, ret)
  | _ -> raise (Errors.LowerMonoTASTError ("Expected function type in clos_mirtyp_trunc_n, but got: " ^ Printmir.string_of_typ mirtyp))

let rec asttyp_to_mirtyp (typ_ast : Ast.typ) : Mir.mirtyp =
  (* create fully saturated version *)
  let rec funcaux argacc arg_typ ret_typ =
    let arg_mirtyp = asttyp_to_mirtyp arg_typ in
    match repr ret_typ with
    | TFun (arg_typ_inner, ret_typ_inner) ->  (funcaux (arg_mirtyp :: argacc) arg_typ_inner ret_typ_inner)
    | _ -> TMIRClos (List.rev (arg_mirtyp :: argacc), asttyp_to_mirtyp ret_typ)
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
  | TTup typ_list -> TMIRTup (List.map asttyp_to_mirtyp typ_list)
  | TVec typ_inner -> vecaux typ_inner 1
  | TVar _ -> raise (Errors.LowerMonoTASTError "Polymorphic Type in MonoTAST, Monomorphizer seems to fail")


(* ========================================================================= *)
(* Ast Captured Variables Helpers                                            *)
(* ========================================================================= *)

let find_capt_vars (banned : UuidSet.t) (*used to avoid capturue of special uuids (like toplevel bindings or resursive functions)*) 
                   (l : tlexp) : (uuid option * string option * Ast.typ) list =
                   
  let rec walk (bound : UuidSet.t) (acc : (string * Ast.typ) UuidMap.t) (expr : tlexp) =
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
          List.fold_left (fun bd binder -> 
            match binder with
            | Some (n, u) -> UuidSet.add u bd
            | None -> bd
          ) bound binders 
        in
        walk bound' acc1 body
    | TupleT (el, _) | VecLitT (el, _) ->
        List.fold_left (walk bound) acc el
    | I32LitT _ | I8LitT _ | UnitLitT _ -> 
        acc
    | UopI32T (_, e, _) | UopI8T (_, e, _) | VeclenT (e, _) ->
        walk bound acc e
    | BopI32T (_, e1, e2, _) | BopI8T (_, e1, e2, _) ->
        let acc1 = walk bound acc e1 in
        walk bound acc1 e2
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
  let captured_map = walk banned UuidMap.empty l in
  UuidMap.fold (fun u (n, typ) lst -> (Some u, Some n, typ) :: lst) captured_map []


(* ========================================================================= *)
(* Lowering Env                                                              *)
(* ========================================================================= *)

type mirval = 
  | MIRSsaid of ssaid
  | MIRFuncid of funcid
  | MIRGlobalid of globalid

type env = mirval UuidMap.t

let env_put (env : mirval UuidMap.t) (uuid : uuid) (mirval : mirval) : mirval UuidMap.t =
  UuidMap.add uuid mirval env

let env_get (env : mirval UuidMap.t) (uuid : uuid) : mirval option =
  UuidMap.find_opt uuid env

let env_merge (env1 : mirval UuidMap.t) (env2 : mirval UuidMap.t) : mirval UuidMap.t =
  UuidMap.union (fun _ _ v2 -> Some v2) env1 env2

let sprint_env (env : mirval UuidMap.t) : string =
  let entries = UuidMap.bindings env in
  let entry_strings = List.map (fun (uuid, mirval) ->
    let mirval_str = match mirval with
      | MIRSsaid ssaid -> Printf.sprintf "MIRSsaid(%d)" ssaid
      | MIRFuncid funcid -> Printf.sprintf "MIRFuncid(%d)" funcid
      | MIRGlobalid globalid -> Printf.sprintf "MIRGlobalid(%d)" globalid
    in
    Printf.sprintf "%d: %s" uuid mirval_str
  ) entries in
  String.concat ", " entry_strings

(* ========================================================================= *)
(* Declaring Functions and Globals                                           *)
(* ========================================================================= *)

let declare_func (b : builder) 
                 (env : mirval UuidMap.t) 
                 (name : string) 
                 (capture_banned_uuids_lst : uuid list) 
                 (l_out : tlexp) 
                 : func * (uuid list) * mirval UuidMap.t * tlexp =
  let rec declare_func_aux (arg_acc : (uuid option * string option * Ast.typ) list) (l : tlexp) =
    match l with
    | LamT (name, uuid, body, typ) -> (
      let arg_typ = match repr typ with
        | TFun (arg_typ, _) -> arg_typ
        | _ -> raise (Errors.LowerMonoTASTError "Expected function type in LamT")
      in
      declare_func_aux ((Some uuid, Some name, arg_typ) :: arg_acc) body
    )
    | LamUnitT (body, _) -> (
      declare_func_aux ((None, None, Ast.TUnit) :: arg_acc) body
    )
    | _ -> (

      (*the caputure banded uuids are used to bann the caputre of any globals in the initial declare phase (all are known globaly)
        and to ban the capture a local recursive function in a let rec ... in (is reconstructed in the function body)*)
      let capture_banned_uuids = UuidSet.of_list capture_banned_uuids_lst in
      (*uuids that are in the env stored as a MIRFuncid are functions that dont caputre anything, 
        hence we dont need to capture / lambda lift them as they are globaly known*)
      let capture_banned_uuids = UuidMap.fold (fun u mirval acc ->
                                                  match mirval with
                                                  | MIRSsaid _ -> acc
                                                  | MIRFuncid _ 
                                                  | MIRGlobalid _ -> UuidSet.add u acc
                                                ) env capture_banned_uuids in
      let args_lamlift = find_capt_vars capture_banned_uuids l_out in
      let args_lam = List.rev arg_acc in

      (*get args ssaids and put it in usefull forms*)
      let next_ssaid = ref 0 in
      let loc_env, func_args_rev = List.fold_left (fun (env_acc, args_acc) (uuid_opt, name_opt, typ) ->
        let mirtyp = asttyp_to_mirtyp typ in
        let arg_ssaid = !next_ssaid in
        next_ssaid := arg_ssaid + 1;
        let env_acc' = match uuid_opt with
          | Some uuid -> UuidMap.add uuid (MIRSsaid arg_ssaid) env_acc
          | None -> env_acc
        in
        (env_acc', (arg_ssaid, name_opt, mirtyp) :: args_acc)
      ) (UuidMap.empty, []) (args_lamlift @ args_lam) in

      (*create mir func*)
      let ret_mirtyp = asttyp_to_mirtyp (tlexp_get_type l) in
      let func = create_func b name (List.rev func_args_rev) ret_mirtyp None in

      let lamlift_uuid = List.map (fun (uuid_opt, _, _) -> Option.get uuid_opt) args_lamlift in

      (func, lamlift_uuid, loc_env, l)
    )
  in
  declare_func_aux [] l_out

type decl = 
  | FuncDecl of func * tlexp * env        (* Mir Function where to lower the tlexp with some environment additions (for arguments)*)
  | GlobalDecl of global * tlexp          (* Global where the result of the lowering of the tlexp should be stored, the lowering should live in @init_globals*)

let declare (b : builder) (builtins_env : mirval UuidMap.t) (monotast : Ast.monotast) : decl list * env =
  let toplevel_uuids = List.map (fun (_, uuid, _) -> uuid) monotast in
  let decls, toplvl_env_lst =
  List.split @@
  List.map ( fun (name, uuid, l) ->
    match l with
    | LamT _ | LamUnitT _ -> (
        let (func, lamlift_uuid, loc_env, l_body) = 
            declare_func b builtins_env name toplevel_uuids l in
        if lamlift_uuid <> [] then
          raise (Errors.LowerMonoTASTError ("Function has captured variables, this should not happen as the function should have been lambda lifted " ^ name ^ " uuids: " ^ String.concat ", " (List.map string_of_int lamlift_uuid)))
        else
          (if name = "main" then b.program.main_funcid <- Some func.funcid);
          (FuncDecl (func, l_body, loc_env), (uuid, MIRFuncid func.funcid))
      )
    | _ -> (
      let mirtyp = asttyp_to_mirtyp (tlexp_get_type l) in
      let global = create_global b mirtyp in
      (GlobalDecl (global, l), (uuid, MIRGlobalid global.globalid))
      )
    ) monotast in
  let toplvl_env = List.fold_left (fun env_acc (uuid, mirval) -> UuidMap.add uuid mirval env_acc) UuidMap.empty toplvl_env_lst in
  (decls, toplvl_env)


(* ========================================================================= *)
(* Lowering Bodies                                                           *)
(* ========================================================================= *)

let eta_expansion (b : builder) (unsat_ssaid : ssaid) : ssaid =
  (*idea if dublication observed in real code: create a cache with eta func signatures for reuse*)
  
  (*extract all args and return type of the fully saturated version, 
    the inner list indicates the points where calls are needed*)
  let rec extract_sat_args (acc : (ssaid * string option * mirtyp) list list) 
                           (next_ssaid : ssaid) 
                           (unsat_mirtyp : mirtyp) 
                           : ((ssaid * string option * mirtyp) list list) * mirtyp =
    match unsat_mirtyp with
    | TMIRClos (args_mirtyp, ret_mirtyp) -> 
      let args_part = List.mapi (fun i arg_mirtyp -> (next_ssaid + i, None, arg_mirtyp)) args_mirtyp in
      extract_sat_args (args_part :: acc) (next_ssaid + List.length args_mirtyp) ret_mirtyp
    | _ -> (List.rev acc, unsat_mirtyp)
  in
  let unsat_mirtyp = get_mirtyp b unsat_ssaid in
  let sat_args, sat_ret_mirtyp = extract_sat_args [] 1 unsat_mirtyp in

  let cp = cp_set b in

  (*declare eta expansion function wrapper*)
  let eta_func = create_func b 
                             ("eta_expansion_for_ssaid_" ^ string_of_int unsat_ssaid)
                             ((0, None, unsat_mirtyp) :: (List.flatten sat_args))
                             sat_ret_mirtyp
                             None 
                            in
  switch_func b eta_func;
  let bbentry = create_bb b "entry" [] in
  switch_bb b bbentry;

  (*fill body of eta expansion function*)
  let res_ssaid = 
    List.fold_left 
      ( fun closure_ssaid args_part ->
        let pack_ssaids = List.map (fun (ssaid, _, _) -> ssac ssaid) args_part in
        let pack_ssaid = fresh_ssaid b in
        emit_op b ( Pack (pack_ssaid, ssac closure_ssaid, pack_ssaids ) );
        let res_ssaid = fresh_ssaid b in
        emit_op b ( CallClosure (res_ssaid, ssac pack_ssaid) );
        res_ssaid
      ) 0 sat_args (*0 is the hardcoded closure argument*)
  in
  emit_term b (Ret (ssac res_ssaid));

  cp_ret b cp;
  
  (*pack the unsaturated closure object as the first argument to the eta expansion function*)
  let eta_rawfunc_ssaid = fresh_ssaid b in
  emit_op b ( Func (eta_rawfunc_ssaid, eta_func.funcid) );
  let eta_func_ssaid = fresh_ssaid b in
  emit_op b ( Pack (eta_func_ssaid, ssac eta_rawfunc_ssaid, [ ssac unsat_ssaid]) );
  eta_func_ssaid

  
(* Takes a Function and a list of captured UUIDs
   Creates a closure with all the captured variables packed*)
let func_to_closure (b : builder) (env : mirval UuidMap.t) (func : func) (cap_uuids : uuid list) : ssaid =
    let func_ssaid = fresh_ssaid b in
    emit_op b (Func (func_ssaid, func.funcid));
    if cap_uuids = [] then
      func_ssaid
    else (
      let closure_ssaid = fresh_ssaid b in
      let pack_ssaids = List.map 
        (fun uuid -> 
          match UuidMap.find_opt uuid env with
          | Some (MIRSsaid ssaid) -> ssac ssaid
          | Some (MIRFuncid _) -> raise (Errors.LowerMonoTASTError "passing function with no captured vars as a captured var, this should not happen as this function does not need to be caputured")
          | Some (MIRGlobalid _) -> raise (Errors.LowerMonoTASTError "passing global as a captured var, this should not happen as this function does not need to be caputured")
          | None -> raise (Errors.LowerMonoTASTError ("captured variables ssaid not found in environment, or mb some weird thing with unit" ^ string_of_int uuid))
        ) cap_uuids in
      emit_op b (Pack (closure_ssaid, ssac func_ssaid, pack_ssaids));
      closure_ssaid
    )

(* Lowers ast expression, 
   Assumes that the cursor is already in the correct place to emit the lowered code,
   Returns the ssaid with the expression result *)
let rec lower_body (b : builder) (env : mirval UuidMap.t) (l : tlexp) : ssaid =
  match l with
  | VarT (_, uuid, _) -> (
    match env_get env !uuid with
    | Some (MIRSsaid ssaid) -> ssaid
    | Some (MIRFuncid funcid) -> (
      (*function id in the env means this is a function that is 
        compiletime known and does not does not have any lambda lifted catpured vars
        so we can just instanciate it directly*)
      let ssaid = fresh_ssaid b in
      let op = Func (ssaid, funcid) in
      emit_op b op;
      ssaid
    )
    | Some (MIRGlobalid globalid) -> (
      let ssaid = fresh_ssaid b in
      emit_op b (LoadGlobal (ssaid, globalid));
      ssaid
    )
    | None -> raise (Errors.LowerMonoTASTError "Variable not found in environment")
  )
  | LamT _ 
  | LamUnitT _ -> (
    (*anonymous lambda*)
    let func, cap_uuids = lower_loc_func b env "anonlam" None l in
    func_to_closure b env func cap_uuids
  )
  | AppT (e1, e2, _) -> (
    let ssaid_clos = lower_body b env e1 in
    let arg_ssaid = lower_body b env e2 in
    let sat_arg_ssaid = 
      match get_mirtyp b arg_ssaid with
      | TMIRClos (_, TMIRClos _) -> 
          (*Functions that return functions are not fully saturated, but all functions
            are lowered such that arguments are expected to have functions in fully saturated form.
            Hence I put a wrapper around the unsaturated functions or in other words an eta expansion.*)
          eta_expansion b arg_ssaid
      | _ -> arg_ssaid
    in
    let pack_ssaid = fresh_ssaid b in
    emit_op b (Pack (pack_ssaid, ssac ssaid_clos, [ssac sat_arg_ssaid]));
    match get_mirtyp b pack_ssaid with (* if the closure is full we need to call *)
    | TMIRClos ( [], _) -> (
        let res_ssaid = fresh_ssaid b in
        emit_op b (CallClosure (res_ssaid, ssac pack_ssaid));
        res_ssaid
      )
    | TMIRClos ( _, _) -> pack_ssaid
    | _ -> raise (Errors.LowerMonoTASTError "Expected function type after packing in AppT lowering")
  )
  | SeqT (e1, e2, _) -> (
    let _ = lower_body b env e1 in
    lower_body b env e2
  )
  | IfT (cond, t_branch, e_branch, _) -> (
    (*setup bbs*)
    let bb_then = create_bb b "then" [] in
    let bb_else = create_bb b "else" [] in
    let merge_res_ssaid = fresh_ssaid b in
    let bb_merge = create_bb b "merge" [(merge_res_ssaid, asttyp_to_mirtyp (tlexp_get_type t_branch))] in

    (*lower cond*)
    let ssaid_cond = lower_body b env cond in
    emit_term b (Cbr (ssaid_cond, brac bb_then.bbid [], brac bb_else.bbid []));

    (*lower if branch*)
    switch_bb b bb_then;
    let then_res_ssaid = lower_body b env t_branch in
    let then_res_mirtyp = get_mirtyp b then_res_ssaid in
    let sat_then_res_ssaid = 
      match then_res_mirtyp with
      | TMIRClos (_, TMIRClos _) -> eta_expansion b then_res_ssaid
      | _ -> then_res_ssaid
    in
    emit_term b (Br (brac bb_merge.bbid [ssac sat_then_res_ssaid]));

    (*lower else branch*)
    switch_bb b bb_else;
    let else_res_ssaid = lower_body b env e_branch in
    let else_res_mirtyp = get_mirtyp b else_res_ssaid in
    let sat_else_res_ssaid = 
      match else_res_mirtyp with
      | TMIRClos (_, TMIRClos _) -> eta_expansion b else_res_ssaid
      | _ -> else_res_ssaid
    in
    emit_term b (Br (brac bb_merge.bbid [ssac sat_else_res_ssaid]));

    switch_bb b bb_merge;
    merge_res_ssaid
  )
  | LetinT (n, u, e, body, _) -> (
      match e with
      | LamT _ | LamUnitT _ -> (
          let func, cap_uuids = lower_loc_func b env n None e in
          let env' = 
            (*this allows local helpers that dont lamlift to be used 
              directly when captured somewhere else and not passed as a lamlifted arg*)
            if cap_uuids = [] then (
              env_put env u (MIRFuncid func.funcid)
            ) else (
              let closure_ssaid = func_to_closure b env func cap_uuids in
              env_put env u (MIRSsaid closure_ssaid)
            )
          in
          lower_body b env' body
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
        | LamT _ | LamUnitT _ -> lower_loc_func b env n (Some u) e 
        | _ -> raise (Errors.LowerMonoTASTError "Letrec bound to non-lambda expression")
      in
      let env' = 
        (*this allows local helpers that dont lamlift to be used 
          directly when captured somewhere else and not passed as a lamlifted arg*)
        if cap_uuids = [] then (
          env_put env u (MIRFuncid func.funcid)
        ) else (
          let closure_ssaid = func_to_closure b env func cap_uuids in
          env_put env u (MIRSsaid closure_ssaid)
        )
      in
      lower_body b env' body
    )
  | LetinTupleT (elms, e, body, _) -> (
    (*the default behavior is to borrow form the tuple ie. use the tupview*)
    let tup_ssaid = lower_body b env e in
    let elms_ssaid = List.map (fun _ -> fresh_ssaid b) elms in
    emit_op b (Tupview (elms_ssaid, tup_ssaid));
    let env' = 
      List.fold_left2 (fun env_acc elm_opt elm_ssaid ->
        match elm_opt with
        | Some (_, uuid) -> env_put env_acc uuid (MIRSsaid elm_ssaid)
        | None -> env_acc
      ) env elms elms_ssaid
    in
    lower_body b env' body
  )
  | TupleT (explst, _) -> (
      let ssaid_lst = List.map 
        (fun elm -> 
          let elm_ssaid = lower_body b env elm in
          let elm_mirtyp = get_mirtyp b elm_ssaid in
          match elm_mirtyp with
          | TMIRClos (_, TMIRClos _) -> 
              (*Functions that return functions are not fully saturated, to avoid ever having a tuple 
                with an unsaturated function I just do an eta expansion at creation if needed*)
              ssac @@ eta_expansion b elm_ssaid
          | _ -> ssac elm_ssaid
        ) explst 
      in
      let tup_ssaid = fresh_ssaid b in
      emit_op b (Tupinit (tup_ssaid, ssaid_lst));
      tup_ssaid
    )
  | I32LitT (i, _) -> (
      let ssaid = fresh_ssaid b in
      emit_op b (Immi32 (ssaid, Int32.of_int i));
      ssaid
    )
  | I8LitT (i, _) -> (
      let ssaid = fresh_ssaid b in
      emit_op b (Immi8 (ssaid, i));
      ssaid
    )
  | UnitLitT _ -> (
      let ssaid = fresh_ssaid b in
      emit_op b (ImmUnit ssaid);
      ssaid
    )
  | UopI32T (op, e, _) -> (
      let ssaid_e = lower_body b env e in
      let res_ssaid = fresh_ssaid b in
      emit_op b (Uopi32 (res_ssaid, convert_uopi32 op, ssaid_e));
      res_ssaid
    )
  | UopI8T (op, e, _) -> (
      let ssaid_e = lower_body b env e in
      let res_ssaid = fresh_ssaid b in
      emit_op b (Uopi8 (res_ssaid, convert_uopi8 op, ssaid_e));
      res_ssaid
    )
  | BopI32T (op, e1, e2, _) -> (
      let ssaid_e1 = lower_body b env e1 in
      let ssaid_e2 = lower_body b env e2 in
      let res_ssaid = fresh_ssaid b in
      emit_op b (Bopi32 (res_ssaid, convert_bopi32 op, ssaid_e1, ssaid_e2));
      res_ssaid
    )
  | BopI8T (op, e1, e2, _) -> (
      let ssaid_e1 = lower_body b env e1 in
      let ssaid_e2 = lower_body b env e2 in
      let res_ssaid = fresh_ssaid b in
      emit_op b (Bopi8 (res_ssaid, convert_bopi8 op, ssaid_e1, ssaid_e2));
      res_ssaid
    )
  | VecLitT (elst, asttyp) -> (
      let ssaid_lst = List.map (fun e -> ssac @@ lower_body b env e) elst in
      let vec_ssaid = fresh_ssaid b in
      emit_op b (Veclit (vec_ssaid, ssaid_lst));
      vec_ssaid
    )
  | VecmkT (defval, sizes, asttyp) -> (
      let ssaid_defval = lower_body b env defval in
      let ssaid_sizes = List.map (lower_body b env) sizes in
      let vec_ssaid = fresh_ssaid b in
      emit_op b (Vecinit (vec_ssaid, ssaid_defval, ssaid_sizes));
      vec_ssaid
    )
  | VeclenT (vec, _) -> (
      let ssaid_vec = lower_body b env vec in
      let res_ssaid = fresh_ssaid b in
      emit_op b (Veclen (res_ssaid, ssaid_vec));
      res_ssaid
  )
  | VecgetT (vec, idxs, asttyp) -> (
      let ssaid_vec = lower_body b env vec in
      let ssaid_idxs = List.map (lower_body b env) idxs in
      let res_ssaid = fresh_ssaid b in
      emit_op b (Vecread (res_ssaid, ssaid_vec, ssaid_idxs));
      res_ssaid
  )
  | VecsetT (vec, v, idxs, asttyp) -> (
      let ssaid_vec = lower_body b env vec in
      let ssaid_v = lower_body b env v in
      let ssaid_idxs = List.map (lower_body b env) idxs in
      let res_ssaid = fresh_ssaid b in
      (
      match repr (tlexp_get_type v) with
      | TI32 | TI8 -> emit_op b (Vecwrite (res_ssaid, ssac ssaid_vec, ssaid_v, ssaid_idxs))
      | TVec _ -> emit_op b (Vecinsert (res_ssaid, ssac ssaid_vec, ssac ssaid_v, ssaid_idxs))
      | _ -> raise (Errors.LowerMonoTASTError "VecsetT with unexpected type, veccheck seems to fail")
      );
      res_ssaid
  )
  | VecsliceT (vec, start, len, asttyp) -> (
      let ssaid_vec = lower_body b env vec in
      let ssaid_start = lower_body b env start in
      let ssaid_len = lower_body b env len in
      let res_ssaid = fresh_ssaid b in
      emit_op b (Vecslice (res_ssaid, ssaid_vec, ssaid_start, ssaid_len));
      res_ssaid
  )
  | VecextendT (vec, lit, off, asttyp) -> (
      let ssaid_vec = lower_body b env vec in
      let ssaid_lit = lower_body b env lit in
      let ssaid_off = lower_body b env off in
      let res_ssaid = fresh_ssaid b in
      emit_op b (Vecextend (res_ssaid, ssaid_vec, ssaid_lit, ssaid_off));
      res_ssaid
  )

    
and lower_loc_func (b : builder) 
                   (env : mirval UuidMap.t) 
                   (name : string) 
                   (rec_u : uuid option) 
                   (l : tlexp) 
                   : func * (uuid list) =
                     
  let cp = cp_set b in

  (*setup function*)
  let func, lamlift_uuids, env_func, l_body = declare_func b env name (match rec_u with Some u -> [u] | None -> []) l in
  switch_func b func;
  let bbentry = create_bb b "entry" [] in
  switch_bb b bbentry;

  (*add local env from func decl to the outer env*)
  let env' = env_merge env env_func in
  (*make sure recursive functoin have themselves in the env*)
  let env'' = 
    match rec_u with
    | Some u when lamlift_uuids = [] -> (
      (*no captured vars so we can keep it as a global function in the env*)
      env_put env' u (MIRFuncid func.funcid)
    )
    | Some u -> (
      (*captured vars so we need to recreate the closure*)
      let closure_ssaid = func_to_closure b env func lamlift_uuids in
      env_put env' u (MIRSsaid closure_ssaid)
    )
    | None -> (
      (*not a recursive function so no ahead of time knowledge needed*)
      env'
    )
  in

  let res_ssaid = lower_body b env'' l_body in
  emit_term b (Ret (ssac res_ssaid));

  cp_ret b cp;

  (func, lamlift_uuids)


let lower_decls (b : builder) (decls : decl list) (toplvl_env : mirval UuidMap.t) : unit =

  (*setup @init_globals*)
  let init_globals_func = create_func b "init_globals" [] TMIRUnit None in
  switch_func b init_globals_func;
  let bbentry = create_bb b "entry" [] in
  switch_bb b bbentry;
  b.program.init_globals_funcid <- Some init_globals_func.funcid;

  (*lower all the declarations*)
  let init_global_cp = ref (cp_set b) in
  List.iter (fun decl ->
    match decl with
    | FuncDecl (func, l_body, env_func) -> (
        switch_func b func;
        let bbentry = create_bb b "entry" [] in
        switch_bb b bbentry;
        let env = env_merge toplvl_env env_func in
        let res_ssaid = lower_body b env l_body in
        emit_term b (Ret (ssac res_ssaid))
      )
    | GlobalDecl (global, l_init) -> (
        cp_ret b !init_global_cp;
        let res_ssaid = lower_body b toplvl_env l_init in
        emit_op b (StoreGlobal (global.globalid, ssac res_ssaid));
        init_global_cp := cp_set b;
      )
  ) decls;
  
  (*emit return on @init_globals*)
  cp_ret b !init_global_cp;
  let unit_ssaid = fresh_ssaid b in
  emit_op b (ImmUnit unit_ssaid);
  emit_term b (Ret (ssac unit_ssaid))
  
let lower_builtins (b : builder) (builtins : Ast.typenv) : mirval UuidMap.t =
  List.fold_left (fun env_acc (name , (schema , uuid)) ->
    match schema with
    | Ast.Forall ([], TFun (arg_typ, ret_typ)) -> (
        let arg_mirtyp = asttyp_to_mirtyp arg_typ in
        let ret_mirtyp = asttyp_to_mirtyp ret_typ in
        let func = create_func b name [(0, None, arg_mirtyp)] ret_mirtyp (Some name) in (*here would mb go other external names*)
        env_put env_acc uuid (MIRFuncid func.funcid)
      )
    | _ -> raise (Errors.LowerMonoTASTError "Builtin function has unexpected type schema")
  ) UuidMap.empty builtins

let lower_monotast (monotast : Ast.monotast) : builder = 
    let b = create_builder () in
  try
    let builtins_env = lower_builtins b Ast.builtins in
    let decls, toplvl_env = declare b builtins_env monotast in
    let global_env = env_merge builtins_env toplvl_env in
    lower_decls b decls global_env;
    b
  with e ->
    let msg = Printexc.to_string e in
    let backtrace = Printexc.get_backtrace () in
    Printf.eprintf "%s\n" (Printmir.string_of_program b.program);
    let curr_fun, curr_bb = 
      match b.cursor with
      | (Some func, Some bb) -> ("func_" ^ string_of_int func.funcid, "bb_" ^ string_of_int bb.bbid)
      | _,_ -> ("None", "None")
    in
    Printf.eprintf "Cursor: %s %s\n" curr_fun curr_bb;
    Printf.eprintf "Error during lowering: %s\nBacktrace:\n%s\n" msg backtrace;
    raise e