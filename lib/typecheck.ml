open Ast
open Errors
open PrintIntlang

(*CONFIGURATION*)

let print_log = false

module SSet = Set.Make(String)

let log = ref ""

let log_append (s : string) : unit =
  log := !log ^ s

let log_appendln (s : string) : unit =
  log := !log ^ s ^ "\n"

(*some short hands to make the code less convoluted*)
let sple = PrintIntlang.sprint_lexp
let sples = PrintIntlang.sprint_lexp_shallow 2
let spt = PrintIntlang.sprint_typ
let spc = PrintIntlang.sprint_constraint

let spcs (cs : constraints) : string = String.concat ", " (List.map (fun c -> spc c) cs)
let spf = Printf.sprintf

(*UNIFICATION ENGINE*)
let rec unify (t1 : typ) (t2 : typ) : unit =
  (*you put a tvar v into this that has a link => not good*)
  let rec occurscheck (v : tvar) (t : typ) : unit =
      match repr t with
      | TVar v' ->  if v.id = v'.id then 
                      raise (Errors.TypeError ("[occurscheck] Occurs Check Failed: Recursive types are not allowed. Found tvar: t" ^ string_of_int v.id ^ " again")) 
                    else 
                      ()
      | TFun (t1, t2) -> occurscheck v t1; occurscheck v t2
      | TInt -> () 
      | TVec t_inner -> occurscheck v t_inner
  in

  match (repr t1, repr t2) with
  | (TInt, TInt) -> ()
  | (TFun (t1f, t1x), TFun (t2f, t2x)) -> 
      unify t1f t2f;
      unify t1x t2x
  | (TVec t1_inner, TVec t2_inner) -> unify t1_inner t2_inner;
  | (TVar v, t) | (t, TVar v) -> (
      match repr t with
      | TVar v' when v.id = v'.id -> () (*t0 does not occur in t0 but it would in TVec[t0]*)
      | _ -> (
          occurscheck v t;
          log_appendln (spf "Linking %s to %s" (spt (TVar v)) (spt t));
          v.link <- Some t
      )
    )
  | _ -> raise (Errors.TypeError ("Type mismatch: Cannot unify " ^ sprint_typ (repr t1) ^ " with " ^ sprint_typ (repr t2)))  

let rec generalize (t : typ) : schema =
  match repr t with
  | TInt -> Forall ([], TInt)
  | TFun (t1, t2) ->
      let Forall (vars1, t1_gen) = generalize t1 in
      let Forall (vars2, t2_gen) = generalize t2 in
      let unique_vars = List.sort_uniq compare (vars1 @ vars2) in
      Forall (unique_vars, TFun (t1_gen, t2_gen))
  | TVec t_inner ->
      let Forall (vars_inner, t_inner_gen) = generalize t_inner in
      Forall (vars_inner, TVec t_inner_gen)
  | TVar v -> Forall ([v.id], TVar v)

let instantiate(Forall (vars, t) : schema) (uuid : int) : typ =
  let fresh_tvar_map = List.map (fun var -> var, fresh_tvar ()) vars in
  let rec instaux (varmap : (int * tvar) list) (t: typ) : typ =
    match repr t with
      | TVar v -> (
          match List.assoc_opt v.id varmap with
            | Some fresh_t -> TVar fresh_t
            | None -> t
        )
      | TFun (t1, t2) -> TFun (instaux varmap t1, instaux varmap t2)
      | TVec t_inner -> TVec (instaux varmap t_inner)
      | TInt -> TInt 
  in instaux fresh_tvar_map t


let rec typecheck_lexp (env : typenv) (e : lexp) : constraints * lexpt = 
  match e with
    | Var x -> (
      match List.assoc_opt x env with
      | Some (s, uuid) ->
          let t = instantiate s uuid in
          ([], VarT (ref x, ref uuid, t))
      | None -> raise (Errors.TypeError ("Unbound variable: " ^ x))
      )
    | Lam (x, b) -> (
      let tv = TVar (fresh_tvar ()) in
      let uuid = fresh_uuid () in
      let env' = (x, (Forall ([], tv), uuid)) :: env in (*it is enough to stitch it to the front as assoc_opt just finds the first one*)
      let cs, bt = typecheck_lexp env' b in
      let t_body = lexpt_get_type bt in
      log_appendln (spf "Lam %s: uuid=%d, tv=%s" (sples e) uuid (spt tv));
      (cs, LamT (x, uuid, bt, TFun (tv, t_body)))
      )
    | LamUnit b -> (
      let cs, bt = typecheck_lexp env b in
      let t_body = lexpt_get_type bt in
      log_appendln (spf "LamUnit %s:" (sples e));
      (cs, LamUnitT (bt, TFun (TUnit, t_body)))
    )
    | App (f, x) -> (
      let t_out = TVar (fresh_tvar ()) in
      let cs_f, ft = typecheck_lexp env f in
      let cs_x, xt = typecheck_lexp env x in
      let t_f = lexpt_get_type ft in
      let t_x = lexpt_get_type xt in
      log_appendln (spf "App %s: t_out=%s, constr= %s" (sples e) (spt t_out) (spc (t_f, TFun (t_x, t_out))) );
      ((t_f, TFun (t_x, t_out)) :: (cs_f @ cs_x), AppT (ft, xt, t_out))
    )
    | Seq (e1, e2) -> (
      let cs1, et1 = typecheck_lexp env e1 in
      let cs2, et2 = typecheck_lexp env e2 in
      let t_e1 = lexpt_get_type et1 in
      log_appendln (spf "Seq %s: constr= %s" (sples e) (spc (t_e1, TUnit)) );
      ((t_e1, TUnit) :: cs1 :: cs2, et2)
    )
    | If (c, t, els) -> (
      let cs_c, ct = typecheck_lexp env c in
      let cs_t, tt = typecheck_lexp env t in
      let cs_e, et = typecheck_lexp env els in
      let t_c = lexpt_get_type ct in
      let t_t = lexpt_get_type tt in
      let t_e = lexpt_get_type et in
      log_appendln (spf "If %s: constr= %s, %s" (sples e) (spc (t_c, TInt)) (spc (t_t, t_e)) );
      ((t_c, TInt) :: (t_t, t_e) :: (cs_c @ cs_t @ cs_e), IfT (ct, tt, et, t_t))
    )
    | Letin (x, exp, b) -> (
      let t_letin = TVar (fresh_tvar ()) in
      let uuid = fresh_uuid () in
      let env' = (x, (schema_of_typ t_letin, uuid)) :: env in
      let cs_e, et = typecheck_lexp env exp in
      let cs_b, bt = typecheck_lexp env' b in
      let t_e = lexpt_get_type et in
      let t_b = lexpt_get_type bt in
      log_appendln (spf "Letin %s: t_letin=%s, uuid=%d, constr= %s" (sples e) (spt t_letin) uuid (spc (t_letin, t_e)) );
      ((t_letin, t_e) :: (cs_e @ cs_b), LetinT (x, uuid, et, bt, t_b))
    )
    | Letrecin (x, exp, b) -> (
      let t_letin = TVar (fresh_tvar ()) in
      let uuid = fresh_uuid () in
      let env' = (x, (schema_of_typ t_letin, uuid)) :: env in
      let cs_e, et = typecheck_lexp env' exp in
      let cs_b, bt = typecheck_lexp env' b in
      let t_e = lexpt_get_type et in
      let t_b = lexpt_get_type bt in
      log_appendln (spf "Letrecin %s: t_letin=%s, uuid=%d, constr= %s" (sples e) (spt t_letin) uuid (spc (t_letin, t_e)) );
      ((t_letin, t_e) :: (cs_e @ cs_b), LetrecinT (x, uuid, et, bt, t_b))
    )
    | LetinTuple (ids, exp, b) -> (
      (*uuid and tvar for each of the non blank tuple elements is needed all over the place 
        so here I create them once and then transform the list when needed*)
      let id_uuid_tv = List.map (fun id -> if id <> "_" then Some (id, fresh_uuid (), TVar (fresh_tvar ())) else None) ids in
      let env' = List.fold_left (fun envacc tup_opt-> 
                                  match tup_opt with
                                  | Some (id, uuid, tv) -> (id, (schema_of_typ tv, uuid)) :: envacc
                                  | None -> envacc
                                ) env id_uuid_tv in
      (*While the blanks dont need an uuid and their tvar not tracked, 
        it is most convinient to just put some tvar in the constructed type even for blanks*)
      let t_tup_constr = TTup (List.map (fun tup_opt -> match tup_opt with | Some (_, _, tv) -> tv | None -> fresh_tvar ()) id_uuid_tv) in
      let id_uuids = List.map (fun tup_opt -> match tup_opt with | Some (id, uuid, _) -> Some (id, uuid) | None -> None) id_uuid_tv in
      let cs_e, et = typecheck_lexp env exp in
      let cs_b, bt = typecheck_lexp env' b in
      let t_e = lexpt_get_type et in
      let t_b = lexpt_get_type bt in
      log_appendln (spf "LetinTupleT %s: (id, uuid, tv) = %s, constr= %s" (sples e) 
        (String.concat ", " (List.map (fun tup_opt -> match tup_opt with | Some (id, uuid, tv) -> spf "(%s, %d, %s)" id uuid (spt tv) | None -> "_") id_uuid_tv)) 
        (spc (t_e, t_tup_constr)) );
      ((t_e, t_tup_constr) :: (cs_e @ cs_b), LetinTupleT (id_uuids, et, bt, t_b))
    )
    | Tuple els -> (
      let cs_ls, etls = List.split (List.map (fun ei -> typecheck_lexp env ei) els) in
      let cs = List.flatten cs_ls in
      let t_tup = TTup (List.map (fun et -> lexpt_get_type et) etls) in
      log_appendln (spf "Tuple %s: " (sples e) );
      (cs, TupleT (etls, t_tup))
    )
    | I32Lit i -> ([], I32LitT (i, TI32))
    | I8Lit i -> ([], I8LitT (i, TI8))
    | UnitLit -> ([], UnitLitT (TUnit))
    | UopI32 (uop, e0) -> (
      let cs, et0 = typecheck_lexp env e0 in
      let t_e0 = lexpt_get_type et0 in
      log_appendln (spf "UopI32 %s: constr= %s" (sples e) (spc (t_e0, TI32)) );
      ((t_e0, TI32) :: cs, UopI32T (uop, et0, TI32))
    )
    | UopI8 (uop, e0) -> (
      let cs, et0 = typecheck_lexp env e0 in
      let t_e0 = lexpt_get_type et0 in
      log_appendln (spf "UopI8 %s: constr= %s" (sples e) (spc (t_e0, TI8)) );
      ((t_e0, TI8) :: cs, UopI8T (uop, et0, TI8))
    )
    | BopI32 (bop, e1, e2) -> (
      let cs1, e1t = typecheck_lexp env e1 in
      let cs2, e2t = typecheck_lexp env e2 in
      let t1 = lexpt_get_type e1t in
      let t2 = lexpt_get_type e2t in
      log_appendln (spf "BopI32 %s: constr= %s, %s" (sples e) (spc (t1, TI32)) (spc (t2, TI32)) );
      ((t1, TI32) :: (t2, TI32) :: (cs1 @ cs2), BopI32T (bop, e1t, e2t, TI32))
    )
    | BopI8 (bop, e1, e2) -> (
      let cs1, e1t = typecheck_lexp env e1 in
      let cs2, e2t = typecheck_lexp env e2 in
      let t1 = lexpt_get_type e1t in
      let t2 = lexpt_get_type e2t in
      log_appendln (spf "BopI8 %s: constr= %s, %s" (sples e) (spc (t1, TI8)) (spc (t2, TI8)) );
      ((t1, TI8) :: (t2, TI8) :: (cs1 @ cs2), BopI8T (bop, e1t, e2t, TI8))
    )
    | Veclit ls -> (
      if List.length ls = 0 then raise (Errors.TypeError "Cannot infer type of empty vector literal, how did this get past the parser?") 
      else
        let cs_ls, ls_t = List.split (List.map (fun lexp_i -> typecheck_lexp env lexp_i) ls) in
        let cs = List.flatten cs_ls in
        let t_elem = TVar (fresh_tvar ()) in (*while a new TVar is not strictly needed here, it does lend itself better to ocaml programming imo*)
        let elem_constraints = List.map (fun i_t -> (lexpt_get_type i_t, t_elem)) ls_t in
        log_appendln (spf "Veclit %s: t_elem=%s, constr= %s" (sples e) (spt t_elem) (spcs elem_constraints) );
        (elem_constraints @ cs, VeclitT (ls_t, (TVec t_elem)))
    )
    | Vecmk (defval, size_list) -> (
      let cs_defval, defval_t = typecheck_lexp env defval in
      let t_defval = lexpt_get_type defval_t in
      let cs_sizerec_nf, size_list_t = List.split (List.map (fun ei -> typecheck_lexp env ei) size_list) in
      let cs_sizerec = List.flatten cs_sizerec_nf in
      let cs_size = List.map (fun et -> (lexpt_get_type et, TI32)) size_list_t in
      let t_vec_constr = List.fold_left (fun acc et -> TVec acc) t_defval size_list_t in
      log_appendln (spf "Vecmk %s: constr= %s" (sples e) (spcs cs_size) );
      (cs_size @ cs_sizerec @ cs_defval, VecmkT (defval_t, size_list_t, t_vec_constr))
    )
    | Veclen v -> (
      let cs_v, v_t = typecheck_lexp env v in
      let t_v = lexpt_get_type v_t in
      let t_vec_of = TVec(TVar (fresh_tvar ())) in
      log_appendln (spf "Veclen %s: vec_of=%s, constr= %s" (sples e) (spt t_vec_of) (spc (t_v, t_vec_of)) );
      ((t_v, t_vec_of) :: cs_v, VeclenT (v_t, TInt))
    )
    | Vecget (v, idx_list) -> (
      let cs_v, v_t = typecheck_lexp env v in
      let t_v = lexpt_get_type v_t in
      let cs_idxrec_nf, idx_list_t = List.split (List.map (fun ei -> typecheck_lexp env ei) idx_list) in
      let cs_idxrec = List.flatten cs_idxrec_nf in
      let cs_idx = List.map (fun et -> (lexpt_get_type et, TI32)) idx_list_t in
      let t_vec_of = TVar (fresh_tvar ()) in
      let t_vec_constr = List.fold_left (fun acc et -> TVec acc) t_vec_of idx_list_t in
      log_appendln (spf "Vecget %s: t_vec_of=%s, constr= %s, %s" (sples e) (spt t_vec_of) (spcs ((t_v, t_vec_constr) :: cs_idx)) );
      ((t_v, t_vec_constr) :: (cs_idx @ cs_idxrec @ cs_v), VecgetT (v_t, idx_list_t, t_vec_of))
    )
    | Vecset (v, value, idx_list) -> (
      let cs_v, v_t = typecheck_lexp env v in
      let t_v = lexpt_get_type v_t in
      let cs_val, val_t = typecheck_lexp env value in
      let t_val = lexpt_get_type val_t in
      let cs_idxrec_nf, idx_list_t = List.split (List.map (fun ei -> typecheck_lexp env ei) idx_list) in
      let cs_idxrec = List.flatten cs_idxrec_nf in
      let cs_idx = List.map (fun et -> (lexpt_get_type et, TI32)) idx_list_t in
      let t_vec_of = TVar (fresh_tvar ()) in
      let t_vec_constr = List.fold_left (fun acc et -> TVec acc) t_vec_of idx_list_t in
      log_appendln (spf "Vecset %s: t_vec_of=%s, constr= %s" (sples e) (spt t_vec_of) (spcs ((t_v, t_vec_constr) :: (t_val, t_vec_of) :: cs_idx)) );
      ((t_v, t_vec_constr) :: (t_val, t_vec_of) :: (cs_idx @ cs_idxrec @ cs_val @ cs_v), VecsetT (v_t, val_t, idx_list_t, t_vec_constr))
    )
    | Vecresz (v, newlen, idx_list) -> (
      let cs_v, v_t = typecheck_lexp env v in
      let t_v = lexpt_get_type v_t in
      let cs_newlen, newlen_t = typecheck_lexp env newlen in
      let t_newlen = lexpt_get_type newlen_t in
      let cs_idxrec_nf, idx_list_t = List.split (List.map (fun ei -> typecheck_lexp env ei) idx_list) in
      let cs_idxrec = List.flatten cs_idxrec_nf in
      let cs_idx = List.map (fun et -> (lexpt_get_type et, TI32)) idx_list_t in
      let t_vec_of = TVar (fresh_tvar ()) in
      let t_vec_constr = List.fold_left (fun acc et -> TVec acc) t_vec_of idx_list_t in
      log_appendln (spf "Vecresz %s: t_vec_of=%s, constr= %s" (sples e) (spt t_vec_of) (spcs ((t_v, t_vec_constr) :: (t_newlen, TI32) :: cs_idx)) );
      ((t_v, t_vec_constr) :: (t_newlen, TI32) :: (cs_idx @ cs_idxrec @ cs_newlen @ cs_v), VecreszT (v_t, newlen_t, idx_list_t, t_vec_constr))
    )

let typecheck_let (id: string) (e: lexp) (env : typenv) : typenv * polytast =

  log_appendln "------------------------ LET -------------------------";
  
  (*interate left expression*)
  let cs, et = typecheck_lexp env e in

  (*unify constraints*)
  List.iter (fun (t1, t2) -> 
    unify t1 t2;
  ) cs;

  (*generalize*)
  let t_e = lexpt_get_type et in
  let s = generalize t_e in
  let genvars = genvars_of_schema s in

  (*gen uuid*)
  let uuid = fresh_uuid () in

  ( (id, (s, uuid)) :: env, [(id, uuid, genvars, et)])


let typecheck_letrecblk (letblk : (string * lexp) list) (env : typenv) : typenv * polytast =
  log_appendln "----------------------- LETBLK -----------------------";

  (*add all let defs to env*)
  let env_with_letdefs = List.fold_left 
    (fun env' (name, lexp) -> 
      let let_tvar = TVar (fresh_tvar ()) in
      let uuid = fresh_uuid () in
      (name, (schema_of_typ let_tvar, uuid)) :: env'
    )  
    env letblk in
  
  (*interate AST*)
  let constraints, lexptblk = List.fold_left 
    (fun (cs,ltb) (name, lexp) -> 
      let (Forall (_, let_tvar), uuid) = List.assoc name env_with_letdefs in (*it is impossible to have something generalized here*)
      log_appendln (spf "let %s = : tvar=%s, uuid=%d" name (spt let_tvar) uuid);
      let cs', lt = typecheck_lexp env_with_letdefs lexp  in (*we add this binding in the step before, it must exist so no need to check *)
      let t_l = lexpt_get_type lt in
      ((let_tvar, t_l) :: (cs' @ cs), ltb @ [(name, uuid, [], lt)])
    )
    ([],[]) letblk in

  (*unify all constraints*)
  List.iter (fun (t1, t2) -> 
    unify t1 t2;
  ) constraints;

  (*generalize all types and add to env*)
  let generalized_env, lexptblk_gen = List.fold_right 
    (fun (name, uuid, vars, lt) (env', letblkt')  -> 
      let (olds, _) = List.assoc name env_with_letdefs in
      let tv = typ_of_schema olds in
      let s = generalize tv in
      let genvars = genvars_of_schema s in
      ( (name, (s, uuid)) :: env', (name, uuid, genvars, lt) :: letblkt')
    )
    lexptblk (env, [])
  in
  (generalized_env, lexptblk_gen)

let typecheck (ast : ast) : polytast =
  (*reset counters for consistency*)
  tvar_counter := 0;
  uuid_counter := 0;
  log := "";
  log_appendln "----------------------- TYPECHECK -----------------------";

  try
    (*iterate AST*)
    let env, tast = List.fold_left 
      (fun (env, tast) stmt -> 
        match stmt with
        | Let (id, e) -> (
            let env', tast' = typecheck_let id e env in
            (env', tast @ tast')
          )
        | Letrec llst -> (
            let env', tast' = typecheck_letrecblk llst env in
            (env', tast @ tast'))
          )
        | _ -> raise (Errors.TypeError "encountered Include AST node in typechecker, probably a include pass bug")
    (gen_builtins (), []) ast in
    
    (*Make sure there is no main or main is of type unit -> unit*)
    match List.assoc_opt "main" env with
    | Some (Forall ([], TFun(TUnit, TUnit)), _) ->(
      log_appendln "Typechecking successful!";
      if print_log then Printf.printf "\n%s\n" !log;
      (letblkt @ letblkt_main, List.rev env_main)
    )
    | Some (s,_) -> raise (Errors.TypeError ("main has type " ^ sprint_schema s ^ " but expected with type: unit -> unit"))
    | None -> (
      log_appendln "Typechecking successful! (no main)";
      if print_log then Printf.printf "\n%s\n" !log;
      (letblkt, List.rev env)
    )

  with Errors.TypeError msg -> (
    if print_log then Printf.printf "\n%s\n" !log;
    raise (Errors.TypeError ("Type Error: " ^ msg))
  )