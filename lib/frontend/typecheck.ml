(*

  Type Checks the AST returning a Polymorphic Typed AST (TAST)

*)


open Ast
open Errors
open PrintIntlang

let print_log = false

module SSet = Set.Make(String)

let log = ref ""

let log_append (s : string) : unit =
  log := !log ^ s

let log_appendln (s : string) : unit =
  log := !log ^ s ^ "\n"

(*some short hands to make the code less convoluted*)
let sple = PrintIntlang.sprint_lexp 0
let sples = PrintIntlang.sprint_lexp_wdepth (Some 2) 0
let spt = PrintIntlang.sprint_typ
let spc = PrintIntlang.sprint_constraint

let spcs (cs : constraints) : string = String.concat ", " (List.map (fun c -> spc c) cs)
let spf = Printf.sprintf

let rec unify (t1 : typ) (t2 : typ) : unit =
  (*you put a tvar v into this that has a link => not good*)
  let rec occurscheck (v : tvar) (t : typ) : unit =
      match repr t with
      | TUnit -> ()
      | TI32 -> ()
      | TI8 -> ()
      | TFun (t1, t2) -> occurscheck v t1; occurscheck v t2
      | TTup ts -> List.iter (occurscheck v) ts
      | TVec t_inner -> occurscheck v t_inner
      | TVar v' ->  if v.id = v'.id then 
                      raise (Errors.TypeError ("[occurscheck] Occurs Check Failed: Recursive types are not allowed. Found tvar: t" ^ string_of_int v.id ^ " again")) 
                    else 
                      ()
  in

  match (repr t1, repr t2) with
  | (TUnit, TUnit) -> ()
  | (TI32, TI32) -> ()
  | (TI8, TI8) -> ()
  | (TFun (t1f, t1x), TFun (t2f, t2x)) -> 
      unify t1f t2f;
      unify t1x t2x
  | (TTup ts1, TTup ts2) ->
    if List.length ts1 <> List.length ts2 then
      raise (Errors.TypeError ("Type mismatch: Cannot unify " ^ spt (repr t1) ^ " with " ^ spt (repr t2) ^ " because they have different lengths"))
    else
      List.iter2 unify ts1 ts2
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
  | _ -> raise (Errors.TypeError ("Type mismatch: Cannot unify " ^ spt (repr t1) ^ " with " ^ spt (repr t2)))  

let generalize (t : typ) : schema =
  let rec freevars (t : typ) : int list =
    match repr t with
    | TUnit -> []
    | TI32 -> []
    | TI8 -> []
    | TFun (t1, t2) -> List.sort_uniq compare ((freevars t1) @ (freevars t2 ))
    | TTup ts -> List.sort_uniq compare (List.flatten (List.map freevars ts))
    | TVec t_inner -> freevars t_inner
    | TVar v -> [v.id]
  in
  Forall (freevars t, t)

let instantiate (Forall (vars, t) : schema) : typ =
  let fresh_tvar_map = List.map (fun var -> var, fresh_tvar ()) vars in
  let rec instaux (varmap : (int * tvar) list) (t: typ) : typ =
    match repr t with
      | TUnit -> TUnit
      | TI32 -> TI32
      | TI8 -> TI8
      | TFun (t1, t2) -> TFun (instaux varmap t1, instaux varmap t2)
      | TTup ts -> TTup (List.map (instaux varmap) ts)
      | TVec t_inner -> TVec (instaux varmap t_inner)
      | TVar v -> (
          match List.assoc_opt v.id varmap with
            | Some fresh_t -> TVar fresh_t
            | None -> t)
  in 
  instaux fresh_tvar_map t


let rec typecheck_lexp (env : typenv) (e : lexp) : constraints * tlexp = 
  match e with
    | Var x -> (
      match List.assoc_opt x env with
      | Some (s, uuid) ->
          let t = instantiate s in
          ([], VarT (ref x, ref uuid, t))
      | None -> raise (Errors.TypeError ("Unbound variable: " ^ x))
      )
    | Lam (x, inT_opt, outT_opt, b) -> (
      let tv = TVar (fresh_tvar ()) in
      let uuid = fresh_uuid () in
      let env' = (x, (Forall ([], tv), uuid)) :: env in (*it is enough to stitch it to the front as assoc_opt just finds the first one*)
      let cs, bt = typecheck_lexp env' b in
      let t_body = tlexp_get_type bt in
      let cs_inT = match inT_opt with | Some t -> [(t, tv)] | None -> [] in
      let cs_outT = match outT_opt with | Some t -> [(t, t_body)] | None -> [] in
      log_appendln (spf "Lam %s: uuid=%d, tv=%s. constr= %s" (sples e) uuid (spt tv) (spcs @@ cs_inT @ cs_outT));
      (cs_inT @ cs_outT @ cs, LamT (x, uuid, bt, TFun (tv, t_body)))
      )
    | LamUnit b -> (
      let cs, bt = typecheck_lexp env b in
      let t_body = tlexp_get_type bt in
      log_appendln (spf "LamUnit %s:" (sples e));
      (cs, LamUnitT (bt, TFun (TUnit, t_body)))
    )
    | App (f, x) -> (
      let t_out = TVar (fresh_tvar ()) in
      let cs_f, ft = typecheck_lexp env f in
      let cs_x, xt = typecheck_lexp env x in
      let t_f = tlexp_get_type ft in
      let t_x = tlexp_get_type xt in
      log_appendln (spf "App %s: t_out=%s, constr= %s" (sples e) (spt t_out) (spc (t_f, TFun (t_x, t_out))) );
      ((t_f, TFun (t_x, t_out)) :: (cs_f @ cs_x), AppT (ft, xt, t_out))
    )
    | Seq (e1, e2) -> (
      let cs1, et1 = typecheck_lexp env e1 in
      let cs2, et2 = typecheck_lexp env e2 in
      let t_e1 = tlexp_get_type et1 in
      let t_e2 = tlexp_get_type et2 in
      log_appendln (spf "Seq %s: constr= %s" (sples e) (spc (t_e1, TUnit)) );
      ((t_e1, TUnit) :: cs1 @ cs2, SeqT( et1, et2, t_e2))
    )
    | If (c, t, els) -> (
      let cs_c, ct = typecheck_lexp env c in
      let cs_t, tt = typecheck_lexp env t in
      let cs_e, et = typecheck_lexp env els in
      let t_c = tlexp_get_type ct in
      let t_t = tlexp_get_type tt in
      let t_e = tlexp_get_type et in
      log_appendln (spf "If %s: constr= %s, %s" (sples e) (spc (t_c, TI32)) (spc (t_t, t_e)) );
      ((t_c, TI32) :: (t_t, t_e) :: (cs_c @ cs_t @ cs_e), IfT (ct, tt, et, t_t))
    )
    | Letin (x, exp, b) -> (
      let t_letin = TVar (fresh_tvar ()) in
      let uuid = fresh_uuid () in
      let env' = (x, (schema_of_typ t_letin, uuid)) :: env in
      let cs_e, et = typecheck_lexp env exp in
      let cs_b, bt = typecheck_lexp env' b in
      let t_e = tlexp_get_type et in
      let t_b = tlexp_get_type bt in
      log_appendln (spf "Letin %s: t_letin=%s, uuid=%d, constr= %s" (sples e) (spt t_letin) uuid (spc (t_letin, t_e)) );
      ((t_letin, t_e) :: (cs_e @ cs_b), LetinT (x, uuid, et, bt, t_b))
    )
    | Letrecin (x, exp, b) -> (
      let t_letin = TVar (fresh_tvar ()) in
      let uuid = fresh_uuid () in
      let env' = (x, (schema_of_typ t_letin, uuid)) :: env in
      let cs_e, et = typecheck_lexp env' exp in
      let cs_b, bt = typecheck_lexp env' b in
      let t_e = tlexp_get_type et in
      let t_b = tlexp_get_type bt in
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
      let t_tup_constr = TTup (List.map (fun tup_opt -> match tup_opt with | Some (_, _, tv) -> tv | None -> TVar (fresh_tvar ())) id_uuid_tv) in
      let id_uuids = List.map (fun tup_opt -> match tup_opt with | Some (id, uuid, _) -> Some (id, uuid) | None -> None) id_uuid_tv in
      let cs_e, et = typecheck_lexp env exp in
      let cs_b, bt = typecheck_lexp env' b in
      let t_e = tlexp_get_type et in
      let t_b = tlexp_get_type bt in
      log_appendln (spf "LetinTupleT %s: (id, uuid, tv) = %s, constr= %s" (sples e) 
        (String.concat ", " (List.map (fun tup_opt -> match tup_opt with | Some (id, uuid, tv) -> spf "(%s, %d, %s)" id uuid (spt tv) | None -> "_") id_uuid_tv)) 
        (spc (t_e, t_tup_constr)) );
      ((t_e, t_tup_constr) :: (cs_e @ cs_b), LetinTupleT (id_uuids, et, bt, t_b))
    )
    | Tuple els -> (
      let cs_ls, etls = List.split (List.map (fun ei -> typecheck_lexp env ei) els) in
      let cs = List.flatten cs_ls in
      let t_tup = TTup (List.map (fun et -> tlexp_get_type et) etls) in
      log_appendln (spf "Tuple %s: " (sples e) );
      (cs, TupleT (etls, t_tup))
    )
    | I32Lit i -> ([], I32LitT (i, TI32))
    | I8Lit i -> ([], I8LitT (i, TI8))
    | UnitLit -> ([], UnitLitT (TUnit))
    | UopI32 (uop, e0) -> (
      let cs, et0 = typecheck_lexp env e0 in
      let t_e0 = tlexp_get_type et0 in
      log_appendln (spf "UopI32 %s: constr= %s" (sples e) (spc (t_e0, TI32)) );
      ((t_e0, TI32) :: cs, UopI32T (uop, et0, TI32))
    )
    | UopI8 (uop, e0) -> (
      let cs, et0 = typecheck_lexp env e0 in
      let t_e0 = tlexp_get_type et0 in
      log_appendln (spf "UopI8 %s: constr= %s" (sples e) (spc (t_e0, TI8)) );
      ((t_e0, TI8) :: cs, UopI8T (uop, et0, TI8))
    )
    | BopI32 (bop, e1, e2) -> (
      let cs1, e1t = typecheck_lexp env e1 in
      let cs2, e2t = typecheck_lexp env e2 in
      let t1 = tlexp_get_type e1t in
      let t2 = tlexp_get_type e2t in
      log_appendln (spf "BopI32 %s: constr= %s, %s" (sples e) (spc (t1, TI32)) (spc (t2, TI32)) );
      ((t1, TI32) :: (t2, TI32) :: (cs1 @ cs2), BopI32T (bop, e1t, e2t, TI32))
    )
    | BopI8 (bop, e1, e2) -> (
      let cs1, e1t = typecheck_lexp env e1 in
      let cs2, e2t = typecheck_lexp env e2 in
      let t1 = tlexp_get_type e1t in
      let t2 = tlexp_get_type e2t in
      let ret_typ = if bop = Eqi8 || bop = Neqi8 || bop = Lti8 || bop = Gti8 || bop = LtEqi8 || bop = GtEqi8 then TI32 else TI8 in
      log_appendln (spf "BopI8 %s: constr= %s, %s" (sples e) (spc (t1, TI8)) (spc (t2, TI8)) );
      ((t1, TI8) :: (t2, TI8) :: (cs1 @ cs2), BopI8T (bop, e1t, e2t, ret_typ))
    )
    | VecLit ls -> (
      let cs_ls, ls_t = List.split (List.map (fun lexp_i -> typecheck_lexp env lexp_i) ls) in
      let cs = List.flatten cs_ls in
      let t_elem = TVar (fresh_tvar ()) in
      let elem_constraints = List.map (fun i_t -> (tlexp_get_type i_t, t_elem)) ls_t in
      log_appendln (spf "Veclit %s: t_elem=%s, constr= %s" (sples e) (spt t_elem) (spcs elem_constraints) );
      (elem_constraints @ cs, VecLitT (ls_t, (TVec t_elem)))
    )
    | Vecmk (defval, size_list) -> (
      let cs_defval, defval_t = typecheck_lexp env defval in
      let t_defval = tlexp_get_type defval_t in
      let cs_sizerec_nf, size_list_t = List.split (List.map (fun ei -> typecheck_lexp env ei) size_list) in
      let cs_sizerec = List.flatten cs_sizerec_nf in
      let cs_size = List.map (fun et -> (tlexp_get_type et, TI32)) size_list_t in
      let t_vec_constr = List.fold_left (fun acc et -> TVec acc) t_defval size_list_t in
      log_appendln (spf "Vecmk %s: constr= %s" (sples e) (spcs cs_size) );
      (cs_size @ cs_sizerec @ cs_defval, VecmkT (defval_t, size_list_t, t_vec_constr))
    )
    | Veclen v -> (
      let cs_v, v_t = typecheck_lexp env v in
      let t_v = tlexp_get_type v_t in
      let t_vec_constr = TVec(TVar (fresh_tvar ())) in
      log_appendln (spf "Veclen %s: vec_of=%s, constr= %s" (sples e) (spt t_vec_constr) (spc (t_v, t_vec_constr)) );
      ((t_v, t_vec_constr) :: cs_v, VeclenT (v_t, TI32))
    )
    | Vecget (v, idx_list) -> (
      let cs_v, v_t = typecheck_lexp env v in
      let t_v = tlexp_get_type v_t in
      let cs_idxrec_nf, idx_list_t = List.split (List.map (fun ei -> typecheck_lexp env ei) idx_list) in
      let cs_idxrec = List.flatten cs_idxrec_nf in
      let cs_idx = List.map (fun et -> (tlexp_get_type et, TI32)) idx_list_t in
      let t_vec_of = TVar (fresh_tvar ()) in
      let t_vec_constr = List.fold_left (fun acc et -> TVec acc) t_vec_of idx_list_t in
      log_appendln (spf "Vecget %s: t_vec_of=%s, constr= %s" (sples e) (spt t_vec_of) (spcs ((t_v, t_vec_constr) :: cs_idx)) );
      ((t_v, t_vec_constr) :: (cs_idx @ cs_idxrec @ cs_v), VecgetT (v_t, idx_list_t, t_vec_of))
    )
    | Vecset (v, value, idx_list) -> (
      let cs_v, v_t = typecheck_lexp env v in
      let t_v = tlexp_get_type v_t in
      let cs_val, val_t = typecheck_lexp env value in
      let t_val = tlexp_get_type val_t in
      let cs_idxrec_nf, idx_list_t = List.split (List.map (fun ei -> typecheck_lexp env ei) idx_list) in
      let cs_idxrec = List.flatten cs_idxrec_nf in
      let cs_idx = List.map (fun et -> (tlexp_get_type et, TI32)) idx_list_t in
      let t_vec_of = TVar (fresh_tvar ()) in
      let t_vec_constr = List.fold_left (fun acc et -> TVec acc) t_vec_of idx_list_t in
      log_appendln (spf "Vecset %s: t_vec_of=%s, constr= %s" (sples e) (spt t_vec_of) (spcs ((t_v, t_vec_constr) :: (t_val, t_vec_of) :: cs_idx)) );
      ((t_v, t_vec_constr) :: (t_val, t_vec_of) :: (cs_idx @ cs_idxrec @ cs_val @ cs_v), VecsetT (v_t, val_t, idx_list_t, t_vec_constr))
    )
    | Vecslice(v, start, len) -> (
      let cs_v, v_t = typecheck_lexp env v in
      let t_v = tlexp_get_type v_t in
      let cs_start, start_t = typecheck_lexp env start in
      let t_start = tlexp_get_type start_t in
      let cs_len, len_t = typecheck_lexp env len in
      let t_len = tlexp_get_type len_t in
      let t_vec_of = TVar (fresh_tvar ()) in
      let t_vec_constr = TVec (t_vec_of) in
      log_appendln (spf "Vecslice %s: t_vec_of=%s, constr= %s" (sples e) (spt t_vec_of) (spcs ((t_v, t_vec_constr) :: (t_start, TI32) :: (t_len, TI32) :: [])) );
      ((t_v, t_vec_constr) :: (t_start, TI32) :: (t_len, TI32) :: (cs_len @ cs_start @ cs_v), VecsliceT (v_t, start_t, len_t, t_vec_constr))
    )
    | Vecextend(v, lit, off) -> (
      let cs_v, v_t = typecheck_lexp env v in
      let t_v = tlexp_get_type v_t in
      let cs_lit, lit_t = typecheck_lexp env lit in
      let t_lit = tlexp_get_type lit_t in
      let cs_off, off_t = typecheck_lexp env off in
      let t_off = tlexp_get_type off_t in
      let t_vec_of = TVar (fresh_tvar ()) in
      let t_vec_constr = TVec (t_vec_of) in
      log_appendln (spf "Vecextend %s: t_vec_of=%s, constr= %s" (sples e) (spt t_vec_of) (spcs ((t_v, t_vec_constr) :: (t_lit, t_vec_of) :: (t_off, TI32) :: [])) );
      ((t_v, t_vec_constr) :: (t_lit, t_vec_of) :: (t_off, TI32) :: (cs_off @ cs_lit @ cs_v), VecextendT (v_t, lit_t, off_t, t_vec_constr))
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
  let t_e = tlexp_get_type et in
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
      let t_l = tlexp_get_type lt in
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
            (env', tast @ tast')
          )
        | _ -> raise (Errors.TypeError "encountered Include AST node in typechecker, probably a include pass bug"))
    (Ast.builtins, []) ast in
    
    (*Make sure there is no main or main is of type unit -> unit*)
    match List.assoc_opt "main" env with
    | Some (Forall ([], maintyp), _) when cmp_typ maintyp (TFun (TUnit, TUnit)) -> (
      log_appendln "Typechecking successful!";
      if print_log then Printf.printf "\n%s\n" !log;
      tast
    )
    | Some (s,_) -> raise (Errors.TypeError ("main has type " ^ sprint_schema s ^ " but expected with type: unit -> unit"))
    | None -> (
      log_appendln "Typechecking successful! (no main)";
      if print_log then Printf.printf "\n%s\n" !log;
      tast
    )

  with Errors.TypeError msg -> (
    if print_log then Printf.printf "\n%s\n" !log;
    raise (Errors.TypeError ("Type Error: " ^ msg))
  )