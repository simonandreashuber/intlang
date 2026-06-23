open Ast
open Errors
open PrintIntlang

(*CONFIGURATION*)

let print_log = false

module SSet = Set.Make(String)

let log = ref ""

let instreg = ref []

let log_append (s : string) : unit =
  log := !log ^ s

let log_appendln (s : string) : unit =
  log := !log ^ s ^ "\n"

(*some short hands to make the code less convoluted*)
let sple = PrintIntlang.sprint_lexp
let sples = PrintIntlang.sprint_lexp_shallow 2
let spt = PrintIntlang.sprint_typ
let spc = PrintIntlang.sprint_constraint
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
  instreg := (uuid, fresh_tvar_map) :: !instreg;
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
    | App (f, x) -> (
      let t_out = TVar (fresh_tvar ()) in
      let cs_f, ft = typecheck_lexp env f in
      let cs_x, xt = typecheck_lexp env x in
      let t_f = lexpt_get_type ft in
      let t_x = lexpt_get_type xt in
      log_appendln (spf "App %s: t_out=%s, constr= %s" (sples e) (spt t_out) (spc (t_f, TFun (t_x, t_out))) );
      ((t_f, TFun (t_x, t_out)) :: (cs_f @ cs_x), AppT (ft, xt, t_out))
      )
    | Int i -> ([], IntT (i, TInt))
    | Bop (bop, e1, e2) -> (
      let cs1, e1t = typecheck_lexp env e1 in
      let cs2, e2t = typecheck_lexp env e2 in
      let t1 = lexpt_get_type e1t in
      let t2 = lexpt_get_type e2t in
      log_appendln (spf "Bop %s: constr= %s, %s" (sples e) (spc (t1, TInt)) (spc (t2, TInt)) );
      ((t1, TInt) :: (t2, TInt) :: (cs1 @ cs2), BopT (bop, e1t, e2t, TInt)) (*decided against a TInt and TIntNonZero type since it does complicated things and I prefer to just user runtime checks anyways*)
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
      let cs_e, et = typecheck_lexp env' exp in
      let cs_b, bt = typecheck_lexp env' b in
      let t_e = lexpt_get_type et in
      let t_b = lexpt_get_type bt in
      log_appendln (spf "Letin %s: t_letin=%s, uuid=%d, constr= %s" (sples e) (spt t_letin) uuid (spc (t_letin, t_e)) );
      ((t_letin, t_e) :: (cs_e @ cs_b), LetinT (x, uuid, et, bt, t_b))
      )
    | Veclit ls -> (
      if List.length ls = 0 then raise (Errors.TypeError "Cannot infer type of empty vector literal, how did this get past the parser?") 
      else
        let cs_ls, ls_t = List.split (List.map (fun lexp_i -> typecheck_lexp env lexp_i) ls) in
        let cs = List.flatten cs_ls in
        let t_elem = TVar (fresh_tvar ()) in (*while a new TVar is not strictly needed here, it does lend itself better to ocaml programming imo*)
        let elem_constraints = List.map (fun i_t -> (lexpt_get_type i_t, t_elem)) ls_t in
        log_appendln (spf "Veclit %s: t_elem=%s, constr= %s" (sples e) (spt t_elem) (String.concat ", " (List.map (fun c -> spc c) elem_constraints)) );
        (elem_constraints @ cs, VeclitT (ls_t, (TVec t_elem)))
      )
    | Vecmk (defval, count) -> (
      let cs_defval, defval_t = typecheck_lexp env defval in
      let cs_count, count_t = typecheck_lexp env count in
      let t_defval = lexpt_get_type defval_t in
      let t_count = lexpt_get_type count_t in
      log_appendln (spf "Vecmk %s: constr= %s" (sples e) (spc (t_count, TInt)));
      ((t_count, TInt) :: (cs_defval @ cs_count), VecmkT (defval_t, count_t, TVec (t_defval)))
    )
    | Veclen v -> (
      let cs_v, v_t = typecheck_lexp env v in
      let t_v = lexpt_get_type v_t in
      let t_vec_of = TVec(TVar (fresh_tvar ())) in
      log_appendln (spf "Veclen %s: vec_of=%s, constr= %s" (sples e) (spt t_vec_of) (spc (t_v, t_vec_of)) );
      ((t_v, t_vec_of) :: cs_v, VeclenT (v_t, TInt))
    )
    | Vecget (v, i) -> (
      let cs_v, v_t = typecheck_lexp env v in
      let cs_i, i_t = typecheck_lexp env i in
      let t_v = lexpt_get_type v_t in
      let t_i = lexpt_get_type i_t in
      let t_vec_of = TVar (fresh_tvar ()) in
      let t_vec = TVec t_vec_of in
      log_appendln (spf "Vecget %s: t_vec_of=%s, constr= %s, %s" (sples e) (spt t_vec_of) (spc (t_v, t_vec)) (spc (t_i, TInt)));
      ((t_v, t_vec) :: (t_i, TInt) :: cs_v @ cs_i, VecgetT (v_t, i_t, t_vec_of))
    )
    | Vecset (v, i, value) -> (
      let cs_v, v_t = typecheck_lexp env v in
      let cs_i, i_t = typecheck_lexp env i in
      let cs_val, val_t = typecheck_lexp env value in
      let t_v = lexpt_get_type v_t in
      let t_i = lexpt_get_type i_t in
      let t_val = lexpt_get_type val_t in
      let t_vec_of = TVar (fresh_tvar ()) in
      let t_vec = TVec t_vec_of in
      log_appendln (spf "Vecset %s: t_vec_of=%s, constr= %s, %s, %s" (sples e) (spt t_vec_of) (spc (t_v, t_vec)) (spc (t_i, TInt)) (spc (t_val, t_vec_of)));
      ((t_v, t_vec) :: (t_i, TInt) :: (t_val, t_vec_of) :: cs_v @ cs_i @ cs_val, VecsetT (v_t, i_t, val_t, t_vec))
    )

let typecheck_letblk (letblk : letblk) (env : typenv) (letblkid : int) : typenv * letblkpolyt =
  (*debug things*)
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
  List.iteri (fun i (t1, t2) -> 
    unify t1 t2;
  ) constraints;

  (*generalize all types and add to env*)
  let generalized_env, lexptblk_gen = List.fold_right 
    (fun (name, uuid, vars, lt) (env', letblkt')  -> 
      let (Forall (_, tv), _) = List.assoc name env_with_letdefs in
      let s = generalize tv in
      let Forall (genvars, _) = s in
      ( (name, (s, uuid)) :: env', (name, uuid, genvars, lt) :: letblkt')
    )
    lexptblk (env, [])
  in
  (generalized_env, lexptblk_gen)

(*
  expects: a program (with all Nlexp except for the last one being a lexp)
  returns: unit if type checks, otherwise raises Errors.TypeError with an error message
*)
let typecheck ((global_letblk, mainlexp_opt) : prog) : letblkpolyt * typenv =
  (*reset counters for consistency*)
  tvar_counter := 0;
  uuid_counter := 0;
  log := "";
  instreg := [];
  log_appendln "----------------------- TYPECHECK -----------------------";

  try
    (*split letblk into scc*) 
    let letscc = scc_split_letblk global_letblk in
    log_appendln  (sprint_scc letscc);

    (*typecheck each scc*)
    let scc_cnt = ref 0 in
    let env, letblkt = List.fold_left (fun (env,letblkt) letblk -> (*funny I thought the let and let rec are not needed, but this code would now work without the distinction*)
      let env', letblkt' = typecheck_letblk letblk env (scc_cnt := !scc_cnt + 1; !scc_cnt) in
      (env', letblkt @ letblkt')
    ) ([], []) letscc in
    
    (*typecheck the main expression*)
    if mainlexp_opt != None then (
      let mainlexp = Option.get mainlexp_opt in
      let env_main, letblkt_main = typecheck_letblk [("@main", mainlexp)] env (scc_cnt := !scc_cnt + 1; !scc_cnt) in
      match List.assoc_opt "@main" env_main with
        | Some (Forall ([], TInt), _) ->(
          log_appendln "Typechecking successful!";
          if print_log then Printf.printf "\n%s\n" !log;
          (letblkt @ letblkt_main, List.rev env_main)
        )
        | Some (s,_) -> raise (Errors.TypeError ("Final expression has type " ^ sprint_schema s ^ " but expected int (this is intlang ;))"))
        | None -> raise (Errors.TypeError "Internal Error")
    )
    else (
      if print_log then Printf.printf "\n%s\n" !log;
      (letblkt, List.rev env)
    )

  with Errors.TypeError msg -> (
    if print_log then Printf.printf "\n%s\n" !log;
    raise (Errors.TypeError ("Type Error: " ^ msg))
  )