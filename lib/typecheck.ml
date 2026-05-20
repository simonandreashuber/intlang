open Ast

type typ =
  | TInt
  | TFun of typ * typ
  | TVar of tvar
  
and tvar = {
  id : int;
  (*mutable level : int;
    A note on levels: If one was to introduce a "let in" concept, then one would need a level system to prevent
                      the generalization of some vars that are fixed by higher scopes, but since Intlang does not have this.....
  *)
  mutable link : typ option; (* None = unsolved, Some t = solved *)
}

type schema = Forall of int list * typ

type constraints = (typ * typ) list

type type_env = (string * schema) list

type letblk = (string * Ast.lexp) list

(*
  allow recursive types
  it is on by default but there is the switch here to turn it of for some sanity ;)
*)
let allowrectypes = ref true

exception TypeError of string

let counter = ref 0

let fresh_tvar : tvar =
  let id = !counter in
  counter := id + 1;
  { id; link = None }

let repr (t : typ) : typ =
  let rec aux t visited =
    match t with
    | TVar {id; link = Some t_linked; _ } -> 
        if List.mem id visited then
          if !allowrectypes then t else raise (TypeError "Recursive types are disabled (you can enable them)")
        else
          aux t_linked (id :: visited)
    | _ -> t
  in aux t []

let sprint_typ (t : typ) : string =
  let rec aux (t : typ)  (vis : (int * int) list) : string * (int* int) list =
    match t with
    | TInt -> "int", vis
    | TFun (t1, t2) -> (
        let t1str, vis_t1 = aux t1 vis in
        let left = match t1 with
          | TFun _ -> "(" ^ t1str ^ ")"
          | _ -> t1str
        in
        let t2str, vis_t2 = aux t2 vis_t1 in
        left ^ " -> " ^ t2str, vis_t2
    )
    | TVar {id; link = Some t_linked; _ } -> 
        (
          match List.assoc_opt id vis with
            | Some count ->
                if !allowrectypes then "t" ^ string_of_int id, (id, count+1) :: vis
                else raise (TypeError "Recursive types are disabled (you can enable them)")
            | None -> (
              let t_linked_str, vis_t_linked = aux t_linked ((id, 0) :: vis) in
              match List.assoc_opt id vis_t_linked with
                | Some count when count > 0 -> t_linked_str ^ "as t" ^ string_of_int id, vis_t_linked
                | _ -> t_linked_str, vis_t_linked
            )
        ) 
    | TVar {id; link = None; _ } -> "t" ^ string_of_int id, vis
  in 
  let res_str, _ = aux t [] in
  res_str



(*you put a tvar v into this that has a link => not good*)
let rec occurs_check (v : tvar) (t : typ) : bool =
  if !allowrectypes then false else
  match repr t with
  | TVar v' -> v.id = v'.id
  | TFun (t1, t2) -> occurs_check v t1 || occurs_check v t2
  | TInt -> false

let rec unify (t1 : typ) (t2 : typ) : unit =
  match (repr t1, repr t2) with
  | (TInt, TInt) -> ()
  | (TFun (t1f, t1x), TFun (t2f, t2x)) -> 
      unify t1f t2f;
      unify t1x t2x
  | (TVar v1, TVar v2) when v1.id = v2.id -> () (* already the same var *)
  | (TVar v, t) | (t, TVar v) ->
      if occurs_check v t then
        raise (TypeError "Occurs check failed: Recursive types are disabled (you can enable them)")
      else
        v.link <- Some t
  | _ -> raise (TypeError ("Type mismatch: cannot unify " ^ sprint_typ t1 ^ " with " ^ sprint_typ t2))  

let rec generalize (t : typ) : schema =
  match t with
  | TInt -> Forall ([], TInt)
  | TFun (t1, t2) ->
      let Forall (vars1, t1_gen) = generalize t1 in
      let Forall (vars2, t2_gen) = generalize t2 in
      Forall (vars1 @ vars2, TFun (t1_gen, t2_gen))
  | TVar v -> Forall ([v.id], TVar v)

let rec instantiate (Forall (vars, t) : schema) : typ =
  match t with
  | TVar v when List.mem v.id vars -> TVar fresh_tvar
  | TFun (t1, t2) -> TFun (instantiate (Forall (vars, t1)), instantiate (Forall (vars, t2)))
  | TInt -> TInt
  | _ -> t
  (* placeholder *)

(*should this return a type env, I think not as any binding from a var to t var that matters outside of a let are other lets*)
let rec typecheck_lexp (e : lexp) (env : type_env) : constraints * typ =
  match e with
    | Var x -> (
      match List.assoc_opt x env with
      | Some s -> ([], instantiate s) 
      | None -> raise (TypeError ("Unbound variable: " ^ x))
      )
    | Lam (x, eb) -> (
      let tv = TVar fresh_tvar in
      let env' = (x, Forall ([], tv)) :: env in (*it is enough to stitch it to the front as assoc_opt just finds the first one*)
      let cs, t_body = typecheck_lexp eb env' in
      (cs, TFun (tv, t_body))
      )
    | App (f, x) -> (
      let t_out = TVar fresh_tvar in
      let cs_f, t_f = typecheck_lexp f env in
      let cs_x, t_x = typecheck_lexp x env in
      ((t_f, TFun (t_x, t_out)) :: (cs_f @ cs_x), t_out)
      )
    | Int i -> ([], TInt)
    | Bop (bop, e1, e2) -> (
      let cs1, t1 = typecheck_lexp e1 env in
      let cs2, t2 = typecheck_lexp e2 env in
      ((t1, TInt) :: (t2, TInt) :: (cs1 @ cs2), TInt)
      )

let typecheck_letblk (letblk : letblk) (env : type_env) : type_env =

  (*add all let defs to env*)
  let env_with_letdefs = List.fold_left 
    (fun env' (name, lexp) -> (name, Forall ([], TVar fresh_tvar)) :: env') 
    env letblk in

  (*collect all constraints*)
  let constraints = List.fold_left 
    (fun cs (name, lexp) -> 
      let cs', t = typecheck_lexp lexp env_with_letdefs in (*we add this binding in the step before, it must exist so no need to check *)
      let Forall (_, tv) = List.assoc name env_with_letdefs in (*it is impossible to have something generalized here*)
      (tv, t) :: (cs' @ cs)) 
    [] letblk in

  (*unify all constraints*)
  List.iter (fun (t1, t2) -> unify t1 t2) constraints;

  (*generalize all types and add to env*)
  List.fold_left 
    (fun env' (name, lexp) -> 
      let Forall (_, tv) = List.assoc name env_with_letdefs in
      let gen_type = generalize tv in
      (name, gen_type) :: env')
    env letblk

let scc_split_letblk (letblk : letblk) : letblk list =
  [letblk] (* placeholder *)

(*No checks to prevent wrong Nlexp and Lexp orders*)
let rec split_prog (p:prog) : letblk * (Ast.lexp option) =
  List.fold_right 
  (fun stmt (letblk, lexp_opt) -> 
    match stmt with
      | Nlexp (name, e) -> ( (name, e) :: letblk, lexp_opt)
      | Lexp e -> (letblk, Some e)
    )
  p ([], None)

(*
  expects: a program (with all Nlexp except for the last one being a lexp)
  returns: unit if type checks, otherwise raises TypeError with an error message
*)
let typecheck (p : prog) :  unit =
let global_letblk, lexp_opt = split_prog p in
  let letscc = scc_split_letblk global_letblk in
  let env = List.fold_left (fun env letblk -> typecheck_letblk letblk env) [] letscc in
  match lexp_opt with
    | Some lexp -> let _ = typecheck_lexp lexp env in ()
    | None -> ()