(*
Heavy inspiration from:
https://github.com/mgrabmueller/AlgorithmW
*)

open Ast

module StrSet = Set.Make(String)
module StrMap = Map.Make(String)

type typ =
    | Tvar of string
    | Tint
    | Tfun of typ * typ

type schema = Schema of string list * typ

(*
print helpers
*)

let print_typ (t: typ) : string =
    let rec aux t =
        match t with
        | Tvar v -> v
        | Tint -> "int"
        | Tfun (t1, t2) -> 
            let left = match t1 with
                | Tfun _ -> "(" ^ aux t1 ^ ")"
                | _ -> aux t1
            in
            left ^ " -> " ^ aux t2
    in aux t

let print_schema (Schema (vars, t) : schema) : string =
    if vars = [] then print_typ t
    else "forall " ^ String.concat " " vars ^ ". " ^ print_typ t


type subst = typ StrMap.t

type type_env = TypeEnv of schema StrMap.t

exception TypeError of string

(* 
global counter for generating fresh type variables
*)
let counter = ref 0

(* 
Function to generate a fresh, unique type variable name
*)
let next_var () =
  let id = !counter in
  incr counter;
  (* Generates names like "a", "b" ... "z", then "a1", "b1" etc. *)
  let name = String.make 1 (Char.chr (97 + (id mod 26))) ^ 
             (if id < 26 then "" else string_of_int (id / 26)) in
  name

let reset_counter () = counter := 0
(*
find free type variables in a type, schema or type environment
*)
let rec ftv (t:typ) : StrSet.t =
    match t with
    | Tvar v -> StrSet.singleton v
    | Tint -> StrSet.empty
    | Tfun (t1, t2) -> StrSet.union (ftv t1) (ftv t2)

let rec ftv_schema (Schema (vars, t) : schema) : StrSet.t =
    StrSet.diff (ftv t) (StrSet.of_list vars)

let rec ftv_env (TypeEnv env) : StrSet.t =
    StrMap.fold (fun _ schema acc -> StrSet.union (ftv_schema schema) acc) env StrSet.empty

(*
apply a substitution to a type, schema or type environment
*)
let rec apply (s: subst) (t: typ) : typ =
    match t with
    | Tvar v -> (match StrMap.find_opt v s with Some t' -> t' | None -> t)
    | Tint -> Tint
    | Tfun (t1, t2) -> Tfun (apply s t1, apply s t2)

let rec apply_schema (s: subst) (Schema (vars, t) : schema) : schema =
    let s' = List.fold_left (fun acc v -> StrMap.remove v acc) s vars in
    Schema (vars, apply s' t)

let rec apply_env (s: subst) (TypeEnv env) : type_env =
    TypeEnv (StrMap.map (apply_schema s) env)

(*
compose two substitutions, s1 is applied to the range of s2
ie. a substitution s that is equivalent to first applying s2 then s1
*)
let compose_subst (s1: subst) (s2: subst) : subst =
    let s2' = StrMap.map (apply s1) s2 in
    StrMap.union (fun _ t1 _ -> Some t1) s1 s2'

(*
Remove some binding from the type environment
*)
let remove (TypeEnv env) (var: string) : type_env =
    TypeEnv (StrMap.remove var env)

(*
generalize a type to a schema by quantifying over all free type variables 
that are not free in the type environment
*)
let generalize (env: type_env) (t: typ) : schema =
    let env_ftv = ftv_env env in
    let t_ftv = ftv t in
    let vars = StrSet.elements (StrSet.diff t_ftv env_ftv) in
    Schema (vars, t)

(*
instantiate a schema to a type by replacing all quantified variables with fresh type variables
*)

let instantiate (Schema (vars, t) : schema) : typ =
    let subst = List.fold_left 
        (fun acc var -> StrMap.add var (Tvar (next_var ())) acc) 
        StrMap.empty vars in
    apply subst t

(*
mgu: unify two types and return a substitution that makes them equal
     raises an exception if the types cannot be unified
*)

let rec mgu (t1: typ) (t2: typ) : subst =
    match (t1, t2) with
    | (Tfun (l1, r1), Tfun (l2, r2)) ->
        let sl = mgu l1 l2 in
        let sr = mgu (apply sl r1) (apply sl r2) in
        compose_subst sr sl
    | (Tvar v, t) | (t, Tvar v) ->
        if t = Tvar v then StrMap.empty
        else if StrSet.mem v (ftv t) then raise (TypeError ("Occurs check failed (no recursive types in Hindley-Milner): " ^ (print_typ t1) ^ " ~ " ^ (print_typ t2)))
        else StrMap.singleton v t
    | (Tint, Tint) -> StrMap.empty
    | _ -> raise (TypeError ("Types do not unify: " ^ (print_typ t1) ^ " ~ " ^ (print_typ t2)))

(*
ti: type inference function
*)

let rec ti (env: type_env) (e: lexp) : subst * typ =
    match e with
    | Var x -> (
        let TypeEnv env_map = env in
        (match StrMap.find_opt x env_map with
        | Some schema -> (StrMap.empty, instantiate schema)
        | None -> raise (TypeError ("Unbound variable: " ^ x)))
    )
    | Lam (x, body) -> (
        let tv = Tvar (next_var ()) in
        let TypeEnv env_map = env in
        (*remove not needed since the union is picking from the first map*)
        let env' = TypeEnv (StrMap.union (fun _ t1 _ -> Some t1) (StrMap.singleton x (Schema ([], tv))) env_map) in
        let sb, tb = ti env' body in
        let t_fun = Tfun (apply sb tv, tb) in
        (sb, t_fun)
    )
    | App (e1, e2) -> (
        let tv = Tvar (next_var ()) in
        let s1, t1 = ti env e1 in
        let s2, t2 = ti (apply_env s1 env) e2 in
        let s3 = mgu (apply s2 t1) (Tfun (t2, tv)) in
        let s = compose_subst s3 (compose_subst s2 s1) in
        (s, apply s3 tv)
    )
    | Int _ -> (StrMap.empty, Tint)
    | Bop (bop, e1, e2) -> (
        let s1, t1 = ti env e1 in
        let s2, t2 = ti (apply_env s1 env) e2 in
        let s3 = mgu (apply s2 t1) Tint in
        let s4 = mgu (apply s3 t2) Tint in
        let s = compose_subst s4 (compose_subst s3 (compose_subst s2 s1)) in
        (s, Tint)
    )

let typecheck (env: type_env) (e: lexp) : typ =
    reset_counter ();
    let s, t = ti env e in
    apply s t