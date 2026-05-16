(*
Heavy inspiration from:
https://github.com/mgrabmueller/AlgorithmW
*)

open Ast
open PrintIntlang

module StrSet = Set.Make(String)
module StrMap = Map.Make(String)

type typ =
    | Tvar of string
    | Tint
    | Tfun of typ * typ

(*[typ vars] * typ*)
type schema = Schema of string list * typ

(*typ var -> typ*)
type subst = typ StrMap.t

(*
    var -> schema
    Note: the var is from the AST and is not a type var
*)
type type_env = TypeEnv of schema StrMap.t

exception TypeError of string



(*
print helpers
*)
let sprint_typ (t: typ) : string =
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

let sprint_schema (Schema (vars, t) : schema) : string =
    "forall " ^ String.concat " " vars ^ ". " ^ sprint_typ t


let sprint_subst (s: subst) : string =
    let bindings = StrMap.bindings s in
    let binding_strs = List.map (fun (v, t) -> v ^ " ↦ " ^ sprint_typ t) bindings in
    "{ " ^ String.concat ", " binding_strs ^ " }"
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
Explanation/debugging support
*)
let explain_on = ref false
let explanation = ref ""

let add_explain (msg: string) : unit =
  if !explain_on then
    explanation := !explanation ^ msg ^ "\n"

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
not even used I think, since the union for maps in ocaml does force a specific decision any ways
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
        let sr = mgu (apply sl r1) (apply sl r2) in (*no unify the right sides but apply the constraints found during left side unification*)
        compose_subst sr sl
    | (Tvar v, t) | (t, Tvar v) ->
        if t = Tvar v then StrMap.empty
        else if StrSet.mem v (ftv t) then raise (TypeError ("Occurs check failed (no recursive types in Hindley-Milner): " ^ (sprint_typ t1) ^ " ~ " ^ (sprint_typ t2)))
        else StrMap.singleton v t
    | (Tint, Tint) -> StrMap.empty
    | _ -> raise (TypeError ("Types do not unify: " ^ (sprint_typ t1) ^ " ~ " ^ (sprint_typ t2)))

(*
ti: type inference function
*)

let rec ti (env: type_env) (e: lexp) : subst * typ =
    match e with
    | Var x -> (
        let TypeEnv env_map = env in
        (match StrMap.find_opt x env_map with (*use of var needs to be in env by lambda or let statement*)
        | Some schema -> 
            let inst_typ = instantiate schema in (*the binding might be a schema so get a fresh instantiation*)
            add_explain (Printf.sprintf "Var: %s: constraints: %s, finaltype: %s" (PrintIntlang.sprint_lexp e) (sprint_subst StrMap.empty) (sprint_typ inst_typ));
            (StrMap.empty, inst_typ) (*no constraints induced, and type is the new instantiation*)
        | None -> raise (TypeError ("Unbound variable: " ^ x))) (*quick note: this is where a var that is "globally free" would be caught*)
    )
    | Lam (x, body) -> (
        let tv = Tvar (next_var ()) in (*we dont know the schema of x yet so we just say, here is a fresh typ var go and have fun with it*)
        let TypeEnv env_map = env in
        (*remove not needed since the union is picking from the first map*)
        let env' = TypeEnv (StrMap.union (fun _ t1 _ -> Some t1) (StrMap.singleton x (Schema ([], tv))) env_map) in (*we remove any existing x since lambdas overshadow outside defs*)
        let sb, tb = ti env' body in (*recurse on the body with new env, this returns the body type and the constraints that were found*)
        let t_fun = Tfun (apply sb tv, tb) in (*clearly the type is func typ from tv to tb but the type inference on the body may have introduced constraints, that need to be applied to tv*)
        add_explain (Printf.sprintf "Lam: %s: constraints: %s, finaltype: %s" (PrintIntlang.sprint_lexp e) (sprint_subst sb) (sprint_typ t_fun));
        (sb, t_fun) (*note, the constraints form the body are still valid and needed so we return them*)
    )
    | App (e1, e2) -> (
        let tv = Tvar (next_var ()) in (*new typ var for the "out type" of e1, which is the type of the App (e1, e2)*)
        let s1, t1 = ti env e1 in (*recurse on e1*)
        let s2, t2 = ti (apply_env s1 env) e2 in (*recurse on e2 but update the env with the constraints from e1*)
        let t1_applied = apply s2 t1 in (*there might be new constraints from type inference on e2 these may change t1*)
        let s3 = mgu t1_applied (Tfun (t2, tv)) in (*unify both recursion types*)
        let s = compose_subst s3 (compose_subst s2 s1) in (*collect all constraints*)
        let tfv = apply s3 tv in
        add_explain (Printf.sprintf "App: %s , %s: constraints: %s, finaltype: %s" (PrintIntlang.sprint_lexp e1) (PrintIntlang.sprint_lexp e2) (sprint_subst s) (sprint_typ tfv));
        (s, tfv) (*our "out type" is returned but first we apply the final substitutions, not we could use apply 
                            s tv but s1 and s2 really had no idea that tv even existed and hence will not affect tv*)
    )
    | Int _ -> (StrMap.empty, Tint)
    | Bop (bop, e1, e2) -> (
        let s1, t1 = ti env e1 in (*left side*)
        let s2, t2 = ti (apply_env s1 env) e2 in (*right side with constraints from left side*)
        let s3 = mgu (apply s2 t1) Tint in (*unify left side with int*)
        let s4 = mgu (apply s3 t2) Tint in (*unify right side with int*)
        let s = compose_subst s4 (compose_subst s3 (compose_subst s2 s1)) in (*stitch all constraints together*)
        add_explain (Printf.sprintf "Bop: %s: constraints: %s, final type: int" (PrintIntlang.sprint_lexp e) (sprint_subst s));
        (s, Tint)
    )

let typecheck (env: type_env) (e: lexp) : typ =
    reset_counter ();
    let s, t = ti env e in
    apply s t (*redundant btw*)

let explain_typecheck (env: type_env) (e: lexp) : typ * string =
    reset_counter ();
    explanation := "";
    explain_on := true;
    let result_typ = 
      try typecheck env e
      with TypeError msg ->
        add_explain (Printf.sprintf "ERROR: %s" msg);
        raise (TypeError msg)
    in
    explain_on := false;
    let expl = !explanation in
    explanation := "";
    (result_typ, expl)