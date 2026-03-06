open Ast

(** The types of values our interpreter can produce. *)
type value =
  | VInt of int
  | VClosure of string * lexp * env
  (** A Thunk is an unevaluated expression and the environment it lives in.
      This is the secret sauce for your "accidental" lazy evaluation. *)
  | VThunk of lexp * env

(** An environment is a simple association list mapping names to values. *)
and env = (string * value) list

exception Runtime_error of string

(** [lookup] finds a variable in the environment. 
    If it finds a Thunk, it evaluates it (Call-by-Name). *)
let rec eval_value v =
  match v with
  | VThunk (e, env) -> eval e env
  | _ -> v

and lookup x env =
  match List.assoc_opt x env with
  | Some v -> eval_value v
  | None -> raise (Runtime_error ("Unbound variable: " ^ x))

(** The core evaluation function. *)
and eval (e : lexp) (env : env) : value =
  match e with
  | Int n -> VInt n
  
  | Var x -> lookup x env
  
  | Lam (x, body) -> VClosure (x, body, env)
  
  | App (e1, e2) ->
      (match eval e1 env with
       | VClosure (x, body, c_env) -> 
           (* Call-by-Name: We don't evaluate e2 yet. 
              We wrap it in a Thunk and put it in the environment. *)
           eval body ((x, VThunk (e2, env)) :: c_env)
       | _ -> raise (Runtime_error "Application of a non-function"))

  | Bop (op, e1, e2) ->
      let v1 = eval e1 env in
      (match v1 with
       | VInt n1 ->
           (* Special Short-Circuiting for Multiplication:
              If the left side is 0, we return 0 WITHOUT evaluating e2.
              This allows: (n == 0) * 1 + (n > 0) * (recurse) *)
           if op = Mul && n1 = 0 then VInt 0
           else
             (match eval e2 env with
              | VInt n2 ->
                  (match op with
                   | Add -> VInt (n1 + n2)
                   | Sub -> VInt (n1 - n2)
                   | Mul -> VInt (n1 * n2)
                   | Lt  -> VInt (if n1 < n2 then 1 else 0)
                   | Eq  -> VInt (if n1 = n2 then 1 else 0))
              | _ -> raise (Runtime_error "Binary op expects integers"))
       | _ -> raise (Runtime_error "Binary op expects integers"))

(** Interprets the whole program. 
    It processes statements in order, building up a global environment. *)
let interp_prog (p : prog) : int =
  let rec loop statements global_env =
    match statements with
    | [] -> raise (Runtime_error "Empty program")
    | [Lexp e] -> 
        (match eval e global_env with
         | VInt n -> n
         | _ -> raise (Runtime_error "Program ended with a function, expected int"))
    | Nlexp (id, e) :: rest ->
        (* THE FIX: We use OCaml's 'let rec' to create a circular environment.
           The 'new_env' now contains a mapping for 'id', and the Thunk 
           inside that mapping points back to 'new_env'. *)
        let rec new_env = (id, VThunk (e, new_env)) :: global_env in
        loop rest new_env
    | Lexp _ :: _ -> 
        raise (Runtime_error "Expression found in the middle of let statements")
  in
  loop p []
