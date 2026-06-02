open Ast
open Errors

(** The types of values our interpreter can produce. *)
type value =
  | VInt of int
  | VVec of value array
  | VClosure of string * lexp * env
  (** A Thunk is an unevaluated expression and the environment it lives in.
      This is the secret sauce for your "accidental" lazy evaluation. *)
  | VThunk of lexp * env

(** An environment is a mutable reference to an association list mapping names to values. *)
and env = (string * value) list ref

let rec string_of_value = function
  | VInt n -> string_of_int n
  | VVec arr ->
      let elements = Array.map string_of_value arr |> Array.to_list in
      "vec[" ^ String.concat ", " elements ^ "]"
  | VClosure (param, _, _) -> 
      Printf.sprintf "<closure: \\%s. ...>" param
  | VThunk (_, _) -> 
      "<thunk>"

(* Pretty-prints the environment reference on a single line *)
let string_of_env env =
  let bindings = !env in
  let formatted_bindings = 
    List.map (fun (name, v) -> 
      Printf.sprintf "%s: %s" name (string_of_value v)
    ) bindings 
  in
  "{" ^ String.concat ", " formatted_bindings ^ "}"

let pp_env fmt env = 
  Format.pp_print_string fmt (string_of_env env)

(** [lookup] finds a variable in the environment. 
    If it finds a Thunk, it evaluates it (Call-by-Name). *)
let rec eval_value v =
  match v with
  | VThunk (e, env) -> eval e env
  | _ -> v

and lookup x env =
  match List.assoc_opt x !env with
  | Some v -> eval_value v
  | None -> raise (Errors.InterpError ("Unbound variable: " ^ x))

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
           let param_env = ref ((x, VThunk (e2, env)) :: !c_env) in
           eval body param_env
       | _ -> raise (Errors.InterpError "Application of a non-function"))
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
                   | Div -> if n2 = 0 then raise (Errors.InterpError "Division by zero") else VInt (n1 / n2)
                   | Lt  -> VInt (if n1 < n2 then 1 else 0)
                   | Eq  -> VInt (if n1 = n2 then 1 else 0))
              | _ -> raise (Errors.InterpError "Binary op expects integers"))
       | _ -> raise (Errors.InterpError "Binary op expects integers"))
   | If (cond, then_branch, else_branch) ->
        let interp_cond = eval cond env in
        (*Printf.printf "Evaluating condition of if: %s, n: %s, i: %s, arr: %s, Condition evaluated to: %s\n" 
            (PrintIntlang.sprint_lexp cond) 
            (match lookup "n" env with  (VInt n) -> string_of_int n | _ -> "unbound") 
            (match lookup "i" env with  (VInt n) -> string_of_int n | _ -> "unbound") 
            (match lookup "arr" env with  (VVec arr) -> String.concat "," (Array.to_list (Array.map (fun v -> match eval_value v with VInt x -> string_of_int x | _ -> "non-integer") arr)) | _ -> "not a vector") 
            (match interp_cond with VInt n -> string_of_int n | _ -> "non-integer"); flush stdout;*)
        (match interp_cond with
        | VInt n -> if n <> 0 then eval then_branch env else eval else_branch env
        | _ -> raise (Errors.InterpError "Condition in if must be an integer"))
   | Letin (x, e1, e2) -> 
          (* Call-by-Name: We don't evaluate e1 yet. 
          We wrap it in a Thunk and put it in the environment. *)
          let rec param_env = ref ((x, VThunk (e1, param_env)) :: !env) in
          eval e2 param_env
    | Veclit es -> 
        let vals = Array.of_list (List.map (fun e -> VThunk(e, env)) es) in
        VVec vals
    | Vecmk (defval, count) -> 
        (match eval count env with
         | VInt n when n >= 0 ->
             let vals = Array.init n (fun _ -> VThunk(defval, env)) in
             VVec vals
         | _ -> raise (Errors.InterpError "vecmk count must be a non-negative integer"))
    | Veclen v ->
        (match eval v env with
         | VVec arr -> VInt (Array.length arr)
         | _ -> raise (Errors.InterpError "veclen expects a vector"))
    | Vecget (v, i) ->
        (match eval v env with
         | VVec arr ->
             (match eval i env with
              | VInt idx when idx >= 0 && idx < Array.length arr -> eval_value arr.(idx)
              | _ -> raise (Errors.InterpError "vecget index out of bounds"))
         | _ -> raise (Errors.InterpError "vecget expects a vector"))
    | Vecset (v, i, val_e) ->
        (match eval v env with
         | VVec arr ->
             (match eval i env with
              | VInt idx when idx >= 0 && idx < Array.length arr ->
                  (*what do here?? is this ok?*)
                  let new_arr = Array.copy arr in
                  new_arr.(idx) <- VThunk (val_e, env);
                  VVec new_arr
              | _ -> raise (Errors.InterpError "vecset index out of bounds"))
         | _ -> raise (Errors.InterpError "vecset expects a vector"))

(** Interprets the whole program. 
    It processes statements in order, building up a global environment. *)
let interp_prog ((letblk, main_opt) : prog) : int option =
  let global_env_ref = ref [] in
  List.iter (fun (id, e) -> global_env_ref := (id, VThunk (e, global_env_ref)) :: !global_env_ref) letblk;
  match main_opt with
    | Some main -> (match (eval main global_env_ref) with
                       | VInt n -> Some n
                       | _ -> raise (Errors.InterpError "Program ended something else, expected int"))
    | None -> None
