open Ast
open Errors

(** The types of values our interpreter can produce. *)
type value =
    | VInt of int
    | VVec of value array
    | VClosure of string * lexp * env
    | VBlackhole (* Represents an uninitialized recursive binding *)

(** An environment is a mutable reference to an association list mapping names to values. *)
and env = (string * value) list ref

(** [lookup] finds a variable in the environment. 
    If it finds a Thunk, it evaluates it (Call-by-Name). *)
let rec lookup x env =
  match List.assoc_opt x !env with
  | Some VBlackhole -> 
      raise (Errors.InterpError ("Circular dependency detected: " ^ x ^ " used before initialization"))
  | Some v -> v
  | None -> raise (Errors.InterpError ("Unbound variable: " ^ x))

(** The core evaluation function. *)
and eval (e : lexp) (env : env) : value =
  match e with
    | Int n -> VInt n
    | Var x -> lookup x env
    | Lam (x, body) -> VClosure (x, body, env)
    | App (e1, e2) ->(
            let v2 = eval e2 env in
            match eval e1 env with
            | VClosure (x, body, c_env) -> 
                let param_env = ref ((x, v2) :: !c_env) in
                eval body param_env
            | _ -> raise (Errors.InterpError "Application of a non-function")
        )
    | Bop (op, e1, e2) -> (
        let v1 = eval e1 env in
        let v2 = eval e2 env in
        (match v1, v2 with
        | VInt n1, VInt n2 ->
            (match op with
            | Add -> VInt (n1 + n2)
            | Sub -> VInt (n1 - n2)
            | Mul -> VInt (n1 * n2)
            | Div -> if n2 = 0 then raise (Errors.InterpError "Division by zero") else VInt (n1 / n2)
            | Lt  -> VInt (if n1 < n2 then 1 else 0)
            | Eq  -> VInt (if n1 = n2 then 1 else 0))
        | _ -> raise (Errors.InterpError "Binary op expects integers"))
        )
    | If (cond, then_branch, else_branch) ->
        let interp_cond = eval cond env in
        (match interp_cond with
        | VInt n -> if n <> 0 then eval then_branch env else eval else_branch env
        | _ -> raise (Errors.InterpError "Condition in if must be an integer"))
    | Letin (x, e1, e2) -> 
          let param_env = ref ((x, VBlackhole) :: !env) in
          let v1 = eval e1 param_env in
          param_env := (x, v1) :: !env;
          eval e2 param_env
    | Veclit es -> 
        let vals = Array.of_list (List.map (fun e -> eval e env) es) in
        VVec vals
    | Vecmk (defval, count) -> 
        (match eval count env with
         | VInt n when n >= 0 ->
             let vals = Array.init n (fun _ -> eval defval env) in
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
              | VInt idx when idx >= 0 && idx < Array.length arr -> arr.(idx)
              | _ -> raise (Errors.InterpError "vecget index out of bounds"))
         | _ -> raise (Errors.InterpError "vecget expects a vector"))
    | Vecset (v, i, val_e) ->
        (match eval v env with
         | VVec arr ->
             (match eval i env with
              | VInt idx when idx >= 0 && idx < Array.length arr ->
                  (*what do here?? is this ok?*)
                  let new_arr = Array.copy arr in
                  new_arr.(idx) <- eval val_e env;
                  VVec new_arr
              | _ -> raise (Errors.InterpError "vecset index out of bounds"))
         | _ -> raise (Errors.InterpError "vecset expects a vector"))



let interp_prog ((letblk, main_opt) : prog) : int option =
  let global_env_ref = ref [] in

  (*stitch all bindings into the env, but with VBlackhole*)
  List.iter (fun (id, _) -> 
    global_env_ref := (id, VBlackhole) :: !global_env_ref
  ) letblk;
  
  List.iter (fun (id, e) -> 
    let v = eval e global_env_ref in
    (*go and replace the VBlackhole used before in the env reference*)
    global_env_ref := List.map (fun (id', v') -> 
      if id' = id then (id', v) else (id', v')
    ) !global_env_ref
  ) letblk;

  match main_opt with
    | Some main -> (match (eval main global_env_ref) with
                       | VInt n -> Some n
                       | _ -> raise (Errors.InterpError "Program ended with something else, expected int"))
    | None -> None
