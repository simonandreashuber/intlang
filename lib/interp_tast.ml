open Ast
open Errors

(** The types of values our interpreter can produce. *)
type value =
    | VInt of int
    | VVec of value array
    | VClosure of uuid * lexpt * env
    | VBlackhole (* Represents an uninitialized recursive binding *)

(** An environment is a mutable reference to an association list mapping uuids to values. *)
and env = (uuid * value) list ref

(** [lookup] finds a variable in the environment by uuid. 
    If it finds a Thunk, it evaluates it (Call-by-Name). *)
let rec lookup uuid env =
  match List.assoc_opt uuid !env with
  | Some VBlackhole -> 
      raise (Errors.InterpError ("Circular dependency detected: uuid " ^ string_of_int uuid ^ " used before initialization"))
  | Some v -> v
  | None -> raise (Errors.InterpError ("Unbound variable: uuid " ^ string_of_int uuid))

(** The core evaluation function. *)
and eval (e : lexpt) (env : env) : value =
  match e with
    | IntT (n, _) -> VInt n
    | VarT (_, uuid_ref, _) -> lookup !uuid_ref env
    | LamT (x, param_uuid, body, _) -> VClosure (param_uuid, body, env)
    | AppT (e1, e2, _) ->(
            let v2 = eval e2 env in
            match eval e1 env with
            | VClosure (param_uuid, body, c_env) -> 
                let param_env = ref ((param_uuid, v2) :: !c_env) in
                eval body param_env
            | _ -> raise (Errors.InterpError "Application of a non-function")
        )
    | BopT (op, e1, e2, _) -> (
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
    | IfT (cond, then_branch, else_branch, _) ->
        let interp_cond = eval cond env in
        (match interp_cond with
        | VInt n -> if n <> 0 then eval then_branch env else eval else_branch env
        | _ -> raise (Errors.InterpError "Condition in if must be an integer"))
    | LetinT (x, param_uuid, e1, e2, _) -> 
          let param_env = ref ((param_uuid, VBlackhole) :: !env) in
          let v1 = eval e1 param_env in
          param_env := (param_uuid, v1) :: !env;
          eval e2 param_env
    | VeclitT (es, _) -> 
        let vals = Array.of_list (List.map (fun e -> eval e env) es) in
        VVec vals
    | VecmkT (defval, count, _) -> 
        (match eval count env with
         | VInt n when n >= 0 ->
             let vals = Array.init n (fun _ -> eval defval env) in
             VVec vals
         | _ -> raise (Errors.InterpError "vecmk count must be a non-negative integer"))
    | VeclenT (v, _) ->
        (match eval v env with
         | VVec arr -> VInt (Array.length arr)
         | _ -> raise (Errors.InterpError "veclen expects a vector"))
    | VecgetT (v, i, _) ->
        (match eval v env with
         | VVec arr ->
             (match eval i env with
              | VInt idx when idx >= 0 && idx < Array.length arr -> arr.(idx)
              | _ -> raise (Errors.InterpError "vecget index out of bounds"))
         | _ -> raise (Errors.InterpError "vecget expects a vector"))
    | VecsetT (v, i, val_e, _) ->
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



let interp_prog (letblk : letblkmonot) : int option =
  let global_env_ref = ref [] in

  (*stitch all bindings into the env, but with VBlackhole*)
  List.iter (fun (_, uuid, _) -> 
    global_env_ref := (uuid, VBlackhole) :: !global_env_ref
  ) letblk;
  
  let main_uuid = ref None in
  
  List.iter (fun (name, uuid, e) -> 
    let v = eval e global_env_ref in
    (*go and replace the VBlackhole used before in the env reference*)
    global_env_ref := List.map (fun (uuid', v') -> 
      if uuid' = uuid then (uuid', v) else (uuid', v')
    ) !global_env_ref;
    (*detect if this is the main binding*)
    if name = "@main" then main_uuid := Some uuid
  ) letblk;

  match !main_uuid with
    | Some uuid -> (match (lookup uuid global_env_ref) with
                       | VInt n -> Some n
                       | _ -> raise (Errors.InterpError "Program ended with something else, expected int"))
    | None -> None
