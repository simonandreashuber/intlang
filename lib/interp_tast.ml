open Ast
open Errors

(** The types of values our interpreter can produce. *)
type value =
    | VI32 of Int32.t
    | VI8 of char
    | VUnit
    | VClosure of uuid * lexpt * env
    | VTup of value list
    | VVec of value array
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
    | VarT (_, uuid_ref, _) -> lookup !uuid_ref env
    | LamT (x, param_uuid, body, _) -> VClosure (param_uuid, body, env)
    | AppT (e1, e2, _) -> (
            let v2 = eval e2 env in
            match eval e1 env with
            | VClosure (param_uuid, body, c_env) -> 
                let param_env = ref ((param_uuid, v2) :: !c_env) in
                eval body param_env
            | _ -> raise (Errors.InterpError "Application of a non-function")
        )
    | SeqT (e1, e2, _) -> let _ = eval e1 env in eval e2 env
    | IfT (cond, then_branch, else_branch, _) ->
        let interp_cond = eval cond env in
        (match interp_cond with
        | VI32 n -> if n <> 0 then eval then_branch env else eval else_branch env
        | _ -> raise (Errors.InterpError "Condition in if must be an integer"))
    | LetinT (x, param_uuid, e1, e2, _) -> 
        let v1 = eval e1 env in
        let param_env = ref ((param_uuid, v1) :: !env) in
        eval e2 param_env
    | LetinT (x, param_uuid, e1, e2, _) -> 
        let param_env = ref ((param_uuid, VBlackhole) :: !env) in
        let v1 = eval e1 param_env in
        param_env := (param_uuid, v1) :: !env;
        eval e2 param_env
    | LetintupleT (iduuid_opt_list, e1, e2, _) ->
        let v1 = eval e1 env in
        (match v1 with
        | VTup vals when List.length iduuid_opt_list = List.length vals ->
            let new_env = ref !env in
            List.iter2 (fun iduuid_opt v ->
            match iduuid_opt with
            | Some (id, uuid) -> new_env := (uuid, v) :: !new_env
            | None -> ()
            ) iduuid_opt_list vals;
            eval e2 new_env
        | VTup _ -> raise (Errors.InterpError "Tuple length mismatch in let-in-tuple")
        | _ -> raise (Errors.InterpError "Expected a tuple for let-in-tuple"))
    | TupleT (es, _) -> VTup (List.map (fun e -> eval e env) es)
    | I32LitT (n, _) -> VI32 (Int32.of_int n)
    | I8LitT (c, _) -> VI8 c
    | UnitLitT (_) -> VUnit
    | UopI32T (op, e, _) -> (
        let v = eval e env in
        match v with
        | VI32 n -> (
            match op with
            | Negi32 -> VI32 (Int32.neg n)
            | Noti32 -> VI32 (Int32.lognot n)
        )
        | _ -> raise (Errors.InterpError "Unary operation on non-integer"))
    | UopI8T (op, e, _) -> (
        let v = eval e env in
        match v with
        | VI8 c -> (
            match op with
            | Negi8 -> VI8 (Char.chr (-(Char.code c)))
            | Noti8 -> VI8 (Char.chr (lnot (Char.code c)))
        )
        | _ -> raise (Errors.InterpError "Unary operation on non-i8"))
    | BopI32T (op, e1, e2, _) -> (
        let v1 = eval e1 env in
        let v2 = eval e2 env in
        match v1, v2 with
        | VI32 n1, VI32 n2 -> (
            match op with
            | Eqi32 -> VI32 (if n1 = n2 then 1l else 0l)
            | Neqi32 -> VI32 (if n1 <> n2 then 1l else 0l)
            | Lti32 -> VI32 (if n1 < n2 then 1l else 0l)
            | Gti32 -> VI32 (if n1 > n2 then 1l else 0l)
            | LtEqi32 -> VI32 (if n1 <= n2 then 1l else 0l)
            | GtEqi32 -> VI32 (if n1 >= n2 then 1l else 0l)
            (*note on the unsigned ops, ints in ocaml are 63 bits so we mask them to 32 bits which gives us the unsigned behavior *)
            | ULti32 -> VI32 (if Int32.unsigned_compare n1 n2 < 0 then 1l else 0l)
            | UGti32 -> VI32 (if Int32.unsigned_compare n1 n2 > 0 then 1l else 0l)
            | ULtEqi32 -> VI32 (if Int32.unsigned_compare n1 n2 <= 0 then 1l else 0l)
            | UGtEqi32 -> VI32 (if Int32.unsigned_compare n1 n2 >= 0 then 1l else 0l)
            | Muli32 -> VI32 (Int32.mul n1 n2)
            | Subi32 -> VI32 (Int32.sub n1 n2)
            | Addi32 -> VI32 (Int32.add n1 n2)
            | Divi32 -> if n2 = 0l then raise (Errors.InterpError "Division by zero") else VI32 (Int32.div n1 n2)
            | Modi32 -> if n2 = 0l then raise (Errors.InterpError "Modulo by zero") else VI32 (Int32.rem n1 n2)
            | UDivi32 -> if n2 = 0l then raise (Errors.InterpError "Unsigned division by zero") else VI32 (Int32.unsigned_div n1 n2)
            | UModi32 -> if n2 = 0l then raise (Errors.InterpError "Unsigned modulo by zero") else VI32 (Int32.unsigned_rem n1 n2)
            | Andi32 -> VI32 (Int32.logand n1 n2)
            | Ori32 -> VI32 (Int32.logor n1 n2)
            | Xori32 -> VI32 (Int32.logxor n1 n2)
            | Shli32 -> if n2 < 0l || n2 >= 32l then raise (Errors.InterpError "Shift amount out of bounds") else VI32 (Int32.shift_left n1 n2)
            | Shri32 -> if n2 < 0l || n2 >= 32l then raise (Errors.InterpError "Shift amount out of bounds") else VI32 (Int32.shift_right n1 n2)
            | UShri32 -> if n2 < 0l || n2 >= 32l then raise (Errors.InterpError "Shift amount out of bounds") else VI32 (Int32.shift_right_logical n1 n2)
        | _ -> raise (Errors.InterpError "Binary operation on non-integers"))
    | BopI8T (op, e1, e2, _) -> (
        let v1 = eval e1 env in
        let v2 = eval e2 env in
        match v1, v2 with
        | VI8 c1, VI8 c2 -> (
            match op with
            | Eqi8 -> VI32 (if c1 = c2 then 1l else 0l)
            | Neqi8 -> VI32 (if c1 <> c2 then 1l else 0l)
            | Lti8 -> VI32 (if c1 < c2 then 1l else 0l)
            | Gti8 -> VI32 (if c1 > c2 then 1l else 0l)
            | LtEqi8 -> VI32 (if c1 <= c2 then 1l else 0l)
            | GtEqi8 -> VI32 (if c1 >= c2 then 1l else 0l)
            | Addi8 -> VI8 (Char.chr ((Char.code c1) + (Char.code c2)))
            | Subi8 -> VI8 (Char.chr ((Char.code c1) - (Char.code c2)))
            | Andi8 -> VI8 (Char.chr ((Char.code c1) land (Char.code c2)))
            | Ori8 -> VI8 (Char.chr ((Char.code c1) lor (Char.code c2)))
            | Xori8 -> VI8 (Char.chr ((Char.code c1) lxor (Char.code c2)))
        )
        | _ -> raise (Errors.InterpError "Binary operation on non-i8"))
    | VecLitT (es, _) -> 
        let vals = Array.of_list (List.map (fun e -> eval e env) es) in
        VVec vals
    | VecmkT (defval, size_list, typ) -> (
        let mk_vec size fillval = 
            match eval size env with 
            | I32 n -> VVec ( Array.init (Int32.to_int n) (fun _ -> eval fillval env) )
            | _ -> raise (Errors.InterpError "vecmk size must be an integer")
        in
        match size_list with
        | [last_size] -> mk_vec last_size defval
        | size :: tl -> mk_vec size (VecmkT (defval, tl, typ))
        | [] -> raise (Errors.InterpError "vecmk requires at least one size argument"))
    | VeclenT (v, _) ->
        (match eval v env with
         | VVec arr -> VInt (Int32.of_int @@ Array.length arr)
         | _ -> raise (Errors.InterpError "veclen expects a vector"))
    | VecgetT (v, idx_list, _) -> 
        List.fold_left (fun v_val idx_val ->
            let idx = match idx_val with
                | VInt n -> Int32.to_int n
                | _ -> raise (Errors.InterpError "vecget index must be an integer")
            in
            match v_val with
            | VVec arr when idx >= 0 && idx < Array.length arr -> arr.(idx)
            | _ -> raise ((Errors.InterpError "vecget expects a vector"))
        ) (eval v env) idx_list
    | VecsetT (v, setval, idx_list, typ) -> (
        match idx_list with
        | idx :: tl when List.length tl > 0 ->
            let nuuid = fresh_uuid() in
            let nuuid_v = fresh_uuid() in
            (* I used blank and TUnit as unimportant place holders
               The outer let is needed as v might not satisfy
               referential transparency ie. v might involve some
               IO hence evaluating twice is invalid
            *)
            let expansion = LetinT ( "blank", nuuid_v, v,
                            LetinT ("blank", nuuid, 
                                    VecgetT (VarT (ref "blank", ref nuuid_v, TUnit), [idx], TUnit), 
                                    Vecset (VarT (ref "blank", ref nuuid_v, TUnit), 
                                            VecsetT (VarT (ref "blank", ref nuuid, TUnit), setval, tl, TUnit),
                                            [idx], TUnit),
                            TUnit),
                            TUnit)
            in
            eval expansion env
        | [idx_exp] -> (
            match eval v env, eval idx_exp env with
            | VVec arr, VInt idx when idx >= 0 && idx < Array.length arr -> (
                (*simple rule, never mutate existing memory, so we copy and then mutate the copy*)
                let new_arr = Array.copy arr in
                new_arr.(idx) <- eval setval env;
                VVec new_arr)
            | _, _ -> raise (Errors.InterpError "vecset expects a vector, or index out of bounds")
        )
    )



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
