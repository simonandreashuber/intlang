open Ast
open Errors

(** The types of values our interpreter can produce. *)
type value =
    | VI32 of Int32.t
    | VI8 of char
    | VUnit
    | VClosure of uuid * tlexp * env
    | VClosureUnit of tlexp * env
    | VTup of value list
    | VVec of value array
    | VBlackhole (* Represents an uninitialized recursive binding *)
    | VBuiltin of (value -> value)

(** An environment is a mutable reference to an association list mapping uuids to values. *)
and env = (uuid * value) list ref

let get_builtin_fun (name : string) : (value -> value) =
  match name with
  | "readi8" -> (fun _ -> VI8 (input_char stdin))
  | "writei8" -> (fun v -> match v with
                            | VI8 c -> let _ = output_char stdout c in VUnit 
                            | _ -> raise (Errors.InterpError "writei8 expects an i8"))
  | "flush" -> (fun _ -> flush stdout; VUnit)
  | "i32_to_i8" -> (fun v -> match v with
                             | VI32 n when n >= 0l && n <= 255l -> VI8 (Char.chr (Int32.to_int n))
                             | _ -> raise (Errors.InterpError "i32_to_i8 expects an i32 in the range [0, 255]"))
  | "i8_to_i32" -> (fun v -> match v with
                             | VI8 c -> VI32 (Int32.of_int (Char.code c))
                             | _ -> raise (Errors.InterpError "i8_to_i32 expects an i8"))
  | _ -> raise (Errors.InterpError ("Unknown builtin function: " ^ name))

(** [lookup] finds a variable in the environment by uuid. 
    If it finds a Thunk, it evaluates it (Call-by-Name). *)
let rec lookup uuid env =
  match List.assoc_opt uuid !env, List.assoc_opt uuid Ast.builtin_uuid_to_name with
  | Some VBlackhole, None -> 
      raise (Errors.InterpError ("Circular dependency detected: uuid " ^ string_of_int uuid ^ " used before initialization"))
  | Some v, None -> v
  | None, Some builtin_name -> VBuiltin (get_builtin_fun builtin_name)
  | Some _, Some _ -> raise (Errors.InterpError ("Somehow a uuid is bound to both a value and a builtin: uuid " ^ string_of_int uuid))
  | None, None -> raise (Errors.InterpError ("Unbound variable: uuid " ^ string_of_int uuid))

(** The core evaluation function. *)
and eval (e : tlexp) (env : env) : value =
  match e with
    | VarT (_, uuid_ref, _) -> lookup !uuid_ref env
    | LamT (x, param_uuid, body, _) -> VClosure (param_uuid, body, env)
    | LamUnitT (body, _) -> VClosureUnit (body, env)  (* -1 indicates no parameter *)
    | AppT (e1, e2, _) -> (
            let v2 = eval e2 env in
            match eval e1 env with
            | VClosure (param_uuid, body, c_env) -> 
                let param_env = ref ((param_uuid, v2) :: !c_env) in
                eval body param_env
            | VClosureUnit (body, c_env) -> 
                let param_env = ref !c_env in
                eval body param_env
            | VBuiltin f -> f v2
            | _ -> raise (Errors.InterpError "Application of a non-function")
        )
    | SeqT (e1, e2, _) -> let _ = eval e1 env in eval e2 env
    | IfT (cond, then_branch, else_branch, _) ->
        let interp_cond = eval cond env in
        (match interp_cond with
        | VI32 n -> if n <> 0l then eval then_branch env else eval else_branch env
        | _ -> raise (Errors.InterpError "Condition in if must be an integer"))
    | LetinT (x, param_uuid, e1, e2, _) -> 
        let v1 = eval e1 env in
        let param_env = ref ((param_uuid, v1) :: !env) in
        eval e2 param_env
    | LetrecinT (x, param_uuid, e1, e2, _) -> 
        let param_env = ref ((param_uuid, VBlackhole) :: !env) in
        let v1 = eval e1 param_env in
        param_env := (param_uuid, v1) :: !env;
        eval e2 param_env
    | LetinTupleT (iduuid_opt_list, e1, e2, _) ->
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
            | Shli32 -> if n2 < 0l || n2 >= 32l then raise (Errors.InterpError "Shift amount out of bounds") else VI32 (Int32.shift_left n1 (Int32.to_int n2))
            | Shri32 -> if n2 < 0l || n2 >= 32l then raise (Errors.InterpError "Shift amount out of bounds") else VI32 (Int32.shift_right n1 (Int32.to_int n2))
            | UShri32 -> if n2 < 0l || n2 >= 32l then raise (Errors.InterpError "Shift amount out of bounds") else VI32 (Int32.shift_right_logical n1 (Int32.to_int n2)))
        | _, _ -> raise (Errors.InterpError "Binary operation on non-integers"))
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
    | VecmkT (defval, size_list, _) -> (
        List.fold_right (fun size_exp defval ->
            match eval size_exp env with
            | VI32 n when n >= 0l -> 
                let new_arr = Array.make (Int32.to_int n) defval in
                VVec new_arr
            | _ -> raise (Errors.InterpError "vecmk size must be a non-negative integer")
        ) size_list (eval defval env)
    )
    | VeclenT (v, _) ->
        (match eval v env with
         | VVec arr -> VI32 (Int32.of_int @@ Array.length arr)
         | _ -> raise (Errors.InterpError "veclen expects a vector"))
    | VecgetT (v, idx_list, _) -> 
        List.fold_left (fun v_val idx_val ->
            let idx = match eval idx_val env with
                | VI32 n -> Int32.to_int n
                | _ -> raise (Errors.InterpError "vecget index must be an integer")
            in
            match v_val with
            | VVec arr when idx >= 0 && idx < Array.length arr -> arr.(idx)
            | _ -> raise ((Errors.InterpError "vecget expects a vector"))
        ) (eval v env) idx_list
    | VecsetT (v, setval, idx_list, _) -> 
        let rec vecsetaux (v_val : value) (idx_list : tlexp list) : value =
            match idx_list with
            | [idx_exp] -> (
                match v_val, eval idx_exp env with
                | VVec arr, VI32 idx when idx >= 0l && idx < Int32.of_int @@ Array.length arr -> (
                    let new_arr = Array.copy arr in
                    new_arr.(Int32.to_int idx) <- eval setval env;
                    VVec new_arr)
                | _, _ -> raise (Errors.InterpError "vecset expects a vector and a valid index")
            )
            | idx_exp :: tl -> (
                match v_val, eval idx_exp env with
                | VVec arr, VI32 idx when idx >= 0l && idx < Int32.of_int @@ Array.length arr -> (
                    let elm_val = arr.(Int32.to_int idx) in
                    let new_elm_val = vecsetaux elm_val tl in
                    let new_arr = Array.copy arr in
                    new_arr.(Int32.to_int idx) <- new_elm_val;
                    VVec new_arr)
                | _,_ -> raise (Errors.InterpError "vecset called non vector, non I32 index or index out of bounds")
            )
            | _ -> raise (Errors.InterpError "vecset expects an non empty index list")
        in
        vecsetaux (eval v env) idx_list
    | VecreszT (v, defval, newstart, newend, _) -> (
        let defval_val = eval defval env in
        match eval v env, eval newstart env, eval newend env with
        | VVec arr, VI32 start_off, VI32 end_off -> (
            let newlen = - (Int32.to_int start_off) + (Array.length arr) + (Int32.to_int end_off) in
            let new_arr = Array.init newlen 
                            (fun i -> if i < -(Int32.to_int start_off) || i >= (Array.length arr) - (Int32.to_int start_off) then
                                          defval_val
                                      else
                                          arr.(i+(Int32.to_int start_off))) in
            VVec new_arr )
        | _, _, _ -> raise (Errors.InterpError "vecresz expects a vector and valid integer indices")
    )


let interp_monotast (mtast : monotast) : unit =
  let global_env_ref = ref [] in

  (*stitch all bindings into the env, but with VBlackhole*)
  List.iter (fun (_, uuid, _) -> 
    global_env_ref := (uuid, VBlackhole) :: !global_env_ref
  ) mtast;
    
  List.iter (fun (name, uuid, e) -> 
    try
        let v = eval e global_env_ref in
        (*go and replace the VBlackhole used before in the env reference*)
        global_env_ref := List.map (fun (uuid', v') -> 
        if uuid' = uuid then (uuid', v) else (uuid', v')
        ) !global_env_ref;
    with
    | exn -> 
        let msg = Printexc.to_string exn in
        raise (Errors.InterpError ("Error while evaluating binding " ^ name ^ ": " ^ msg))
  ) mtast;
  try
    match List.find_opt (fun (name, uuid, _) -> name = "main") mtast with
        | Some (_,_ , main_fun) -> ignore (eval (AppT (main_fun, UnitLitT (TUnit), TUnit)) global_env_ref)
        | None -> ()
  with
    | exn -> 
        let msg = Printexc.to_string exn in
        raise (Errors.InterpError ("Error while evaluating main function " ^ ": " ^ msg))