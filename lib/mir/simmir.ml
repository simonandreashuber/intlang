open Mir
open Printmir
open Errors

type value =
  | Vunit
  | Vi32 of Int32.t
  | Vi8 of char
  | Vtup of value list
  | Vclos of funcid * funcid * (value list) (*not such a accurate representation since the data ie here the value list is going to be heap allocated and here it is immutable but well*)
  | Vvec of int * int * (value array)

let is_memval (v : value) =
    match v with
    | Vunit | Vi32 _ | Vi8 _ -> false
    | Vtup _ | Vclos _ | Vvec _ -> true

let vecval_unpack (v : value) = match v with Vvec (off, len, arr) when off + len <= Array.length arr -> off, len, arr | _ -> failwith "vecval_unpack on non-vector or illegal off and len for array"
let i32val_unpack (v : value) = match v with | Vi32 i -> i | _ -> failwith "i32val_unpack: did not get an Vi32"

let global_values : (globalid, value) Hashtbl.t = Hashtbl.create 32

let store_global (globalid : globalid) (v : value) : unit =
  Hashtbl.replace global_values globalid v

let load_global (globalid : globalid) : value =
  try Hashtbl.find global_values globalid
  with Not_found -> failwith (Printf.sprintf "Simulator Error: Global ID %d not found" globalid)

let reset_globals () =
  Hashtbl.clear global_values


let rec simmir_func (p : program) (funcid : funcid) (args : value list) : value =
  let func = try FuncMap.find funcid p.funcs
  with Not_found -> failwith (Printf.sprintf "Simulator Error: Function ID %d not found" funcid) in

  (* 1. Handle external functions (e.g., standard library calls) *)
  match func.extern_name with
  | Some ext_name ->
      (match ext_name, args with
        | "readi8", [Vunit] -> (Vi8 (input_char stdin))
        | "writei8", [Vi8 c]  -> (let _ = output_char stdout c in Vunit)
        | "flush", [Vunit] -> (flush stdout; Vunit)
        | "i32_to_i8", [Vi32 n] when n >= 0l && n <= 255l -> Vi8 (Char.chr (Int32.to_int n))
        | "i8_to_i32", [Vi8 c] -> (Vi32 (Int32.of_int (Char.code c)))
        | _ -> failwith (Printf.sprintf "Simulator: External function '%s' not implemented or arg mismatch" ext_name))
  | None ->
      
  (* 2. def up the local environment *)
  let values : (ssaid, value) Hashtbl.t = Hashtbl.create 32 in

  let use (s : ssaid) = 
    try Hashtbl.find values s 
    with Not_found -> failwith (Printf.sprintf "Simulator Error: SSA ID %d used before definition in %s" s func.name) 
  in

  let consume (sc : ssaconsume) = 
    let v = use sc.ssaid in
    if is_memval v
    then (if sc.consume then (Hashtbl.remove values sc.ssaid; v) else v)
    else ( if sc.consume then failwith "consume: consumed a non memory value this should not happen" else v )
  in

  let def (s : ssaid) (v : value) = Hashtbl.replace values s v in

  let copy_memvalue (v : value) : value =
    let rec copy_memvalue_aux (v : value) : value =
        match v with
        | Vunit -> Vunit
        | Vi32 n -> Vi32 n
        | Vi8 c -> Vi8 c
        | Vtup lst -> Vtup (List.map copy_memvalue_aux lst)
        | Vclos (fid, fid2, env) -> Vclos (fid, fid2, List.map copy_memvalue_aux env)
        | Vvec (off, len, arr) -> Vvec (off, len ,Array.map copy_memvalue_aux arr)
        in
    if is_memval v 
    then copy_memvalue_aux v 
    else failwith "copy_memvalue: Cannot (or more like IR shouldnt) copy primitive values"
  in

  let copy_value (v : value) : value =
    if is_memval v 
    then copy_memvalue v 
    else v
  in

  let consume_or_copy (sc : ssaconsume) = 
    let v = use sc.ssaid in
    if is_memval v
    then (if sc.consume then (Hashtbl.remove values sc.ssaid; v) else copy_memvalue v)
    else ( if sc.consume then failwith "consume_or_copy: consumed a non memory value this should not happen" else v )
  in

  let drop (ssaid : ssaid ) = 
    Hashtbl.remove values ssaid
  in

  (* 3. Bind function arguments to their respective SSA IDs *)
  if List.length func.args <> List.length args then
    failwith (Printf.sprintf "Simulator Error: %s expected %d args, got %d" func.name (List.length func.args) (List.length args));
  List.iter2 (fun (ssaid, _) v -> def ssaid v) func.args args;

  (* 4. Operation Evaluator *)
  let eval_op (op : op) =
    match op with
    | Func (res, fid_ref, fid2opt_ref) -> (
        match !fid2opt_ref with
        | Some fid2 -> def res (Vclos (!fid_ref, fid2, []))
        | None -> def res (Vclos (!fid_ref, !fid_ref, [])) (*externals need this*)
    )
    | Pack (res, clos_sc, args_sc_list) ->
        let fid, fid2, env = match consume_or_copy clos_sc with Vclos (f, f2, e) -> (f, f2, e) | _ -> failwith "Pack on non-closure" in
        let new_args = List.map (consume_or_copy) args_sc_list in
        def res (Vclos (fid, fid2, env @ new_args))
    | CallClosure (res, clos_sc) ->
        let fid, fid2, env = match consume clos_sc with Vclos (f, f2, e) -> (f, f2, e) | _ -> failwith "CallClosure on non-closure" in
        let resv = if clos_sc.consume then simmir_func p fid2 env else simmir_func p fid env in
        def res resv
    | CallDirect (res, fid_ref, args_sc_list) ->
        let evaled_args = List.map consume args_sc_list in
        def res (simmir_func p !fid_ref evaled_args)
    | Copy (res, ssaid) -> 
        def res (copy_memvalue @@ use ssaid)
    | Drop ssaid_list -> List.iter drop ssaid_list
    | StoreGlobal (gid, sc) -> 
        store_global gid (consume_or_copy sc)
    | LoadGlobal (res, gid) -> 
        def res (load_global gid)
    | Immi32 (res, i) -> def res (Vi32 i)
    | Immi8 (res, c) -> def res (Vi8 c)
    | ImmUnit res -> def res Vunit
    
    | Uopi32 (res, op, a) ->
        let av = match use a with Vi32 v -> v | _ -> failwith "Uopi32 type mismatch" in
        let out = match op with
          | Negi32 -> Int32.neg av
          | Noti32 -> Int32.lognot av
        in def res (Vi32 out)

    | Bopi32 (res, op, a, b) ->
        let n1 = match use a with Vi32 v -> v | _ -> failwith "Bopi32 arg1 mismatch" in
        let n2 = match use b with Vi32 v -> v | _ -> failwith "Bopi32 arg2 mismatch" in
        let out = match op with
            | Eqi32 -> Vi32 (if n1 = n2 then 1l else 0l)
            | Neqi32 -> Vi32 (if n1 <> n2 then 1l else 0l)
            | Lti32 -> Vi32 (if n1 < n2 then 1l else 0l)
            | Gti32 -> Vi32 (if n1 > n2 then 1l else 0l)
            | LtEqi32 -> Vi32 (if n1 <= n2 then 1l else 0l)
            | GtEqi32 -> Vi32 (if n1 >= n2 then 1l else 0l)
            (*note on the unsigned ops, ints in ocaml are 63 bits so we mask them to 32 bits which gives us the unsigned behavior *)
            | ULti32 -> Vi32 (if Int32.unsigned_compare n1 n2 < 0 then 1l else 0l)
            | UGti32 -> Vi32 (if Int32.unsigned_compare n1 n2 > 0 then 1l else 0l)
            | ULtEqi32 -> Vi32 (if Int32.unsigned_compare n1 n2 <= 0 then 1l else 0l)
            | UGtEqi32 -> Vi32 (if Int32.unsigned_compare n1 n2 >= 0 then 1l else 0l)
            | Muli32 -> Vi32 (Int32.mul n1 n2)
            | Subi32 -> Vi32 (Int32.sub n1 n2)
            | Addi32 -> Vi32 (Int32.add n1 n2)
            | Divi32 -> if n2 = 0l then raise (Errors.MirSimError "Division by zero") else Vi32 (Int32.div n1 n2)
            | Modi32 -> if n2 = 0l then raise (Errors.MirSimError "Modulo by zero") else Vi32 (Int32.rem n1 n2)
            | UDivi32 -> if n2 = 0l then raise (Errors.MirSimError "Unsigned division by zero") else Vi32 (Int32.unsigned_div n1 n2)
            | UModi32 -> if n2 = 0l then raise (Errors.MirSimError "Unsigned modulo by zero") else Vi32 (Int32.unsigned_rem n1 n2)
            | Andi32 -> Vi32 (Int32.logand n1 n2)
            | Ori32 -> Vi32 (Int32.logor n1 n2)
            | Xori32 -> Vi32 (Int32.logxor n1 n2)
            | Shli32 -> if n2 < 0l || n2 >= 32l then raise (Errors.MirSimError "Shift amount out of bounds") else Vi32 (Int32.shift_left n1 (Int32.to_int n2))
            | Shri32 -> if n2 < 0l || n2 >= 32l then raise (Errors.MirSimError "Shift amount out of bounds") else Vi32 (Int32.shift_right n1 (Int32.to_int n2))
            | UShri32 -> if n2 < 0l || n2 >= 32l then raise (Errors.MirSimError "Shift amount out of bounds") else Vi32 (Int32.shift_right_logical n1 (Int32.to_int n2))
        in 
        def res out

    | Uopi8 (res, op, a) ->
        let c = match use a with Vi8 c -> c | _ -> failwith "Uopi8 mismatch" in
        let out = match op with
            | Negi8 -> Vi8 (Char.chr (Int.logand (256-(Char.code c)) 0xFF))
            | Noti8 -> Vi8 (Char.chr (Int.logand (Int.lognot (Char.code c)) 0xFF))
        in def res out

    | Bopi8 (res, op, a, b) ->
        let c1 = match use a with Vi8 c -> c | _ -> failwith "Bopi8 arg1 mismatch" in
        let c2 = match use b with Vi8 c -> c | _ -> failwith "Bopi8 arg2 mismatch" in
        let out = match op with
            | Eqi8 -> Vi32 (if c1 = c2 then 1l else 0l)
            | Neqi8 -> Vi32 (if c1 <> c2 then 1l else 0l)
            | Lti8 -> Vi32 (if c1 < c2 then 1l else 0l)
            | Gti8 -> Vi32 (if c1 > c2 then 1l else 0l)
            | LtEqi8 -> Vi32 (if c1 <= c2 then 1l else 0l)
            | GtEqi8 -> Vi32 (if c1 >= c2 then 1l else 0l)
            | Addi8 -> Vi8 (Char.chr (((Char.code c1) + (Char.code c2)) mod 256))
            | Subi8 -> Vi8 (Char.chr ((256 + (Char.code c1) - (Char.code c2)) mod 256))
            | Andi8 -> Vi8 (Char.chr (Int.logand ((Char.code c1) land (Char.code c2)) 0xFF))
            | Ori8 -> Vi8 (Char.chr (Int.logand ((Char.code c1) lor (Char.code c2)) 0xFF))
            | Xori8 -> Vi8 (Char.chr (Int.logand ((Char.code c1) lxor (Char.code c2)) 0xFF))
        in def res out

    | Tupwrp (res, sc_list) ->
        def res (Vtup (List.map consume_or_copy sc_list))
        
    | Tupuwrp (res_list, tup_sc) ->
        let tup = match consume tup_sc with Vtup t -> t | _ -> failwith "Tupuwrp on non-tuple" in
        if List.length res_list <> List.length tup then failwith "Tuple destructuring arity mismatch";
        List.iter2 def res_list tup

    | Veclit (res, sc_list) ->
        def res (Vvec (0, List.length sc_list, Array.of_list (List.map consume_or_copy sc_list)))

    | Vecinit (res, defval, dims) ->
        let int_of_dim d = match use d with Vi32 n -> Int32.to_int n | _ -> failwith "Vecinit dim not i32" in
        let defvalv = use defval in
        let rec mkvec remaining_dims =
            match remaining_dims with
            | [lastdim] -> (Vvec (0, (int_of_dim lastdim), Array.init (int_of_dim lastdim) (fun _ -> copy_value defvalv)))
            | h :: tl -> (
                let dimint = int_of_dim h in
                let new_vec = Array.init dimint (fun _ -> mkvec tl) in
                Vvec (0, dimint, new_vec)
            )
            | [] -> failwith "Vecinit: no dimensions provided"
        in
        def res (mkvec dims)

    | Veclen (res, vec) ->
        let _, len, _ = vecval_unpack (use vec) in
        def res (Vi32 (Int32.of_int (len)))

    | Vecread (res, vec, idx_list) ->
        let rec readvec (v : value) (idxs : ssaid list) : value =
            match idxs with
            | [] -> v
            | idx_sc :: rest ->
                let off, len, arr = vecval_unpack v in
                let idx = match use idx_sc with Vi32 i -> Int32.to_int i | _ -> failwith "Vecread idx not i32" in
                if idx < 0 || idx >= len then failwith "Vecread index out of bounds";
                readvec arr.(off + idx) rest
        in
        def res (readvec (use vec) idx_list)

    | Vecwrite (res, vec, val_ins, idx_list) ->
        let new_val = use val_ins in
        if is_memval new_val then failwith "Vecwrite: value to write must be i32 or i8";
        let rec writevec (v : value) (idxs : ssaid list) : unit =
            match idxs with
            | [lastidx] -> (
                let off, len, arr = vecval_unpack v in
                let idx = match use lastidx with Vi32 i -> Int32.to_int i | _ -> failwith "Vecwrite idx not i32" in
                if idx < 0 || idx >= len then failwith "Vecwrite index out of bounds";
                Array.set arr (off + idx) new_val
            )
            | idx_sc :: rest -> (
                let off, len, arr = vecval_unpack v in
                let idx = match use idx_sc with Vi32 i -> Int32.to_int i | _ -> failwith "Vecwrite idx not i32" in
                if idx < 0 || idx >= len then failwith "Vecwrite index out of bounds";
                let subvec = arr.(off + idx) in
                writevec subvec rest
            )
            | [] -> failwith "Vecwrite: index list cannot be empty"
        in
        let new_vec = consume_or_copy vec in
        writevec new_vec idx_list;
        def res new_vec

    | Vecinsert (res, vec, val_ins, idx_list) ->
        let new_val = consume_or_copy val_ins in
        let rec writevec (v : value) (idxs : ssaid list) : unit =
            match idxs with
            | [lastidx] -> (
                let off, len, arr = vecval_unpack v in
                let idx = match use lastidx with Vi32 i -> Int32.to_int i | _ -> failwith "Vecwrite idx not i32" in
                if idx < 0 || idx >= len then failwith "Vecwrite index out of bounds";
                Array.set arr (off + idx) new_val
            )
            | idx_sc :: rest -> (
                let off, len, arr = vecval_unpack v in
                let idx = match use idx_sc with Vi32 i -> Int32.to_int i | _ -> failwith "Vecwrite idx not i32" in
                if idx < 0 || idx >= len then failwith "Vecwrite index out of bounds";
                let subvec = arr.(off + idx) in
                writevec subvec rest
            )
            | [] -> failwith "Vecinsert: index list cannot be empty"
        in
        let new_vec = consume_or_copy vec in
        writevec new_vec idx_list;
        def res new_vec

    | Vecslice (res, vec, start, nlen) ->
        let off, olen, arr = vecval_unpack (use vec) in
        let startv = Int32.to_int @@ i32val_unpack (use start) in
        let nlenv = Int32.to_int @@ i32val_unpack (use nlen) in 
        if startv < 0 then failwith "vecslice got negative start index";
        if startv + nlenv > olen then failwith "vecslice out of bounds";
        def res (Vvec (off + startv, nlenv, arr))

    | Vecextend (res, vec, lit, off) -> (
        let litv = use lit in
        let offint = Int32.to_int @@ i32val_unpack (use off) in
        let oldoff, oldlen, arr = vecval_unpack (use vec) in
        let newlen = oldlen + (abs offint) in
        let new_arr = 
            if 0 <= offint then
                Array.init newlen (fun i -> 
                    if oldlen <= i then copy_value litv else copy_value arr.(oldoff + i))
            else 
                Array.init newlen (fun i -> 
                    if offint < -i then copy_value litv else copy_value arr.(oldoff + i + offint))
            in
        def res (Vvec (0, newlen, new_arr)) 
    )
  in

  (* 5. Basic Block Evaluator Loop *)
  let rec eval_bb (bb_id : bbid) (bb_args : value list) : value =
    let bb = find_bb_func func bb_id in
    
    (* A. Bind block arguments (Simulating Phi nodes / TCO args) *)
    if List.length bb.args <> List.length bb_args then
      failwith (Printf.sprintf "Simulator: Block %d in %s expected %d args, got %d" bb_id func.name (List.length bb.args) (List.length bb_args));
    List.iter2 def bb.args bb_args;

    (* B. Evaluate operations (Crucial: Reversing the list!) *)
    let ops = List.rev bb.ops in
    List.iter eval_op ops;

    (* C. Evaluate terminator *)
    match bb.term with
    | None -> failwith (Printf.sprintf "Simulator: Block %d in %s has no terminator!" bb_id func.name)
    | Some (Br (next_bb, args_sc)) ->
        let next_args = List.map consume args_sc in
        eval_bb next_bb next_args
    | Some (Cbr (cond, true_bb, false_bb)) ->
        let cond_int = Int32.to_int @@ i32val_unpack (use cond) in
        if cond_int <> 0 then eval_bb true_bb [] else eval_bb false_bb []
    | Some (Ret retssaid) ->(
        Hashtbl.iter (fun live_ssaid _ -> if get_ownership_func func live_ssaid = Owned && live_ssaid <> retssaid then 
            failwith (Printf.sprintf "Memory leak detected in function %s: SSA ID %d is owned but not consumed before return" func.name live_ssaid)
        ) values;
        use retssaid)
  in

  (* Start execution at the entry block *)
  match func.entry_bb with
  | Some entry_id -> eval_bb entry_id []
  | None -> failwith (Printf.sprintf "Simulator: Function %s has no entry block" func.name)

let simmir_program (p : program) = 
    try
        reset_globals ();
        (
        match p.init_globals_funcid with
        | Some fid -> if simmir_func p fid [Vunit] = Vunit then () else failwith "init_globals should return unit"
        | None -> ()
        );
        (
        match p.main_funcid with
        | Some fid -> if simmir_func p fid [Vunit] = Vunit then () else failwith "main should return unit"
        | None -> ()
        );
        (
        match p.uninit_globals_funcid with
        | Some fid -> if simmir_func p fid [Vunit] = Vunit then () else failwith "init_globals should return unit"
        | None -> ()
        )
    with e ->
        let msg = Printexc.to_string e in
        let backtrace = Printexc.get_backtrace () in
        (*Printf.eprintf "%s\n" (Printmir.string_of_program p);*)
        Printf.eprintf "Error during MirSim: %s\nBacktrace:\n%s\n" msg backtrace;
        raise e
