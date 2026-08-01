open Mir

(* ========================================================================= *)
(* String Helpers for Identifier and Primitive Types                         *)
(* ========================================================================= *)

let string_of_ssa (id : ssaid) : string =
  Printf.sprintf "%%%d" id

let string_of_funcid (id : funcid) : string =
  Printf.sprintf "@%d" id

let string_of_globalid (id : globalid) : string =
  Printf.sprintf "@g%d" id

let string_of_bbid (id : bbid) : string =
  Printf.sprintf "bb_%d" id

let string_of_vecinnertype = function
  | TMIRVECI32 -> "i32"
  | TMIRVECI8  -> "i8"

let rec string_of_typ = function
  | TMIRUnit -> "unit"
  | TMIRI32 -> "i32"
  | TMIRI8 -> "i8"
  | TMIRClos (args, ret) -> 
      Printf.sprintf "clos(%s -> %s)" 
        (String.concat ", " (List.map string_of_typ args)) 
        (string_of_typ ret)
  | TMIRTup typs -> "(" ^ String.concat ", " (List.map string_of_typ typs) ^ ")"
  | TMIRVec (dim, inner) -> Printf.sprintf "vec<%d, %s>" dim (string_of_vecinnertype inner)

let string_of_args (args : ssaid list) : string =
  String.concat " " (List.map string_of_ssa args)

(* ========================================================================= *)
(* Operators                                                                 *)
(* ========================================================================= *)

let string_of_uopi32 = function
  | Negi32 -> "negi32" | Noti32 -> "noti32"

let string_of_bopi32 = function
  | Eqi32 -> "eqi32"     | Neqi32 -> "neqi32"   | Lti32 -> "lti32" 
  | Gti32 -> "gti32"     | LtEqi32 -> "lteqi32" | GtEqi32 -> "gteqi32"
  | ULti32 -> "ulti32"   | UGti32 -> "ugti32"   | ULtEqi32 -> "ulteqi32" 
  | UGtEqi32 -> "ugteqi32"
  | Muli32 -> "muli32"   | Subi32 -> "subi32"   | Addi32 -> "addi32" 
  | Divi32 -> "divi32"   | Modi32 -> "modi32"   | UDivi32 -> "udivi32" 
  | UModi32 -> "umodi32"
  | Andi32 -> "andi32"   | Ori32 -> "ori32"     | Xori32 -> "xori32" 
  | Shli32 -> "shli32"   | Shri32 -> "shri32"   | UShri32 -> "ushri32"

let string_of_uopi8 = function
  | Negi8 -> "negi8" | Noti8 -> "noti8"

let string_of_bopi8 = function
  | Eqi8 -> "eqi8"       | Neqi8 -> "neqi8"     | Lti8 -> "lti8" 
  | Gti8 -> "gti8"       | LtEqi8 -> "lteqi8"   | GtEqi8 -> "gteqi8"
  | Addi8 -> "addi8"     | Subi8 -> "subi8"     | Andi8 -> "andi8" 
  | Ori8 -> "ori8"       | Xori8 -> "xori8"

(* ========================================================================= *)
(* Instructions (op)                                                         *)
(* ========================================================================= *)

let string_of_op = function
  | Func (dst, t, fid) ->
      Printf.sprintf "%s: %s = func %s" (string_of_ssa dst) (string_of_typ t) (string_of_funcid fid)
  | Pack (dst, t, oldclos, args) ->
      let args_str = if args = [] then "" else " " ^ string_of_args args in
      Printf.sprintf "%s: %s = pack %s%s" (string_of_ssa dst) (string_of_typ t) (string_of_ssa oldclos) args_str
  | CallClosure (dst, t, clos) ->
      Printf.sprintf "%s: %s = callclosure %s" (string_of_ssa dst) (string_of_typ t) (string_of_ssa clos)
  | CallDirect (dst, t, fid, args) ->
      let args_str = if args = [] then "" else " " ^ string_of_args args in
      Printf.sprintf "%s: %s = calldirect %s%s" (string_of_ssa dst) (string_of_typ t) (string_of_funcid fid) args_str
  | StoreGlobal (dst, t, gid, v) ->
      Printf.sprintf "%s: %s = storeglobal %s %s" (string_of_ssa dst) (string_of_typ t) (string_of_globalid gid) (string_of_ssa v)
  | LoadGlobal (dst, t, gid) ->
      Printf.sprintf "%s: %s = loadglobal %s" (string_of_ssa dst) (string_of_typ t) (string_of_globalid gid)
  | Uopi32 (dst, t, uop, a) ->
      Printf.sprintf "%s: %s = %s %s" (string_of_ssa dst) (string_of_typ t) (string_of_uopi32 uop) (string_of_ssa a)
  | Bopi32 (dst, t, bop, a, b) ->
      Printf.sprintf "%s: %s = %s %s %s" (string_of_ssa dst) (string_of_typ t) (string_of_bopi32 bop) (string_of_ssa a) (string_of_ssa b)
  | Uopi8 (dst, t, uop, a) ->
      Printf.sprintf "%s: %s = %s %s" (string_of_ssa dst) (string_of_typ t) (string_of_uopi8 uop) (string_of_ssa a)
  | Bopi8 (dst, t, bop, a, b) ->
      Printf.sprintf "%s: %s = %s %s %s" (string_of_ssa dst) (string_of_typ t) (string_of_bopi8 bop) (string_of_ssa a) (string_of_ssa b)
  | Immi32 (dst, t, v) ->
      Printf.sprintf "%s: %s = immi32 %s" (string_of_ssa dst) (string_of_typ t) (Int32.to_string v)
  | Immi8 (dst, t, c) ->
      Printf.sprintf "%s: %s = immi8 %d" (string_of_ssa dst) (string_of_typ t) (Char.code c)
  | ImmUnit (dst, t) ->
      Printf.sprintf "%s: %s = immunit" (string_of_ssa dst) (string_of_typ t)
  | Tupinit (dst, t, elms) ->
      Printf.sprintf "%s: %s = tupinit %s" (string_of_ssa dst) (string_of_typ t) (string_of_args elms)
  | Tupget (dst, t, tup, idx) ->
      Printf.sprintf "%s: %s = tupget %s %d" (string_of_ssa dst) (string_of_typ t) (string_of_ssa tup) idx
  | Veclit (dst, t, elms) ->
      Printf.sprintf "%s: %s = veclit %s" (string_of_ssa dst) (string_of_typ t) (string_of_args elms)
  | Vecinit (dst, t, defval, dims) ->
      Printf.sprintf "%s: %s = vecinit %s %s" (string_of_ssa dst) (string_of_typ t) (string_of_ssa defval) (string_of_args dims)
  | Veclen (dst, t, vec) ->
      Printf.sprintf "%s: %s = veclen %s" (string_of_ssa dst) (string_of_typ t) (string_of_ssa vec)
  | Vecread (dst, t, vec, idxs) ->
      Printf.sprintf "%s: %s = vecread %s %s" (string_of_ssa dst) (string_of_typ t) (string_of_ssa vec) (string_of_args idxs)
  | Vecwrite (dst, t, vec, v, idxs) ->
      Printf.sprintf "%s: %s = vecwrite %s %s %s" (string_of_ssa dst) (string_of_typ t) (string_of_ssa vec) (string_of_ssa v) (string_of_args idxs)
  | Vecslice (dst, t, vec, start, len) ->
      Printf.sprintf "%s: %s = vecslice %s %s %s" (string_of_ssa dst) (string_of_typ t) (string_of_ssa vec) (string_of_ssa start) (string_of_ssa len)
  | Vecextend (dst, t, vec, lit, off) ->
      Printf.sprintf "%s: %s = vecextend %s %s %s" (string_of_ssa dst) (string_of_typ t) (string_of_ssa vec) (string_of_ssa lit) (string_of_ssa off)

(* ========================================================================= *)
(* Branches & Terminators                                                    *)
(* ========================================================================= *)

let string_of_branch (target_bb, args) =
  if args = [] then
    string_of_bbid target_bb
  else
    Printf.sprintf "%s(%s)" (string_of_bbid target_bb) (string_of_args args)

let string_of_term = function
  | Br target -> 
      Printf.sprintf "br %s" (string_of_branch target)
  | Cbr (cond, target_then, target_else) ->
      Printf.sprintf "cbr %s %s %s" (string_of_ssa cond) (string_of_branch target_then) (string_of_branch target_else)
  | Ret arg -> 
      Printf.sprintf "ret %s" (string_of_ssa arg)

(* ========================================================================= *)
(* Basic Blocks, Functions, and Program                                      *)
(* ========================================================================= *)

let string_of_bb (bb : bb) : string =
  let args_str =
    if bb.args = [] then "()"
    else
      "(" ^ String.concat ", " (List.map (fun (id, t) -> Printf.sprintf "%s: %s" (string_of_ssa id) (string_of_typ t)) bb.args) ^ ")"
  in
  let header = Printf.sprintf "\tbb: %s (%s) %s :" (string_of_int bb.bbid) bb.name args_str in
  
  (* NOTE: bb.ops is stored in reverse order, so we reverse it here to print chronologically *)
  let ops_chronological = List.rev bb.ops in
  let ops_strs = List.map (fun op -> Printf.sprintf "\t\t%s" (string_of_op op)) ops_chronological in
  
  let term_str =
    match bb.term with
    | Some t -> [Printf.sprintf "\t\t%s" (string_of_term t)]
    | None -> ["\t\t<missing terminator>"]
  in
  String.concat "\n" ([header] @ ops_strs @ term_str)

let string_of_func (f : func) : string =
  let args_str =
    String.concat ", "
      (List.map (fun (id, opt_name, t) -> 
        match opt_name with
        | Some n -> Printf.sprintf "%s(\"%s\"): %s" (string_of_ssa id) n (string_of_typ t)
        | None -> Printf.sprintf "%s: %s" (string_of_ssa id) (string_of_typ t)
      ) f.args)
  in
  let header = Printf.sprintf "fn %s %s(%s) -> %s {" (string_of_funcid f.funcid) f.name args_str (string_of_typ f.rettyp) in

  let body_str =
  match f.extern_name with
  | Some ext_name -> Printf.sprintf "\t<extern: %s>" ext_name
  | None ->
    f.bbs
    |> List.map string_of_bb
    |> String.concat "\n\n"
  in
  header ^ "\n" ^ body_str ^ "\n}"

let string_of_program (prog : program) : string =
  (* 1. Print special entry point metadata *)
  let string_of_opt_funcid prefix = function
    | Some id -> Printf.sprintf "%s: %s" prefix (string_of_funcid id)
    | None -> Printf.sprintf "%s: <none>" prefix
  in
  let meta_str = 
    String.concat "\n" [
      string_of_opt_funcid "init_globals" prog.init_globals_funcid;
      string_of_opt_funcid "main" prog.main_funcid;
      string_of_opt_funcid "uninit_globals" prog.uninit_globals_funcid;
    ]
  in

  (* 2. Print globals (Iterate Map) *)
  let globals_str =
    GlobalMap.bindings prog.globals
    |> List.map (fun (gid, g) -> 
        Printf.sprintf "global %s: %s" (string_of_globalid gid) (string_of_typ g.typ))
    |> String.concat "\n"
  in

  (* 3. Print functions (Iterate Map) *)
  let funcs_str =
    FuncMap.bindings prog.funcs
    |> List.map (fun (_, f) -> string_of_func f)
    |> String.concat "\n\n"
  in

  String.concat "\n\n" [meta_str; globals_str; funcs_str]

let print_program (prog : program) : unit =
  print_endline (string_of_program prog)