open Mir

(* ========================================================================= *)
(* String Helpers for Identifier and Primitive Types                        *)
(* ========================================================================= *)

let string_of_ssa (id : ssaid) : string =
  Printf.sprintf "%%%d" id

let string_of_vecinnertype = function
  | TVECI32 -> "i32"
  | TVECI8  -> "i8"

let rec string_of_typ = function
  | TUnit -> "unit"
  | TI32 -> "i32"
  | TI8 -> "i8"
  | TFun (fid, packed) -> Printf.sprintf "fun(fn_%d, packed=%d)" fid packed
  | TTup typs -> "(" ^ String.concat ", " (List.map string_of_typ typs) ^ ")"
  | TVec (dim, inner) -> Printf.sprintf "vec<%d, %s>" dim (string_of_vecinnertype inner)

let string_of_args (args : ssaid list) : string =
  String.concat " " (List.map string_of_ssa args)

(* ========================================================================= *)
(* Operators                                                                 *)
(* ========================================================================= *)

let string_of_uopi32 = function
  | Negi32 -> "negi32"
  | Noti32 -> "noti32"

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
  | Negi8 -> "negi8"
  | Noti8 -> "noti8"

let string_of_bopi8 = function
  | Eqi8 -> "eqi8"       | Neqi8 -> "neqi8"     | Lti8 -> "lti8" 
  | Gti8 -> "gti8"       | LtEqi8 -> "lteqi8"   | GtEqi8 -> "gteqi8"
  | Addi8 -> "addi8"     | Subi8 -> "subi8"     | Andi8 -> "andi8" 
  | Ori8 -> "ori8"       | Xori8 -> "xori8"

(* ========================================================================= *)
(* Instructions (op)                                                         *)
(* ========================================================================= *)

let string_of_op = function
  | Pack (dst, t, args) ->
      Printf.sprintf "%s: %s = pack %s" (string_of_ssa dst) (string_of_typ t) (string_of_args args)
  | Call (dst, t, fn) ->
      Printf.sprintf "%s: %s = call %s" (string_of_ssa dst) (string_of_typ t) (string_of_ssa fn)
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
  | Vecinit (dst, t, defval, dims) ->
      Printf.sprintf "%s: %s = vecinit %s %s" (string_of_ssa dst) (string_of_typ t) (string_of_ssa defval) (string_of_args dims)
  | Veclit (dst, t, elms) ->
      Printf.sprintf "%s: %s = veclit %s" (string_of_ssa dst) (string_of_typ t) (string_of_args elms)
  | Vecread (dst, t, vec, idxs) ->
      Printf.sprintf "%s: %s = vecread %s %s" (string_of_ssa dst) (string_of_typ t) (string_of_ssa vec) (string_of_args idxs)
  | Vecwrite (dst, t, vec, args) ->
      Printf.sprintf "%s: %s = vecwrite %s %s" (string_of_ssa dst) (string_of_typ t) (string_of_ssa vec) (string_of_args args)
  | Vecslice (dst, t, vec, start, len) ->
      Printf.sprintf "%s: %s = vecslice %s %s %s" (string_of_ssa dst) (string_of_typ t) (string_of_ssa vec) (string_of_ssa start) (string_of_ssa len)
  | Vecextend (dst, t, vec, offset) ->
      Printf.sprintf "%s: %s = vecextend %s %s" (string_of_ssa dst) (string_of_typ t) (string_of_ssa vec) (string_of_ssa offset)

(* ========================================================================= *)
(* Branches & Terminators                                                    *)
(* ========================================================================= *)

let string_of_branch (target_bb, args) =
  if args = [] then
    Printf.sprintf "bb_%d" target_bb
  else
    Printf.sprintf "bb_%d(%s)" target_bb (String.concat ", " (List.map string_of_ssa args))

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

let string_of_bb ?(args : (ssaid * mirtyp) list = []) (bb : bb) : string =
  let args_str =
    if args = [] then "()"
    else
      "(" ^ String.concat ", " (List.map (fun (id, t) -> Printf.sprintf "%s: %s" (string_of_ssa id) (string_of_typ t)) args) ^ ")"
  in
  let header = Printf.sprintf "\t%s %s :" bb.name args_str in
  let ops_strs = List.map (fun op -> Printf.sprintf "\t\t%s" (string_of_op op)) bb.ops in
  let term_str =
    match bb.term with
    | Some t -> [Printf.sprintf "\t\t%s" (string_of_term t)]
    | None -> ["\t\t<missing terminator>"]
  in
  String.concat "\n" ([header] @ ops_strs @ term_str)

let string_of_func (f : func) : string =
  let args_str =
    String.concat ", "
      (List.map (fun (id, t) -> Printf.sprintf "%s: %s" (string_of_ssa id) (string_of_typ t)) f.args)
  in
  let header = Printf.sprintf "fn %s(%s) -> %s {" f.name args_str (string_of_typ f.rettyp) in
  let bbs_str =
    f.bbs
    |> List.mapi (fun i bb ->
        (* Pass the function args to the entry block (first block) *)
        let bb_args = if i = 0 then f.args else [] in
        string_of_bb ~args:bb_args bb)
    |> String.concat "\n\n"
  in
  header ^ "\n" ^ bbs_str ^ "\n}"

let string_of_program (prog : program) : string =
  String.concat "\n\n" (List.map string_of_func prog)

let print_program (prog : program) : unit =
  print_endline (string_of_program prog)