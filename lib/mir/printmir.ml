open Mir
open Rpo
open Preds
open Live
open Borrow
open Dom

(* ========================================================================= *)
(* String Helpers for Identifier and Primitive Types                         *)
(* ========================================================================= *)

let string_of_ssa (id : ssaid) : string =
  Printf.sprintf "%%%d" id

let string_of_ssaconsume (c : ssaconsume) : string =
  if c.consume 
  then Printf.sprintf "%%%d!" c.ssaid
  else Printf.sprintf "%%%d" c.ssaid

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

let string_of_ownership = function
  | Borrowed -> "Borrowed"
  | Owned    -> "Owned"
  | NoMem    -> "NoMem"

let string_of_ssaids (args : ssaid list) : string =
  String.concat " " (List.map string_of_ssa args)

let string_of_ssaconsumes (args : ssaconsume list) : string =
  String.concat " " (List.map string_of_ssaconsume args)

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
  | Func (dst, fid) ->
      Printf.sprintf "%s = func %s" (string_of_ssa dst) (string_of_funcid fid)
  | Pack (dst, oldclos, args) ->
      let args_str = if args = [] then "" else " " ^ string_of_ssaconsumes args in
      Printf.sprintf "%s = pack %s%s" (string_of_ssa dst) (string_of_ssaconsume oldclos) args_str
  | CallClosure (dst, clos) ->
      Printf.sprintf "%s = callclosure %s" (string_of_ssa dst) (string_of_ssaconsume clos)
  | CallDirect (dst, fid, args) ->
      let args_str = if args = [] then "" else " " ^ string_of_ssaconsumes args in
      Printf.sprintf "%s = calldirect %s%s" (string_of_ssa dst) (string_of_funcid fid) args_str
  | GarbageCollect mems ->
      let mems_str = if mems = [] then "" else " " ^ string_of_ssaids mems in
      Printf.sprintf "garbagecollect%s" mems_str
  | StoreGlobal (gid, v) ->
      Printf.sprintf "storeglobal %s %s" (string_of_globalid gid) (string_of_ssaconsume v)
  | LoadGlobal (dst, gid) ->
      Printf.sprintf "%s = loadglobal %s" (string_of_ssa dst) (string_of_globalid gid)
  | Immi32 (dst, v) ->
      Printf.sprintf "%s = immi32 %s" (string_of_ssa dst) (Int32.to_string v)
  | Immi8 (dst, c) ->
      Printf.sprintf "%s = immi8 %d" (string_of_ssa dst) (Char.code c)
  | ImmUnit dst ->
      Printf.sprintf "%s = immunit" (string_of_ssa dst)
  | Uopi32 (dst, uop, a) ->
      Printf.sprintf "%s = uopi32 %s %s" (string_of_ssa dst) (string_of_uopi32 uop) (string_of_ssa a)
  | Uopi8 (dst, uop, a) ->
      Printf.sprintf "%s = uopi8 %s %s" (string_of_ssa dst) (string_of_uopi8 uop) (string_of_ssa a)
  | Bopi32 (dst, bop, a, b) ->
      Printf.sprintf "%s = bopi32 %s %s %s" (string_of_ssa dst) (string_of_bopi32 bop) (string_of_ssa a) (string_of_ssa b)
  | Bopi8 (dst, bop, a, b) ->
      Printf.sprintf "%s = bopi8 %s %s %s" (string_of_ssa dst) (string_of_bopi8 bop) (string_of_ssa a) (string_of_ssa b)
  | Tupinit (dst, elms) ->
      Printf.sprintf "%s = tupinit %s" (string_of_ssa dst) (string_of_ssaconsumes elms)
  | Tupextract (elms, tup) ->
      Printf.sprintf "(%s) = tupextract %s" (string_of_ssaids elms) (string_of_ssaconsume tup)
  | Tupview (elms, tup) ->
      Printf.sprintf "(%s) = tupview %s" (string_of_ssaids elms) (string_of_ssa tup)
  | Veclit (dst, elms) ->
      Printf.sprintf "%s = veclit %s" (string_of_ssa dst) (string_of_ssaconsumes elms)
  | Vecinit (dst, defval, dims) ->
      Printf.sprintf "%s = vecinit %s %s" (string_of_ssa dst) (string_of_ssa defval) (string_of_ssaids dims)
  | Veclen (dst, vec) ->
      Printf.sprintf "%s = veclen %s" (string_of_ssa dst) (string_of_ssa vec)
  | Vecread (dst, vec, idxs) ->
      Printf.sprintf "%s = vecread %s %s" (string_of_ssa dst) (string_of_ssa vec) (string_of_ssaids idxs)
  | Vecwrite (dst, vec, v, idxs) ->
      Printf.sprintf "%s = vecwrite %s %s %s" (string_of_ssa dst) (string_of_ssaconsume vec) (string_of_ssa v) (string_of_ssaids idxs)
  | Vecinsert (dst, vec, vecins, idxs) ->
      Printf.sprintf "%s = vecinsert %s %s %s" (string_of_ssa dst) (string_of_ssaconsume vec) (string_of_ssaconsume vecins) (string_of_ssaids idxs)
  | Vecslice (dst, vec, start, len) ->
      Printf.sprintf "%s = vecslice %s %s %s" (string_of_ssa dst) (string_of_ssa vec) (string_of_ssa start) (string_of_ssa len)
  | Vecextend (dst, vec, lit, off) ->
      Printf.sprintf "%s = vecextend %s %s %s" (string_of_ssa dst) (string_of_ssa vec) (string_of_ssa lit) (string_of_ssa off)
(* ========================================================================= *)
(* Branches & Terminators                                                    *)
(* ========================================================================= *)

let string_of_branch (brn : branch) =
  if brn.args = [] then
    string_of_bbid brn.bbid
  else
    Printf.sprintf "%s(%s)" (string_of_bbid brn.bbid) (string_of_ssaconsumes brn.args)

let string_of_term = function
  | Br target -> 
      Printf.sprintf "br %s" (string_of_branch target)
  | Cbr (cond, target_then, target_else) ->
      Printf.sprintf "cbr %s %s %s" (string_of_ssa cond) (string_of_branch target_then) (string_of_branch target_else)
  | Ret arg -> 
      Printf.sprintf "ret %s" (string_of_ssaconsume arg)

(* ========================================================================= *)
(* Basic Blocks, Functions, and Program                                      *)
(* ========================================================================= *)

let string_of_bb (bb : bb) : string =
  let args_str =
    if bb.args = [] then "()"
    else
      "(" ^ String.concat ", " (List.map (fun arg -> Printf.sprintf "%s" (string_of_ssa arg)) bb.args) ^ ")"
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

let string_of_ssa_info_table ssatyps memown =
  let len1 = Dynarray.length ssatyps in
  let len2 = Dynarray.length memown in
  let len = min len1 len2 in

  let acc = ref "" in

  if len1 <> len2 then
    failwith (Printf.sprintf "string_of_ssa_info_table: ssatyps and memown have different lengths: %d vs %d" len1 len2);

  acc := !acc ^ Printf.sprintf "+-------+--------------------------------+-----------+\n";
  acc := !acc ^ Printf.sprintf "| SSAID | MIR Type                       | Ownership |\n";
  acc := !acc ^ Printf.sprintf "+-------+--------------------------------+-----------+\n";

  for i = 0 to len - 1 do
    let typ_str = string_of_typ (Dynarray.get ssatyps i) in
    let own_str = string_of_ownership (Dynarray.get memown i) in
    
    (* %%%-4d formats as "%123 ", %-30s pads string to 30 chars right *)
    acc := !acc ^ Printf.sprintf "| %%%-4d | %-30s | %-9s |\n" i typ_str own_str
  done;
  
  acc := !acc ^ Printf.sprintf "+-------+--------------------------------+-----------+\n";
  !acc

(* Helpers *)
let string_of_int_list lst =
  "[" ^ String.concat "; " (List.map string_of_int lst) ^ "]"

let string_of_ssaset s =
  "{" ^ String.concat ", " (List.map string_of_int (SsaSet.elements s)) ^ "}"

(* 1. Preds *)
let string_of_preds_info (p : preds_info) =
  let s = ref "--- Preds Info ---\nBBID | Predecessors\n-------------------\n" in
  Array.iteri (fun bbid preds ->
    s := !s ^ Printf.sprintf "%4d | %s\n" bbid (string_of_int_list preds)
  ) p.preds;
  !s

(* 2. RPO *)
let string_of_rpo_info (r : rpo_info) =
  Printf.sprintf "RPO List: %s\n" (String.concat ", " (List.map string_of_int r.rpo_lst))

(* 3. Live *)
let string_of_live_info (l : live_info) =
  let s = ref "--- Live Info ---\nBBID | Live In                           | Live Out\n---------------------------------------------------------------------------\n" in
  let len = min (Array.length l.live_in) (Array.length l.live_out) in
  for bbid = 0 to (len - 1) do
    s := !s ^ Printf.sprintf "%4d | %-33s | %s\n" 
      bbid 
      (string_of_ssaset l.live_in.(bbid)) 
      (string_of_ssaset l.live_out.(bbid))
  done;
  !s

(* 4. Borrow *)
let string_of_borrow_info (b : borrow_info) =
  let s = ref "--- Borrow Info ---\nLender SSAID | Borrowers\n------------------------\n" in
  Array.iteri (fun lender borrowers ->
    match borrowers with
    | [] -> ()
    | _ -> s := !s ^ Printf.sprintf "%12d | %s\n" lender (string_of_int_list borrowers)
  ) b.lender_to_borrowers;
  !s

(* 5. Dom *)
let string_of_dom_info (d : dom_info) =
  let s = ref "--- Dom Info ---\nBBID | IDom\n-----------\n" in
  Array.iteri (fun bbid idom ->
    let idom_str = match idom with Some i -> string_of_int i | None -> "None" in
    s := !s ^ Printf.sprintf "%4d | %s\n" bbid idom_str
  ) d.idom;
  !s

let string_of_func (f : func) : string =
  let args_str =
    String.concat ", "
      (List.map (fun (id, opt_name) -> 
        match opt_name with
        | Some n -> Printf.sprintf "%s(\"%s\"): %s" (string_of_ssa id) n (string_of_typ @@ get_mirtyp_func f id)
        | None -> Printf.sprintf "%s: %s" (string_of_ssa id) (string_of_typ @@ get_mirtyp_func f id)
      ) f.args)
  in
  let header = Printf.sprintf "fn %s %s(%s) -> %s {" (string_of_funcid f.funcid) f.name args_str (string_of_typ f.rettyp) in
  let body_str =
  match f.extern_name with
  | Some ext_name -> Printf.sprintf "\t<extern: %s>" ext_name
  | None ->
    match f.entry_bb with
    | Some entry_bbid ->
        let rpo_info = Rpo.get_rpo_info f in
        let rpo_bbs = List.map (fun bbid -> BBMap.find bbid f.bbs) rpo_info.rpo_lst in
        String.concat "\n\n" (List.map string_of_bb rpo_bbs)
    | None -> "<no entry basic block>" ^ (
        f.bbs
        |> BBMap.bindings
        |> List.map (fun (_, bb) -> string_of_bb bb)
        |> String.concat "\n\n")
  in
  let analysis_str = 
    if f.extern_name <> None then ""
    else
    string_of_ssa_info_table f.ssatyps f.memown ^
    string_of_preds_info (get_preds_info f) ^
    string_of_rpo_info (get_rpo_info f) ^
    string_of_live_info (get_live_info f) ^
    string_of_borrow_info (get_borrow_info f) ^
    string_of_dom_info (get_dom_info f)
  in
  header ^ "\n" ^ body_str ^ "\n}\n" ^ analysis_str

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