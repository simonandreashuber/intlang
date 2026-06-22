open Ast

let rec sprint_typ (t : typ) : string =
    match repr t with
    | TUnit -> "()"
    | TI32 -> "i32"
    | TI8 -> "i8"
    | TTup t_list -> 
        let t_lst_str = List.map sprint_typ t_list in
        "(" ^ (String.concat ", " t_lst_str) ^ ")"
    | TVec t_inner -> "[" ^ (sprint_typ t_inner)  ^ "]"
    | TFun (t1, t2) -> (
        let t1str = sprint_typ t1 in
        let left = match repr t1 with
          | TFun _ -> "(" ^ t1str ^ ")"
          | _ -> t1str
        in
        let t2str = sprint_typ t2 in
        left ^ " -> " ^ t2str
      )
    | TVar tvar -> "t" ^ string_of_int tvar.id

(* Helper to generate indentation string *)
let indent (level : int) : string =
  String.make (level * 4) ' '

(* Sprint binary operator for i32 *)
let sprint_bopi32 (bop : bopi32) : string =
  match bop with
  | Eqi32 -> "=="
  | Neqi32 -> "!="
  | Lti32 -> "<"
  | Gti32 -> ">"
  | LtEqi32 -> "<="
  | GtEqi32 -> ">="
  | ULti32 -> "<u"
  | UGti32 -> ">u"
  | ULtEqi32 -> "<=u"
  | UGtEqi32 -> ">=u"
  | Muli32 -> "*"
  | Subi32 -> "-"
  | Addi32 -> "+"
  | Divi32 -> "/"
  | Modi32 -> "%"
  | UDivi32 -> "/u"
  | UModi32 -> "%u"
  | Andi32 -> "&"
  | Ori32 -> "|"
  | Xori32 -> "^"
  | Shli32 -> "<<"
  | Shri32 -> ">>"
  | UShri32 -> ">>u"

(* Sprint binary operator for i8 *)
let sprint_bopi8 (bop : bopi8) : string =
  match bop with
  | Eqi8 -> "==i8"
  | Neqi8 -> "!=i8"
  | Lti8 -> "<i8"
  | Gti8 -> ">i8"
  | LtEqi8 -> "<=i8"
  | GtEqi8 -> ">=i8"
  | Addi8 -> "+i8"
  | Subi8 -> "-i8"
  | Andi8 -> "&i8"
  | Ori8 -> "|i8"
  | Xori8 -> "^i8"

(* Sprint unary operator for i32 *)
let sprint_uopi32 (uop : uopi32) : string =
  match uop with
  | Negi32 -> "-"
  | Noti32 -> "~"

(* Sprint unary operator for i8 *)
let sprint_uopi8 (uop : uopi8) : string =
  match uop with
  | Negi8 -> "-i8"
  | Noti8 -> "~i8"

(* Note on the tab level: we use indent tab for each newline *)
let rec sprint_lexp (tab : int) (l : lexp) : string =
  let ind = indent tab in
  let ind_next = indent (tab + 1) in
  match l with
  | Var s -> s
  | I32Lit i -> string_of_int i
  | I8Lit c -> "'" ^ Char.escaped c ^ "'"
  | UnitLit -> "()"
  | LamUnit body -> "\\().\n" ^ ind_next ^ sprint_lexp (tab + 1) body
  
  | Lam (id, inT_opt, outT_opt, body) -> (
      let typ_str = match inT_opt, outT_opt with
        | None, None -> ""
        | Some inT, None -> " : " ^ sprint_typ inT
        | Some inT, Some outT -> " : " ^ sprint_typ inT ^ " => " ^ sprint_typ outT
        | None, Some _ -> "" (* shouldn't happen *)
      in
      match body with
      | Lam _ -> "\\" ^ id ^ typ_str ^ ". " ^ sprint_lexp tab body
      | _ -> "\\" ^ id ^ typ_str ^ ".\n" ^ ind_next ^ sprint_lexp (tab + 1) body
    )
  | Letin (id, e, body) ->
      "let " ^ id ^ " =\n" ^ ind_next ^ sprint_lexp (tab + 1) e ^ "\n" ^
      ind ^ "in\n" ^ ind_next ^ sprint_lexp (tab + 1) body
  | Letrecin (id, e, body) ->
      "let rec " ^ id ^ " =\n" ^ ind_next ^ sprint_lexp (tab + 1) e ^ "\n" ^
      ind ^ "in\n" ^ ind_next ^ sprint_lexp (tab + 1) body
  | LetinTuple (ids, e, body) ->
      "let (" ^ String.concat ", " ids ^ ") =\n" ^ ind_next ^ sprint_lexp (tab + 1) e ^ "\n" ^
      ind ^ "in\n" ^ ind_next ^ sprint_lexp (tab + 1) body
  | Tuple exprs ->
      "(" ^ String.concat ", " (List.map (sprint_lexp tab) exprs) ^ ")"
  | App (f, arg) -> (*could be smarter*)
      "(" ^ sprint_lexp tab f ^ ") (" ^ sprint_lexp tab arg ^ ")"
  | UopI32 (uop, e) ->
      sprint_uopi32 uop ^ " " ^ sprint_lexp tab e
  | UopI8 (uop, e) ->
      sprint_uopi8 uop ^ " " ^ sprint_lexp tab e
  | BopI32 (bop, left, right) ->
      "(" ^ sprint_lexp tab left ^ ") " ^ sprint_bopi32 bop ^ " (" ^ sprint_lexp tab right ^ ")"
  | BopI8 (bop, left, right) ->
      "(" ^ sprint_lexp tab left ^ ") " ^ sprint_bopi8 bop ^ " (" ^ sprint_lexp tab right ^ ")"
  | If (cond, then_e, else_e) ->
      "if " ^ sprint_lexp tab cond ^ " then\n" ^
      ind_next ^ sprint_lexp (tab + 1) then_e ^ "\n" ^
      ind ^ "else\n" ^
      ind_next ^ sprint_lexp (tab + 1) else_e ^ "\n" ^
      ind ^ "end"
  | VecLit exprs ->
      "vec[" ^ String.concat ", " (List.map (sprint_lexp tab) exprs) ^ "]"
  | Vecmk (defval, size_list) ->
      "vecmk[" ^ sprint_lexp tab defval ^ ", " ^ String.concat ", " (List.map (sprint_lexp tab) size_list) ^ "]"
  | Veclen v ->
      "veclen[" ^ sprint_lexp tab v ^ "]"
  | Vecget (v, size_list) ->
      "vecget[" ^ sprint_lexp tab v ^ (if size_list = [] then "" else ", ") ^ String.concat ", " (List.map (sprint_lexp tab) size_list) ^ "]"
  | Vecset (v, val_e, idx_list) ->
      "vecset[" ^ sprint_lexp tab v ^ (if idx_list = [] then "" else ", ") ^ String.concat ", " (List.map (sprint_lexp tab) idx_list) ^ ", " ^ sprint_lexp tab val_e ^ "]"
  | Vecresz (v, newlen, idx_list) ->
      "vecresz[" ^ sprint_lexp tab v ^ ", " ^ sprint_lexp tab newlen ^ (if idx_list = [] then "" else ", ") ^ String.concat ", " (List.map (sprint_lexp tab) idx_list) ^ "]"

(* Sprint statements with proper indentation *)
let sprint_stmt (tab : int) (st : stmt) : string =
  let ind = indent tab in
  match st with
  | IncludeGlobal id -> ind ^ "include " ^ id ^ "\n"
  | IncludeRelative path -> ind ^ "include \"" ^ path ^ "\"\n"
  | Let (id, e) ->  ind ^ "let " ^ id ^ " = " ^ sprint_lexp (tab + 1) e ^ ";"
  | Letrec (id, e) -> ind ^ "let rec " ^ id ^ " = " ^ sprint_lexp (tab + 1) e ^ ";"
  | Letrecblk (id, e) -> ind ^ "let recblk " ^ id ^ " = " ^ sprint_lexp (tab + 1) e ^ ";"
  | Lexp e -> ind ^ sprint_lexp tab e

let sprint_parseout p : string = List.fold_left ( fun acc st -> acc ^ (sprint_stmt 0 st) ^ "\n" ) "" p

let print_parseout p : unit = Printf.printf "%s" (sprint_parseout p)

 

(*
let sprint_schema (Forall (vars, t) : schema) : string =
  let vars_str = if vars = [] then "" else "forall " ^ String.concat " " (List.map (fun v -> "t" ^ string_of_int v) vars) ^ ". " in
  vars_str ^ sprint_typ t

let sprint_env (env : typenv) : string =
  let bindings = List.map (fun (name, (Forall (vars, t), uuid)) -> 
    let vars_str = if vars = [] then "" else "forall " ^ String.concat " " (List.map (fun v -> "t" ^ string_of_int v) vars) ^ ". " in
    name ^ "(uuid=" ^ string_of_int uuid ^ ")" ^ " : " ^ vars_str ^ sprint_typ t
  ) env in
  String.concat "\n" bindings

let sprint_constraints (cs : constraints) : string =
  let cs_strs = List.map (fun (t1, t2) -> sprint_typ t1 ^ "==" ^ sprint_typ t2) cs in
  String.concat "\n" cs_strs

let sprint_constraint ((t1, t2) : typ*typ) : string =
  sprint_typ t1 ^ "==" ^ sprint_typ t2

let sprint_scc (topord : letblk list) : string =
  let scc_strs = List.mapi (fun i blk -> 
    let blk_str = String.concat ", " (List.map (fun (name, lexp) -> name) blk) in
    Printf.sprintf "SCC %d:[%s]" i blk_str
  ) topord in
  String.concat "\n" scc_strs

let rec sprint_lexpt (e : lexpt) : string =
  match e with
  | VarT (sref, _, _) -> !sref
  | LamT (s, _, body, _) -> Printf.sprintf "\\%s.(%s)" s (sprint_lexpt body)
  | AppT (e1, e2, _) -> Printf.sprintf "(%s)(%s)" (sprint_lexpt e1) (sprint_lexpt e2)
  | IntT (i, _) -> Printf.sprintf "%d" i
  | BopT (bop, e1, e2, _) -> Printf.sprintf "(%s)%s(%s)" (sprint_lexpt e1) (sprint_bop bop) (sprint_lexpt e2)
  | IfT (c, t, e, _) -> Printf.sprintf "if %s then %s else %s end" (sprint_lexpt c) (sprint_lexpt t) (sprint_lexpt e)
  | LetinT (s, _, e, b, _) -> Printf.sprintf "let %s = %s in %s" s (sprint_lexpt e) (sprint_lexpt b)
  | VeclitT (ls, _) -> Printf.sprintf "vec[%s]" (String.concat ", " (List.map sprint_lexpt ls))
  | VecmkT (defval, count, _) -> Printf.sprintf "vecmk[%s, %s]" (sprint_lexpt defval) (sprint_lexpt count)
  | VeclenT (v, _) -> Printf.sprintf "veclen[%s]" (sprint_lexpt v)
  | VecgetT (v, i, _) -> Printf.sprintf "vecget[%s, %s]" (sprint_lexpt v) (sprint_lexpt i)
  | VecsetT (v, i, value, _) -> Printf.sprintf "vecset[%s, %s, %s]" (sprint_lexpt v) (sprint_lexpt i) (sprint_lexpt value)

let sprint_progmonot letblk : string =
  List.fold_left (fun acc (name, uuid, lexpt) -> 
      acc ^ (Printf.sprintf "let %s (uuid=%d) = (%s);\n" name uuid (sprint_lexpt lexpt))
  ) "" letblk
let rec sprint_lexpt_wtyp (e : lexpt) : string =
  let rec sprint_lexpt_wtyp_aux (e : lexpt) : string =
    match e with
    | VarT (sref, uref, t) -> Printf.sprintf "\027[1;31m%s\027[0m{uuid=%d,%s}\027[1;31m" !sref !uref (sprint_typ t)
    | LamT (s, _, body, t) -> Printf.sprintf "\027[1;31m\\%s.(%s)\027[0m{%s}\027[1;31m" s (sprint_lexpt_wtyp_aux body) (sprint_typ t)
    | AppT (e1, e2, t) -> Printf.sprintf "\027[1;31m(%s)(%s)\027[0m{%s}\027[1;31m" (sprint_lexpt_wtyp_aux e1) (sprint_lexpt_wtyp_aux e2) (sprint_typ t)
    | IntT (i, t) -> Printf.sprintf "\027[1;31m%d\027[0m{%s}\027[1;31m" i (sprint_typ t)
    | BopT (bop, e1, e2, t) -> Printf.sprintf "\027[1;31m(%s)%s(%s)\027[0m{%s}\027[1;31m" (sprint_lexpt_wtyp_aux e1) (sprint_bop bop) (sprint_lexpt_wtyp_aux e2) (sprint_typ t)
    | IfT (c, t_branch, e_branch, t) -> Printf.sprintf "\027[1;31mif %s then %s else %s end\027[0m{%s}\027[1;31m" (sprint_lexpt_wtyp_aux c) (sprint_lexpt_wtyp_aux t_branch) (sprint_lexpt_wtyp_aux e_branch) (sprint_typ t)
    | LetinT (s, _, e, b, t) -> Printf.sprintf "\027[1;31mlet %s = %s in %s\027[0m{%s}\027[1;31m" s (sprint_lexpt_wtyp_aux e) (sprint_lexpt_wtyp_aux b) (sprint_typ t)
    | VeclitT (ls, t) -> Printf.sprintf "\027[1;31mvec[%s]\027[0m{%s}\027[1;31m" (String.concat ", " (List.map sprint_lexpt_wtyp_aux ls)) (sprint_typ t)
    | VecmkT (defval, count, t) -> Printf.sprintf "\027[1;31mvecmk[%s, %s]\027[0m{%s}\027[1;31m" (sprint_lexpt_wtyp_aux defval) (sprint_lexpt_wtyp_aux count) (sprint_typ t)
    | VeclenT (v, t) -> Printf.sprintf "\027[1;31mveclen[%s]\027[0m{%s}\027[1;31m" (sprint_lexpt_wtyp_aux v) (sprint_typ t)
    | VecgetT (v, i, t) -> Printf.sprintf "\027[1;31mvecget[%s, %s]\027[0m{%s}\027[1;31m" (sprint_lexpt_wtyp_aux v) (sprint_lexpt_wtyp_aux i) (sprint_typ t)
    | VecsetT (v, i, value, t) -> Printf.sprintf "\027[1;31mvecset[%s, %s, %s]\027[0m{%s}\027[1;31m" (sprint_lexpt_wtyp_aux v) (sprint_lexpt_wtyp_aux i) (sprint_lexpt_wtyp_aux value) (sprint_typ t)
  in
  (sprint_lexpt_wtyp_aux e) ^ "\027[0m"

let sprint_progpolyt_wtyp letblk : string =
  List.fold_left (fun acc (name, uuid, vars, lexpt) -> 
      acc ^ (Printf.sprintf "\027[1;31mlet %s \027[0m(uuid=%d,[%s])\027[1;31m = (%s);\027[0m\n" name uuid (String.concat ", " (List.map (fun i -> "t" ^ string_of_int i) vars)) (sprint_lexpt_wtyp lexpt))
  ) "" letblk

let sprint_progmonot_wtyp letblk : string =
  List.fold_left (fun acc (name, uuid, lexpt) -> 
      acc ^ (Printf.sprintf "\027[1;31mlet %s \027[0m(uuid=%d)\027[1;31m = (%s);\027[0m\n" name uuid (sprint_lexpt_wtyp lexpt))
  ) "" letblk

let sprint_instreg (instreg : instreg) : string =
  let bindings = List.map (fun (uuid, sublst) -> 
    "uuid = " ^ string_of_int uuid ^ ": " ^ 
          (String.concat ", " 
              (List.map (fun (i, tvar) -> "t" ^ string_of_int i ^ " -> " ^ (sprint_typ (repr (TVar tvar)))) sublst))
  ) instreg in
  String.concat "\n" bindings
  *)