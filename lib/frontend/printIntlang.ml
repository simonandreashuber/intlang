(*

  Generate Strings of the AST and TAST

*)


open Ast

let rec sprint_typ (t : typ) : string =
    match repr t with
    | TUnit -> "()"
    | TI32 -> "i32"
    | TI8 -> "i8"
    | TTup t_list -> 
        let t_lst_str = List.map sprint_typ t_list in
        "(" ^ (String.concat "," t_lst_str) ^ ")"
    | TVec t_inner -> "[" ^ (sprint_typ t_inner)  ^ "]"
    | TFun (t1, t2) -> (
        let t1str = sprint_typ t1 in
        let left = match repr t1 with
          | TFun _ -> "(" ^ t1str ^ ")"
          | _ -> t1str
        in
        let t2str = sprint_typ t2 in
        left ^ "->" ^ t2str
      )
    | TVar tvar -> "t" ^ string_of_int tvar.id

let sprint_schema (Forall (vars, t) : schema) : string =
  let vars_str = if vars = [] then "" else "forall " ^ String.concat " " (List.map (fun v -> "t" ^ string_of_int v) vars) ^ ". " in
  vars_str ^ sprint_typ t

let sprint_env (env : typenv) : string =
  let bindings = List.map (fun (name, (Forall (vars, t), uuid)) -> 
    let vars_str = if vars = [] then "" else "forall " ^ String.concat " " (List.map (fun v -> "t" ^ string_of_int v) vars) ^ ". " in
    name ^ "(uuid=" ^ string_of_int uuid ^ ")" ^ " : " ^ vars_str ^ sprint_typ t
  ) env in
  String.concat "\n" bindings

let sprint_constraint ((t1, t2) : typ*typ) : string =
  sprint_typ t1 ^ "==" ^ sprint_typ t2

let indent (level : int) : string =
  String.make (level * 4) ' '

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

let sprint_uopi32 (uop : uopi32) : string =
  match uop with
  | Negi32 -> "-"
  | Noti32 -> "~"

let sprint_uopi8 (uop : uopi8) : string =
  match uop with
  | Negi8 -> "-i8"
  | Noti8 -> "~i8"

(* note on the tab level: indent tab is used for each newline *)
let rec sprint_lexp_wdepth (d_opt : int option) (tab : int) (l : lexp) : string =
  match d_opt with
  | Some d when d <= 0 -> "..."
  | _ -> (
  let next_d = 
    match d_opt with
    | Some d -> Some (d-1)
    | None -> None in
  let ind = indent tab in
  let ind_next = indent (tab + 1) in
  match l with
  | Var s -> s
  | I32Lit i -> string_of_int i
  | I8Lit c -> "'" ^ Char.escaped c ^ "'"
  | UnitLit -> "()"
  | LamUnit body -> "\\().\n" ^ ind_next ^ sprint_lexp_wdepth next_d (tab + 1) body
  
  | Lam (id, inT_opt, outT_opt, body) -> (
      let typ_str = match inT_opt, outT_opt with
        | None, None -> ""
        | Some inT, None -> " : " ^ sprint_typ inT
        | Some inT, Some outT -> " : " ^ sprint_typ inT ^ " => " ^ sprint_typ outT
        | None, Some _ -> "" (* shouldn't happen *)
      in
      match body with
      | Lam _ -> "\\" ^ id ^ typ_str ^ ". " ^ sprint_lexp_wdepth next_d tab body
      | _ -> "\\" ^ id ^ typ_str ^ ".\n" ^ ind_next ^ sprint_lexp_wdepth next_d (tab + 1) body
    )
  | Letin (id, e, body) ->
      "let " ^ id ^ " =\n" ^ ind_next ^ sprint_lexp_wdepth next_d (tab + 1) e ^ "\n" ^
      ind ^ "in\n" ^ ind_next ^ sprint_lexp_wdepth next_d (tab + 1) body
  | Letrecin (id, e, body) ->
      "let rec " ^ id ^ " =\n" ^ ind_next ^ sprint_lexp_wdepth next_d (tab + 1) e ^ "\n" ^
      ind ^ "in\n" ^ ind_next ^ sprint_lexp_wdepth next_d (tab + 1) body
  | LetinTuple (ids, e, body) ->
      "let (" ^ String.concat ", " ids ^ ") =\n" ^ ind_next ^ sprint_lexp_wdepth next_d (tab + 1) e ^ "\n" ^
      ind ^ "in\n" ^ ind_next ^ sprint_lexp_wdepth next_d (tab + 1) body
  | Tuple exprs ->
      "(" ^ String.concat ", " (List.map (sprint_lexp_wdepth next_d tab) exprs) ^ ")"
  | App (f, arg) -> (*could be smarter*)
      "(" ^ sprint_lexp_wdepth next_d tab f ^ ") (" ^ sprint_lexp_wdepth next_d tab arg ^ ")"
  | Seq (e1, e2) ->
      sprint_lexp_wdepth next_d tab e1 ^ ";\n" ^ sprint_lexp_wdepth next_d tab e2
  | UopI32 (uop, e) ->
      sprint_uopi32 uop ^ " " ^ sprint_lexp_wdepth next_d tab e
  | UopI8 (uop, e) ->
      sprint_uopi8 uop ^ " " ^ sprint_lexp_wdepth next_d tab e
  | BopI32 (bop, left, right) ->
      "(" ^ sprint_lexp_wdepth next_d tab left ^ ") " ^ sprint_bopi32 bop ^ " (" ^ sprint_lexp_wdepth next_d tab right ^ ")"
  | BopI8 (bop, left, right) ->
      "(" ^ sprint_lexp_wdepth next_d tab left ^ ") " ^ sprint_bopi8 bop ^ " (" ^ sprint_lexp_wdepth next_d tab right ^ ")"
  | If (cond, then_e, else_e) ->
      "if " ^ sprint_lexp_wdepth next_d tab cond ^ " then\n" ^
      ind_next ^ sprint_lexp_wdepth next_d (tab + 1) then_e ^ "\n" ^
      ind ^ "else\n" ^
      ind_next ^ sprint_lexp_wdepth next_d (tab + 1) else_e ^ "\n" ^
      ind ^ "end"
  | VecLit exprs ->
      "vec[" ^ String.concat ", " (List.map (sprint_lexp_wdepth next_d tab) exprs) ^ "]"
  | Vecmk (defval, size_list) ->
      "vecmk[" ^ sprint_lexp_wdepth next_d tab defval ^ ", " ^ String.concat ", " (List.map (sprint_lexp_wdepth next_d tab) size_list) ^ "]"
  | Veclen v ->
      "veclen[" ^ sprint_lexp_wdepth next_d tab v ^ "]"
  | Vecget (v, size_list) ->
      "vecget[" ^ sprint_lexp_wdepth next_d tab v ^ (if size_list = [] then "" else ", ") ^ String.concat ", " (List.map (sprint_lexp_wdepth next_d tab) size_list) ^ "]"
  | Vecset (v, val_e, idx_list) ->
      "vecset[" ^ sprint_lexp_wdepth next_d tab v ^ (if idx_list = [] then "" else ", ") ^ String.concat ", " (List.map (sprint_lexp_wdepth next_d tab) idx_list) ^ ", " ^ sprint_lexp_wdepth next_d tab val_e ^ "]"
  | Vecslice(v, start, len) ->
      "vecslice[" ^ sprint_lexp_wdepth next_d tab v ^ ", " ^ sprint_lexp_wdepth next_d tab start ^ ", " ^ sprint_lexp_wdepth next_d tab len ^ "]"
  | Vecextend(v, lit, off) ->
      "vecextend[" ^ sprint_lexp_wdepth next_d tab v ^ ", " ^ sprint_lexp_wdepth next_d tab lit ^ ", " ^ sprint_lexp_wdepth next_d tab off ^ "]"
)

let sprint_lexp (tab : int) (l : lexp) : string = sprint_lexp_wdepth None tab l

let sprint_stmt (tab : int) (st : stmt) : string =
  let ind = indent tab in
  match st with
  | IncludeGlobal id -> ind ^ "include " ^ id ^ "\n"
  | IncludeRelative path -> ind ^ "include \"" ^ path ^ "\"\n"
  | Let (id, e) ->  ind ^ "let " ^ id ^ " = " ^ sprint_lexp (tab + 1) e
  | Letrec lst -> (
      match lst with
      | ((id, e) :: tl) -> List.fold_left 
                              (fun acc (id, e) -> acc ^ "\n" ^ ind ^ "and " ^ id ^ " = " ^ sprint_lexp (tab + 1) e) 
                              (ind ^ "let rec " ^ id ^ " = " ^ sprint_lexp (tab + 1) e) tl
      | _ -> raise (Errors.PrintError "Empty Letrec")
  )

let sprint_ast p : string = List.fold_left ( fun acc st -> acc ^ (sprint_stmt 0 st) ^ "\n" ) "" p

let print_ast p : unit = Printf.printf "%s" (sprint_ast p)

let escfatred = "\027[1;31m"
let escreset = "\027[0m"
let fatred s = escfatred ^ s ^ escreset
let fatredbreak s = escreset ^ s ^ escfatred

let rec sprint_tlexp (tab : int) (l : tlexp) : string =
  let pt t = fatredbreak @@ "{" ^ sprint_typ t ^ "}" in
  let puuid uuid = fatredbreak @@ "{uuid=" ^ string_of_int uuid ^ "}" in

  let ind = indent tab in
  let ind_next = indent (tab + 1) in
  match l with
  | VarT (sref, uuidref, t) -> (fatred !sref) ^ puuid !uuidref ^ pt t
  | LamT (id, uuid, body, t) -> (
      match body with
      (*mb. readability would profit from adding the type only on the outer most lam 
        but it is not trivial to implement (ie. needs a ref or passing one moore param)*)
      | LamT _ -> (fatred @@ "\\" ^ id ^ ". " ^ sprint_tlexp tab body) ^ pt t 
      | _ -> (fatred @@ "\\" ^ id ^ ".\n" ^ ind_next ^ sprint_tlexp (tab + 1) body) ^ pt t
    )
  | LamUnitT (body, t) -> (fatred @@ "\\().\n" ^ ind_next ^ sprint_tlexp (tab + 1) body) ^ pt t
  | AppT (f, arg, t) -> (*could be smarter*)
      (fatred @@ "(" ^ sprint_tlexp tab f ^ ") (" ^ sprint_tlexp tab arg ^ ")") ^ pt t
  | SeqT (e1, e2, t) ->
      (fatred @@ sprint_tlexp tab e1 ^ ";\n" ^ sprint_tlexp tab e2) ^ pt t
  | IfT (cond, then_e, else_e, t) ->
      (fatred @@ "if " ^ sprint_tlexp tab cond ^ " then\n" ^
      ind_next ^ sprint_tlexp (tab + 1) then_e ^ "\n" ^
      ind ^ "else\n" ^
      ind_next ^ sprint_tlexp (tab + 1) else_e ^ "\n" ^
      ind ^ "end") ^ pt t
  | LetinT (id, uuid, e, body, t) ->
      (fatred @@ "let " ^ id ^ puuid uuid ^ " =\n" ^ ind_next ^ sprint_tlexp (tab + 1) e ^ "\n" ^
      ind ^ "in\n" ^ ind_next ^ puuid uuid ^ sprint_tlexp (tab + 1) body) ^ puuid uuid ^ pt t
  | LetrecinT (id, uuid, e, body, t) ->
      (fatred @@ "let rec " ^ id ^ " =\n" ^ ind_next ^ sprint_tlexp (tab + 1) e ^ "\n" ^
      ind ^ "in\n" ^ ind_next ^ sprint_tlexp (tab + 1) body) ^ puuid uuid ^ pt t
  | LetinTupleT (elmlst , e, body, t) -> (
      let elmlst_str = String.concat ", " ( List.map 
            (fun elm_opt -> 
              match elm_opt with
              | Some (id, uuid) -> fatred id ^ puuid uuid
              | None -> "_"
            ) elmlst ) in
      (fatred @@ "let (" ^ elmlst_str ^ ") =\n" ^ ind_next ^ sprint_tlexp (tab + 1) e ^ "\n" ^
      ind ^ "in\n" ^ ind_next ^ sprint_tlexp (tab + 1) body) ^ pt t)
  | TupleT (exprs, t) ->
      (fatred @@ "(" ^ String.concat ", " (List.map (sprint_tlexp tab) exprs) ^ ")") ^ pt t
  | I32LitT (i, t) -> (fatred @@ string_of_int i) ^ pt t
  | I8LitT (c,t) -> (fatred @@ "'" ^ Char.escaped c ^ "'") ^ pt t
  | UnitLitT t -> (fatred @@ "()") ^ pt t
  | UopI32T (uop, e, t) ->
      (fatred @@ sprint_uopi32 uop ^ " " ^ sprint_tlexp tab e) ^ pt t
  | UopI8T (uop, e, t) ->
      (fatred @@ sprint_uopi8 uop ^ " " ^ sprint_tlexp tab e) ^ pt t
  | BopI32T (bop, left, right, t) ->
      (fatred @@ "(" ^ sprint_tlexp tab left ^ ") " ^ sprint_bopi32 bop ^ " (" ^ sprint_tlexp tab right ^ ")") ^ pt t
  | BopI8T (bop, left, right, t) ->
      (fatred @@ "(" ^ sprint_tlexp tab left ^ ") " ^ sprint_bopi8 bop ^ " (" ^ sprint_tlexp tab right ^ ")") ^ pt t
  | VecLitT (exprs, t) ->
      (fatred @@ "vec[" ^ String.concat ", " (List.map (sprint_tlexp tab) exprs) ^ "]") ^ pt t
  | VecmkT (defval, size_list, t) ->
      (fatred @@ "vecmk[" ^ sprint_tlexp tab defval ^ ", " ^ String.concat ", " (List.map (sprint_tlexp tab) size_list) ^ "]") ^ pt t
  | VeclenT (v, t) ->
      (fatred @@ "veclen[" ^ sprint_tlexp tab v ^ "]") ^ pt t
  | VecgetT (v, size_list, t) ->
      (fatred @@ "vecget[" ^ sprint_tlexp tab v ^ (if size_list = [] then "" else ", ") ^ String.concat ", " (List.map (sprint_tlexp tab) size_list) ^ "]") ^ pt t
  | VecsetT (v, val_e, idx_list, t) ->
      (fatred @@ "vecset[" ^ sprint_tlexp tab v ^ (if idx_list = [] then "" else ", ") ^ String.concat ", " (List.map (sprint_tlexp tab) idx_list) ^ ", " ^ sprint_tlexp tab val_e ^ "]") ^ pt t
  | VecsliceT (v, start, len, t) ->
      (fatred @@ "vecslice[" ^ sprint_tlexp tab v ^ ", " ^ sprint_tlexp tab start ^ ", " ^ sprint_tlexp tab len ^ "]") ^ pt t
  | VecextendT (v, lit, off, t) ->
      (fatred @@ "vecextend[" ^ sprint_tlexp tab v ^ ", " ^ sprint_tlexp tab lit ^ ", " ^ sprint_tlexp tab off ^ "]") ^ pt t

let sprint_polytletbnd ((name, uuid, vars, lexpt) : polytletbnd) : string =
  let vars_str = String.concat ", " (List.map (fun i -> "t" ^ string_of_int i) vars) in
  fatred @@ "let " ^ name ^ (fatredbreak @@ " (uuid=" ^ string_of_int uuid ^ ",[" ^ vars_str ^ "])") ^ " = " ^ sprint_tlexp 0 lexpt

let sprint_polytast (tast : polytast) : string =
  (String.concat "\n" (List.map sprint_polytletbnd tast)) ^ "\n"

let sprint_monotletbnd ((name, uuid, lexpt) : monotletbnd) : string =
  fatred @@ "let " ^ name ^ (fatredbreak @@ " (uuid=" ^ string_of_int uuid ^ ")") ^ " = " ^ sprint_tlexp 0 lexpt

let sprint_monotast (tast : monotast) : string =
  (String.concat "\n" (List.map sprint_monotletbnd tast)) ^ "\n"