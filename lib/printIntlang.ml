open Ast

(*PRINTING*)
let sprint_bop bop : string =
    match bop with
      | Eq  -> "=="
      | Lt  -> "<"
      | Mul -> "*"
      | Sub -> "-"
      | Add -> "+"
      | Div -> "/"
      
let rec sprint_lexp l : string =
    match l with
        | Var s -> Printf.sprintf "%s" s
        | Int i -> Printf.sprintf "%d" i
        | Lam (s,l) -> Printf.sprintf "\\%s.(%s)" s (sprint_lexp l)
        | Bop (bop, ll, lr) -> Printf.sprintf "(%s)%s(%s)" (sprint_lexp ll) (sprint_bop bop) (sprint_lexp lr)
        | App (ll, lr) -> Printf.sprintf "(%s)(%s)" (sprint_lexp ll) (sprint_lexp lr)
        | If (c, t, e) -> Printf.sprintf "if %s then %s else %s end" (sprint_lexp c) (sprint_lexp t) (sprint_lexp e)
        | Letin (s, e, b) -> Printf.sprintf "let %s = %s in %s" s (sprint_lexp e) (sprint_lexp b)
        | Veclit ls -> Printf.sprintf "vec[%s]" (String.concat ", " (List.map sprint_lexp ls))
        | Vecmk (defval, count) -> Printf.sprintf "vecmk[%s, %s]" (sprint_lexp defval) (sprint_lexp count)
        | Veclen v -> Printf.sprintf "veclen[%s]" (sprint_lexp v)
        | Vecget (v, i) -> Printf.sprintf "vecget[%s, %s]" (sprint_lexp v) (sprint_lexp i)
        | Vecset (v, i, value) -> Printf.sprintf "vecset[%s, %s, %s]" (sprint_lexp v) (sprint_lexp i) (sprint_lexp value)

let rec sprint_lexp_shallow  (depth : int) (l : lexp) : string =
    if depth <= 0 then "..."
    else
    match l with
        | Var s -> Printf.sprintf "%s" s
        | Int i -> Printf.sprintf "%d" i
        | Lam (s,l) -> Printf.sprintf "\\%s.(%s)" s (sprint_lexp_shallow (depth - 1) l)
        | Bop (bop, ll, lr) -> Printf.sprintf "(%s)%s(%s)" (sprint_lexp_shallow (depth - 1) ll) (sprint_bop bop) (sprint_lexp_shallow (depth - 1) lr)
        | App (ll, lr) -> Printf.sprintf "(%s)(%s)" (sprint_lexp_shallow (depth - 1) ll) (sprint_lexp_shallow (depth - 1) lr)
        | If (c, t, e) -> Printf.sprintf "if %s then %s else %s end" (sprint_lexp_shallow (depth - 1) c) (sprint_lexp_shallow (depth - 1) t) (sprint_lexp_shallow (depth - 1) e)
        | Letin (s, e, b) -> Printf.sprintf "let %s = %s in %s" s (sprint_lexp_shallow (depth - 1) e) (sprint_lexp_shallow (depth - 1) b)
        | Veclit ls -> Printf.sprintf "vec[%s]" (String.concat ", " (List.map (sprint_lexp_shallow (depth - 1)) ls))
        | Vecmk (defval, count) -> Printf.sprintf "vecmk[%s, %s]" (sprint_lexp_shallow (depth - 1) defval) (sprint_lexp_shallow (depth - 1) count)
        | Veclen v -> Printf.sprintf "veclen[%s]" (sprint_lexp_shallow (depth - 1) v)
        | Vecget (v, i) -> Printf.sprintf "vecget[%s, %s]" (sprint_lexp_shallow (depth - 1) v) (sprint_lexp_shallow (depth - 1) i)
        | Vecset (v, i, value) -> Printf.sprintf "vecset[%s, %s, %s]" (sprint_lexp_shallow (depth - 1) v) (sprint_lexp_shallow (depth - 1) i) (sprint_lexp_shallow (depth - 1) value)

let sprint_stmt st : string =
    match st with
        | IncludeGlobal id -> Printf.sprintf "include %s" id
        | IncludeRelative path -> Printf.sprintf "include \"%s\"" path
        | Nlexp (s,l) -> Printf.sprintf "let %s = (%s);" s (sprint_lexp l)
        | Lexp l -> Printf.sprintf "%s" (sprint_lexp l)

let sprint_parseout p : string =
    (*string print the ast, similar to the input but with parenthesies to show the ast structure*)
    List.fold_left ( fun acc st -> acc ^ (sprint_stmt st) ^ "\n" ) "" p

let print_parseout p : unit = Printf.printf "%s" (sprint_parseout p)

let sprint_prog (letblk, lexp_opt) : string =
    let letblk_str = List.fold_left (fun acc (name, e) -> 
        acc ^ (sprint_stmt (Nlexp (name, e))) ^ "\n"
    ) "" letblk in
    let main_str = match lexp_opt with
                    | Some lexp -> sprint_stmt (Lexp lexp)
                    | None -> "" 
            in
    letblk_str ^ main_str ^ "\n"

let print_prog p : unit = Printf.printf "%s" (sprint_prog p)

(*PRINT FUNCTIONS*)

(*not sure if its always correct, all observed bugs were fixed*)
let sprint_typ (t : typ) : string =
  let rec aux (t : typ)  (vis : (int * int) list) : string * (int* int) list =
    match t with
    | TInt -> "int", vis
    | TVec t_inner -> 
        let t_inner_str, vis_t_inner = aux t_inner vis in
        "Vec[" ^ t_inner_str ^ "]", vis_t_inner
    | TFun (t1, t2) -> (
        let t1str, vis_t1 = aux t1 vis in
        let left = match t1 with
          | TFun _ -> "(" ^ t1str ^ ")"
          | _ -> t1str
        in
        let t2str, vis_t2 = aux t2 vis_t1 in
        left ^ " -> " ^ t2str, vis_t2
    )
    | TVar {id; link = Some t_linked; _ } -> 
        (
          match List.assoc_opt id vis with
            | Some count -> raise (Errors.TypeError "Type loop detected during printing: Recursive types are not allowed")
            | None -> (
              let t_linked_str, vis_t_linked = aux t_linked ((id, 0) :: vis) in
              match List.assoc_opt id vis_t_linked with
                | Some count when count > 0 -> "(" ^ t_linked_str ^ " as t" ^ string_of_int id ^ ")", List.filter (fun (i, _) -> i <> id) vis_t_linked
                | _ -> t_linked_str, List.filter (fun (i, _) -> i <> id) vis_t_linked
            )
        ) 
    | TVar {id; link = None; _ } -> "t" ^ string_of_int id (*^ " (lvl: " ^ string_of_int level ^ ")"*), vis
  in 
  let res_str, _ = aux t [] in
  res_str

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
  | VarT (s, _, _) -> s
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

let sprint_progt (letblk, lexpt_opt) : string =
    let letblk_str = List.fold_left (fun acc (name, uuid, lexpt) -> 
        acc ^ (Printf.sprintf "let %s (uuid=%d) = (%s);\n" name uuid (sprint_lexpt lexpt))
    ) "" letblk in
    let main_str = match lexpt_opt with
                    | Some lexpt -> sprint_lexpt lexpt
                    | None -> "" 
            in
    letblk_str ^ main_str ^ "\n"
let rec sprint_lexpt_wtyp (e : lexpt) : string =
  let rec sprint_lexpt_wtyp_aux (e : lexpt) : string =
    match e with
    | VarT (s, _, t) -> Printf.sprintf "\027[1;31m%s\027[0m{%s}\027[1;31m" s (sprint_typ t)
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
  
let sprint_progt_wtyp (letblk, lexpt_opt) : string =
    let letblk_str = List.fold_left (fun acc (name, uuid, lexpt) -> 
        acc ^ (Printf.sprintf "\027[1;31mlet %s \027[0m(uuid=%d)\027[1;31m = (%s);\027[0m\n" name uuid (sprint_lexpt_wtyp lexpt))
    ) "" letblk in
    let main_str = match lexpt_opt with
                    | Some lexpt -> sprint_lexpt_wtyp lexpt
                    | None -> "" 
            in
    letblk_str ^ main_str ^ "\n"

let sprint_instreg (instreg : instreg) : string =
  let bindings = List.map (fun (uuid, sublst) -> 
    "uuid = " ^ string_of_int uuid ^ ": " ^ 
          (String.concat ", " 
              (List.map (fun (i, tvar) -> "t" ^ string_of_int i ^ " -> " ^ (sprint_typ (repr (TVar tvar)))) sublst))
  ) instreg in
  String.concat "\n" bindings