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