open Ast

let sprint_prog p : string =
    (*string print the ast, similar to the input but with parenthesies to show the ast structure*)
    let sprint_bop bop : string =
        match bop with
          | Eq  -> "=="
          | Lt  -> "<"
          | Mul -> "*"
          | Sub -> "-"
          | Add -> "+"
    in 
    let rec sprint_lexp l : string =
        match l with
            | Var s -> Printf.sprintf "%s" s
            | Int i -> Printf.sprintf "%d" i
            | Lam (s,l) -> Printf.sprintf "\\%s.(%s)" s (sprint_lexp l)
            | Bop (bop, ll, lr) -> Printf.sprintf "(%s)%s(%s)" (sprint_lexp ll) (sprint_bop bop) (sprint_lexp lr)
            | App (ll, lr) -> Printf.sprintf "(%s)(%s)" (sprint_lexp ll) (sprint_lexp lr)
    in
    let sprint_stmt st : string =
        match st with
            | Nlexp (s,l) -> Printf.sprintf "let %s = (%s);" s (sprint_lexp l)
            | Lexp l -> Printf.sprintf "%s" (sprint_lexp l)
    in
    List.fold_left ( fun acc st -> acc ^ (sprint_stmt st) ^ "\n" ) "" p

let print_prog p : unit = Printf.printf "%s" (sprint_prog p)

