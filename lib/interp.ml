open Ast

(*PRINTING*)

let sprint_bop bop : string =
    match bop with
      | Eq  -> "=="
      | Lt  -> "<"
      | Mul -> "*"
      | Sub -> "-"
      | Add -> "+"
      
let rec sprint_lexp l : string =
    match l with
        | Var s -> Printf.sprintf "%s" s
        | Int i -> Printf.sprintf "%d" i
        | Lam (s,l) -> Printf.sprintf "\\%s.(%s)" s (sprint_lexp l)
        | Bop (bop, ll, lr) -> Printf.sprintf "(%s)%s(%s)" (sprint_lexp ll) (sprint_bop bop) (sprint_lexp lr)
        | App (ll, lr) -> Printf.sprintf "(%s)(%s)" (sprint_lexp ll) (sprint_lexp lr)

let sprint_stmt st : string =
    match st with
        | Nlexp (s,l) -> Printf.sprintf "let %s = (%s);" s (sprint_lexp l)
        | Lexp l -> Printf.sprintf "%s" (sprint_lexp l)

let sprint_prog p : string =
    (*string print the ast, similar to the input but with parenthesies to show the ast structure*)
    List.fold_left ( fun acc st -> acc ^ (sprint_stmt st) ^ "\n" ) "" p

let print_prog p : unit = Printf.printf "%s" (sprint_prog p)


(*INTERPRETER*)

exception MallformedAST of string
exception UndefinedFreeVarUsed of string
exception EvalLexpUnappliedLambda

type ctxt = { bndg : (string * lexp) list;  (*assoc list of all the bound variables*)
              appstk : lexp list;}          (*applicaiton stack ie. in "(\x.\y.x+y) 5 6" we put 6 and 5 on this stack before they are "consumed" by the lamdas*)

(*lookup some binding*)
let bndg_lookup (id:string) (c:ctxt) : lexp = 
    match List.assoc_opt id c.bndg with
        | Some l -> l
        | None -> raise (UndefinedFreeVarUsed ("Could not find given ID in bindings"))

(*set some binding*)
(*note could also just prepend new bindings but this could create ctxts of recursion depth size, which I think is likely bad :|| *)
let bndg_set (id:string) (l:lexp) (c:ctxt) : ctxt =
    if not (List.mem_assoc id c.bndg) then
        {c with bndg = (id, l) :: c.bndg}
      else
        {c with bndg = List.map (fun (id', l') -> if id' = id then (id, l) else (id', l')) c.bndg }

(*application stack push*)
let appstk_push (l:lexp) (c:ctxt) : ctxt =
    { c with appstk = l :: c.appstk}

(*application stack pop*)
let appstk_pop (c:ctxt) : ctxt * (lexp option) =
    match c.appstk with
        | l::tl -> ({c with appstk = tl}, Some l)
        | [] -> (c, None)

(*sprint ctxt*)
let sprint_ctxt (c:ctxt) : string =
    ((List.fold_left (fun acc (id, l) -> acc ^ "    " ^ id ^ " = " ^ (sprint_lexp l) ^ "\n") "{ bndg: \n" c.bndg))^
    ((List.fold_left (fun acc l -> acc ^ "    " ^ (sprint_lexp l) ^ "\n") "{ appstk: \n" c.appstk)) ^ "}\n"
   

(*interpret/evaluate some lambda expression given some context*)
let rec interp_lexp (l:lexp) (c:ctxt) : int =
    let bop_meta (b:bop) (lint:int) (rint:int) : int =
        match b with
            | Eq  -> if lint = rint then 1 else 0
            | Lt  -> if lint < rint then 1 else 0
            | Add -> lint + rint
            | Sub -> lint - rint
            | Mul -> lint * rint
    in
    (*Printf.printf "interp_lexp %s with context: \n %s" (sprint_lexp l) (sprint_ctxt c);*)
    match l with
        | Int i -> i
        | Var id -> interp_lexp (bndg_lookup id c) c
        | Lam (id, l) -> (
                    match appstk_pop c with
                        | (cn, Some sub_l) -> (
                            let cnn = bndg_set id sub_l cn in
                            interp_lexp l cnn
                        )
                        | (_, None) -> raise EvalLexpUnappliedLambda
                    )
        | App (ll, lr) -> (
                    let lr_eval = interp_lexp lr c in
                    interp_lexp ll (appstk_push (Int lr_eval) c) (*TODO: can also pass function types, which is not supported as of now*)
                    )
        | Bop (b, ll, lr) -> (
                    let ll_eval = interp_lexp ll c in
                    match b, ll_eval with
                        | Mul, 0 -> 0 (*allow for if with * recursion construct*)
                        | _,_ -> (
                            let lr_eval = interp_lexp lr c in
                            bop_meta b ll_eval lr_eval
                        )
                    )

(*interpret/evaluate a program*)
let interp_prog (p:prog) : int = 
    let rec interp_prog_rec (p:prog) (c:ctxt) : int =
        match p with
            | (Nlexp(s, l)) :: tl -> interp_prog_rec tl (bndg_set s l c) (*TODO: enforce ssa form*)
            | [Lexp l] -> interp_lexp l c
            | _ -> raise (MallformedAST ("AST is not of specified structure\n")) 
    in interp_prog_rec p { bndg = []; appstk = [] }
    
