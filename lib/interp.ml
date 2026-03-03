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
exception MallformedAST
exception UndefinedFreeVarUsed
exception EvalLexpUnappliedLambda
exception LetDefsareSSA 

type ctxt = { glbbnd : (string * lexp) list;  (*assoc list of globally bound variables*)
              locbnd : (string * lexp) list;  (*assoc list of localy bound varabiles*)
              appstk : lexp list; }           (*applicaiton stack ie. in "(\x.\y.x+y) 5 6" we put 6 and 5 on this stack before they are "consumed" by the lamdas*)

(*lookup some binding*)
let bndg_lookup (id:string) (c:ctxt) : lexp = 
    match List.assoc_opt id c.locbnd with
        | Some l -> l
        | None -> (
                    match List.assoc_opt id c.glbbnd with
                        | Some l -> l
                        | None -> raise UndefinedFreeVarUsed 
                  )

(*set some local binding*)
(*assoc list functions will always find the first occurence hence prepending is sufficient, also locbnd is O(n) in AST size not recursion depth*)
let locbnd_set (id:string) (l:lexp) (c:ctxt) : ctxt = {c with locbnd = (id, l) :: c.locbnd}

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
    ((List.fold_left (fun acc (id, l) -> acc ^ "    " ^ id ^ " = " ^ (sprint_lexp l) ^ "\n") "{ locbnd: \n" c.locbnd))^
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
    let rec subloc (l:lexp) (c:ctxt) : lexp =
        match l with
            | Var id ->  (
                    (*if the Var is in the local bindings then replace the var with the binding*)
                    match List.assoc_opt id c.locbnd with
                        | Some l_tosub -> l_tosub
                        | None -> Var id
                    )
            | Lam(id, ln) -> (
                    (*if the id that gets localy bound by the lambda and hence shadows some 
                      existing local def remove this existing local binding*)
                    let locbnd_filter = List.filter_map 
                                         (fun (id', l') -> if id' == id then None else Some (id', l')) 
                                         c.locbnd in
                    let cn = {c with locbnd = locbnd_filter} in
                    Lam(id, subloc ln cn)
                    )
            | App (ll, lr) -> App(subloc ll c, subloc lr c)
            | Int i -> Int i
            | Bop (b,ll,lr) -> (
                        match subloc ll c, subloc lr c with
                            | Int i, Int j -> Int (bop_meta b i j)
                            | sub_ll, sub_lr -> Bop(b, sub_ll, sub_lr)
                    )
    in
    (*Printf.printf "interp_lexp %s with context: \n %s" (sprint_lexp l) (sprint_ctxt c);*)
    match l with
        | Int i -> i
        | Var id -> interp_lexp (bndg_lookup id c) c
        | Lam (id, l) -> (
                    match appstk_pop c with
                        | (cn, Some sub_l) -> (
                            let cnn = locbnd_set id sub_l cn in
                            interp_lexp l cnn
                        )
                        | (_, None) -> raise EvalLexpUnappliedLambda
                    )
        | App (ll, lr) -> (
                    let ll_locsub = subloc ll c in
                    let lr_locsub = subloc lr c in
                    let cn = { c with locbnd = [] } in
                    interp_lexp ll_locsub (appstk_push lr_locsub cn) 
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
(*TODO: enforce 
        - A used Var is either defined by some let or bound by some lambda
        - Small type System
        *)
let interp_prog (p:prog) : int = 
    let rec interp_prog_rec (p:prog) (c:ctxt) : int =
        match p with
            | (Nlexp(s, l)) :: tl -> (
                                if List.mem_assoc s c.glbbnd then raise LetDefsareSSA else 
                                interp_prog_rec tl {c with glbbnd = (s,l)::c.glbbnd} 
                                )
            | [Lexp l] -> interp_lexp l c
            | _ -> raise MallformedAST 
    in interp_prog_rec p { glbbnd = []; locbnd = []; appstk = [] }
