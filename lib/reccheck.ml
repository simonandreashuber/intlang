open Ast
open Errors

let rec is_lambda e =
  match e with
  | Lam _ | LamUnit _ -> true
  | Letin (_, _, e2) | LetinTuple (_, _, e2) | Letrecin (_, _, e2) -> is_lambda e2
  | _ -> false

let rec reccheck_lexp (e : lexp) : unit = 
  match e with
      | Var v -> ()
      | Lam (_, _, _, body) -> reccheck_lexp body
      | LamUnit body -> reccheck_lexp body
      | Letin (_, e1, e2) -> reccheck_lexp e1; reccheck_lexp e2
      | Letrecin (id, e1, e2) -> if is_lambda e1 then (reccheck_lexp e1; reccheck_lexp e2) else raise (RecCheckError ("let rec " ^ id ^ " = ... in is not a lambda"))
      | LetinTuple (_, e1, e2) -> reccheck_lexp e1; reccheck_lexp e2
      | Tuple es -> List.iter reccheck_lexp es
      | App (e1, e2) -> reccheck_lexp  e1; reccheck_lexp  e2
      | Seq (e1, e2) -> reccheck_lexp  e1; reccheck_lexp  e2
      | I32Lit n -> ()
      | I8Lit c -> ()
      | UnitLit -> ()
      | UopI32 (_, e) -> reccheck_lexp e
      | BopI32 (_, e1, e2) -> reccheck_lexp e1; reccheck_lexp e2
      | UopI8 (_, e) -> reccheck_lexp e
      | BopI8 (_, e1, e2) -> reccheck_lexp  e1; reccheck_lexp  e2
      | If (c, t, f) -> reccheck_lexp c; reccheck_lexp t; reccheck_lexp f
      | VecLit es -> List.iter reccheck_lexp es
      | Vecmk (defval, size_list) -> reccheck_lexp  defval; List.iter reccheck_lexp size_list
      | Veclen v -> reccheck_lexp v
      | Vecget (v, idx_list) -> reccheck_lexp v; List.iter reccheck_lexp idx_list
      | Vecset (v, value, idx_list) -> reccheck_lexp v; reccheck_lexp value; List.iter reccheck_lexp idx_list
      | Vecresz(v, defval, newstart, newend) -> reccheck_lexp v; reccheck_lexp defval; reccheck_lexp newstart; reccheck_lexp newend
let reccheck  (ast : ast) : unit =
  List.iter (fun stmt ->
    match stmt with
    | Let (id, e) -> reccheck_lexp e
    | Letrec lst -> (
          List.iteri (fun i (id, e) ->
            if is_lambda e then reccheck_lexp e else raise (RecCheckError ((if i = 0 then "let rec " else "and ") ^ id ^ " = ... is not a lambda"))
            ) lst
          )
    | _ -> raise (RecCheckError "Encountered Include during RecCheck, likely an Include Pass Bug")
  ) ast