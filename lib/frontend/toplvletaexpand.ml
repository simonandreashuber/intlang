(*

  Top Level Eta-Expansion

  let abstract_pow = \multf. \a b. ...
  let pow = abstract_pow (\a b. a*b)

  into:

  let abstract_pow = \multf. \a b. ...
  let pow = \a b. abstract_pow (\a b. a*b) a b

  Eta-Expands top level bindings of function type with "application
  structure". This will ensure the mir generation will not lower
  it as a global but rather as a function.

*)

open Ast
open PrintIntlang

let toplvl_eta_expand (mtast : monotast) : monotast =
  (*
    let f = \a b c. ...         : ta -> tb -> tc -> t
    let g = f x                 : tb -> tc -> t

    becomes:

    let f = \a b c. ...         : ta -> tb -> tc -> t
    let g = \b c. f x b c       : tb -> tc -> t

  *)

  let rec eta_expand_typ (argstyplst : typ list) (resttyplst : typ list) (t: typ) : typ list * typ list * typ list =
    let t = repr t in
    match t with
    | TFun (ta, tb) -> (
      eta_expand_typ (ta :: argstyplst) (t :: resttyplst) tb
    )
    | _ -> (
      (* [tc -> t , t], [tb, tc] ,[tc -> t, tb -> tc -> t]*)
      List.tl @@ List.rev (t :: resttyplst), List.rev argstyplst, resttyplst
    )
  in

  List.map (fun (name, toplvluuid, e) ->
    match e with
    | AppT (a, b, t)
      when (match repr t with | TFun _ -> true | _ -> false) -> (
      (* [tc -> t , t], [tb, tc] ,[tc -> t, tb -> tc -> t]*)
      let app_typs, var_typs, lam_typs = eta_expand_typ [] [] t in
      (* [(tb, tc -> t), (tc, t)] *)
      let app_var_typs = List.combine var_typs app_typs in
      (* [b, c] *)
      let app_var_uuids = List.mapi (fun i _ -> ( "toplvleta_" ^ string_of_int i ,fresh_uuid ())) app_typs in
      (* [c, b] *)
      let lam_uuids = List.rev app_var_uuids in

      (* f x b c *)
      let e' = List.fold_left2 (fun e (name, uuid) (var_typ, app_typ) ->
          AppT(e, VarT (ref name, ref uuid, var_typ) , app_typ)
      ) e app_var_uuids app_var_typs in

      (* \b c. f x b c *)
      let e'' = List.fold_left2 (fun e (name, uuid) lam_typ ->
          LamT(name, uuid, e, lam_typ)
      ) e' lam_uuids lam_typs in
      (*Printf.printf "Changed: \n%s \n to \n%s\n" (sprint_monotletbnd (name, toplvluuid, e)) (sprint_monotletbnd (name, toplvluuid, e''));*)
     (name, toplvluuid, e'')
    )
    | _ -> (name, toplvluuid, e)
  ) mtast
