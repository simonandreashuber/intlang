open Ast
open Errors

let rec veccheck_typ (t : typ) : unit =
  match repr t with
  | TUnit | TI32 | TI8 -> ()
  | TFun (t1, t2) ->
      veccheck_typ t1;
      veccheck_typ t2
  | TTup ts ->
      List.iter veccheck_typ ts
  | TVec inner ->
      let resolved_inner = repr inner in
      begin match resolved_inner with
      | TI32 | TI8 | TVec _ -> 
          (* Valid inner type; continue recursively checking nested structures *)
          veccheck_typ resolved_inner
      | TVar v -> raise (Errors.VecCheckError ("Polymorphic vector found: inner type is unsolved tvar t" ^ string_of_int v.id))
      | _ -> raise (Errors.VecCheckError "Invalid vector inner type: vectors of tuples, functions, or unit are not allowed")
      end
  | TVar v -> raise (Errors.VecCheckError ("Polymorphic vector found: inner type is unsolved tvar t" ^ string_of_int v.id))


(* Recursive traversal over the typed expression (tlexp) *)
let rec veccheck_tlexp (e : tlexp) : unit =

  veccheck_typ (tlexp_get_type e);
  
  match e with
  | VarT _ | I32LitT _ | I8LitT _ | UnitLitT _ -> ()
  | LamT (_, _, body, _) -> veccheck_tlexp body
  | LamUnitT (body, _) -> veccheck_tlexp body
  | AppT (e1, e2, _) -> veccheck_tlexp e1; veccheck_tlexp e2
  | SeqT (e1, e2, _) -> veccheck_tlexp e1; veccheck_tlexp e2
  | IfT (e1, e2, e3, _) -> veccheck_tlexp e1; veccheck_tlexp e2; veccheck_tlexp e3
  | LetinT (_, _, e1, e2, _) -> veccheck_tlexp e1; veccheck_tlexp e2
  | LetrecinT (_, _, e1, e2, _) -> veccheck_tlexp e1; veccheck_tlexp e2
  | LetinTupleT (_, e1, e2, _) -> veccheck_tlexp e1; veccheck_tlexp e2
  | TupleT (es, _) -> List.iter veccheck_tlexp es
  | UopI32T (_, e, _) -> veccheck_tlexp e
  | UopI8T (_, e, _) -> veccheck_tlexp e
  | BopI32T (_, e1, e2, _) -> veccheck_tlexp e1; veccheck_tlexp e2
  | BopI8T (_, e1, e2, _) -> veccheck_tlexp e1; veccheck_tlexp e2
  | VecLitT (es, _) -> List.iter veccheck_tlexp es
  | VecmkT (e_def, es_sizes, _) -> 
      veccheck_tlexp e_def; 
      List.iter veccheck_tlexp es_sizes
  | VeclenT (e, _) -> veccheck_tlexp e
  | VecgetT (e_vec, es_idxs, _) -> 
      veccheck_tlexp e_vec; 
      List.iter veccheck_tlexp es_idxs
  | VecsetT (e_vec, e_val, es_idxs, _) -> 
      veccheck_tlexp e_vec; 
      veccheck_tlexp e_val; 
      List.iter veccheck_tlexp es_idxs
  | VecsliceT (e_vec, e_start, e_len, _) -> 
      veccheck_tlexp e_vec; 
      veccheck_tlexp e_start; 
      veccheck_tlexp e_len
  | VecextendT (e_vec, e_lit, e_off, _) -> 
      veccheck_tlexp e_vec; 
      veccheck_tlexp e_lit; 
      veccheck_tlexp e_off

let veccheck_monotast (ast : monotast) : unit =
  List.iter (fun (_, _, expr) -> veccheck_tlexp expr) ast