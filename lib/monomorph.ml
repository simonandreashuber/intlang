open Ast

(*MAP THE POLY VERSION TO THE MONO VERSIONS*)
let poly_to_mono_map : (uuid * (uuid * typ)) list ref = ref []
let poly_bnds : letblkpolyt ref = ref []

let find_poly_letbnd (poly_uuid : uuid) : letbndpolyt option =
  List.find_opt (fun (_, uuid, _, _) -> uuid = poly_uuid) !poly_bnds

let rec cmp_mono_typs (t1 : typ) (t2 : typ) : bool =
  match (repr t1, repr t2) with
  | (TInt, TInt) -> true
  | (TVec t1', TVec t2') -> cmp_mono_typs t1' t2'
  | (TFun (arg1, ret1), TFun (arg2, ret2)) -> cmp_mono_typs arg1 arg2 && cmp_mono_typs ret1 ret2
  | (TVar _,_) | (_, TVar _) -> raise (Errors.TypeError "Monomorphization Error: Expected monomorphic types, but found type variables.")
  | _ -> false

let find_mono_version (poly_uuid : uuid) (spec_typ : typ) : uuid option =
  match List.find_opt 
    (fun (p_uuid, (m_uuid, m_typ)) -> p_uuid = poly_uuid && cmp_mono_typs m_typ spec_typ)
    !poly_to_mono_map with
  | Some (_, (m_uuid,_)) -> Some m_uuid
  | None -> None

let register_mono_version (poly_uuid : uuid) (mono_uuid : uuid) (spec_typ : typ) : unit =
  poly_to_mono_map := (poly_uuid, (mono_uuid, spec_typ)) :: !poly_to_mono_map

let mangle_mono_name (name : string) (spec_typ : typ) : string =
  name ^ "@" ^ (String.concat "" (String.split_on_char ' ' (PrintIntlang.sprint_typ spec_typ)))

let extract_specialization_map (st : typ) (t : typ) : (int * typ) list =
  let rec aux st t acc =
    match (repr st, repr t) with
    | (TInt, TInt) -> acc
    | (TVec st', TVec t') -> aux st' t' acc
    | (TFun (arg_st, ret_st), TFun (arg_t, ret_t)) -> let acc' = aux arg_st arg_t acc in aux ret_st ret_t acc'
    | (TVar {id = var_id; _}, t) -> (var_id, t) :: acc
    | _ -> raise (Errors.TypeError "Monomorphization Error: Type structure mismatch during specialization map extraction.")
  in 
  (*I have an urge to sanitize the map here, but I think it should work without doing so...*)
  aux st t []

let rec apply_specialization_map (smap : (int * typ) list) (t : typ) : typ =
  match repr t with
  | TInt -> TInt
  | TVec t' -> TVec (apply_specialization_map smap t')
  | TFun (arg, ret) -> TFun (apply_specialization_map smap arg, apply_specialization_map smap ret)
  | TVar {id; link} -> 
      (match List.assoc_opt id smap with
      | Some t_subst -> t_subst
      | None -> TVar {id; link})

let specialize ((name, uuid, _, lhs) : string * uuid * (int list) * lexpt) (smap : (int * typ) list) : string * uuid * lexpt =
  let monouuid = fresh_uuid () in
  let sub = apply_specialization_map smap in
  let rec aux lexpt =
    match lexpt with
    | VarT (n, u, oldtyp) -> VarT (ref !n, ref !u, sub oldtyp)
    | LamT (n, u, b, oldtyp) -> LamT (n, u, aux b, sub oldtyp)
    | AppT (f, arg, oldtyp) -> AppT (aux f, aux arg, sub oldtyp)
    | IntT (i, oldtyp) -> IntT (i, sub oldtyp)
    | BopT (op, e1, e2, oldtyp) -> BopT (op, aux e1, aux e2, sub oldtyp)
    | IfT (c, t, e, oldtyp) -> IfT (aux c, aux t, aux e, sub oldtyp)
    | LetinT (n, u, e, b, oldtyp) -> LetinT (n, u, aux e, aux b, sub oldtyp)
    | VeclitT (elems, oldtyp) -> VeclitT (List.map aux elems, sub oldtyp)
    | VecmkT (def, cnt, oldtyp) -> VecmkT (aux def, aux cnt, sub oldtyp)
    | VeclenT (v, oldtyp) -> VeclenT (aux v, sub oldtyp)
    | VecgetT (v, i, oldtyp) -> VecgetT (aux v, aux i, sub oldtyp)
    | VecsetT (v, i, e, oldtyp) -> VecsetT (aux v, aux i, aux e, sub oldtyp)
  in 
  let sub_lhs = aux lhs in
  let mono_typ = lexpt_get_type sub_lhs in
  let mono_name = mangle_mono_name name mono_typ in
  (mono_name, monouuid, sub_lhs)

let rec monomorph_letbnd (lt : lexpt) : (uuid * letbndmonot) list =
  match lt with
  | VarT (nameref, uuidref, typ) -> (
    match find_poly_letbnd !uuidref with
    | Some polybnd -> (
      match find_mono_version !uuidref typ with
      | Some monouuid -> (
        nameref := (mangle_mono_name !nameref typ); 
        uuidref := monouuid; 
        []
      )
      | None -> (
        let (_, polyuuid, _, lhs) = polybnd in
        let st = lexpt_get_type lhs in
        let smap = extract_specialization_map st typ in
        let nmv = specialize polybnd smap in
        let (nmname, nmuuid, _) = nmv in
        nameref := nmname; 
        uuidref := nmuuid; 
        register_mono_version polyuuid nmuuid typ;
        [(polyuuid, nmv)]
      )
    )
    | None -> []
  )
  | LamT (n, u, b, mt) -> monomorph_letbnd b
  | AppT (f, arg, mt) -> (monomorph_letbnd f) @ (monomorph_letbnd arg)
  | IntT (i, mt) -> []
  | BopT (op, e1, e2, mt) -> (monomorph_letbnd e1) @ (monomorph_letbnd e2)
  | IfT (c, t, e, mt) -> (monomorph_letbnd c) @ (monomorph_letbnd t) @ (monomorph_letbnd e)
  | LetinT (n, u, e, b, mt) -> (monomorph_letbnd e) @ (monomorph_letbnd b)
  | VeclitT (elems, mt) -> List.fold_left (@) [] (List.map monomorph_letbnd elems)
  | VecmkT (def, cnt, mt) -> (monomorph_letbnd def) @ (monomorph_letbnd cnt)
  | VeclenT (v, mt) -> monomorph_letbnd v
  | VecgetT (v, i, mt) -> (monomorph_letbnd v) @ (monomorph_letbnd i)
  | VecsetT (v, i, e, mt) -> (monomorph_letbnd v) @ (monomorph_letbnd i) @ (monomorph_letbnd e)

let monomorph_monoblk (blk : letblkmonot) : (uuid * letbndmonot) list =
  List.fold_right (fun (name, uuid, lhs) newletbndacc ->
    (monomorph_letbnd lhs) @ newletbndacc
  ) blk []

let monomorph_progt (letblk_input : letblkpolyt) : letblkmonot =
  (*
    Strategy:
    - iterate monomorphic part of the TAST, if a call to a polymorphic function is encountered:
    - we check if a fitting monomorphic version exists
      - if so we change the call
      - else we create a new fitting monomorphic version and change the call
    - we recurese on the new monomorphic versions until no more monomorphic versions are generated (fixpoint)
  *)

  (* 1. split TAST int monomorphic and polymorphic part*)
  let letblkpoly, letblkmono = List.fold_right
                              (fun (name, uuid, vars, lhs) (lbp, lbm) ->
                                if List.length vars > 0 then 
                                  ((name, uuid, vars, lhs)::lbp, lbm)
                                else 
                                  (lbp, (name, uuid, lhs)::lbm)    
                              ) 
                              letblk_input ([],[])
  in

  (* 2. set global refs*)
  poly_to_mono_map := [];
  poly_bnds := letblkpoly;

  (* 3. Iterate mono TAST until fix point is found*)
  let rec fixpoint (mblk : letblkmonot) : (uuid * letbndmonot) list =
    let nmonovers = monomorph_monoblk mblk in
    if List.length nmonovers = 0 then []
    else
    (fixpoint (List.map (fun (_, nbnd) -> nbnd) nmonovers)) @ nmonovers
  in
  let newmonoletbnd = fixpoint letblkmono in

  let get_all_monovers (polyuuid : uuid) : letblkmonot = 
    let _, mvs = List.split (List.filter (fun (pu, mv) -> pu = polyuuid) newmonoletbnd) in mvs
  in

  (*4. sew the new binding into the place where the old poly bnd was*)
  List.fold_right 
  (fun (name, uuid, vars, lhs) mbacc ->
    if List.length vars > 0 then 
      let mvs = get_all_monovers uuid in
      mvs @ mbacc
    else
      (name, uuid, lhs) :: mbacc
  ) letblk_input []