open Ast

type specmap = (int * typ) list

(* Map the poly version to all its mono versions
  polyuuid -> (monouuid, monotyp)*)
let poly_to_mono_map : (uuid * (uuid * typ)) list ref = ref []
(*registry with all existing poly versions*)
let poly_bnds : polytast ref = ref []

let find_polytletbnd (poly_uuid : uuid) : polytletbnd option =
  List.find_opt (fun (_, uuid, _, _) -> uuid = poly_uuid) !poly_bnds

(*I have basically the same function in ast.ml but there is no error on non linked Vars and I like to have it for sanity
  so thats way there is basically the same code here again*)
let rec cmp_mono_typs (t1 : typ) (t2 : typ) : bool =
  match (repr t1, repr t2) with
  | (TI32, TI32) -> true
  | (TI8, TI8) -> true
  | (TUnit, TUnit) -> true
  | (TFun (arg1, ret1), TFun (arg2, ret2)) -> cmp_mono_typs arg1 arg2 && cmp_mono_typs ret1 ret2
  | (TTup ts1, TTup ts2) -> List.length ts1 = List.length ts2 && List.for_all2 cmp_mono_typs ts1 ts2
  | (TVec tv1, TVec tv2) -> cmp_mono_typs tv1 tv2
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

let extract_specialization_map (polyt : typ) (monot : typ) : specmap =
  let rec aux (acc : specmap) (polyt : typ) (monot : typ) : specmap =
    match (repr polyt, repr monot) with
    | (TI32, TI32) -> acc
    | (TI8, TI8) -> acc
    | (TUnit, TUnit) -> acc
    | (TFun (arg_polyt, ret_polyt), TFun (arg_monot, ret_monot)) -> let acc' = aux acc arg_polyt arg_monot in aux acc' ret_polyt ret_monot
    | (TTup polyt_list, TTup monot_list) -> 
        if List.length polyt_list <> List.length monot_list then
          raise (Errors.TypeError "Monomorphization Error: Tuple length mismatch during specialization map extraction.")
        else
          List.fold_left2 aux acc polyt_list monot_list
    | (TVec polyt', TVec monot') -> aux acc polyt' monot' 
    | (TVar {id = var_id; _}, t) -> (var_id, t) :: acc
    | _ -> raise (Errors.TypeError "Monomorphization Error: Type structure mismatch during specialization map extraction.")
  in 
  (*I have an urge to sanitize the map here, but I think it should work without doing so...*)
  aux [] polyt monot

let rec specialize_typ (smap : specmap) (t : typ) : typ =
  match repr t with
  | TI32 -> TI32
  | TI8 -> TI8
  | TUnit -> TUnit
  | TFun (arg, ret) -> TFun (specialize_typ smap arg, specialize_typ smap ret)
  | TTup ts -> TTup (List.map (specialize_typ smap) ts)
  | TVec t' -> TVec (specialize_typ smap t')
  | TVar {id; link} -> 
      (match List.assoc_opt id smap with
      | Some t_subst -> t_subst
      | None -> TVar {id; link})

let specialize_polytletbnd ((name, uuid, _, lhs) : polytletbnd) (smap : specmap) : monotletbnd =
  let monouuid = fresh_uuid () in
  let sub = specialize_typ smap in
  let newuuidmap = ref [] in (*all local bindings need new uuids*)
  let rec aux lexpt =
    match lexpt with
    | VarT (n, u, oldtyp) -> (
        match List.assoc_opt !u !newuuidmap with
        | Some nuuid -> VarT (ref !n, ref nuuid, sub oldtyp)
        | None -> VarT (ref !n, ref !u, sub oldtyp)
      )
    | LamT (n, u, b, oldtyp) -> (
        let nuuid = fresh_uuid () in
        newuuidmap := (u, nuuid) :: !newuuidmap;
        LamT (n, nuuid, aux b, sub oldtyp)
      )
    | LamUnitT (b, oldtyp) -> LamUnitT (aux b, sub oldtyp)
    | AppT (f, arg, oldtyp) -> AppT (aux f, aux arg, sub oldtyp)
    | SeqT (e1, e2, oldtyp) -> SeqT (aux e1, aux e2, sub oldtyp)
    | IfT (c, t, e, oldtyp) -> IfT (aux c, aux t, aux e, sub oldtyp)
    | LetinT (n, u, e, b, oldtyp) -> (
        let nuuid = fresh_uuid () in
        (*for aux e (u,nuuid) should in principle not be in the newuuidmap
          but since all things are already 'uuided' u will never be referenced in e (no shadowing)*)
        newuuidmap := (u,nuuid) :: !newuuidmap;
        LetinT (n, nuuid, aux e, aux b, sub oldtyp)
      )
    | LetrecinT (n, u, e, b, oldtyp) -> (
        let nuuid = fresh_uuid () in
        newuuidmap := (u,nuuid) :: !newuuidmap;
        LetrecinT (n, nuuid, aux e, aux b, sub oldtyp)
      )
    | LetinTupleT (tupls, e, b, oldtyp) -> (
      let ntupls = List.map 
      (fun iduuid_opt ->
        match iduuid_opt with
        | Some (n, u) -> (
            let nuuid = fresh_uuid () in
            newuuidmap := (u,nuuid) :: !newuuidmap;
            Some (n, nuuid))
        | None -> None ) tupls in
      LetinTupleT (ntupls, aux e, aux b, sub oldtyp)
    )
    | TupleT (els, oldtyp) -> TupleT (List.map aux els, sub oldtyp)
    | I32LitT (i, oldtyp) -> I32LitT (i, sub oldtyp) (*mb I could just default map these since they dont change*)
    | I8LitT (c, oldtyp) -> I8LitT (c, sub oldtyp)
    | UnitLitT oldtyp -> UnitLitT (sub oldtyp)
    | UopI32T (op, e, oldtyp) -> UopI32T (op, aux e, sub oldtyp)
    | UopI8T (op, e, oldtyp) -> UopI8T (op, aux e, sub oldtyp)
    | BopI32T (op, e1, e2, oldtyp) -> BopI32T (op, aux e1, aux e2, sub oldtyp)
    | BopI8T (op, e1, e2, oldtyp) -> BopI8T (op, aux e1, aux e2, sub oldtyp)
    | VecLitT (elems, oldtyp) -> VecLitT (List.map aux elems, sub oldtyp)
    | VecmkT (defval, szlst, oldtyp) -> VecmkT (aux defval, List.map aux szlst, sub oldtyp)
    | VeclenT (v, oldtyp) -> VeclenT (aux v, sub oldtyp)
    | VecgetT (v, idxls, oldtyp) -> VecgetT (aux v, List.map aux idxls, sub oldtyp)
    | VecsetT (v, setval, idxls, oldtyp) -> VecsetT (aux v, aux setval, List.map aux idxls, sub oldtyp)
    | VecsliceT (v, start, len, oldtyp) -> VecsliceT (aux v, aux start, aux len, sub oldtyp)
    | VecextendT (v, lit, off, oldtyp) -> VecextendT (aux v, aux lit, aux off, sub oldtyp)
  in 
  let sub_lhs = aux lhs in
  let mono_typ = tlexp_get_type sub_lhs in
  let mono_name = mangle_mono_name name mono_typ in
  (mono_name, monouuid, sub_lhs)


let rec genmonovers_tlexp (l : tlexp) : (uuid * monotletbnd) list = (*returns new monotletbnd with their original polyversion id attached*)
  match l with
  | VarT (nameref, uuidref, typ) -> (
    match find_polytletbnd !uuidref with
    | Some polybnd -> (
      match find_mono_version !uuidref typ with
      | Some monouuid -> (
        nameref := (mangle_mono_name !nameref typ); 
        uuidref := monouuid; 
        []
      )
      | None -> (
        let (_, polyuuid, _, lhs) = polybnd in
        let st = tlexp_get_type lhs in
        let smap = extract_specialization_map st typ in
        let nmv = specialize_polytletbnd polybnd smap in
        let (nmname, nmuuid, _) = nmv in
        nameref := nmname; 
        uuidref := nmuuid; 
        register_mono_version polyuuid nmuuid typ;
        [(polyuuid, nmv)]
      )
    )
    | None -> []
  )
  | LamT (n, u, b, mt) -> genmonovers_tlexp b
  | LamUnitT (b, mt) -> genmonovers_tlexp b
  | AppT (f, arg, mt) -> (genmonovers_tlexp f) @ (genmonovers_tlexp arg)
  | SeqT (e1, e2, mt) -> (genmonovers_tlexp e1) @ (genmonovers_tlexp e2)
  | IfT (c, t, e, mt) -> (genmonovers_tlexp c) @ (genmonovers_tlexp t) @ (genmonovers_tlexp e)
  | LetinT (n, u, e, b, mt)
  | LetrecinT (n, u, e, b, mt) -> (genmonovers_tlexp e) @ (genmonovers_tlexp b)
  | LetinTupleT (tupls, e, b, mt) -> (genmonovers_tlexp e) @ (genmonovers_tlexp b)
  | TupleT (els, mt) -> List.flatten (List.map genmonovers_tlexp els)
  | I32LitT (i, mt) -> []
  | I8LitT (c, mt) -> []
  | UnitLitT mt -> []
  | UopI32T (op, e, mt) -> genmonovers_tlexp e
  | UopI8T (op, e, mt) -> genmonovers_tlexp e
  | BopI32T (op, e1, e2, mt) -> (genmonovers_tlexp e1) @ (genmonovers_tlexp e2)
  | BopI8T (op, e1, e2, mt) -> (genmonovers_tlexp e1) @ (genmonovers_tlexp e2)
  | VecLitT (elems, mt) -> List.flatten (List.map genmonovers_tlexp elems)
  | VecmkT (defval, szlst, mt) -> (genmonovers_tlexp defval) @ (List.flatten (List.map genmonovers_tlexp szlst))
  | VeclenT (v, mt) -> genmonovers_tlexp v
  | VecgetT (v, idxls, mt) -> (genmonovers_tlexp v) @ (List.flatten (List.map genmonovers_tlexp idxls))
  | VecsetT (v, setval, idxls, mt) -> (genmonovers_tlexp v) @ (genmonovers_tlexp setval) @ (List.flatten (List.map genmonovers_tlexp idxls))
  | VecsliceT (v, start, len, mt) -> (genmonovers_tlexp v) @ (genmonovers_tlexp start) @ (genmonovers_tlexp len)
  | VecextendT (v, lit, off, mt) -> (genmonovers_tlexp v) @ (genmonovers_tlexp lit) @ (genmonovers_tlexp off)

let genmonovers_monotast (mtast : monotast) : (uuid * monotletbnd) list =
  List.fold_right (fun (name, uuid, lhs) newletbndacc ->
    (genmonovers_tlexp lhs) @ newletbndacc
  ) mtast []

let monomorph (ptast_input : polytast) : monotast =
  (*
    Strategy:
    - iterate monomorphic part of the TAST, if a call to a polymorphic function is encountered:
    - we check if a fitting monomorphic version exists
      - if so we change the call
      - else we create a new fitting monomorphic version and change the call
    - we recurese on the new monomorphic versions until no more monomorphic versions are generated (fixpoint)
  *)

  (* 1. split TAST int monomorphic and polymorphic part*)
  let ptast_part, mtast_part = List.fold_right
                              (fun (name, uuid, vars, lhs) (pp, mp) ->
                                if List.length vars > 0 then 
                                  ((name, uuid, vars, lhs)::pp, mp)
                                else 
                                  (pp, (name, uuid, lhs)::mp)    
                              ) 
                              ptast_input ([],[])
  in

  (* 2. set global refs*)
  poly_to_mono_map := [];
  poly_bnds := ptast_part;

  (* 3. Iterate mono TAST until fix point is found*)
  let rec fixpoint (mtast : monotast) : (uuid * monotletbnd) list =
    let nmonovers = genmonovers_monotast mtast in
    if List.length nmonovers = 0 then []
    else
    (fixpoint (List.map (fun (_, nbnd) -> nbnd) nmonovers)) @ nmonovers
  in
  let gen_monovers = fixpoint mtast_part in

  let get_all_monovers (polyuuid : uuid) : monotast = 
    let _, mvs = List.split (List.filter (fun (pu, mv) -> pu = polyuuid) gen_monovers) in mvs
  in

  (*4. sew the new binding into the place where the old poly bnd was*)
  List.fold_right 
  (fun (name, uuid, vars, lhs) mbacc ->
    if List.length vars > 0 then 
      let mvs = get_all_monovers uuid in
      mvs @ mbacc
    else
      (name, uuid, lhs) :: mbacc
  ) ptast_input []