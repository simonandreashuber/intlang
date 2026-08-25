open Mir

open Rpo

let compute_live (f : func) =
  let n = f.next_bbid in
  let live_in = Array.make n SsaSet.empty in
  let live_out = Array.make n SsaSet.empty in

  (* --- STEP 1: Precompute local Use and Def for each block --- *)
  let block_uses = Array.make n SsaSet.empty in
  let block_defs = Array.make n SsaSet.empty in

  BBMap.iter (fun bbid bb ->
    let uses = ref SsaSet.empty in
    let defs = ref SsaSet.empty in

    let apply_use u = uses := SsaSet.add u !uses in
    let apply_uses us = List.iter apply_use us in
    let apply_sc_uses scs = List.iter (fun sc -> apply_use sc.ssaid) scs in
    let apply_def d = defs := SsaSet.add d !defs in
    let apply_defs ds = List.iter apply_def ds in

    (* 1. Accumulate Terminator uses *)
    (match bb.term with
     | Some (Br (_, brargs)) -> 
        apply_sc_uses brargs
     | Some (Cbr (cond, tbbid, fbbid)) -> 
        apply_use cond;
     | Some (Ret ret) -> 
        apply_use ret
     | None -> failwith ("live_analysis: function " ^ string_of_int f.funcid ^ " block " ^ string_of_int bbid ^ " has no terminator")
    );

    (* 2. Accumulate Op uses/defs (processed in reverse order) *)
    List.iter (fun op ->
      match op with
      | Func (res, _, _) -> apply_def res
      | Pack (res, sc, scs) -> apply_def res; apply_use sc.ssaid; apply_sc_uses scs
      | CallClosure (res, sc) -> apply_def res; apply_use sc.ssaid
      | CallDirect (res, _, scs) -> apply_def res; apply_sc_uses scs
      | Copy (res, a) -> apply_def res; apply_use a
      | GarbageCollect mems -> apply_uses mems
      | StoreGlobal (_, sc) -> apply_use sc.ssaid
      | LoadGlobal (res, _) -> apply_def res
      | Immi32 (res, _) | Immi8 (res, _) | ImmUnit res -> apply_def res
      | Uopi32 (res, _, a) | Uopi8 (res, _, a) -> apply_def res; apply_use a
      | Bopi32 (res, _, a, b) | Bopi8 (res, _, a, b) -> apply_def res; apply_uses [a; b]
      | Tupwrp (res, scs) -> apply_def res; apply_sc_uses scs
      | Tupuwrp (res_list, sc) -> apply_defs res_list; apply_use sc.ssaid
      | Veclit (res, scs) -> apply_def res; apply_sc_uses scs
      | Vecinit (res, defval, dims) -> apply_def res; apply_use defval; apply_uses dims
      | Veclen (res, vec) -> apply_def res; apply_use vec
      | Vecread (res, vec, idxs) -> apply_def res; apply_use vec; apply_uses idxs
      | Vecwrite (res, val_sc, vec, idxs) -> apply_def res; apply_use vec; apply_use val_sc.ssaid; apply_uses idxs
      | Vecinsert (res, vec_sc, vecins_sc, idxs) -> apply_def res; apply_use vec_sc.ssaid; apply_use vecins_sc.ssaid; apply_uses idxs
      | Vecslice (res, vec, start, len) -> apply_def res; apply_use vec; apply_use start; apply_use len
      | Vecextend (res, vec, lit, off) -> apply_def res; apply_use vec; apply_use lit; apply_use off
    ) bb.ops;

    (* 3. Block arguments act as definitions at the entry *)
    apply_defs bb.args;

    block_uses.(bbid) <- SsaSet.diff !uses !defs;
    block_defs.(bbid) <- !defs; 
  ) f.bbs;

  (* --- STEP 2: Fast Fixed-Point Loop using Precomputed Sets --- *)
  let changed = ref true in
  let rpo = Rpo.get_rpo_info f in
  let po_bbid = List.rev rpo.rpo_lst in

  while !changed do
    changed := false;

    List.iter (fun bbid ->

      let bb = BBMap.find bbid f.bbs in
      let old_live_in = live_in.(bbid) in

      (* live_out = Union of live_in of all successors *)
      let out_set = match bb.term with
        | Some (Br (brbbid, _)) -> live_in.(brbbid)
        | Some (Cbr (_, tbbid, fbbid)) -> SsaSet.union live_in.(tbbid) live_in.(fbbid)
        | Some (Ret _) | None -> SsaSet.empty
      in
      live_out.(bbid) <- out_set;

      (* live_in = use U (live_out \ def) *)
      let u = block_uses.(bbid) in
      let d = block_defs.(bbid) in
      let new_live_in = SsaSet.union u (SsaSet.diff out_set d) in

      live_in.(bbid) <- new_live_in;

      if not (SsaSet.equal old_live_in new_live_in) then
        changed := true;
    ) po_bbid;
  done;

  (* --- STEP 3: Compute Last Use Information --- *)
  let def = Array.make f.next_ssaid (-1, -1) in
  let last_use = Array.make f.next_ssaid [] in

  BBMap.iter (fun bbid bb ->
    let used_later = ref live_out.(bbid) in
    let add_def ssaid op_index = def.(ssaid) <- (bbid, op_index) in
    let add_use ssaid op_index =
      if not (SsaSet.mem ssaid !used_later) then
        (last_use.(ssaid) <- (bbid, op_index) :: last_use.(ssaid);
        used_later := SsaSet.add ssaid !used_later)
    in
    let add_defs ssaids op_index = List.iter (fun ssaid -> add_def ssaid op_index) ssaids in
    let add_uses ssaids op_index = List.iter (fun ssaid -> add_use ssaid op_index) ssaids in
    let add_sc_uses scs op_index = List.iter (fun sc -> add_use sc.ssaid op_index) scs in

    List.iteri (fun op_index op ->
      match op with
      | Func (res, _, _) -> add_def res op_index
      | Pack (res, sc, scs) -> add_def res op_index; 
                               add_use sc.ssaid op_index; 
                               add_sc_uses scs op_index
      | CallClosure (res, sc) -> add_def res op_index; 
                                 add_use sc.ssaid op_index
      | CallDirect (res, _, scs) -> add_def res op_index;
                                    add_sc_uses scs op_index
      | Copy (res, a) -> add_def res op_index; add_use a op_index
      | GarbageCollect mems -> add_uses mems op_index
      | StoreGlobal (_, sc) -> add_use sc.ssaid op_index
      | LoadGlobal (res, _) -> add_def res op_index
      | Immi32 (res, _) | Immi8 (res, _) | ImmUnit res -> add_def res op_index
      | Uopi32 (res, _, a) | Uopi8 (res, _, a) -> add_def res op_index; add_use a op_index
      | Bopi32 (res, _, a, b) | Bopi8 (res, _, a, b) -> add_def res op_index; add_uses [a; b] op_index
      | Tupwrp (res, scs) -> add_def res op_index; add_sc_uses scs op_index
      | Tupuwrp (res_list, sc) -> add_defs res_list op_index; add_use sc.ssaid op_index
      | Veclit (res, scs) -> add_def res op_index; add_sc_uses scs op_index
      | Vecinit (res, defval, dims) -> add_def res op_index; add_use defval op_index; add_uses dims op_index
      | Veclen (res, vec) -> add_def res op_index; add_use vec op_index
      | Vecread (res, vec, idxs) -> add_def res op_index; add_use vec op_index; add_uses idxs op_index
      | Vecwrite (res, val_sc, vec, idxs) -> add_def res op_index;
                                              add_use vec op_index;
                                              add_use val_sc.ssaid op_index;
                                              add_uses idxs op_index
      | Vecinsert (res, vec_sc, vecins_sc, idxs) -> add_def res op_index;
                                                  add_use vec_sc.ssaid op_index;
                                                  add_use vecins_sc.ssaid op_index;
                                                  add_uses idxs op_index
      | Vecslice (res, vec, start, len) -> add_def res op_index; add_use vec op_index; add_use start op_index; add_use len op_index
      | Vecextend (res, vec, lit, off) -> add_def res op_index; add_use vec op_index; add_use lit op_index; add_use off op_index
    )  bb.ops
  ) f.bbs;


  f.live <- Some { live_in; live_out; def; last_use }


let get_live_info fn =
  if fn.live = None then compute_live fn;
  Option.get fn.live


