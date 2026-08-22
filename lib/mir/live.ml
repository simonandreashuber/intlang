open Mir


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
     | Some (Br br) -> 
        apply_sc_uses br.args
     | Some (Cbr (cond, t, f)) -> 
        apply_use cond;
        apply_sc_uses t.args;
        apply_sc_uses f.args
     | Some (Ret sc) -> 
        apply_use sc.ssaid
     | None -> failwith ("live_analysis: function " ^ string_of_int f.funcid ^ " block " ^ string_of_int bbid ^ " has no terminator")
    );

    (* 2. Accumulate Op uses/defs (processed in reverse order) *)
    List.iter (fun op ->
      match op with
      | Func (res, _) -> apply_def res
      | Pack (res, sc, scs) -> apply_def res; apply_use sc.ssaid; apply_sc_uses scs
      | CallClosure (res, sc) -> apply_def res; apply_use sc.ssaid
      | CallDirect (res, _, scs) -> apply_def res; apply_sc_uses scs
      | GarbageCollect mems -> apply_uses mems
      | StoreGlobal (_, sc) -> apply_use sc.ssaid
      | LoadGlobal (res, _) -> apply_def res
      | Immi32 (res, _) | Immi8 (res, _) | ImmUnit res -> apply_def res
      | Uopi32 (res, _, a) | Uopi8 (res, _, a) -> apply_def res; apply_use a
      | Bopi32 (res, _, a, b) | Bopi8 (res, _, a, b) -> apply_def res; apply_uses [a; b]
      | Tupinit (res, scs) -> apply_def res; apply_sc_uses scs
      | Tupextract (res_list, sc) -> apply_defs res_list; apply_use sc.ssaid
      | Tupview (res_list, tup) -> apply_defs res_list; apply_use tup
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
  let blocks_rev = BBMap.bindings f.bbs |> List.rev in

  while !changed do
    changed := false;

    List.iter (fun (bbid, bb) ->
      let old_live_in = live_in.(bbid) in

      (* live_out = Union of live_in of all successors *)
      let out_set = match bb.term with
        | Some (Br br) -> live_in.(br.bbid)
        | Some (Cbr (_, t, f)) -> SsaSet.union live_in.(t.bbid) live_in.(f.bbid)
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
    ) blocks_rev;
  done;

  f.live <- Some { live_in; live_out }


let get_live_info fn =
  if fn.live = None then compute_live fn;
  Option.get fn.live