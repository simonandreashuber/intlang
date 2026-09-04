open Mir
open Buildmir
open Analysis


(*

  The lowered MIR is
    - not legal (memory leaks, does not consume in places that need ownership transfer)
    - not optimized (unneeded implicit copies)

  This pass makes sure that the code is legal (guarantee) and makes a best effort to have fast and efficient code (best effort, not guaranteed)
*)

(* ========================================================================= *)
(* Memory Signature                                                          *)
(* ========================================================================= *)

type memsig = ownership list
let get_memsig_func (func : func) : memsig =
  List.map (fun (ssaid, _) -> get_ownership_func func ssaid) func.args

let get_memsig_calldirect_args (fn : func) (args : ssaconsume list) : memsig =
  List.map (fun sc -> 
    match get_ownership_func fn sc.ssaid with
    | Owned when sc.consume -> Owned
    | Owned when not sc.consume -> Borrowed
    | Borrowed when sc.consume -> failwith "get_memsig_calldirect_args: consuming borrowed arg"
    | Borrowed when not sc.consume -> Borrowed
    | NoMem when sc.consume -> failwith "get_memsig_calldirect_args: consumed NoMem arg"
    | NoMem when not sc.consume -> NoMem
    | _ -> failwith "get_memsig_calldirect_args: internal this should never happen"
    ) args

let cmp_memsig (sig1 : memsig) (sig2 : memsig) : bool =
  List.for_all2 (fun own1 own2 -> own1 = own2) sig1 sig2

let memsig_all_to (memsig : memsig) (own : ownership) : memsig =
  if own = NoMem then failwith "memsig_all_to: cannot convert to NoMem ownership";
  List.map (fun argown -> if argown = NoMem then argown else own) memsig


(* ========================================================================= *)
(* Memory Optimizer Def and Helpers                                          *)
(* ========================================================================= *)

type mem_optimizer = {
  b : builder;                                            (* Mir Builder *)
  aly : analysis_info;                                    (* Analysis information *)
  exter_vers : func list;                               (* All funcion that are marked as external *)
  orig_vers : func list;                                (* All original versions where all func args are borrowed and all bb args are owned *)
  mutable orig_to_opt_vers : (func list) FuncMap.t;     (* Given some funcid of an original version, find all optimized versions *)
  opt_queue: func Queue.t;                              (* Queue of functions to optimize *)
}

let is_orig_vers opt funcid =
  match List.find_opt (fun ofn -> ofn.funcid = funcid) opt.orig_vers with Some _ -> true | None -> false

let is_exter_vers opt funcid =
  match List.find_opt (fun efn -> efn.funcid = funcid) opt.exter_vers with Some _ -> true | None -> false

let request_func_vers (opt : mem_optimizer) (funcid : funcid) (ownsig : ownership list) : funcid =
  if Option.is_some @@ List.find_opt (fun efn -> efn.funcid = funcid) opt.exter_vers then funcid
  else
  match FuncMap.find_opt funcid opt.orig_to_opt_vers with
  | None -> failwith ("request_func_vers: given funcid is not an original version funcid: " ^ string_of_int funcid)
  | Some optvers -> 
      let existing_vers = List.find_opt 
        (fun versfn -> cmp_memsig ownsig (get_memsig_func versfn)) optvers in
      match existing_vers with
      | Some versfunc ->  versfunc.funcid
      | None -> 
          let fn_copy = copy_func opt.b funcid in
          List.iter2 (fun needed (argssaid, _ ) -> 
            set_ownership_func fn_copy argssaid needed
          ) ownsig fn_copy.args;
          opt.orig_to_opt_vers <- FuncMap.add funcid (fn_copy :: optvers) opt.orig_to_opt_vers;
          Queue.push fn_copy opt.opt_queue;
          fn_copy.funcid

let pop_func_to_opt opt = Queue.pop opt.opt_queue
let has_func_to_opt opt = not (Queue.is_empty opt.opt_queue)

(* ========================================================================= *)
(* BB Arg Borrowed Promotion                                                 *)
(* ========================================================================= *)

(*
  By Default all bb args are lowered owned, this can trigger defensive copies that are not needed
  the goal of this optimization pass is to remove them if possible.
  
  Having some BB arg borrowed is not always legal. For example in this case:

  entry:
    ...
    br loopheader(%0)
  
  loopheader(%1):
    ...
    %2 = vecinit...
    %3 = vecread %1 ...
    cbr .. loopheader(%2) exitbb

  if here %1 were to borrow it would need to borrow from %0 and %2, but after the "redefinition" 
  of %2 in a second iteration of the loopheader bb we basically borrow from something that does
  not exist anymore and can also not really be kept alive

  Thus all things that are borrowed from need to DOMINATE the BB arg definition.

  "promoting" an BB arg from owned to borrowed can destroy some downstream optimization.
  The main reason for this it that there is currently no way to "make last borrower owner" ie. 
  say I have some borrowed value used in a place that needs transfer of ownership, so generally a
  copy is made done. But say the only thing keeping the owner alive is this borrowed value, in theory
  one could make this last borrower the new owner and avoid the copy. But this brings a number of problems
  / complexities like:
    1. Vecget on multidimensional vectors, Vecslice, Tupuwrp without consumption: In all of these cases 
       the borrowed value does not see the entire thing it borrows from anymore.
    2. BB args with more then one predecessor that borrow: In this cases one value can borrow from
       more than one owner.

  While I dont think these problems are impossible to solve, I have decided to not do it now.
  But here is an idea no how it could mb work: I mean it is basically a drop operation but instead
  of just freeing everything, one avoids freeing the last borrower. In principle quite simple but 
  there are some technicalities that come to mind, like a Vecslice does not point to the beginning
  of the allocated memory range. So one could either store a "free" ptr for each vector or write a 
  memory allocator that works with free ptrs in the middle of an allocated range (prob not worth it).

  For now all bb args that can legally be borrowed are borrowed except if there is a direct path via a 
  chain of bb args to a ret. This is a common pattern that occurs with if/else and a return is a place
  that needs transfer of ownership. Its a simple heuristic but does improve things for some common cases.
*)

let bbarg opt fn =

  let pred_info = get_preds_info opt.aly fn in

  let canret = ref [] in

  let rec find_canret mask succbbid bbid =
    let bb = BBMap.find bbid fn.bbs in
    let appmask = List.iter2 (fun is_canret brarg -> if is_canret then canret := brarg.ssaid :: !canret) mask in
    (match bb.term with
    | Some (Ret ret) -> canret := ret :: !canret
    | Some (Br (_, brargs))-> appmask brargs
    | Some (Cbr _) -> ()
    | None -> failwith "borrbbargop: try find canret but found bb with not term");
    let mask = List.map (fun argssaid -> List.mem argssaid !canret) bb.args in
    List.iter (fun predbbid ->
      if does_strictly_dominate opt.aly fn predbbid bbid then find_canret mask bbid predbbid
    ) pred_info.preds.(bbid)
  in

  (match BBMap.fold (fun bbid bb acc -> (*could do this with dom as well but this is ok I think*)
    match bb.term with | Some (Ret retval) -> bbid :: acc | _ -> acc ) fn.bbs [] with
  | [retbbid] -> find_canret [] (-1) retbbid
  | _ -> failwith (Printf.sprintf "borrbbarg_opt_func: funcid %d has no or multiple ret bbs" fn.funcid));


  (* Check for each bb arg if its legal and desirable 
     to promote it to borrowed *)
  let live_info = get_live_info opt.aly fn in
  BBMap.iter ( fun _ bb ->
    List.iter (fun arg ->
        if is_memtyp (get_mirtyp_func fn arg) &&
           (get_ownership_func fn arg = Owned) then (
          let pot_owners = find_funclocal_owners opt.aly fn arg in
          if
          List.for_all (fun owner_ssaid ->
            let (owner_bbid,_) = live_info.def.(owner_ssaid) in
            (does_strictly_dominate opt.aly fn owner_bbid bb.bbid) &&
            (not @@ List.mem arg !canret)
          ) pot_owners
          then (
            set_ownership_func fn arg Borrowed
          )
        )
    ) bb.args
  ) fn.bbs

(* ========================================================================= *)
(* Consume                                                                   *)
(* ========================================================================= *)

(*
  There are some uses in the MIR that can consume (those with ssaconsume)
  only memory types can be consumed but apart from that for a value to 
  be consumed 2 things need to be true:

    1. The value is owned
    2. The value or anyone borrowing from the value is not live after the use

  This pass makes a function local best effort to consume as many values as possible.
  
  Tupuwrp changes the ownership of the ssa defs depending on wether or not the 
  tuple is consumed. Thus the core pass is run itertively until a fixpoint is reached.

  This pass leaves calls with funcids that do not match the ownership transfer indicated by the call (see monofunc pass).
  This pass potentially leaves bb args that need ownership transfer without such (see inscopy pass).

  Also a small not on the correct ness of this pass: The live and borrow analysis are not updated, during this pass.
  That is because consumption and or ownership of ssa values does not impact any of them. This is maybe a bit confusing
  at first for the borrowing but in the borrowing graph all places where a borrowing relation ship could occur are in there
  and then the actull borrowers and owners are determinded by walking the graph and refering to ssa value ownership during
  the walk (see the borrow analysis in analysis.ml).
*)


let consume (opt : mem_optimizer) fn =

  let live_info = get_live_info opt.aly fn in

  let changed = ref true in

  while !changed do
    changed := false;
    BBMap.iter (fun bbid bb ->

      let ul = ref live_info.live_out.(bbid) in
      (
      match bb.term with
      | Some (Br (brbbid, _)) -> (
        let succbb = find_bb_func fn brbbid in
        List.iter (fun succbbarg -> 
          if get_ownership_func fn succbbarg = Borrowed then
            ul := SsaSet.add succbbarg !ul
        ) succbb.args
      )
      | Some (Cbr _) | Some (Ret _) | None-> ()
      );

      let try_consume sc =
        if get_ownership_func fn sc.ssaid = Owned then (
          let cannot_be_used_later = sc.ssaid :: find_borrowers opt.aly fn sc.ssaid in
          if List.for_all (fun ssaid -> not (SsaSet.mem ssaid !ul)) cannot_be_used_later then (
            sc.consume <- true
          )
        );
        ul := SsaSet.add sc.ssaid !ul;
      in

      let add_use ssaid = ul := SsaSet.add ssaid !ul in

      (*lists get reversed since the mir is left to right and we go backwards*)
      let try_consume_lst scs = List.iter (try_consume) (List.rev scs) in

      let add_uses ssaids = List.iter (add_use) (List.rev ssaids) in

      let try_consume_br brbbid brargs = 
        let target_bb = match BBMap.find_opt brbbid fn.bbs with
          | Some bb -> bb | None -> failwith (Printf.sprintf "try_consume_br: bb %d has no target bb %d" bbid brbbid) in

        let bbargs_memsig = List.map (fun ssa -> get_ownership_func fn ssa) target_bb.args in
        List.iter2 (fun sc own -> 
            if own = Owned then try_consume sc (*only consume if the target bb arg is owned*)
            else add_use sc.ssaid
          ) (List.rev brargs) (List.rev bbargs_memsig)
      in


      (match bb.term with
      | Some (Br (brbbid, brargs)) -> try_consume_br brbbid brargs
      | Some (Cbr (cond , _, _)) -> 
          add_use cond
      | Some (Ret retval) -> add_use retval
      | _ -> failwith (Printf.sprintf "consume_opt_func: bb %d has no term" bbid)
      );
      
      List.iter (fun op ->
        match op with
        | Func _ -> ()
        | Pack (_, sc, scs) -> try_consume_lst scs; try_consume sc
        | CallClosure (_, sc) -> try_consume sc
        | CallDirect (_, funcid_ref, scs) -> try_consume_lst scs
        | Copy (_, orig) -> add_use orig
        | Drop mems -> add_uses mems
        | LoadGlobal _ -> ()
        | StoreGlobal (_, sc) -> try_consume sc
        | Immi32 _ | Immi8 _ | ImmUnit _ -> ()
        | Uopi32 (_, _, a) | Uopi8 (_, _, a) ->  add_use a
        | Bopi32 (_, _, a, b) | Bopi8 (_, _, a, b) -> add_use b; add_use a
        | Tupwrp (_, scs) ->  try_consume_lst scs
        | Tupuwrp (elms, sc) -> (
          if get_ownership_func fn sc.ssaid = Owned && (not sc.consume) then (
            let cannot_be_used_later = sc.ssaid :: find_borrowers_excludelist elms opt.aly fn sc.ssaid in
            if List.for_all (fun ssaid -> not (SsaSet.mem ssaid !ul)) cannot_be_used_later then (
              sc.consume <- true;
              List.iter (fun elmssaid -> 
                let elmtyp = get_mirtyp_func fn elmssaid in
                if is_memtyp (elmtyp) then set_mirtyp_ownership_func fn elmssaid elmtyp Owned ) elms;
              changed := true
            )
          );
          ul := SsaSet.add sc.ssaid !ul
        )
        | Veclit (_, scs) ->  try_consume_lst scs
        | Vecinit (_, defval, dims) ->  add_uses dims; add_use defval
        | Veclen (_, vec) ->  add_use vec
        | Vecread (_, vec, idxs) -> add_uses idxs; add_use vec
        | Vecwrite (_, sc, vec, idxs) -> add_uses idxs; add_use vec; try_consume sc;
        | Vecinsert (_, vec_sc, vecins_sc, idxs) ->  add_uses idxs; try_consume vecins_sc; try_consume vec_sc
        | Vecslice (_, vec, start, len) ->  add_use len; add_use start; add_use vec
        | Vecextend (_, vec, lit, off) ->  add_use off; add_use lit; add_use vec
      )  bb.ops
    ) fn.bbs;
  done


(* ========================================================================= *)
(* Monomorph Func                                                            *)
(* ========================================================================= *)

(*
  When a value becomes newly consumed by a calldirect this means that now inside
  the callees body argument can be considered owned. Thus the callee will be copied,
  have their respective arguments set as owned and then put in the optimization queue.
  That is of course if the needed version does not yet exist.
*)

let monofunc (opt : mem_optimizer) (fn : func) =

  BBMap.iter (fun bbid bb ->
    List.iter (fun op ->
      match op with
      | Func (def, funcid1, funcid2_opt) 
          when is_orig_vers opt !funcid1 && Option.is_none !funcid2_opt -> (
          let orig_fn = find_func opt.b !funcid1 in
          let orig_memsig = get_memsig_func orig_fn in
          let all_borr_sig = memsig_all_to orig_memsig Borrowed in
          let all_own_sig = memsig_all_to orig_memsig Owned in
          let borr_funcid = request_func_vers opt !funcid1 all_borr_sig in
          let own_funcid = request_func_vers opt !funcid1 all_own_sig in
          funcid1 := borr_funcid;
          funcid2_opt := Some own_funcid
      )
      | Func (_, funcid1 , _) when is_exter_vers opt !funcid1 -> ()
      | Func (_, funcid1, _) -> failwith (Printf.sprintf "monofunc: bb %d has func op with funcid %d that is not an original version" bbid !funcid1)
      | CallDirect (_, funcid_ref, args) -> (
        let call_memsig = get_memsig_calldirect_args fn args in
        try
          let mono_funcid = request_func_vers opt !funcid_ref call_memsig in
          funcid_ref := mono_funcid
        with e -> 
          failwith (Printf.sprintf "monofunc: in func %d bb %d calldirect to func %d" fn.funcid bbid !funcid_ref)
      )
      | Pack _ | CallClosure _ 
      | Copy _ | Drop _ | StoreGlobal _ | LoadGlobal _
      | Immi32 _ | Immi8 _ | ImmUnit _ | Uopi32 _
      | Uopi8 _ | Bopi32 _ | Bopi8 _ | Tupwrp _
      | Tupuwrp _ | Veclit _ | Vecinit _ | Vecread _
      | Veclen _ | Vecwrite _ | Vecinsert _
      | Vecextend _ | Vecslice _ -> ()
    ) bb.ops
  ) fn.bbs

(* ========================================================================= *)
(* Insert Explicit Copies at Terms                                           *)
(* ========================================================================= *)

(*
  Ret terms and Br term args where the respecitve bb args are owned need to be consumed
  but sometimes this is not possible it is also not implicitly possible to define this 
  as part of the br and ret semantics since then some things (ie the borrowed values owner 
  beeing copied) can not be dropped in time so explicit copies are inserted in these cases.
*)

let inscopy (opt : mem_optimizer) (fn : func) =

  BBMap.iter (fun bbid bb ->
    match bb.term with
    | Some (Ret retval) -> (
      if get_ownership_func fn retval = Borrowed then (
        switch_func opt.b fn;
        switch_bb opt.b bb;
        let retval' = fresh_ssaid opt.b in
        emit_op opt.b (Copy (retval', retval));
        bb.term <- Some (Ret retval')
      )
    )
    | Some (Cbr _) -> ()
    | Some (Br (brbbid, brargs)) -> (      
        switch_func opt.b fn;
        switch_bb opt.b bb;
        let targetbb = find_bb_func fn brbbid in
        let brargs'=
        List.map2 (fun brarg bbarg -> 
            if (not brarg.consume) && (get_ownership_func fn bbarg = Owned) then
              let brarg' = fresh_ssaid opt.b in
              emit_op opt.b (Copy (brarg', brarg.ssaid));
              { ssaid = brarg' ; consume = true }
            else
              brarg
          ) brargs targetbb.args in
        bb.term <- Some (Br (brbbid, brargs'))
    )
    | None -> failwith "inscopy: no term are u kidding me"
  ) fn.bbs

(* ========================================================================= *)
(* Insert Drop                                                               *)
(* ========================================================================= *)

let collect_consumed_ssaids (ops : op list) : SsaSet.t =
  let check acc sc =
    if sc.consume then SsaSet.add sc.ssaid acc else acc
  in
  let check_list acc scl =
    List.fold_left check acc scl
  in
  let process_op acc = function
    | Pack (_, sc, scl)          -> check_list (check acc sc) scl
    | CallClosure (_, sc)        -> check acc sc
    | CallDirect (_, _, scl)     -> check_list acc scl
    | StoreGlobal (_, sc)        -> check acc sc
    | Tupwrp (_, scl)            -> check_list acc scl
    | Tupuwrp (_, sc)            -> check acc sc
    | Veclit (_, scl)            -> check_list acc scl
    | Vecwrite (_, sc, _, _)     -> check acc sc
    | Vecinsert (_, sc1, sc2, _) -> check (check acc sc1) sc2
    | Drop _ -> failwith "collect_consumed_ssaids: drop op should not be in the ops list"
    | Func _ | Copy _ | LoadGlobal _
    | Immi32 _ | Immi8 _ | ImmUnit _
    | Uopi32 _ | Uopi8 _ | Bopi32 _ | Bopi8 _
    | Vecinit _ | Veclen _ | Vecread _
    | Vecslice _ | Vecextend _  -> acc
  in
  List.fold_left process_op SsaSet.empty ops

let insdrop (opt : mem_optimizer) (fn : func) =

  let live_info = get_live_info opt.aly fn in

  (*dead_ssaids_owners contains all the owners of the ssaids 
      from dead_ssaids ie. the things that are actually candidates to be 
      droped*)
    let find_dead_ssaids_owners dead_ssaids =
      SsaSet.fold (fun dead_ssaid owners_acc ->
        match get_ownership_func fn dead_ssaid with
        | NoMem -> owners_acc
        | Owned -> SsaSet.add dead_ssaid owners_acc
        | Borrowed -> (
          let owners = SsaSet.of_list @@ find_funclocal_owners opt.aly fn dead_ssaid in
          SsaSet.union owners owners_acc
        )
      ) dead_ssaids SsaSet.empty
    in

    (* a owner can be dropped if no borrower is alive at the start of
       the target bb and the value has not been consumed *)
    let find_drop_ssaids banned_owners illegal_ssaids dead_ssaids_owners =
      SsaSet.fold (fun dead_ssaid_owner drop_acc ->
        let borrowers = find_borrowers opt.aly fn dead_ssaid_owner in
        if 
          not (SsaSet.mem dead_ssaid_owner illegal_ssaids) &&
          List.for_all (fun borrower -> not (SsaSet.mem borrower illegal_ssaids)) borrowers &&
          not (SsaSet.mem dead_ssaid_owner banned_owners)
        then dead_ssaid_owner :: drop_acc
        else drop_acc
      ) dead_ssaids_owners [] 
    in

  BBMap.iter (fun bbid bb ->
    switch_func opt.b fn;
    switch_bb opt.b bb;

    if bb.term = None then failwith (Printf.sprintf "insdrop: bb %d has no term" bbid);

    let term_use_ssaids, bbarg_ssaids =
      match Option.get bb.term with
      | Br (brbbid, brargs) -> (
        let targetbb = find_bb_func fn brbbid in
        List.map (fun brarg -> brarg.ssaid) brargs, targetbb.args
      )
      | Ret retval -> [retval], []
      | Cbr _ -> [], [] (* the cond on the cbr is term use but on an ssaid guarantied to be of NoMem ownership*)
    in

    (*dead_ssaids are all ssaids that were alive coming into the bb
      or were defined in the bb but are dead after the last op just before
      the term *)
    let dead_ssaids = 
      SsaSet.diff
      (SsaSet.union live_info.live_in.(bb.bbid) live_info.block_defs.(bb.bbid))
      (SsaSet.union live_info.live_out.(bb.bbid) (SsaSet.of_list term_use_ssaids))
    in

    let dead_ssaids_owners = find_dead_ssaids_owners dead_ssaids in

    (*illegal_ssaids are the ssaids that are live at the beginning of the
      target bb and hence can not be in the borrowers of some potentially dropped
      owned value *)
    let illegal_ssaids = SsaSet.union live_info.live_out.(bb.bbid) (SsaSet.of_list bbarg_ssaids) in

    let consumed_ssaids = collect_consumed_ssaids bb.ops in

    let drop_ssaids = find_drop_ssaids consumed_ssaids illegal_ssaids dead_ssaids_owners in

    if List.length drop_ssaids > 0 then emit_op opt.b ( Drop drop_ssaids );


    (*
      cbr can require some drop ops to be placed in a separate
      intermediate bb, eg. a vecread on a owned vector in a loop body
    *)
    match Option.get bb.term with
    | Cbr (cond, ifbbid, elsebbid) -> (
    
      (* Ssaids that were either consumed or dropped in the general pass just above *)
      let consumed_or_dropped_ssaids = SsaSet.union consumed_ssaids (SsaSet.of_list drop_ssaids) in

      let cbredgedrop br_bbid =
        let br_dead_ssaids = SsaSet.diff live_info.live_out.(bb.bbid)  live_info.live_in.(br_bbid) in
        let br_dead_ssaids_owners = find_dead_ssaids_owners br_dead_ssaids in
        let br_illegal_ssaids = live_info.live_in.(br_bbid) in
        let br_drop_ssaids = find_drop_ssaids consumed_or_dropped_ssaids br_illegal_ssaids br_dead_ssaids_owners in

        if List.length br_drop_ssaids > 0 then (
        let brbb = create_bb opt.b "cbredgedrop_br" [] in
        switch_bb opt.b brbb;
        emit_op opt.b ( Drop br_drop_ssaids );
        emit_term opt.b ( Br (br_bbid, []) );
        brbb.bbid
        )
        else br_bbid
      in
      
      let ifbbid' = cbredgedrop ifbbid in
      let elsebbid' = cbredgedrop elsebbid in
      bb.term <- Some (Cbr (cond, ifbbid', elsebbid'))
    )
    | _ -> ()

  ) fn.bbs


(* ========================================================================= *)
(* Memory Optimizer Main Loop                                                *)
(* ========================================================================= *)

(* assumes all function in the program are unoptimized lowered functions *)
let create_mem_optimizer (b : builder) (aly : analysis_info) : mem_optimizer =
  let orig, exter = FuncMap.fold 
    (fun k f (origacc, exteracc) -> 
      assert (f.funcid = k);
      match f.extern_name with
      | Some _ -> (origacc, f :: exteracc)
      | None -> (f :: origacc, exteracc)
    ) b.program.funcs ([],[]) in
  { 
    b; 
    aly;
    exter_vers = exter;
    orig_vers = orig; 
    orig_to_opt_vers = List.fold_left (fun mapacc orig_func -> FuncMap.add orig_func.funcid [] mapacc ) FuncMap.empty orig; 
    opt_queue = Queue.create ();
  }

let push_canonical_funcvers opt =
  List.iter (fun orig_fn -> 
    let orig_memsig = get_memsig_func orig_fn in
    let all_borr_memsig = memsig_all_to orig_memsig Borrowed in
    ignore(request_func_vers opt orig_fn.funcid all_borr_memsig)
  ) opt.orig_vers

let finalize_mem_optimizer opt =
  let find_single_copy origfuncid = 
    match FuncMap.find_opt origfuncid opt.orig_to_opt_vers with 
    | Some ([singlecopyfunc]) -> singlecopyfunc.funcid
    | _ -> failwith "find_single_copy: there is not single copy" 
  in
  List.iter (fun orig_fn -> 
    delete_func opt.b orig_fn;
    if opt.b.program.init_globals_funcid = Some orig_fn.funcid then opt.b.program.init_globals_funcid <- Some (find_single_copy orig_fn.funcid);
    if opt.b.program.main_funcid = Some orig_fn.funcid then opt.b.program.main_funcid <- Some (find_single_copy orig_fn.funcid);
    if opt.b.program.uninit_globals_funcid = Some orig_fn.funcid then opt.b.program.uninit_globals_funcid <- Some (find_single_copy orig_fn.funcid);

  ) opt.orig_vers


let mem_opt (b : builder) (aly : analysis_info) =
    let opt = create_mem_optimizer b aly in
    push_canonical_funcvers opt;                  (* starts the optimizer worklist with a all borrowed and all owned function versions *)

    while has_func_to_opt opt do
      let fn = pop_func_to_opt opt in
      bbarg opt fn;             (* select bb args should borrow *)
      consume opt fn;           (* try to consume in as many places as possible *)
      monofunc opt fn;          (* Monomorphize Functions suited to the specific signature of what args can be consumed *)
      inscopy opt fn;           (* inserts explicit copies for term uses that cant be consumed but need to be consumed *)
      insdrop opt fn            (* drop all memory objects when they are not consumed on last use *)
    done;

    finalize_mem_optimizer opt
