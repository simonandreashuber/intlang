open Mir
open Buildmir

open Live
open Borrow
open Dom

(*
  There are some uses in the MIR that can consume (those with ssaconsume)
  only memory types can even be consume but apart from that for a value to 
  be consume 2 things need to be true:

    1. The value is owned
    2. The value or anyone borrowing from the value is not live after the use

  This pass just checks all places where a value could be consumed and if 
  it can it does so.

  When a value becomes newly consumed by a calldirect this means that now inside
  the callees body argument can be considered owned. Thus the callee will be copied,
  have their respective arguments set as owned and then put in the optimization queue.
  That is of course if the needed version does not yet exist.

  As for now the monomorpization of functions is all local to this optimization pass.
  In the sense that no other opt can run between the monomorphization and this pass
  going over the monomorphed function. I might want to change this later but for now the 
  borrbbarg opt does not care about the borrowing state of function args so it really has 
  no tangible benefit as of now.

*)

let consume_opt_func (b : builder) funcid =

  let fn = match FuncMap.find_opt funcid b.program.funcs with
    | Some fn -> fn
    | None -> failwith (Printf.sprintf "consume_opt_func: funcid %d has no function" funcid)
  in
  let live_info = Live.get_live_info fn in

  (* all newly created funcids go here to be returned *)
  let opt_queue = ref [] in


  BBMap.iter (fun bbid bb ->

    let try_consume used_later sc  =
      if get_ownership_func fn sc.ssaid = Owned then (
        let cannot_be_used_later = sc.ssaid :: Borrow.find_borrowers fn sc.ssaid in
        if List.for_all (fun ssaid -> not (SsaSet.mem ssaid !used_later)) cannot_be_used_later then (
          sc.consume <- true
        )
      );
      used_later := SsaSet.add sc.ssaid !used_later;
    in

    let add_use used_later ssaid = 
      used_later := SsaSet.add ssaid !used_later
    in

    let try_consume_lst used_later scs = List.iter (try_consume used_later) scs in

    let try_consume_br used_later (br : branch) = 
      let target_bb = match BBMap.find_opt br.bbid fn.bbs with
        | Some bb -> bb
        | None -> failwith (Printf.sprintf "consume_opt_func: bb %d has no target bb %d" bbid br.bbid)
      in
      let bbargs_own = List.map (fun ssa -> get_ownership_func fn ssa) target_bb.args in
      List.iter2 (fun sc own -> 
          if own = Owned then try_consume used_later sc (*only consume if the bb arg is owned*)
          else add_use used_later sc.ssaid
        ) br.args bbargs_own
    in


    let add_uses used_later ssaids = List.iter (add_use used_later) ssaids in

    (* for the used last set the live out is used, 
       except for cbr instructions where the live in of the succ is used*)
    let ul = ref live_info.live_out.(bbid) in

    (match bb.term with
    | Some (Br br) -> try_consume_lst (ul) br.args
    | Some (Cbr (_, ibr, ebr)) -> 
        let iflivein = ref live_info.live_in.(ibr.bbid) in
        let elselivein = ref live_info.live_in.(ebr.bbid) in
        try_consume_br iflivein ibr; 
        try_consume_br elselivein ebr;
        ul := SsaSet.union !ul (SsaSet.union !iflivein !elselivein) (*make sure the uses in the cbr are known for the ops*)
    | Some (Ret retval) -> try_consume ul retval
    | _ -> failwith (Printf.sprintf "consume_opt_func: bb %d has no term" bbid)
    );
    
    List.iter (fun op ->
      match op with
      | Pack (_, sc, scs) -> try_consume ul sc; try_consume_lst ul scs
      | CallClosure (_, sc) -> try_consume ul sc
      | CallDirect (_, funcid_ref, scs) -> (
          try_consume_lst ul scs;
          let ownsig = List.map (fun sc -> 
              if is_memtyp (get_mirtyp_func fn sc.ssaid) then
                if sc.consume then Owned else Borrowed
              else NoMem
              ) scs 
          in
          let vers = match FuncMap.find_opt !funcid_ref b.func_vers with
            | Some vers -> vers
            | None -> failwith (Printf.sprintf "consume_opt_func: funcid %d has no versions" !funcid_ref)
          in
          let existing_vers = List.find_opt (fun versfuncid ->
              let versfn = 
                match FuncMap.find_opt versfuncid b.program.funcs with
                | Some fn -> fn
                | None -> failwith (Printf.sprintf "consume_opt_func: funcid %d has no function" versfuncid)
              in
              List.for_all2 (fun needed (argssaid, _ ) -> 
                  needed = get_ownership_func versfn argssaid) ownsig versfn.args 
              || versfn.extern_name <> None (*external functions can not be monomorphized*)
            ) vers in
          match existing_vers with
          | Some versfuncid ->  funcid_ref := versfuncid
          | None -> 
              let fn_copy = copy_func b !funcid_ref in
              List.iter2 (fun needed (argssaid, _ ) -> 
                set_ownership_func fn_copy argssaid needed
              ) ownsig fn_copy.args;
              funcid_ref := fn_copy.funcid;
              opt_queue := fn_copy.funcid :: !opt_queue;
        )
      | GarbageCollect mems -> add_uses ul mems
      | StoreGlobal (_, sc) -> try_consume ul sc
      | Uopi32 (_, _, a) | Uopi8 (_, _, a) ->  add_use ul a
      | Bopi32 (_, _, a, b) | Bopi8 (_, _, a, b) ->  add_use ul a; add_use ul b
      | Tupinit (_, scs) ->  try_consume_lst ul scs
      | Tupextract (_, sc) -> try_consume ul sc
      | Tupview (_, tup) -> add_use ul tup
      | Veclit (_, scs) ->  try_consume_lst ul scs
      | Vecinit (_, defval, dims) -> add_use ul defval; add_uses ul dims
      | Veclen (_, vec) ->  add_use ul vec
      | Vecread (_, vec, idxs) ->  add_use ul vec; add_uses ul idxs
      | Vecwrite (_, sc, vec, idxs) -> add_use ul vec; try_consume ul sc; add_uses ul idxs
      | Vecinsert (_, vec_sc, vecins_sc, idxs) -> try_consume ul vec_sc; try_consume ul vecins_sc; add_uses ul idxs
      | Vecslice (_, vec, start, len) ->  add_use ul vec; add_use ul start; add_use ul len
      | Vecextend (_, vec, lit, off) ->  add_use ul vec; add_use ul lit; add_use ul off
      | _ -> () (* no uses*)
    )  bb.ops
  ) fn.bbs;
  !opt_queue


let consume_opt (b : builder) : unit =
  let worklist = Queue.create () in
  FuncMap.iter (fun orig_funcid vers -> 
    assert (List.length vers = 0);
    let fn = match FuncMap.find_opt orig_funcid b.program.funcs with
      | Some fn -> fn
      | None -> failwith (Printf.sprintf "consume_opt: funcid %d has no function" orig_funcid)
    in
    match fn.extern_name with
    | Some _ -> ()
    | None -> (
        (* the intitially lowered functions are just kept for copying an unoptimized version *)
        let new_func = copy_func b fn.funcid in
        Queue.add new_func.funcid worklist)
  ) b.func_vers;

  while not (Queue.is_empty worklist) do
    let funcid = Queue.pop worklist in
    let new_funcids = consume_opt_func b funcid in
    List.iter (fun new_funcid -> Queue.add new_funcid worklist) new_funcids
  done