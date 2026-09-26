(*

  MIR inline pass

  Should inline small functions, especially if closures are passed (eg. folds or maps).
  Should run after calldirectopt and toc, but calldirectopt should run a second time
  after this pass.

  Inlining is impossible if:
    1. caller = callee
    2. callee is external

  There are two phases:

    1. Single Call Site
      - functions of anysize with a single callsite
      - that are not exported are inlined
      - iterate functions in "call direct" post order
      - the left over function are then deleted during dead function elimination

    2. Heuristics
      - small functions should be inlined (removes abi call overhead)
      - function that have a closures as function args should be inlined
        (can enable callclosure => calldirect optimization which can avoid
        potentially massive copies that happen due to the closure ownership semantics)

*)

open Mir
open Printmir
open Buildmir
open Analysis



let inline_opt_func (decide_inline : builder -> analysis_info -> funcid -> funcid -> bool) (b : builder) (aly : analysis_info) (fn : func) : bool =

  let did_inline = ref false in

  (*create worklist with all bbs initially in the function*)
  let q = Queue.create () in
  Queue.add_seq q (Seq.map (fun (bbid, bb) -> assert (bbid = bb.bbid); bbid) (BBMap.to_seq fn.bbs));

  while not (Queue.is_empty q) do
    let pre_inline_bbid = Queue.pop q in
    let pre_inline_bb = find_bb_func fn pre_inline_bbid in

    (* find the first opportunity in bb to inline *)
    let inline_callop_opt, pre_inline_ops, post_inline_ops =
      List.fold_left (fun (inline_callop_opt, pre_inline_ops, post_inline_ops) op ->
        match op with
        | CallDirect (def_ssaid, callee_funcid_ref, args_sc)
            when Option.is_none inline_callop_opt && decide_inline b aly fn.funcid !callee_funcid_ref
              -> (Some op, pre_inline_ops, post_inline_ops)
        | _ when Option.is_some inline_callop_opt
              -> (inline_callop_opt, pre_inline_ops, op :: post_inline_ops)
        | _   -> (inline_callop_opt, op :: pre_inline_ops, post_inline_ops)
      ) (None, [], []) (List.rev pre_inline_bb.ops) in


    match inline_callop_opt with
    | None -> ()
    | Some (CallDirect (def_ssaid, callee_funcid_ref, args_sc)) -> (

      (* inlining does happen so place builder cursor at caller *)
      switch_func b fn;

      (*new post inline bb with post_inline_ops and term form old pre inline bb*)
      let post_inline_bb = create_bb b "post_inline_bb" [] in
      post_inline_bb.ops <- post_inline_ops;
      post_inline_bb.term <- pre_inline_bb.term;

      (*new inline cfg with all func args substituted and the ret substituted by br to post inline bb*)
      let callee_fn = try find_func b !callee_funcid_ref with Not_found -> failwith "inline_opt_func: callee function not found" in

      (*main ssaid/bbid strat is to just "reserve" a number range above
        the current max ssaid/bbid and then offset all inlined ssaids/bbids*)
      let ssaid_off = fn.next_ssaid in
      fn.next_ssaid <- ssaid_off + callee_fn.next_ssaid;
      Dynarray.append_array fn.ssatyps (Dynarray.to_array callee_fn.ssatyps);
      Dynarray.append_array fn.memown (Dynarray.to_array callee_fn.memown);
      assert (Dynarray.length fn.ssatyps = fn.next_ssaid && Dynarray.length fn.memown = fn.next_ssaid);
      let bbid_off = fn.next_bbid in
      fn.next_bbid <- bbid_off + callee_fn.next_bbid;

      (* callee function args need to be subed by the ssaids used in the direct call *)
      let callee_sub = List.map2 (fun  (callee_arg_ssaid, _ ) caller_arg_sc ->
          (ssaid_off + callee_arg_ssaid, caller_arg_sc.ssaid) (* already use the ssaid_offset *)
      ) callee_fn.args args_sc in

      (*one could turn the ret into a br with one bb arg but!! there is no pass in the mir
        the could detect that this bbarg is not needed and remove it which can create copy
        limitations later so I rather go and do a substitution over the existing cfg here (see below)*)
      let caller_sub = ref [] in

      (* take callee cfg copy with the right offset and substitution *)
      let inline_bbs = ref BBMap.empty in
      BBMap.iter (fun bbid bb ->
        let nbb = copy_bb ssaid_off bbid_off bb in
        nbb.ops <- sub_ops_uses callee_sub nbb.ops;
        nbb.term <- sub_term_uses callee_sub nbb.term;
        nbb.term <- (
          match nbb.term with
          | Some (Ret (ret_ssaid)) ->
              (if !caller_sub = [] then
                caller_sub := [def_ssaid, ret_ssaid]
              else
                failwith ("inline_opt_func: two or more rets in callee: " ^ callee_fn.name ^ " (ssaid: " ^ string_of_int ret_ssaid ^ ")"));
            Some (Br (post_inline_bb.bbid, []))
          | _ -> nbb.term );
        inline_bbs := BBMap.add nbb.bbid nbb !inline_bbs
      ) callee_fn.bbs;

      (*"old" bb becomes pre inline bb with pre_inline_ops and br term to the inline cfg entry bb*)
      pre_inline_bb.ops <- pre_inline_ops;
      let inline_entry_bbid = try bbid_off + Option.get callee_fn.entry_bb with Not_found -> failwith "inline_opt_func: no entry bb in callee" in
      pre_inline_bb.term <- Some (Br (inline_entry_bbid, []));

      (*sub old ret ssaid*)
      BBMap.iter (fun _bbid bb ->
        bb.ops <- sub_ops_uses !caller_sub bb.ops;
        bb.term <- sub_term_uses !caller_sub bb.term;
      ) fn.bbs;

      (*merge inline bbs*)
      fn.bbs <- BBMap.union (fun bbid bb_caller bb_callee ->
        failwith (Printf.sprintf "Merge inline bbs conflict on bbid: %d caller_bbid: %d callee_bbid: %d, next_bbid: %d, bbid_off: %d"
         bbid bb_caller.bbid bb_callee.bbid fn.next_bbid bbid_off)
        ) fn.bbs !inline_bbs;

      (*all bbs with ops that have not been fully scanned for inlining go into the queue*)
      Queue.add post_inline_bb.bbid q;
      Queue.add_seq q (Seq.map (fun (bbid, bb) -> assert (bbid = bb.bbid); bbid) (BBMap.to_seq !inline_bbs));

      invalidate_all_analysis aly fn.funcid;

      did_inline := true;
    )
    | _-> failwith "inline_opt_func: unexpected callop"
  done;
  !did_inline


module FuncidSet = Set.Make(Int)

let get_post_order (func_map : func FuncMap.t) (visited : FuncidSet.t ref) (call_counts : int array) (rec_marked : bool array) (origin_funcid : funcid) : funcid list =
  let acc = ref [] in

  let rec dfs (stack : funcid list) (funcid : funcid) =
    let stack = funcid :: stack in
    if not (FuncidSet.mem funcid !visited) then begin
      visited := FuncidSet.add funcid !visited;
      let fn = try FuncMap.find funcid func_map with Not_found -> failwith "Function not found" in
      BBMap.iter (fun _ bb ->
        List.iter (function
          | CallDirect (_, funcid_ref, _) ->
              let callee = !funcid_ref in
              call_counts.(callee) <- call_counts.(callee) + 1;
              if List.mem callee stack then rec_marked.(callee) <- true;
              (*Printf.printf "detected direct call %d to callee @%d from caller @%d \"%s\"\n" call_counts.(callee) callee fn.funcid fn.name;*)
              dfs stack callee
          | Func (_, funcid_ref, funcid_opt_ref) -> (
            (* while func ops dont count in terms of the call graph, appearing in closure
               creation matters as this closure will probably result in a call somewhere*)
            let callee = !funcid_ref in
            call_counts.(callee) <- call_counts.(callee) + 1;
            if List.mem callee stack then rec_marked.(callee) <- true;
            (*Printf.printf "detected clos creation %d to callee @%d from caller @%d \"%s\"\n" call_counts.(callee) callee fn.funcid fn.name;*)
            match !funcid_opt_ref with
            | Some callee -> (
              call_counts.(callee) <- call_counts.(callee) + 1;
              if List.mem callee stack then rec_marked.(callee) <- true;
            )
            | None -> ()
          )
          | Pack _ | CallClosure _
          | Copy _ | Drop _ | StoreGlobal _ | LoadGlobal _ | DropGlobal _
          | Immi32 _ | Immi8 _ | ImmUnit _ | Uopi32 _ | Uopi8 _ | Bopi32 _ | Bopi8 _
          | Tupwrp _ | Tupuwrp _ | Tupborr _ | Veclit _ | Vecinit _ | Veclen _ | Vecread _
          | Vecwrite _ | Vecinsert _ | Vecslice _ | Vecextend _ -> ()
        ) (List.rev bb.ops)
      ) fn.bbs;
      (* post order: append node after all reachable callees are visited *)
      acc := funcid :: !acc
    end
  in
  dfs [] origin_funcid;
  List.rev !acc

let inline_opt (b : builder) (aly : analysis_info) : unit =

  (*Phase 1: Single Callsite*)
  let visited = ref FuncidSet.empty in
  let call_counts = Array.make (FuncMap.cardinal b.program.funcs) 0 in
  let rec_marked = Array.make (FuncMap.cardinal b.program.funcs) false in

  let post_order = ref [] in

  let aux origin_funcid_opt : unit =
    match origin_funcid_opt with
    | Some origin_funcid -> post_order := (get_post_order b.program.funcs visited call_counts rec_marked origin_funcid) @ !post_order
    | None -> ()
  in
  aux b.program.init_globals_funcid;
  aux b.program.main_funcid;
  aux b.program.uninit_globals_funcid;
  FuncMap.iter (fun _fid fn ->
    if Option.is_none fn.extern_name && fn.exported then
      post_order := (get_post_order b.program.funcs visited call_counts rec_marked fn.funcid) @ !post_order
    else
      ()
  ) b.program.funcs;

  let singlecallsite_decide_inline (b : builder) (aly : analysis_info) (caller : funcid) (callee : funcid) : bool =
    let callee_fn = try find_func b callee with Not_found -> failwith "singlecallsite_decide_inline: callee not found" in
    if Option.is_some callee_fn.extern_name || callee = caller || callee_fn.exported
    then false
    else if call_counts.(callee) = 1 then ((*Printf.printf "inline single call function %s \n" callee_fn.name;*) true) else false (* to do make smarter inline heuristics *)
  in

  List.iter (fun funcid ->
    let fn = try find_func b funcid with Not_found -> failwith "inline_opt: phase 1 function not found" in
    ignore(inline_opt_func singlecallsite_decide_inline b aly fn)
  ) !post_order;

  (*Phase 2: Heuristics*)
  let heuristics_decide_inline (b : builder) (aly : analysis_info) (caller : funcid) (callee : funcid) : bool =
    let callee_fn = try find_func b callee with Not_found -> failwith "heuristics_decide_inline: callee not found" in
    let rec mirtyp_is_closure_free (mirtyp : mirtyp) : bool =
      match mirtyp with
      | TMIRClos _ -> false
      | TMIRTup elmlst -> List.for_all mirtyp_is_closure_free elmlst
      | TMIRUnit | TMIRI8 | TMIRI32 | TMIRVec _ -> true
    in
    if Option.is_some callee_fn.extern_name || callee = caller
    then false
    else if (
      (* funciton with 1 bb is likely a small helper that is probably not worth the call overhead *)
      BBMap.cardinal callee_fn.bbs == 1 ||
      (* passing a closure means not devirtualizing => inline *)
      (not @@ List.for_all (fun (arg_ssaid, _) -> mirtyp_is_closure_free (get_mirtyp_func callee_fn arg_ssaid) ) callee_fn.args)
      (* callees that are marked as recurive are not worth inlining *)
      ) && (not @@ rec_marked.(callee))
    then ((*Printf.printf "inline function %s \n" callee_fn.name;*) true)
    else ((*Printf.printf "NOT inlining function %s, bbs: %d, rec_mark: %b \n" callee_fn.name (BBMap.cardinal callee_fn.bbs) rec_marked.(callee);*) false) (* to do make smarter inline heuristics *)
  in

  let q = Queue.create () in
  Queue.add_seq q (Seq.map (fun (funcid, fn) -> assert (funcid = fn.funcid); fn) (FuncMap.to_seq b.program.funcs));

  while not (Queue.is_empty q) do
    let fn = Queue.pop q in
    ignore(inline_opt_func heuristics_decide_inline b aly fn)
  done
