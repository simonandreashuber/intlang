(*

  MIR Tail Call Optimization

  Just a classic TCO the only thing that is maybe noteworthy is
  that the implementation does not require a return directly
  in the bb where a recursive call in tail position is but
  if the returned result is just passed via bb args to the 
  ret term this is also discoverd and valid.

*)



open Mir
open Printmir
open Buildmir
open Analysis

(* Stores the raw definition payload of a closure SSA ID *)
type tc_bbtyp =
  (*
    ...
    %a = calldirect @self_rec_funcid ...
    ret %a 
  *)
  | TailCall of ssaconsume list
  (*
    ...
    %a = calldirect @self_rec_funcid ...
    br somebb(%a) 
  *)
  | TailCallOrigin of bbid * (ssaconsume list)
  (*
    bb: x (%a) :
      br somebb(%a) 
  *)
  | TailCallForward of bbid
  (*
    bb: x (%a) :
      ret %a 
  *)
  | TailCallSink 


type tc_info = {
  bbid : bbid;
  args : ssaconsume list;
}

let tco_opt_func (b : builder) (aly : analysis_info) (fn : func) : unit =


  let tcbbs : (bbid, tc_bbtyp) Hashtbl.t = Hashtbl.create 32 in
  let fid = fn.funcid in

  (* -------------------------------------------------------------------- *)
  (* Pass 1: Iterate all bbs and classify them                            *)
  (* -------------------------------------------------------------------- *)
  BBMap.iter (fun _ (bb : bb) ->
    match bb.args, bb.ops, bb.term with
    | _, (CallDirect (callres, callfuncid, callargs)) :: tl, Some (Ret ret) 
      when callres = ret && !callfuncid = fid
          -> Hashtbl.add tcbbs bb.bbid (TailCall callargs)
    | _, (CallDirect (callres, callfuncid, callargs)) :: tl, Some (Br (targetbbid, [arg])) 
      when callres = arg.ssaid && !callfuncid = fid
          -> Hashtbl.add tcbbs bb.bbid (TailCallOrigin (targetbbid, callargs))
    | [res], [], Some (Br (targetbbid, [arg])) 
      when res = arg.ssaid 
          -> Hashtbl.add tcbbs bb.bbid (TailCallForward targetbbid)
    | [res], [], Some (Ret ret) 
      when res = ret 
          -> Hashtbl.add tcbbs bb.bbid (TailCallSink)
    | _ -> ()
  ) fn.bbs;

  (* -------------------------------------------------------------------- *)
  (* Lazy Recursive Resolver                                              *)
  (* -------------------------------------------------------------------- *)
  let resolve (bbid : bbid) : tc_info option =
    let rec resolveaux (bbid : bbid) : bool =
      match Hashtbl.find_opt tcbbs bbid with
      | Some (TailCallSink) -> true
      | Some (TailCallForward targetbbid) -> resolveaux targetbbid
      | _ -> false
    in
    match Hashtbl.find_opt tcbbs bbid with
    | Some (TailCall args) ->
        Some { bbid = bbid; args = args }
    | Some (TailCallOrigin (targetbbid, args)) ->
        if resolveaux targetbbid then
          Some { bbid = bbid; args = args }
        else
          None
    | _ -> None
  in

  (* -------------------------------------------------------------------- *)
  (* Pass 2: Find all valid tail call positions                           *)
  (* -------------------------------------------------------------------- *)
  let tcpos = BBMap.fold (fun _ (bb : bb) acc ->
      match resolve bb.bbid with
      | Some info -> info :: acc
      | None -> acc
  ) fn.bbs []
  in

  
  if tcpos = [] then
    ()
  else
  (* -------------------------------------------------------------------- *)
  (* Step 3: If there are valid tail calls modify CFG                     *)
  (* -------------------------------------------------------------------- *)

  (*find fixed args, 
    fixed args are just passed down in their original argument position on all tail calls 
    ie. they are constant for all tail calls hence they do not need to be passed on the backedge*)
  let fixed_args = 
    List.fold_left (fun acc tc_info ->
      List.map2 (fun acc_arg_opt tcp_arg -> 
        match acc_arg_opt, tcp_arg with
        | Some acc_arg, {ssaid = tcp_ssaid} when acc_arg = tcp_ssaid -> Some acc_arg
        | _,_ -> None
        ) acc tc_info.args
    ) (List.map (fun (arg_ssaid, _) -> Some arg_ssaid) fn.args) tcpos in

  switch_func b fn;

  (*find substitution mapping from old ssaid to new ssaid for all non-fixed args
    the strategy of this tco is to replace all non-fixed arguments with fresh ssaids
    this brings the benefit of avoiding ssaid changes in the entire cfg*)
  let sub = 
    List.rev @@
    List.fold_left2 (fun subacc (arg_ssaid, _) fixed_arg_opt ->
      match fixed_arg_opt with
      | Some _ -> subacc (*arg fixed => no substitution*)
      | None -> (arg_ssaid, fresh_ssaid b) :: subacc (*arg not fixed => substitution*)
    ) [] fn.args fixed_args 
  in

  (* change the function args according to the substitution mapping *)
  fn.args <- List.map (fun (arg_ssaid, arg_name_opt) ->
    match List.assoc_opt arg_ssaid sub with
    | Some new_arg_ssaid -> (
      set_mirtyp_ownership_func fn new_arg_ssaid (get_mirtyp_func fn arg_ssaid) (get_ownership_func fn arg_ssaid);
      (new_arg_ssaid, arg_name_opt)
    )
    | None -> (arg_ssaid, arg_name_opt)
  ) fn.args;

  (* give the old entry bb, which now becomes the loop header
     the bb arguments that are passed on the backedge *)
  let old_entry_bb = find_bb b fn.funcid (Option.get fn.entry_bb) in
  old_entry_bb.args <- 
    List.map (fun (orig_ssaid, _) -> 
      if get_ownership_func fn orig_ssaid = Borrowed then
        set_ownership_func fn orig_ssaid Owned; (*default bb arg behavior is owned*)
      orig_ssaid
      ) sub;
  old_entry_bb.name <- "tco_loop_header";

  (* create the new entry bb that propagates the arguments *)
  let new_entry_bb = create_bb b "tco_new_entry" [] in  
  set_entry_bb b new_entry_bb.bbid;
  switch_bb b new_entry_bb;
  emit_term b (Br (old_entry_bb.bbid, List.map (fun (_, arg_ssaid) -> ssac arg_ssaid) sub));

  (* iterate all tail calls: remove the directcalls and put the backedge *)
  List.iter (fun tcp -> 
    let bb = find_bb b fid tcp.bbid in
    bb.ops <- (
      match bb.ops with
      | (CallDirect _ ) :: tl -> tl
      | _ -> raise (Errors.MirError ("TCO: BB that was previously classified as tail call no longer has a tail call in its ops, internal bug")) 
    );
    let passed_args =
      List.rev @@
      List.fold_left2 (fun subacc arg fixed_arg_opt ->
        match fixed_arg_opt with
        | Some _ -> subacc (*arg fixed => dont pass on backedge*)
        | None -> arg :: subacc (*arg not fixed => pass on backedge*)
      ) [] tcp.args fixed_args 
    in
    bb.term <- Some (Br (old_entry_bb.bbid, passed_args))
    ) tcpos;

    invalidate_all_analysis aly fn.funcid



let tco_opt (b : builder) (aly : analysis_info) : unit =
  FuncMap.iter (fun _fid fn -> 
    match fn.extern_name with
    | Some _ -> ()
    | None -> tco_opt_func b aly fn
  ) b.program.funcs