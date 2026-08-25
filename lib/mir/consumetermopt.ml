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

let consumeterm_opt_func (b : builder) (fn : func) =

  let live_info = Live.get_live_info fn in

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

    let try_consume_br used_later brbbid brargs = 
      let target_bb = match BBMap.find_opt brbbid fn.bbs with
        | Some bb -> bb | None -> failwith (Printf.sprintf "try_consume_br: bb %d has no target bb %d" bbid brbbid) in

      let bbargs_own = List.map (fun ssa -> get_ownership_func fn ssa) target_bb.args in
      List.iter2 (fun sc own -> 
          if own = Owned then try_consume used_later sc (*only consume if the target bb arg is owned*)
          else add_use used_later sc.ssaid
        ) brargs bbargs_own
    in

    (* for the used last set the live out is used, 
       except for cbr instructions where the live in of the succ is used*)

    let ul = ref live_info.live_out.(bbid) in

    (match bb.term with
    | Some (Br (brbbid, brargs)) -> try_consume_br ul brbbid brargs
    | Some (Cbr (cond , _, _)) -> 
        add_use ul cond
    | Some (Ret retval) -> add_use ul retval
    | _ -> failwith (Printf.sprintf "consume_opt_func: bb %d has no term" bbid)
    )

  ) fn.bbs

let consumeterm_opt_funcidlst (b : builder) (lst : funcid list) : unit =
  List.iter (fun funcid -> 
    let fn = match FuncMap.find_opt funcid b.program.funcs with
      | Some fn -> fn
      | None -> failwith (Printf.sprintf "consume_opt: funcid %d has no function" funcid)
    in
    match fn.extern_name with
    | Some _ -> ()
    | None -> consumeterm_opt_func b fn
  ) lst