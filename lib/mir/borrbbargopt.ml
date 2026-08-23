open Mir
open Buildmir

open Live
open Borrow
open Dom

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

  It is possible that "promoting" an BB arg from owned to borrowed destroys some
  downstream optimization. Like in this case:

  say %0 and %1 are vectors passed owned
  ...
  if:
    br merge(%0)
  else:
    br merge(%1)
  merge (%2)
    %3 = vecset %0 ..
    %4 = vecset %1 ..
    %5 = vecset %2 ..
  ...

  If %2 is borrowed each vecset needs to copy but if %2 is owned
  none of the vecset need to copy and only the bb arg needs one copy.
  So two unneeded copies. Of course this example is a bit "manufcatured"
  but say we would not have vecset but two directcalls for the def of %3 and %4
  which then combined are used in a condition for the cbr and each of tbe 
  cbr successors then does a vecset on %2. This is more realistic and also
  induces then 2 extra copies. Additionally it become much harder to just 
  reorder instructions to avoid things even with the BB arg borrowing.

  As of now the optimization just promotes all BB args to borrowed that are legal
*)

let borrbbarg_opt_func fn =

  Mir.invalidate_all_analysis fn;

  let live_info = Live.get_live_info fn in

  BBMap.iter ( fun _ bb ->
    List.iter (fun arg ->
        if is_memtyp (get_mirtyp_func fn arg) &&
           (get_ownership_func fn arg = Owned) then (
          let pot_owners = Borrow.find_funclocal_owners fn arg in
          if
          List.for_all (fun owner_ssaid ->
            let (owner_bbid,_) = live_info.def.(owner_ssaid) in
            Dom.does_strictly_dominate fn owner_bbid bb.bbid
          ) pot_owners
          then (
            set_ownership_func fn arg Borrowed
          )
        )
    ) bb.args
  ) fn.bbs


let borrbbarg_opt (b : builder) : unit =
  FuncMap.iter (fun _fid fn -> 
    match fn.extern_name with
    | Some _ -> ()
    | None -> borrbbarg_opt_func fn
  ) b.program.funcs