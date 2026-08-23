open Mir
open Buildmir
open Printmir

open Calldirectopt
open Tco
open Dceopt
open Compactcfgopt

open Borrbbargopt
open Consumeopt

(*
  One module to collect all the Passes
*)

let run_passes (b : builder) : unit =
  try
    Calldirectopt.calldirect_opt b;
    Tco.tco_opt b;
    Dceopt.dce_opt b;
    Compactcfgopt.compactcfg_opt b;
    Borrbbargopt.borrbbarg_opt b;
    Consumeopt.consume_opt b
  with e ->
    let msg = Printexc.to_string e in
    let backtrace = Printexc.get_backtrace () in
    Printf.eprintf "%s\n" (Printmir.string_of_program b.program);
    let curr_fun, curr_bb = 
      match b.cursor with
      | (Some func, Some bb) -> ("func_" ^ string_of_int func.funcid, "bb_" ^ string_of_int bb.bbid)
      | _,_ -> ("None", "None")
    in
    Printf.eprintf "Cursor: %s %s\n" curr_fun curr_bb;
    Printf.eprintf "Error during lowering: %s\nBacktrace:\n%s\n" msg backtrace;
    raise e