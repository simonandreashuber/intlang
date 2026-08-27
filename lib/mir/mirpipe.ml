open Mir
open Buildmir
open Printmir
open Analysis

open Calldirectopt
open Tco
open Dceopt
open Compactcfgopt


open Memopt

(*
  One module to collect all the Passes

  The Mir has 2 Phases of passes:
    1. Ownership agnostic passes, these passes do not care about ownership
       and run on the originally lowered versions (for example TOC or DCE).
    2. Optimizing optimizing the ownership of bb and function args +
       enabling uses to consume memory objects. Theses run on and produce new
       optimized versions of functions
*)

let run_pipeline (b : builder) : unit =
  try

    let aly = create_analysis_info () in

    (* Run the passes in the order they are defined in the pipeline *)

    (* 1. Ownership agnostic Passes *)
    Calldirectopt.calldirect_opt b aly;
    Tco.tco_opt b aly;
    Dceopt.dce_opt b aly;
    Compactcfgopt.compactcfg_opt b aly;

    (* 2. Ownership and Consumption Passes *)
    Memopt.mem_opt b aly;
    
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
    Printf.eprintf "Error during Mir Pipeline: %s\nBacktrace:\n%s\n" msg backtrace;
    raise e