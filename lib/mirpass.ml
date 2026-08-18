open Mir
open Buildmir

open Calldirectopt
open Tco
open Compactcfgopt

(*
  One module to collect all the Passes
*)

let run_passes (b : builder) : unit =
  Calldirectopt.calldirect_opt b;
  Tco.tco_opt b;
  Compactcfgopt.compactcfg_opt b
