open Mir
open Buildmir

open Calldirectopt
open Tco

(*
  One module to collect all the Passes
*)

let run_passes (b : builder) : unit =
  Calldirectopt.calldirect_opt b;
  Tco.tco_opt b