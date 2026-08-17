open Mir
open Buildmir

open Calldirectopt

(*
  One module to collect all the Passes
*)

let run_passes (b : builder) : unit =
  Calldirectopt.calldirect_opt b