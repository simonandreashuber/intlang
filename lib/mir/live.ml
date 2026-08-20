open Mir
open Printmir


let live_analysis (fn : func) : Mir.live_info =
  let live = {live_in = Array.make 0 SsaSet.empty; live_out = Array.make 0 SsaSet.empty} in
  live