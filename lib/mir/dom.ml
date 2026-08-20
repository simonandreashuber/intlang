open Mir 
open Printmir

let dom_analysis (fn : func) : Mir.dom_info =
  let dom = {idom = Array.make 0 None} in
  dom