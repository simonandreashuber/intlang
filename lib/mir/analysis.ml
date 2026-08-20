open Mir
open Printmir
open Buildmir

open Live
open Borrow
open Dom

let analyze_func (fn : func) : unit =
  let live = Live.live_analysis fn in
  let borrow = Borrow.borrow_analysis fn in
  let dom = Dom.dom_analysis fn in
  fn.analysis <- Some { live; borrow; dom }