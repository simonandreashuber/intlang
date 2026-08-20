open Mir
open Printmir

let borrow_analysis (fn : func) : Mir.borrow_graph =
  let borrow = {lender_to_borrowers = Hashtbl.create 10; borrower_to_lenders = Hashtbl.create 10} in
  borrow