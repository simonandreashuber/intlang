open Mir

open Preds

let compute_borrow (fn : func) : unit =

  let lender_to_borrowers = Array.make fn.next_ssaid [] in
  let borrower_to_lenders = Array.make fn.next_ssaid [] in

  (* a borrows from b*)
  let borrow borrower lender =
    lender_to_borrowers.(lender) <- borrower :: lender_to_borrowers.(lender);
    borrower_to_lenders.(borrower) <- lender :: borrower_to_lenders.(borrower)
  in

  (*helper for the terms*)
  let bbargs_borrow (br : branch) =
    let targbb = BBMap.find br.bbid fn.bbs in
    List.iter2 (fun brarg bbarg ->
    if Mir.is_memtyp (Mir.get_mirtyp_func fn bbarg) &&
       Mir.get_ownership_func fn bbarg = Borrowed 
    then borrow bbarg brarg.ssaid  
    ) br.args targbb.args
  in

  BBMap.iter (fun _ bb ->

    (*ops borrows*)
    List.iter (fun op ->
      match op with
      | Tupview (deflst, tup) -> List.iter (fun def -> borrow def tup) deflst
      | Vecread (def, vec, idxlst) -> (
        match Mir.get_mirtyp_func fn vec with
        | TMIRVec (vecdim, _) when List.length idxlst < vecdim -> borrow def vec
        | _ -> ()
      ) 
      | Vecslice (def, vec, _, _) -> borrow def vec
      | _ -> ()
    ) bb.ops;

    (*term borrows*)
    match bb.term with
    | Some ( Br br ) -> bbargs_borrow br
    | Some ( Cbr (_, ibr, ebr)) -> bbargs_borrow ibr; bbargs_borrow ebr
    | _ -> ();

  ) fn.bbs;
  
  fn.borrow <- Some ( { lender_to_borrowers; borrower_to_lenders} )

let get_borrow_info fn =
  if fn.borrow = None then compute_borrow fn;
  Option.get fn.borrow