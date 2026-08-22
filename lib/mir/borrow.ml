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
    (*
      I put all the potential borrows in the graph while this makes
      the resolvers a bit more complex it saves a massive headache 
      when answering questions like: is this bb arg borrow legal and/or
      does it extend the live range of the owners
    *)
    if Mir.is_memtyp (Mir.get_mirtyp_func fn bbarg)
    then borrow bbarg brarg.ssaid  
    ) br.args targbb.args
  in

  BBMap.iter (fun _ bb ->

    (*ops borrows*)
    List.iter (fun op ->
      match op with
      | Tupview (deflst, tup) -> (
        List.iter (fun def ->
           if Mir.is_memtyp (Mir.get_mirtyp_func fn def)
           then borrow def tup
        ) deflst
      )
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

(* Find all borrowers of a given lender not just the direct ones (can contain duplicates)*)
let find_borrowers (fn : func) (lender : ssaid) : ssaid list =
  let borrow_info = get_borrow_info fn in
  let rec transacc acc lender =
    let borrowers = borrow_info.lender_to_borrowers.(lender) in
    List.fold_left (fun acc borrower -> 
      match get_ownership_func fn borrower with
      | Borrowed -> transacc (borrower :: acc) borrower 
      | Owned -> acc
      | NoMem -> failwith (Printf.sprintf "find_borrowers: ssaid %d is not a memory type but in borrowers" borrower)
     ) acc borrowers
  in
  transacc [] lender

(* 
  Find all function local owners of a given borrower
  DOES NOT CHECK IF PASSED BORROWER IS A BORROWER
  this is usefull in the case where we would mb
  want to put it owned but atm its borrowed
*)
let find_funclocal_owners (fn : func) (borrower : ssaid) : ssaid list =
  let borrow_info = get_borrow_info fn in
  let rec transacc acc borrower =
    let lenders = borrow_info.borrower_to_lenders.(borrower) in
    (* Function args that are passed borrowed are ignored *)
    List.fold_left (fun acc lender ->
      match get_ownership_func fn lender with
      | Borrowed -> transacc acc lender
      | Owned -> (lender :: acc)
      | NoMem -> failwith (Printf.sprintf "find_owners: ssaid %d is not a memory type but in lenders" borrower)
    ) acc lenders
  in
  transacc [] borrower