open Mir

(* Predecessor Analysis *)
let get_preds predsarr bbid = Array.get predsarr bbid

let add_pred predsarr pred_bbid succ_bbid =
  let old_pred = Array.get predsarr succ_bbid in 
  Array.set predsarr succ_bbid (pred_bbid :: old_pred)

let rem_pred predsarr pred_bbid succ_bbid =
  let old_pred = Array.get predsarr succ_bbid in 
  let new_pred = List.filter (fun x -> x <> pred_bbid) old_pred in
  Array.set predsarr succ_bbid new_pred

let compute_preds fn =
  let predsarr = Array.make (fn.next_bbid) [] in

  (* Accumulate Predecessor Info *)
  BBMap.iter (fun _ bb ->
    match bb.term with
    | Some (Br br) -> (
        add_pred predsarr bb.bbid br.bbid
    )
    | Some (Cbr (_, ibr, ebr)) -> (
        add_pred predsarr bb.bbid ibr.bbid;
        add_pred predsarr bb.bbid ebr.bbid
    )
    | Some _ -> ()
    | None -> failwith ("preds: func: " ^ fn.name ^ " bb " ^ string_of_int bb.bbid ^ " has no term")
  ) fn.bbs;

  fn.preds <- Some { preds = predsarr }

  let get_preds_info fn =
    if fn.preds = None then compute_preds fn;
    Option.get fn.preds