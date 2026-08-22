open Mir 

open Preds
open Rpo

let compute_dom (fn : func) =
  
  let rpo_list = (Rpo.get_rpo_info fn).rpo_lst in
  let rpo_idx = (Rpo.get_rpo_info fn).rpo_idx in
  
  let get_preds = Preds.get_preds (Preds.get_preds_info fn).preds in

  (* --- Cooper-Harvey-Kennedy Algorithm Setup --- *)
  let idom = Array.make fn.next_bbid None in

  let entry_id = Option.get fn.entry_bb in
  idom.(entry_id) <- fn.entry_bb;

  (* Intersect function: walks up the dominator tree until two paths meet *)
  let intersect b1 b2 =
    let f1 = ref b1 in
    let f2 = ref b2 in
    while !f1 <> !f2 do
      while rpo_idx.(!f1) > rpo_idx.(!f2) do
        match idom.(!f1) with
        | Some parent -> f1 := parent
        | None -> failwith "dom_analysis: malformed idom during intersect"
      done;
      while rpo_idx.(!f2) > rpo_idx.(!f1) do
        match idom.(!f2) with
        | Some parent -> f2 := parent
        | None -> failwith "dom_analysis: malformed idom during intersect"
      done;
    done;
    !f1
  in

  (* --- STEP 4: Fixed-Point Iteration over RPO --- *)
  let rpo_nodes_except_entry = List.filter (fun b -> b <> entry_id) rpo_list in
  let changed = ref true in

  while !changed do
    changed := false;
    List.iter (fun b ->
      let b_preds = get_preds b in
      (* Filter predecessors that already have an idom assigned *)
      let processed_preds = List.filter (fun p -> idom.(p) <> None) b_preds in

      match processed_preds with
      | [] -> ()
      | first_p :: rest_p ->
          let new_idom = List.fold_left (fun acc p ->
            intersect p acc
          ) first_p rest_p in

          if idom.(b) <> Some new_idom then begin
            idom.(b) <- Some new_idom;
            changed := true
          end
    ) rpo_nodes_except_entry;
  done;

  (* Entry block has no strict dominator *)
  idom.(entry_id) <- None;

  (* --- STEP 5: Build Dominator Tree --- *)
  let dom_tree = Array.make fn.next_bbid [] in
  Array.iteri (fun b_id opt_idom ->
    match opt_idom with
    | Some parent_id -> dom_tree.(parent_id) <- b_id :: dom_tree.(parent_id)
    | None -> ()
  ) idom;

  fn.dom <- Some { idom = idom; dom_tree = dom_tree }


let get_dom_info fn =
  if fn.dom = None then compute_dom fn;
  Option.get fn.dom


let does_strictly_dominate fn dominator dominated =
  let dom_info = get_dom_info fn in
  let rec check_dom b =
    if b = dominator then true
    else match dom_info.idom.(b) with
      | Some parent -> check_dom parent
      | None when fn.entry_bb = Some b -> false
      | _ -> failwith "does_strictly_dominate: reached a block with no idom that is not the entry"
  in
  if dominator = dominated 
  then false 
  else check_dom dominated