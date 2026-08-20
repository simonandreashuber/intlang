open Mir
open Buildmir

let compactcfg_opt_func (fn : func) : unit =

  fn.analysis <- None;  (* Clear any existing analysis *)

  (* DynArray for predecessor information and helpers*)
  let preds_dynarr = Dynarray.make fn.next_bbid [] in

  let get_preds bbid = Dynarray.get preds_dynarr bbid in

  let add_pred pred_bbid succ_bbid =
    let old_pred = Dynarray.get preds_dynarr succ_bbid in 
    Dynarray.set preds_dynarr succ_bbid (pred_bbid :: old_pred)
  in

  let rem_pred pred_bbid succ_bbid =
    let old_pred = Dynarray.get preds_dynarr succ_bbid in 
    let new_pred = List.filter (fun x -> x <> pred_bbid) old_pred in
    Dynarray.set preds_dynarr succ_bbid new_pred
  in

  (* Accumulate Predecessor Info *)
  BBMap.iter (fun _ bb ->
    match bb.term with
    | Some (Br br) -> (
        add_pred bb.bbid br.bbid
    )
    | Some (Cbr (_, ibr, ebr)) -> (
        add_pred bb.bbid ibr.bbid;
        add_pred bb.bbid ebr.bbid
    )
    | Some _ -> ()
    | None -> failwith ("compactcfgopt: bb " ^ string_of_int bb.bbid ^ " has no term")
  ) fn.bbs;
  
  (* worlist *)
  let wl = Queue.create () in
  BBMap.iter (fun bbid _ -> Queue.push bbid wl) fn.bbs;

  while Queue.is_empty wl = false do
    let bbid = Queue.pop wl in    
    match BBMap.find_opt bbid fn.bbs with
    | Some bb when Some (bb.bbid) <> fn.entry_bb -> (

      let preds = get_preds bbid in
      match preds, bb.ops, bb.term with
      (* Unreachable BB removal *)
      | [], _ , Some (Br br) -> (
        fn.bbs <- BBMap.remove bbid fn.bbs;
        rem_pred bb.bbid br.bbid;
        Queue.push br.bbid wl
      )
      | [], _ , Some (Cbr (_, ibr, ebr)) -> (
        fn.bbs <- BBMap.remove bbid fn.bbs;
        rem_pred bb.bbid ibr.bbid;
        rem_pred bb.bbid ebr.bbid;
        Queue.push ibr.bbid wl;
        Queue.push ebr.bbid wl
      )
      (* Trampoline BB removal *)
      | _, [], Some (Br tbr) when tbr.bbid <> bbid-> (
          let sub_branch (pbr : branch) : branch =
            if pbr.bbid <> bbid then pbr else (*only sub branches that branch to the trampoline bb*)
            let sub = List.map2 (fun bbarg brarg -> (bbarg, brarg.ssaid)) bb.args pbr.args in
            sub_branch sub tbr
          in
          List.iter (
            fun predbbid ->
              let predbb = BBMap.find predbbid fn.bbs in
                (match predbb.term with
                | Some (Br pbr) -> 
                    predbb.term <- Some (Br (sub_branch pbr))
                | Some (Cbr (cond, pibr, pebr)) -> 
                    predbb.term <- Some (Cbr (cond, sub_branch pibr, sub_branch pebr))
                | _ -> failwith "predbb term is not a branch");
                rem_pred bb.bbid tbr.bbid;
                add_pred predbb.bbid tbr.bbid;
                Queue.push predbb.bbid wl
          ) preds;
          fn.bbs <- BBMap.remove bbid fn.bbs;
          rem_pred bbid tbr.bbid;
          Queue.add tbr.bbid wl
      )
      (* Successor BB chain compaction*)
      | _, _, Some (Br br) when get_preds br.bbid = [bbid] -> (
        let succbb = BBMap.find br.bbid fn.bbs in
        let sub = List.map2 (fun bbarg brarg -> (bbarg, brarg.ssaid)) succbb.args br.args in
        bb.ops <- (sub_ops_uses sub succbb.ops) @ bb.ops;
        bb.term <- Some (sub_term sub (Option.get succbb.term));
        fn.bbs <- BBMap.remove br.bbid fn.bbs;
        rem_pred bb.bbid br.bbid;
        Queue.push bb.bbid wl;
        match bb.term with
        | Some (Br nbr) -> (
          rem_pred succbb.bbid nbr.bbid;
          add_pred bb.bbid nbr.bbid;
          Queue.push nbr.bbid wl
        )
        | Some (Cbr (_, nibr, nebr)) -> (
          rem_pred succbb.bbid nibr.bbid;
          rem_pred succbb.bbid nebr.bbid;
          add_pred bb.bbid nibr.bbid;
          add_pred bb.bbid nebr.bbid;
          Queue.push nibr.bbid wl;
          Queue.push nebr.bbid wl
        )
        | _ -> (); (*ret or no term (no term should not happen)*)
      )
      | _ -> () (* no compact opportunity*)
    )
    | _ -> () (* already removed bb or entry bb*);
    done

let compactcfg_opt (b : builder) : unit =
  FuncMap.iter (fun _fid fn -> 
    match fn.extern_name with
    | Some _ -> ()
    | None -> compactcfg_opt_func fn
  ) b.program.funcs