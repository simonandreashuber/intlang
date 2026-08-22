open Mir
open Buildmir

open Preds

let compactcfg_opt_func (fn : func) : unit =

  let predsarr = (Preds.get_preds_info fn).preds in

  let get_preds = Preds.get_preds predsarr in
  let add_pred = Preds.add_pred predsarr in
  let rem_pred = Preds.rem_pred predsarr in
  
  (* the actual array for the predecessors is "save" as local var predsarr so Setting to None is ok*)
  Mir.invalidate_all_analysis fn;

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
    done;

    (* The predecessor info is updated and valid so no reason to not keep it *)
    fn.preds <- Some { preds = predsarr }

let compactcfg_opt (b : builder) : unit =
  FuncMap.iter (fun _fid fn -> 
    match fn.extern_name with
    | Some _ -> ()
    | None -> compactcfg_opt_func fn
  ) b.program.funcs