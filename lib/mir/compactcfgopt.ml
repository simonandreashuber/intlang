open Mir
open Buildmir
open Analysis

let compactcfg_opt_func (aly : analysis_info) (fn : func) : unit =

  let predsarr = (get_preds_info aly fn).preds in

  let get_preds = Analysis.get_preds predsarr in
  let add_pred = Analysis.add_pred predsarr in
  let rem_pred = Analysis.rem_pred predsarr in

  (* worlist *)
  let wl = Queue.create () in
  BBMap.iter (fun bbid _ -> Queue.push bbid wl) fn.bbs;

  while Queue.is_empty wl = false do
    let bbid = Queue.pop wl in    
    match BBMap.find_opt bbid fn.bbs with
    | Some bb when Some (bb.bbid) <> fn.entry_bb -> (

      let preds = get_preds bbid in
      match preds, bb.args, bb.ops, bb.term with
      (* Unreachable BB removal *)
      | [], _, _ , Some (Br (brbbid, brargs)) -> (
        fn.bbs <- BBMap.remove bbid fn.bbs;
        rem_pred bb.bbid brbbid;
        Queue.push brbbid wl
      )
      | [], _, _ , Some (Cbr (_, ibr, ebr)) -> (
        fn.bbs <- BBMap.remove bbid fn.bbs;
        rem_pred bb.bbid ibr;
        rem_pred bb.bbid ebr;
        Queue.push ibr wl;
        Queue.push ebr wl
      )
      (* Trampoline BB removal, no args*)
      | _, [], [], Some (Br (tbrbbid, [])) when tbrbbid <> bbid-> (
          List.iter (
            fun predbbid ->
              let predbb = BBMap.find predbbid fn.bbs in
                (match predbb.term with
                | Some (Br (pbrbbid, [])) when bbid = pbrbbid-> 
                    predbb.term <- Some (Br (pbrbbid, []))
                | Some (Cbr (cond, pibrbbid, pebrbbid)) -> 
                    let pibrbbid' = if pibrbbid = bbid then tbrbbid else pibrbbid in
                    let pebrbbid' = if pebrbbid = bbid then tbrbbid else pebrbbid in
                    predbb.term <- Some (Cbr (cond, pibrbbid', pebrbbid'))
                | _ -> failwith "compactcfg_opt_func: predbb term is not of the fromat expected for a trampoline bb with no args");
                rem_pred predbbid bbid;
                add_pred predbbid tbrbbid;
                Queue.push predbbid wl
          ) preds;
          fn.bbs <- BBMap.remove bbid fn.bbs;
          rem_pred bbid tbrbbid;
          Queue.add tbrbbid wl
      )
      (* Trampoline BB removal, with args *)
      | _, bbargs, [], Some (Br (tbrbbid, tbrargs)) when List.length bbargs > 0 && tbrbbid <> bbid-> (
          List.iter (
            fun predbbid ->
              let predbb = BBMap.find predbbid fn.bbs in
                (match predbb.term with
                | Some (Br (pbrbbid, pbrargs)) when pbrbbid = bbid -> 
                    let sub = List.map2 (fun bbarg brarg -> (bbarg, brarg.ssaid)) bbargs pbrargs in
                    let tbrargs' = sub_sc_list sub tbrargs in
                    predbb.term <- Some (Br (tbrbbid, tbrargs'))
                | _ -> failwith "compactcfg_opt_func: predbb term is not of the fromat expected for a trampoline bb with args");
                rem_pred predbbid bbid;
                add_pred predbbid tbrbbid;
                Queue.push predbbid wl
          ) preds;
          fn.bbs <- BBMap.remove bbid fn.bbs;
          rem_pred bbid tbrbbid;
          Queue.add tbrbbid wl
      )
      (* Successor BB chain compaction*)
      | _, _, _, Some (Br (brbbid, brargs)) when get_preds brbbid = [bbid] -> (
        let succbb = BBMap.find brbbid fn.bbs in
        let sub = List.map2 (fun sbbarg brarg -> (sbbarg, brarg.ssaid)) succbb.args brargs in
        bb.ops <- (sub_ops_uses sub succbb.ops) @ bb.ops;
        (match succbb.term with
        | Some (Br (sbrbbid, sbrargs)) -> (
          bb.term <- Some (Br (sbrbbid, sub_sc_list sub sbrargs));
          rem_pred brbbid sbrbbid;
          add_pred bbid sbrbbid;
          Queue.push sbrbbid wl
        )
        | Some (Cbr (cond, sibrbbid, sebrbbid)) -> (
          bb.term <- Some (Cbr (sub_id sub cond, sibrbbid, sebrbbid));
          rem_pred brbbid sibrbbid;
          rem_pred brbbid sebrbbid;
          add_pred bbid sibrbbid;
          add_pred bbid sebrbbid;
          Queue.push sibrbbid wl;
          Queue.push sebrbbid wl;
        )
        | Some (Ret ret) -> (
          bb.term <- Some (Ret (sub_id sub ret));
        )
        | None -> failwith "compactcfg_opt_func: succbb term is not of the fromat expected for a trampoline bb with args"); 
        fn.bbs <- BBMap.remove brbbid fn.bbs;
        rem_pred bbid brbbid;
        Queue.push bbid wl;
      )
      | _ -> () (* no compact opportunity*)
    )
    | _ -> () (* already removed bb or entry bb*);
    done;

    invalidate_all_analysis aly fn.funcid

let compactcfg_opt (b : builder) (aly : analysis_info) : unit =
  FuncMap.iter (fun _fid fn -> 
    match fn.extern_name with
    | Some _ -> ()
    | None -> compactcfg_opt_func aly fn
  ) b.program.funcs