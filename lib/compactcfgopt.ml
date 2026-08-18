open Mir
open Buildmir

let compactcfg_opt_func (b : builder) (fn : func) : unit =

  (* Accumulate Predecessor Info *)
  let pred = Dynarray.make fn.next_bbid [] in

  let add_pred bbid pred_bbid =
    let old_pred = Dynarray.get pred pred_bbid in 
    Dynarray.set pred pred_bbid (bbid :: old_pred)
  in

  let rem_pred bbid pred_bbid =
    let old_pred = Dynarray.get pred pred_bbid in 
    let new_pred = List.filter (fun x -> x <> bbid) old_pred in
    Dynarray.set pred pred_bbid new_pred
  in

  BBMap.iter (fun _ bb ->
    match bb.term with
    | Some (Br sbr) -> (
        add_pred bb.bbid sbr.bbid
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
    if Some bbid <> fn.entry_bb then
    match BBMap.find_opt bbid fn.bbs with
    | None -> () (* already removed *)
    | Some bb -> (
      let preds = Dynarray.get pred bbid in
      match preds, bb.term, bb.ops with
      (* Unreachable BB removal *)
      | [], _ ,_ -> (
        fn.bbs <- BBMap.remove bbid fn.bbs;
        match bb.term with
        | Some (Br sbr) -> (
          rem_pred bb.bbid sbr.bbid;
          Queue.push sbr.bbid wl
        )
        | Some (Cbr (_, ibr, ebr)) -> (
            rem_pred bb.bbid ibr.bbid;
            rem_pred bb.bbid ebr.bbid;
            Queue.push ibr.bbid wl;
            Queue.push ebr.bbid wl
          )
        | _ -> ()
      )
      (* Trampoline BB removal *)
      | _, Some (Br tbr), [] -> (
          let sub_branch (pbr : branch) : branch =
            if pbr.bbid <> bbid then pbr else
            let sub = List.combine bb.args pbr.args in
            let args_sub = List.map (fun arg -> match List.assoc_opt arg.ssaid sub with
              | Some v -> v
              | None -> arg
            ) tbr.args in
            brac tbr.bbid args_sub
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
      | [predbbid], _, _ -> (
        let pbb = BBMap.find predbbid fn.bbs in
        match pbb.term with 
        | Some (Br pbr) -> (
          let sub = List.map2 (fun arg barg -> (arg, barg.ssaid)) bb.args pbr.args in
          let new_ops = substitute_ops_uses sub bb.ops in
          pbb.ops <- new_ops @ pbb.ops;
          let new_term = substitute_term_uses sub (Option.get bb.term) in
          pbb.term <- Some new_term;
          fn.bbs <- BBMap.remove bbid fn.bbs;
          rem_pred pbb.bbid bb.bbid;
          Queue.push pbb.bbid wl;
          match bb.term with
          | Some (Br sbr) -> (
            rem_pred bb.bbid sbr.bbid;
            add_pred pbb.bbid sbr.bbid;
            Queue.push sbr.bbid wl
          )
          | Some (Cbr (_, ibr, ebr)) -> (
            rem_pred bb.bbid ibr.bbid;
            rem_pred bb.bbid ebr.bbid;
            add_pred pbb.bbid ibr.bbid;
            add_pred pbb.bbid ebr.bbid;
            Queue.push ibr.bbid wl;
            Queue.push ebr.bbid wl
          )
          | _ -> ()
        )
        | _ -> () (*single pred is cbr*)
      )
      | _ -> () (* no compact opportunity*)
    );
    done

let compactcfg_opt (b : builder) : unit =
  FuncMap.iter (fun _fid fn -> 
    match fn.extern_name with
    | Some _ -> ()
    | None -> compactcfg_opt_func b fn
  ) b.program.funcs