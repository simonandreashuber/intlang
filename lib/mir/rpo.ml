open Mir

let compute_rpo (fn : func) =
  let visited = Hashtbl.create (BBMap.cardinal fn.bbs) in
  let rpo = ref [] in

  let rec dfs bbid =
    if not (Hashtbl.mem visited bbid) then begin
      Hashtbl.add visited bbid ();
      match BBMap.find_opt bbid fn.bbs with
      | Some bb ->
          (* 1. Extract successors *)
          let succs = match bb.term with
            | Some (Br (brbbid, _)) -> [brbbid]
            | Some (Cbr (_, br_t, br_f)) -> [br_f; br_t]
            | Some (Ret _) | None -> []
          in
          (* 2. Visit children (Post-Order step) *)
          List.iter dfs succs;
          (* 3. Prepend to list (implicitly Reverses the Post-Order) *)
          rpo := bb.bbid :: !rpo
      | None -> ()
    end;
  in

  (match fn.entry_bb with
  | Some entry_bb -> dfs entry_bb
  | None -> failwith ("compute_rpo: function " ^ fn.name ^ " has no entry basic block"));
  let rpo_idx = Array.make fn.next_bbid (-1) in
  List.iteri (fun idx bbid -> rpo_idx.(bbid) <- idx) !rpo;
  fn.rpo <- Some { rpo_lst = !rpo; rpo_idx = rpo_idx }

let get_rpo_info fn =
  if fn.rpo = None then compute_rpo fn;
  Option.get fn.rpo