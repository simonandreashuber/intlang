open Mir
open Buildmir

(* Stores the raw definition payload of a closure SSA ID *)
type raw_def =
  | DefFunc of funcid
  | DefPack of ssaconsume * ssaconsume list (* oldclos, new_args *)

type closure_info = {
  base_func : funcid;
  captured_args : ssaconsume list;
}

let calldirect_opt_func (fn : func) : unit =

  fn.analysis <- None;  (* Clear any existing analysis *)

  let defs : (ssaid, raw_def) Hashtbl.t = Hashtbl.create 32 in

  (* -------------------------------------------------------------------- *)
  (* Pass 1: Record all closure definitions across ALL basic blocks       *)
  (* -------------------------------------------------------------------- *)
  BBMap.iter (fun _ bb ->
    List.iter (function
      | Func (dst, fid) ->
          Hashtbl.add defs dst (DefFunc fid)
      | Pack (dst, oldclos, newargs) ->
          Hashtbl.add defs dst (DefPack (oldclos, newargs))
      | _ -> ()
    ) bb.ops
  ) fn.bbs;

  (* -------------------------------------------------------------------- *)
  (* Lazy Recursive Resolver                                              *)
  (* -------------------------------------------------------------------- *)
  let rec resolve (id : ssaid) : closure_info option =
    match Hashtbl.find_opt defs id with
    | Some (DefFunc fid) ->
        Some { base_func = fid; captured_args = [] }

    | Some (DefPack (oldclos, newargs)) ->
        (match resolve oldclos.ssaid with
         | Some info ->
             Some { 
               base_func = info.base_func; 
               captured_args = info.captured_args @ newargs 
             }
         | None -> None)

    (* If an SSA ID is not in [defs], it comes from a BB argument,        *)
    (* function argument, or unknown source. Return None safely.          *)
    | None -> None
  in

  (* -------------------------------------------------------------------- *)
  (* Pass 2: Devirtualize callclosure instructions                       *)
  (* -------------------------------------------------------------------- *)
  BBMap.iter (fun _ bb ->
    let chron_ops = List.rev bb.ops in
    let new_ops_rev = List.fold_left (fun acc op ->
      match op with
      | CallClosure (dst, clos) ->
          (match resolve clos.ssaid with
           | Some info ->
               CallDirect (dst, info.base_func, info.captured_args) :: acc
           | None -> op :: acc)
      | _ -> op :: acc
    ) [] chron_ops in
    bb.ops <- new_ops_rev
  ) fn.bbs


let calldirect_opt (b : builder) : unit =
  FuncMap.iter (fun _fid fn -> 
    match fn.extern_name with
    | Some _ -> ()
    | None -> calldirect_opt_func fn
  ) b.program.funcs