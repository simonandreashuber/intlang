open Errors

(* ========================================================================= *)
(* MIR Def                                                                   *)
(* ========================================================================= *)

type ssaid = int
type bbid = int
type funcid = int
type globalid = int


type mirtyp =
  | TMIRUnit                              (* unit type used to make the mir more uniform gets "compiled away" in the backend *)
  | TMIRI32                               (* 32 bit integer type *)
  | TMIRI8                                (* 8 bit integer type *)
  | TMIRClos of (mirtyp list) * mirtyp    (* closure type, list of argument types and return type *)
  | TMIRTup of mirtyp list                (* tuple type, list of element types *)
  | TMIRVec of int * vecinnertype         (* vector type, dimension and inner type *)

and vecinnertype = | TMIRVECI32 | TMIRVECI8


type uopi32arg = 
    | Negi32 | Noti32

type bopi32arg = 
    | Eqi32 | Neqi32 | Lti32 | Gti32 | LtEqi32 | GtEqi32
    | ULti32 | UGti32 | ULtEqi32 | UGtEqi32
    | Muli32 | Subi32 | Addi32 | Divi32 | Modi32
    | UDivi32 | UModi32
    | Andi32 | Ori32 | Xori32
    | Shli32 | Shri32 | UShri32

type uopi8arg = 
    | Negi8 | Noti8

type bopi8arg = 
    | Eqi8 | Neqi8 | Lti8 | Gti8 | LtEqi8 | GtEqi8
    | Addi8 | Subi8
    | Andi8 | Ori8 | Xori8

type op =                                                      (* Textual Representation                              Performance Implications            *)
    | Func of ssaid * mirtyp * funcid                          (* %res = func @funcid                                 ALLOCATES CLOSURE MEMORY            *)
    | Pack of ssaid * mirtyp * ssaid * (ssaid list)            (* %res = pack %oldclos %arg0 %arg1 ...                COPIES CLOSURE MEMORY BY DEFAULT    *)
    | CallClosure of ssaid * mirtyp * ssaid                    (* %res = callclosure %clos                            CALLS FUNCTION VIA CLOSURE WRAPPER  *)
    | CallDirect of ssaid * mirtyp * funcid * (ssaid list)     (* %res = calldirect @funcid %arg0 %arg1 ...                                               *)
    | StoreGlobal of ssaid * mirtyp * globalid * ssaid         (* %res = storeglobal @gid %val                                                            *)
    | LoadGlobal of ssaid * mirtyp * globalid                  (* %res = loadglobal @gid                                                                  *)
    | Immi32 of ssaid * mirtyp * Int32.t                       (* %res = immi32 1234                                                                      *)
    | Immi8 of ssaid * mirtyp * char                           (* %res = immi8 123                                                                        *)
    | ImmUnit of ssaid * mirtyp                                (* %res = immunit                                                                          *)
    | Uopi32 of ssaid * mirtyp * uopi32arg * ssaid             (* %res = uopi32 arg %a                                                                    *)
    | Uopi8 of ssaid * mirtyp * uopi8arg * ssaid               (* %res = uopi8 arg %a                                                                     *)    
    | Bopi32 of ssaid * mirtyp * bopi32arg * ssaid * ssaid     (* %res = bopi32 arg %a %b                                                                 *)
    | Bopi8 of ssaid * mirtyp * bopi8arg * ssaid * ssaid       (* %res = bopi8 arg %a %b                                                                  *)
    | Tupinit of ssaid * mirtyp * (ssaid list)                 (* %res = tupinit %elm0 %elm1 ...                      ALLOCATES TUPLE MEMORY              *)
    | Tupget of ssaid * mirtyp * ssaid * int                   (* %res = tupget %tup idx                                                                  *)
    | Veclit of ssaid * mirtyp * (ssaid list)                  (* %res = veclit %elm0 %elm1 ...                       ALLOCATES VECTOR MEMORY             *)
    | Vecinit of ssaid * mirtyp * ssaid * (ssaid list)         (* %res = vecinit %defval %dim0sz1 ...                 ALLOCATES VECTOR MEMORY             *)
    | Veclen of ssaid * mirtyp * ssaid                         (* %res = veclen %vec                                                                      *)
    | Vecread of ssaid * mirtyp * ssaid * (ssaid list)         (* %res = vecread %vec %idx0 ...                                                           *)
    | Vecwrite of ssaid * mirtyp * ssaid  * ssaid * (ssaid list)        (* %res = vecwrite %vec %val %idx0 ...                 COPIES VECTOR MEMORY BY DEFAULT     *)
    | Vecslice of ssaid * mirtyp * ssaid * ssaid * ssaid       (* %res = vecslice %vec %start %len                                                        *)
    | Vecextend of ssaid * mirtyp * ssaid * ssaid * ssaid      (* %res = vecextend %vec %lit %off                     COPIES VECTOR MEMORY                *)

type branch = bbid * (ssaid list)  (* target bbid, args *)

type term =
    | Br of branch
    | Cbr of ssaid * branch * branch
    | Ret of ssaid

type bb = {
    bbid: int;
    name: string;
    args: (ssaid * mirtyp) list;
    mutable ops: op list; (* rev order !!!*)
    mutable term: term option;
}

type func = {
    funcid: funcid;
    name: string;
    args: (ssaid * (string option) * mirtyp) list;
    rettyp: mirtyp;
    extern_name: string option;
    mutable next_ssaid: ssaid;
    mutable next_bbid: bbid;
    mutable bbs: bb list;
}

type global = {
    globalid: globalid;
    typ: mirtyp;
}

module FuncMap = Map.Make(Int)
module GlobalMap = Map.Make(Int)

type program = {
  mutable globals : global GlobalMap.t;
  mutable funcs   : func FuncMap.t;
  mutable init_globals_funcid : funcid option;
  mutable main_funcid    : funcid option;
  mutable uninit_globals_funcid : funcid option;
}

(* ========================================================================= *)
(* MIR Helpers                                                               *)
(* ========================================================================= *)

let get_mirtyp (op : op) : mirtyp =
  match op with
  | Func (_, typ, _) -> typ
  | Pack (_, typ, _, _) -> typ
  | CallClosure (_, typ, _) -> typ
  | CallDirect (_, typ, _, _) -> typ
  | StoreGlobal (_, typ, _, _) -> typ
  | LoadGlobal (_, typ, _) -> typ
  | Uopi32 (_, typ, _, _) -> typ
  | Bopi32 (_, typ, _, _, _) -> typ
  | Uopi8 (_, typ, _, _) -> typ
  | Bopi8 (_, typ, _, _, _) -> typ
  | Immi32 (_, typ, _) -> typ
  | Immi8 (_, typ, _) -> typ
  | ImmUnit (_, typ) -> typ
  | Tupinit (_, typ, _) -> typ
  | Tupget (_, typ, _, _) -> typ
  | Vecinit (_, typ, _, _) -> typ
  | Veclit (_, typ, _) -> typ
  | Veclen (_, typ, _) -> typ
  | Vecread (_, typ, _, _) -> typ
  | Vecwrite (_, typ, _, _, _) -> typ
  | Vecslice (_, typ, _, _, _) -> typ
  | Vecextend (_, typ, _, _, _) -> typ

let get_ssaid (op : op) : ssaid =
  match op with
  | Func (ssaid, _, _) -> ssaid
  | Pack (ssaid, _, _, _) -> ssaid
  | CallClosure (ssaid, _, _) -> ssaid
  | CallDirect (ssaid, _, _, _) -> ssaid
  | StoreGlobal (ssaid, _, _, _) -> ssaid
  | LoadGlobal (ssaid, _, _) -> ssaid
  | Uopi32 (ssaid, _, _, _) -> ssaid
  | Bopi32 (ssaid, _, _, _, _) -> ssaid
  | Uopi8 (ssaid, _, _, _) -> ssaid
  | Bopi8 (ssaid, _, _, _, _) -> ssaid
  | Immi32 (ssaid, _, _) -> ssaid
  | Immi8 (ssaid, _, _) -> ssaid
  | ImmUnit (ssaid, _) -> ssaid
  | Tupinit (ssaid, _, _) -> ssaid
  | Tupget (ssaid, _, _, _) -> ssaid
  | Vecinit (ssaid, _, _, _) -> ssaid
  | Veclit (ssaid, _, _) -> ssaid
  | Veclen (ssaid, _, _) -> ssaid
  | Vecread (ssaid, _, _, _) -> ssaid
  | Vecwrite (ssaid, _, _, _, _) -> ssaid
  | Vecslice (ssaid, _, _, _, _) -> ssaid
  | Vecextend (ssaid, _, _, _, _) -> ssaid

(* ========================================================================= *)
(* Builder State Context                                                     *)
(* ========================================================================= *)

type cursor = func option * bb option

type builder = {
  program : program;
  mutable next_funcid : int;
  mutable next_globalid : int;
  mutable cursor : cursor;
}

let create_builder () : builder = {
  program = { 
        globals = GlobalMap.empty; 
        funcs = FuncMap.empty; 
        init_globals_funcid = None; 
        main_funcid = None; 
        uninit_globals_funcid = None };
  next_funcid = 0;
  next_globalid = 0;
  cursor = (None, None);
  }

let get_program (b : builder) : program =
  b.program

(* ========================================================================= *)
(* Cursor Checkpoints                                                        *)
(* ========================================================================= *)

let cp_get (b : builder) : cursor =
  b.cursor

let cp_ret (b : builder) (cp : cursor) : unit =
  b.cursor <- cp

(* ========================================================================= *)
(* Function & Basic Block Cursors                                            *)
(* ========================================================================= *)

let create_func (b : builder) 
                (name : string)
                (args : (ssaid * (string option) * mirtyp) list)
                (rettyp : mirtyp)
                (extern_name : string option)
                : func =
  let fid = b.next_funcid in
  b.next_funcid <- b.next_funcid + 1;
  let max_ssaid = List.fold_left (fun acc (arg_ssaid, _, _) -> max acc arg_ssaid) 0 args in
  let fn = {funcid = fid;
            name = name;
            args = args;
            rettyp = rettyp;
            extern_name = extern_name;
            next_ssaid = max_ssaid + 1;
            next_bbid = 0;
            bbs = [] } in
  let p = b.program in
  p.funcs <- FuncMap.add fid fn p.funcs;
  fn

let switch_func (b : builder) (target_fn : func) : unit =
  b.cursor <- (Some target_fn, None)

let create_bb (b : builder) 
              (name : string) 
              (args : (ssaid * mirtyp) list) : bb =
  match b.cursor with
  | (None, _) -> failwith "Builder Error: Cannot create basic block without an active function!"
  | (Some fn, _) ->
      let id = fn.next_bbid in
      fn.next_bbid <- fn.next_bbid + 1;
      let new_bb = { bbid = id; 
                     name; 
                     args; 
                     ops = []; 
                     term = None } in
      fn.bbs <- fn.bbs @ [new_bb];
      new_bb

let switch_bb (b : builder) (target_bb : bb) : unit =
  (*does not check if the bb is in the function !!!!*)
  match b.cursor with
  | (None, _) -> failwith "Builder Error: Cannot switch basic block without an active function!"
  | (Some fn, _) -> b.cursor <- (Some fn, Some target_bb)
  

let create_global (b : builder) (typ : mirtyp) : global =
  let globalid = b.next_globalid in
  b.next_globalid <- b.next_globalid + 1;
  let global = { globalid; typ } in
  b.program.globals <- GlobalMap.add globalid global b.program.globals;
  global

(* ========================================================================= *)
(* Emitting Instructions & Terminators                                       *)
(* ========================================================================= *)

let emit_op (b : builder) (op : op) : unit =
  match b.cursor with
  | (_, None) -> failwith "Builder Error: Cannot emit op without an active basic block!"
  | (_, Some bb) -> bb.ops <- op :: bb.ops

let emit_term (b : builder) (term : term) : unit =
  match b.cursor with
  | (_, None) -> failwith "Builder Error: Cannot emit terminator without an active basic block!"
  | (_, Some bb) ->
      match bb.term with
      | Some _ -> failwith (Printf.sprintf "Builder Error: Basic block '%s' already has a terminator!" bb.name)
      | None -> bb.term <- Some term

(* ========================================================================= *)
(* Generating SSA IDs                                                        *)
(* ========================================================================= *)

let fresh_ssaid (b : builder) : ssaid =
  match b.cursor with
  | (None, _) -> failwith "Builder Error: Cannot generate SSA ID without an active function!"
  | (Some fn, _) ->
      let id = fn.next_ssaid in
      fn.next_ssaid <- fn.next_ssaid + 1;
      id

(* ========================================================================= *)
(* Finding Things                                                            *)
(* ========================================================================= *)

let find_func_opt (b : builder) (fid : funcid) : func option =
  FuncMap.find_opt fid b.program.funcs

let find_func (b : builder) (fid : funcid) : func =
  match find_func_opt b fid with
  | Some fn -> fn
  | None -> raise (Errors.MirError (Printf.sprintf "Function with id %d not found" fid))

let find_global_opt (b : builder) (gid : globalid) : global option =
  GlobalMap.find_opt gid b.program.globals

let find_global (b : builder) (gid : globalid) : global =
  match find_global_opt b gid with
  | Some g -> g
  | None -> raise (Errors.MirError (Printf.sprintf "Global with id %d not found" gid))


let find_ssa_mirtyp_opt (b : builder) (ssaid : ssaid) : mirtyp option =
  (*idea for speedup: in emit_op, create_func and create_bb build an ssaid -> op cache to use here*)
  match b.cursor with
  | (None, _) -> None
  | (Some fn, _) ->
    match List.find_opt (fun (arg_ssaid, _, _) -> arg_ssaid = ssaid) fn.args with
    | Some (_, _, mirtyp) -> Some mirtyp
    | None -> (
      let rec find_in_bbs (bbs : bb list) : mirtyp option =
        match bbs with
        | [] -> None
        | bb :: rest ->
            let found_args = List.find_opt (fun (arg_ssaid, _) -> arg_ssaid = ssaid) bb.args in
            let found_ops = List.find_opt (fun op -> get_ssaid op = ssaid) bb.ops in
            match found_args, found_ops with
            | Some (_, typ), _ -> Some typ
            | _, Some op -> Some (get_mirtyp op)
            | None, None -> find_in_bbs rest
      in
      find_in_bbs fn.bbs
    )

let find_ssa_mirtyp (b : builder) (ssaid : ssaid) : mirtyp =
  match find_ssa_mirtyp_opt b ssaid with
  | Some typ -> typ
  | None -> raise (Errors.MirError (Printf.sprintf "SSA ID %d not found in current function" ssaid))

