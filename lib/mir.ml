open Errors

type ssaid = int
type bbid = int
type funcid = int


type typmir =
  | TMIRUnit
  | TMIRI32
  | TMIRI8
  | TMIRFun of (typmir list) * typmir
  | TMIRTup of typmir list
  | TMIRVec of int * vecinnertype (*int is dimension *)

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
    | Func of ssaid * typmir * funcid                          (* %res = func %funcid                                 ALLOCATES CLOSURE MEMORY            *)
    | Pack of ssaid * typmir * ssaid * (ssaid list)            (* %newfun = pack %oldfun %packval0 %packval1 ...      COPIES CLOSURE MEMORY BY DEFAULT    *)
    | CallClosure of ssaid * typmir * ssaid                    (* %res = callclosure %fun                             CALLS FUNCTION VIA CLOSURE WRAPPER  *)
    | CallDirect of ssaid * typmir * funcid * (ssaid list)     (* %res = calldirect %funcid %arg0 %arg1 ...                                               *)
    | Immi32 of ssaid * typmir * Int32.t                       (* %res = immi32 1234                                                                      *)
    | Immi8 of ssaid * typmir * char                           (* %res = immi8 123                                                                        *)
    | ImmUnit of ssaid * typmir                                (* %res = immunit                                                                          *)
    | Uopi32 of ssaid * typmir * uopi32arg * ssaid             (* %res = uopi32 arg %a                                                                    *)
    | Uopi8 of ssaid * typmir * uopi8arg * ssaid               (* %res = uopi8 arg %a                                                                     *)    
    | Bopi32 of ssaid * typmir * bopi32arg * ssaid * ssaid     (* %res = bopi32 arg %a %b                                                                 *)
    | Bopi8 of ssaid * typmir * bopi8arg * ssaid * ssaid       (* %res = bopi8 arg %a %b                                                                  *)
    | Tupinit of ssaid * typmir * (ssaid list)                 (* %res = tupinit %elm0 %elm1 ...                      ALLOCATES TUPLE MEMORY              *)
    | Tupget of ssaid * typmir * ssaid * int                   (* %res = tupget %tup idx                                                                  *)
    | Veclit of ssaid * typmir * (ssaid list)                  (* %res = veclit %elm0 %elm1 ...                       ALLOCATES VECTOR MEMORY             *)
    | Vecinit of ssaid * typmir * ssaid * (ssaid list)         (* %res = vecinit %defval %dim0sz1 ...                 ALLOCATES VECTOR MEMORY             *)
    | Vecread of ssaid * typmir * ssaid * (ssaid list)         (* %res = vecread %vec %idx0 ...                                                           *)
    | Vecwrite of ssaid * typmir * ssaid * (ssaid list)        (* %res = vecwrite %vec %val %idx0 ...                 COPIES VECTOR MEMORY BY DEFAULT     *)
    | Vecslice of ssaid * typmir * ssaid * ssaid * ssaid       (* %res = vecslice %vec %start %len                                                        *)
    | Vecextend of ssaid * typmir * ssaid * ssaid * ssaid              (* %res = vecextend %vec %lit %off                     COPIES VECTOR MEMORY                *)

let get_typmir (op : op) : typmir =
  match op with
  | Func (_, typ, _) -> typ
  | Pack (_, typ, _) -> typ
  | CallDirect (_, typ, _, _) -> typ
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
  | Vecread (_, typ, _, _) -> typ
  | Vecwrite (_, typ, _, _) -> typ
  | Vecslice (_, typ, _, _, _) -> typ
  | Vecextend (_, typ, _, _) -> typ

let get_ssaid (op : op) : ssaid =
  match op with
  | Func (ssaid, _, _) -> ssaid
  | Pack (ssaid, _, _) -> ssaid
  | CallDirect (ssaid, _, _, _) -> ssaid
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
  | Vecread (ssaid, _, _, _) -> ssaid
  | Vecwrite (ssaid, _, _, _) -> ssaid
  | Vecslice (ssaid, _, _, _, _) -> ssaid
  | Vecextend (ssaid, _, _, _) -> ssaid

type branch = int * (ssaid list)  (* target bbid, args *)

type term =
    | Br of branch
    | Cbr of ssaid * branch * branch
    | Ret of ssaid

type bb = {
    bbid: int;
    name: string;
    mutable args: (ssaid * typmir) list;
    mutable ops: op list;
    mutable term: term option;
}

type func = {
    funcid: funcid;
    name: string;
    rettyp: typmir;
    args: (ssaid * typmir) list;
    mutable bbs: bb list;
}

type program = func list

(* ========================================================================= *)
(* Builder State Context                                                     *)
(* ========================================================================= *)

type builder = {
  mutable program : program;
  mutable current_func : func option;
  mutable current_bb : bb option;
  mutable next_funcid : int;
  mutable next_bbid : int;
  mutable next_ssaid : int;
}

let create_builder () : builder = {
  program = [];
  current_func = None;
  current_bb = None;
  next_funcid = 0;
  next_bbid = 0;
  next_ssaid = 0;
}

let get_program (b : builder) : program =
  b.program

(* ========================================================================= *)
(* Function & Basic Block Cursors                                            *)
(* ========================================================================= *)

(*at times I would find it convinient to be able to bild in a sort of cached funtion iteratively adding arguments
  and then in the end finalize it, but also works without*)
let create_func (b : builder) (name_opt : string option) (rettyp : typmir) (args : (ssaid * typmir) list) : func =
  let fid = b.next_funcid in
  b.next_funcid <- b.next_funcid + 1;
  let name = match name_opt with | Some n -> n | None -> "anon_func_" ^ string_of_int fid in
  let fn = { funcid = fid; name; rettyp; args; bbs = [] } in
  b.program <- b.program @ [fn];
  fn

let switch_func (b : builder) (target_fn : func) : unit =
  b.current_func <- Some target_fn;
  b.current_bb <- None

let create_bb (b : builder) (name : string) (args : (ssaid * typmir) list) : bb =
  match b.current_func with
  | None -> failwith "Builder Error: Cannot create basic block without an active function!"
  | Some fn ->
      let id = b.next_bbid in
      b.next_bbid <- b.next_bbid + 1;
      let new_bb = { bbid = id; name; args; ops = []; term = None } in
      fn.bbs <- fn.bbs @ [new_bb];
      new_bb

let switch_bb (b : builder) (target_bb : bb) : unit =
  b.current_bb <- Some target_bb

(* ========================================================================= *)
(* Emitting Instructions & Terminators                                       *)
(* ========================================================================= *)

let emit_op (b : builder) (op : op) : unit =
  match b.current_bb with
  | None -> failwith "Builder Error: Cannot emit op without an active basic block!"
  | Some bb -> bb.ops <- bb.ops @ [op]

let emit_term (b : builder) (term : term) : unit =
  match b.current_bb with
  | None -> failwith "Builder Error: Cannot emit terminator without an active basic block!"
  | Some bb ->
      match bb.term with
      | Some _ -> failwith (Printf.sprintf "Builder Error: Basic block '%s' already has a terminator!" bb.name)
      | None -> bb.term <- Some term

(* ========================================================================= *)
(* Generating SSA IDs                                                        *)
(* ========================================================================= *)

let fresh_ssaid (b : builder) : ssaid =
  let id = b.next_ssaid in
  b.next_ssaid <- b.next_ssaid + 1;
  id

let find_mirtyp_opt (b : builder) (ssaid : ssaid) : typmir option =
  match b.current_func with
  | None -> None
  | Some fn ->
      let rec find_in_bbs bbs =
        match bbs with
        | [] -> None
        | bb :: rest ->
            let found = List.find_opt (fun op -> get_ssaid op = ssaid) in
            match found with
            | Some op -> Some (get_typmir op)
            | None -> find_in_bbs rest
      in
      find_in_bbs fn.bbs

let find_mirtyp (b : builder) (ssaid : ssaid) : typmir =
  match find_mirtyp_opt b ssaid with
  | Some typ -> typ
  | None -> raise (Errors.LowerMonoTASTError (Printf.sprintf "SSA ID %d not found in current function" ssaid))

let find_func_by_id (b : builder) (fid : funcid) : func option =
  List.find_opt (fun fn -> fn.funcid = fid) b.program

let func_aryness (b : builder) (fid : funcid) : int option =
  match find_func_by_id b fid with
  | None -> None
  | Some fn -> Some (List.length fn.args)

let func_get_mirtyp (b : builder) (fid : funcid) : typmir =
  match find_func_by_id b fid with
  | None -> raise (Errors.LowerMonoTASTError (Printf.sprintf "Function with id %d not found" fid))
  | Some fn -> TMIRFun (List.map snd fn.args, fn.rettyp)


type cursor_checkpoint = func option * bb option

let funcbb_checkpoint (b : builder) : cursor_checkpoint =
  (b.current_func, b.current_bb)

let funcbb_restore (b : builder) (cp : cursor_checkpoint) : unit =
  let (f, bb) = cp in
  b.current_func <- f;
  b.current_bb <- bb