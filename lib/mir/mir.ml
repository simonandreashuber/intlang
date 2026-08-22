open Errors

type ssaid = int
type bbid = int
type funcid = int
type globalid = int

(* ========================================================================= *)
(* MIR Types                                                                 *)
(* ========================================================================= *)

type mirtyp =
  | TMIRUnit                              (* unit type used to make the mir more uniform gets "compiled away" in the backend *)
  | TMIRI32                               (* 32 bit integer type *)
  | TMIRI8                                (* 8 bit integer type *)
  | TMIRClos of (mirtyp list) * mirtyp    (* closure type, list of argument types and return type *)
  | TMIRTup of mirtyp list                (* tuple type, list of element types *)
  | TMIRVec of int * vecinnertype         (* vector type, dimension and inner type *)

and vecinnertype = | TMIRVECI32 | TMIRVECI8

(* 
  Borrowed: no gc obligation, no consumption allowed
  Owned: gc obligation, consumption allowed
  NoMem: mirtyp is TMIRUnit or TMIRI32 or TMIRI8, this ssa value does not represent a memory object
*)
type ownership = | Borrowed | Owned | NoMem

(* ========================================================================= *)
(* MIR Operations and Terminators                                            *)
(* ========================================================================= *)

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

(* Use of a ssa value where the ssa value can be consumed
   If such a use is not consumed, some less optimal behavior will occur
   most of the time a copy is made *)
type ssaconsume = {
  ssaid: ssaid;
  mutable consume: bool;
}

let ssac = fun ssaid -> { ssaid; consume = false }

type op =                                                           (* Textual Representation                                                                           *)
    | Func of ssaid * funcid                                        (* %res         = func @funcid                                                                      *)
    | Pack of ssaid *  ssaconsume * (ssaconsume list)               (* %res         = pack %oldclos %arg0 %arg1 ...                                                     *)
    | CallClosure of ssaid * ssaconsume                             (* %res         = callclosure %clos                                                                 *)
    | CallDirect of ssaid * funcid * (ssaconsume list)              (* %res         = calldirect @funcid %arg0 %arg1 ...                                                *)
    | GarbageCollect of ssaid list                                  (*                garbagecollect %mem0 %mem1 ...                                                    *)
    | StoreGlobal of globalid * ssaconsume                          (*                storeglobal @gid %val                                                             *)
    | LoadGlobal of ssaid * globalid                                (* %res         = loadglobal @gid                                                                   *)
    | Immi32 of ssaid * Int32.t                                     (* %res         = immi32 1234                                                                       *)
    | Immi8 of ssaid * char                                         (* %res         = immi8 123                                                                         *)
    | ImmUnit of ssaid                                              (* %res         = immunit                                                                           *)
    | Uopi32 of ssaid * uopi32arg * ssaid                           (* %res         = uopi32 arg %a                                                                     *)
    | Uopi8 of ssaid * uopi8arg * ssaid                             (* %res         = uopi8 arg %a                                                                      *)
    | Bopi32 of ssaid * bopi32arg * ssaid * ssaid                   (* %res         = bopi32 arg %a %b                                                                  *)
    | Bopi8 of ssaid * bopi8arg * ssaid * ssaid                     (* %res         = bopi8 arg %a %b                                                                   *)
    | Tupinit of ssaid * (ssaconsume list)                          (* %res         = tupinit %elm0 %elm1 ...                                                           *)
    | Tupextract of (ssaid list) * ssaconsume                       (* (%elm0, ...) = tupextract %tup                                                                   *)
    | Tupview of (ssaid list) * ssaid                               (* (%elm0, ...) = tupview %tup                                                                      *)
    | Veclit of ssaid * (ssaconsume list)                           (* %res         = veclit %elm0 %elm1 ...                                                            *)
    | Vecinit of ssaid * ssaid * (ssaid list)                       (* %res         = vecinit %defval %dim0sz1 ...                                                      *)
    | Veclen of ssaid * ssaid                                       (* %res         = veclen %vec                                                                       *)
    | Vecread of ssaid * ssaid * (ssaid list)                       (* %res         = vecread %vec %idx0 ...                                                            *)
    | Vecwrite of ssaid * ssaconsume  * ssaid * (ssaid list)        (* %res         = vecwrite %vec %val %idx0 ...                                                      *)
    | Vecinsert of ssaid * ssaconsume * ssaconsume * (ssaid list)   (* %res         = vecinsert %vec %vecins %idx0 ...                                                  *)
    | Vecslice of ssaid * ssaid * ssaid * ssaid                     (* %res         = vecslice %vec %start %len                                                         *)
    | Vecextend of ssaid * ssaid * ssaid * ssaid                    (* %res         = vecextend %vec %lit %off                                                          *)

type branch = {
  bbid: bbid;
  args: (ssaconsume list);
}

let brac = fun bbid args -> { bbid; args }

type term =
    | Br of branch
    | Cbr of ssaid * branch * branch
    | Ret of ssaconsume

(* ========================================================================= *)
(* MIR Control Flow Graph                                                    *)
(* ========================================================================= *)

type bb = {
    bbid: int;                    (* unique identifier of basic block *)
    mutable name: string;         (* only debug info *)
    mutable args: ssaid list;     (* mutable for TCO *)
    mutable ops: op list;         (* rev order, to make building faster*)
    mutable term: term option;    
}
module BBMap = Map.Make(Int)

(* ========================================================================= *)
(* MIR Analysis                                                              *)
(* ========================================================================= *)

module SsaSet = Set.Make(Int)

type preds_info = {
  preds: bbid list array; (* preds[bbid] = list of predecessor bbids *)
}

type rpo_info = {
  rpo_lst: bbid list;      (* reverse post order of basic blocks *)
  rpo_idx: int array;    (* rpo_index[bbid] = index of bbid in rpo *)
}

type live_info = {
  live_in  : SsaSet.t array;
  live_out : SsaSet.t array;
}

type borrow_info = {
  (* Given ssaid, find all DIRECT borrowers *)
  lender_to_borrowers : ssaid list array;
  (* Given ssaid, find all DIRECT lenders *)
  borrower_to_lenders : ssaid list array;
}

type dom_info = {
  (* idom[bbid] = immediate parent in dominator tree *)
  idom : bbid option array;
}

(* ========================================================================= *)
(* MIR Function                                                              *)
(* ========================================================================= *)

type func = {
    funcid: funcid;                                       (* unique identifier of func *)
    name: string;                                         (* only debug info *)
    mutable args: ( ssaid * (string option) ) list;       (* string only debug info, mutable for TCO *)
    rettyp: mirtyp;                                       
    extern_name: string option;                           (* if Some externalname then bbs is ignored and a extern function gets linked *)
    mutable next_ssaid: ssaid;                            (* lowest unused ssaid, should always be in sync with the length of ssatyps and memown *)
    mutable next_bbid: bbid;                              (* lowest unused bbid *)
    mutable entry_bb: bbid option;                        (* entry basic block id *)
    mutable bbs: bb BBMap.t;                              
    ssatyps: mirtyp Dynarray.t;                           (* stores mirtypes of all ssa values *)
    memown: ownership Dynarray.t;                         (* stores ownership information of all ssa values *)
    (* Analysis information *)
    mutable preds       : preds_info option;
    mutable rpo         : rpo_info option; 
    mutable live        : live_info option;
    mutable borrow      : borrow_info option;
    mutable dom         : dom_info option;
}

module FuncMap = Map.Make(Int)

(* ========================================================================= *)
(* MIR Globals                                                               *)
(* ========================================================================= *)

type global = {
    globalid: globalid;
    typ: mirtyp;
}
module GlobalMap = Map.Make(Int)

(* ========================================================================= *)
(* MIR Program                                                               *)
(* ========================================================================= *)

type program = {
  mutable globals : global GlobalMap.t;
  mutable funcs   : func FuncMap.t;
  mutable init_globals_funcid : funcid option;
  mutable main_funcid    : funcid option;
  mutable uninit_globals_funcid : funcid option;
}

(* ========================================================================= *)
(* MIR Helpers (Finding Things)                                              *)
(* ========================================================================= *)

let get_mirtyp_func (func : func) (ssaid : ssaid) : mirtyp =
  if ssaid < 0 || ssaid >= func.next_ssaid then
    raise (Errors.MirError (Printf.sprintf "SSA ID %d is out of bounds for function %s" ssaid func.name));
  Dynarray.get func.ssatyps ssaid

let get_ownership_func (func : func) (ssaid : ssaid) : ownership =
  if ssaid < 0 || ssaid >= func.next_ssaid then
    raise (Errors.MirError (Printf.sprintf "SSA ID %d is out of bounds for function %s" ssaid func.name));
  Dynarray.get func.memown ssaid

let invalidate_all_analysis (func : func) : unit =
  func.preds <- None;
  func.rpo <- None;
  func.live <- None;
  func.borrow <- None;
  func.dom <- None

let is_memtyp (typ : mirtyp) : bool =
  match typ with
  | TMIRUnit -> false
  | TMIRI32 -> false
  | TMIRI8 -> false
  | _ -> true