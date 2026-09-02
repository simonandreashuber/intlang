open Mir
open Analysis (*process bbs in rpo *)

open Llvm
open Llvm_target
open Llvm_executionengine
open Errors

(*
  TODO:
    - impl lower mir op
    - copy, init, drop codegen
    - uninit globals in mir impl (just load global and then drop if mem object)
*)

(* ========================================================================= *)
(* Program Context                                                           *)
(* ========================================================================= *)

type llfunc_info = {
  mir_funcid : Mir.funcid;
  func_t : lltype;
  func : llvalue;
  mutable closwrpr :  llvalue option;
}

type clos_helper_info = {
  signature : mirtyp list;
  copy_func : llvalue;
  drop_func : llvalue;
}

type proggen_ctx = {
  llcontext : llcontext;
  llmodule  : llmodule;

  void_t : lltype;
  unit_t : lltype;
  i1_t  : lltype;
  i8_t  : lltype;
  i32_t  : lltype;
  i64_t : lltype;
  vec_t : lltype;
  clos_t : lltype;
  ptr_t : lltype;

  malloc_t : lltype;
  malloc_func : llvalue;
  free_t : lltype;
  free_func : llvalue;
  memcpy_t : lltype;
  memcpy_func : llvalue;

  globals_env : (globalid, llvalue) Hashtbl.t;                    (* mir globalid -> llvm glob *)
  func_env : (funcid, llfunc_info) Hashtbl.t;                     (* mir func -> llvm func *)
  closhelper_env : (mirtyp list, clos_helper_info) Hashtbl.t;     (* closure data layout *)

  miranalysis : analysis_info;
}

let ctx_add_llfunc_info (ctx : proggen_ctx) (funcid : funcid) (llfunc_info : llfunc_info) =
  Hashtbl.add ctx.func_env funcid llfunc_info

let find_llfunc_info (ctx : proggen_ctx) (funcid : funcid) : llfunc_info =
  try Hashtbl.find ctx.func_env funcid
  with Not_found -> raise (LlvmgenError ("find_llfunc: function not found in env: " ^ string_of_int funcid))
  




(* ========================================================================= *)
(* Function Context                                                          *)
(* ========================================================================= *)

type fgen_ctx = {
  proggen_ctx : proggen_ctx;
  builder : llbuilder;
  mirfunc : func;
  ssa_env : Llvm.llvalue option array;
  bb_env  : (bbid, Llvm.llbasicblock) Hashtbl.t;
  llfunc_info : llfunc_info;
}

let create_fgen_ctx (ctx : proggen_ctx) (mirfunc : Mir.func) =
  let builder = builder ctx.llcontext in
  let ssa_env = Array.make mirfunc.next_ssaid None in
  let bb_env = Hashtbl.create (BBMap.cardinal mirfunc.bbs) in
  let llfunc_info = find_llfunc_info ctx mirfunc.funcid in
  { proggen_ctx = ctx; builder; mirfunc; ssa_env; bb_env ; llfunc_info }

let get_llfunc (fgen_ctx : fgen_ctx) : llvalue =
  fgen_ctx.llfunc_info.func

let get_llbb (fgen_ctx : fgen_ctx) (bbid : bbid) : llbasicblock =
  try Hashtbl.find fgen_ctx.bb_env bbid
  with Not_found -> raise (LlvmgenError ("get_llbb: bb not found in env: " ^ string_of_int bbid))

let get_llssa (fgen_ctx : fgen_ctx) (ssaid : ssaid) : llvalue =
  match fgen_ctx.ssa_env.(ssaid) with
  | Some llval -> llval
  | None -> raise (LlvmgenError ("get_llssa: ssa not found in env: " ^ string_of_int ssaid))

let set_llssa (fgen_ctx : fgen_ctx) (ssaid : ssaid) (llval : llvalue) : unit =
  if Option.is_some fgen_ctx.ssa_env.(ssaid) then
    raise (LlvmgenError ("set_llssa: ssa already set in env: " ^ string_of_int ssaid))
  else
  fgen_ctx.ssa_env.(ssaid) <- Some llval



  
(* ========================================================================= *)
(* Mir Types to Llvm Types                                                   *)
(* ========================================================================= *)

let rec mirtyp_get_lltyp (ctx : proggen_ctx) (mirtyp : mirtyp) : lltype =
  match mirtyp with
  | TMIRUnit -> ctx.unit_t
  | TMIRI32 -> ctx.i32_t
  | TMIRI8 -> ctx.i8_t
  | TMIRClos _ -> ctx.clos_t
  | TMIRVec _ -> ctx.vec_t
  | TMIRTup elms -> (
    let elms_lltype = List.map (mirtyp_get_lltyp ctx) elms in
    Llvm.struct_type ctx.llcontext (Array.of_list elms_lltype)
  )

let mirtyplst_get_lltyparr (ctx : proggen_ctx) (mirtyplst : mirtyp list) : lltype array =
  Array.of_list (List.map (mirtyp_get_lltyp ctx) mirtyplst)




(* ========================================================================= *)
(* Declare Globals and Functions                                             *)
(* ========================================================================= *)

let decl_global (ctx : proggen_ctx) (glob : Mir.global) : unit =
  let glob_lltyp = mirtyp_get_lltyp ctx glob.typ in
  let llglobal = declare_global glob_lltyp (string_of_int glob.globalid) ctx.llmodule in
  Hashtbl.add ctx.globals_env glob.globalid llglobal


let decl_func (ctx : proggen_ctx) (mirfunc : Mir.func) : unit =
  let args_mirtyps = List.map (fun (ssaid, _) -> get_mirtyp_func mirfunc ssaid) mirfunc.args in
  let ret_mirtyp = mirfunc.rettyp in
  let args_lltyps = mirtyplst_get_lltyparr ctx args_mirtyps in
  let ret_lltyp = mirtyp_get_lltyp ctx ret_mirtyp in
  let llfunc_t = function_type ret_lltyp args_lltyps in
  let llfunc = declare_function (string_of_int mirfunc.funcid) llfunc_t ctx.llmodule in
  ctx_add_llfunc_info ctx mirfunc.funcid { mir_funcid = mirfunc.funcid; func_t = llfunc_t; func = llfunc; closwrpr = None; }





(* ========================================================================= *)
(* Copy, Drop and Init Helpers                                               *)
(* ========================================================================= *)

(*
  gen_loop generates the following llvm code
  current_bb:
    ...
    branch header_bb

  header_bb:
    idx = phi (0, current_bb) (next_idx, body_bb')
    cond = cmp lt idx upper_bound
    cond_branch cond body_bb exit_bb

  body_bb:
    effects of (gen_body idx)

  body_bb':
    ...
    next_idx = add idx 1
    branch header_bb
  
  exit_bb:
*)
let gen_loop (ctx : proggen_ctx) (builder : llbuilder) (llfunc : llvalue) (gen_body : llvalue -> unit) (upper_bound : llvalue) : unit =

  let current_bb = insertion_block builder in
  let header_bb = append_block ctx.llcontext "gen_loop_header_bb" llfunc in
  let body_bb = append_block ctx.llcontext "gen_loop_body_bb" llfunc in

  (*branch to loop header*)
  ignore (build_br header_bb builder);

  (*loop header*)
  position_at_end header_bb builder;
  let phi_index = build_phi [(Llvm.const_int ctx.i64_t 0, current_bb)] "gen_loop_index" builder in
  let cond = build_icmp Icmp.Slt phi_index upper_bound "loop_gen_cond" builder in

  (*loop body*)
  position_at_end body_bb builder;
  gen_body phi_index;
  (*note these follwing instructions migth be in a bb that is different 
    from body_bb since gen_body can create new bbs*)
  let body_bb' = insertion_block builder in
  let next_index = build_add phi_index (Llvm.const_int ctx.i64_t 1) "next_index" builder in
  ignore (add_incoming (next_index, body_bb') phi_index);
  ignore (build_br header_bb builder);

  (*loop exit*)
  (* append exit_block here to make the bb ordering nicer *)
  let exit_bb = append_block ctx.llcontext "gen_loop_exit_bb" llfunc in 
  position_at_end header_bb builder;
  ignore (build_cond_br cond body_bb exit_bb builder);
  position_at_end exit_bb builder;
  ()


let rec copy_vec (ctx : proggen_ctx) (builder : llbuilder) (llfunc : llvalue) (mirtyp : mirtyp) ( origvec : llvalue) : llvalue =
  match mirtyp with
  | TMIRUnit | TMIRI32 | TMIRI8 | TMIRClos _ | TMIRTup _ -> raise (LlvmgenError "copy_vec non vec mirtyp passed")
  | TMIRVec (0, _) -> raise (LlvmgenError "copy_vec vec with dim 0")
  | TMIRVec (1, inner_mirtyp) -> (
    let origvec_ptr = Llvm.build_extractvalue origvec 0 "vecptr" builder in
    let origvec_len = Llvm.build_extractvalue origvec 1 "veclen" builder in
    let len_i64 = Llvm.build_zext origvec_len ctx.i64_t "len_i64" builder in
    let bytesz = 
      match inner_mirtyp with
      | TMIRVECI32 -> (
        let i32_bytesz = Llvm.const_int ctx.i64_t 4 in 
        build_mul len_i64 i32_bytesz "bytesz" builder )
      | TMIRVECI8 -> len_i64
    in

    (*malloc new vec memory*)
    let copyvec_ptr = build_call ctx.malloc_t ctx.malloc_func [| bytesz |] "copyvec_ptr" builder in
    
    (*do memcpy*)
    let is_volatile = Llvm.const_int ctx.i1_t 0 in
    ignore (build_call ctx.memcpy_t ctx.memcpy_func [| copyvec_ptr; origvec_ptr; bytesz; is_volatile |] "" builder);

    (*build new vec struct*)
    const_struct ctx.llcontext [| copyvec_ptr; origvec_len |]
  )
  | TMIRVec (n, inner_mirtyp) -> (
    let origvec_ptr = Llvm.build_extractvalue origvec 0 "vecptr" builder in
    let origvec_len = Llvm.build_extractvalue origvec 1 "veclen" builder in
    let len_i64 = Llvm.build_zext origvec_len ctx.i64_t "len_i64" builder in
    let vec_bytesz = Llvm.size_of ctx.vec_t in
    let bytesz = build_mul len_i64 vec_bytesz "bytesz" builder in

    (*malloc new vec memory*)
    let copyvec_ptr = build_call ctx.malloc_t ctx.malloc_func [| bytesz |] "copyvec_ptr" builder in

    let copy_elm (idx : llvalue) : unit =
      let origvec_elm_ptr = build_in_bounds_gep ctx.vec_t origvec_ptr [| idx |] "origvec_elm_ptr" builder in
      let orig_elm = build_load ctx.vec_t origvec_elm_ptr "elm" builder in
      let copy_elm = copy_vec ctx builder llfunc (TMIRVec (n-1, inner_mirtyp)) orig_elm in
      let copyvec_elm_ptr = build_in_bounds_gep ctx.vec_t copyvec_ptr [| idx |] "copyvec_elm_ptr" builder in
      ignore (build_store copy_elm copyvec_elm_ptr builder);
    in
    gen_loop ctx builder llfunc copy_elm len_i64;

    const_struct ctx.llcontext [| copyvec_ptr; origvec_len |]
  )

let copy_clos (ctx : proggen_ctx) (builder : llbuilder) (llfunc : llvalue) (mirtyp : mirtyp) ( origclos : llvalue) : llvalue =
  match mirtyp with
  | TMIRUnit | TMIRI32 | TMIRI8 | TMIRVec _ | TMIRTup _ -> raise (LlvmgenError "copy_clos non vec mirtyp passed")
  | TMIRClos _ -> (
    let borr_llfunc = build_extractvalue origclos 0 "clos_borr_fptr" builder in
    let own_llfunc = build_extractvalue origclos 1 "clos_own_fptr" builder in
    let copy_llfunc = build_extractvalue origclos 2 "clos_copy_fptr" builder in
    let drop_llfunc = build_extractvalue origclos 3 "clos_drop_fptr" builder in
    let data_ptr = build_extractvalue origclos 4 "clos_data_ptr" builder in
    let off = build_extractvalue origclos 5 "clos_off" builder in
    let copy_func_t = Llvm.function_type (ctx.ptr_t) [| ctx.ptr_t; ctx.i64_t |] in
    let datacopy_ptr = build_call copy_func_t copy_llfunc [| data_ptr; off |] "clos_copy" builder in
    Llvm.const_struct ctx.llcontext [| borr_llfunc; own_llfunc; copy_llfunc; drop_llfunc; datacopy_ptr; off |]
  )

let rec copy_tup (ctx : proggen_ctx) (builder : llbuilder) (llfunc : llvalue) (mirtyp : mirtyp) ( origtup : llvalue) : llvalue =
  match mirtyp with
  | TMIRUnit | TMIRI32 | TMIRI8 | TMIRClos _ | TMIRVec _ -> raise (LlvmgenError "copy_tup non vec mirtyp passed")
  | TMIRTup elms -> (
    let copy_elms = List.mapi (fun i elm_mirtyp ->
      let orig_elm = build_extractvalue origtup i ("tup_elm_" ^ string_of_int i) builder in
      match elm_mirtyp with
      | TMIRUnit | TMIRI32 | TMIRI8 -> orig_elm
      | TMIRTup _ -> copy_tup ctx builder llfunc elm_mirtyp orig_elm
      | TMIRClos _ -> copy_clos ctx builder llfunc elm_mirtyp orig_elm
      | TMIRVec _ -> copy_vec ctx builder llfunc elm_mirtyp orig_elm
    ) elms in
    Llvm.const_struct ctx.llcontext (Array.of_list copy_elms)
  )

let copy (ctx : proggen_ctx) (builder : llbuilder) (llfunc : llvalue) (mirtyp : mirtyp) ( origval : llvalue) : llvalue =
  match mirtyp with
  | TMIRUnit | TMIRI32 | TMIRI8 -> origval
  | TMIRTup _ -> copy_tup ctx builder llfunc mirtyp origval
  | TMIRClos _ -> copy_clos ctx builder llfunc mirtyp origval
  | TMIRVec _ -> copy_vec ctx builder llfunc mirtyp origval


let rec drop_vec (ctx : proggen_ctx) (builder : llbuilder) (llfunc : llvalue) (mirtyp : mirtyp) ( origvec : llvalue) : unit =
  match mirtyp with
  | TMIRUnit | TMIRI32 | TMIRI8 | TMIRClos _ | TMIRTup _ -> raise (LlvmgenError "drop_vec non vec mirtyp passed")
  | TMIRVec (0, _) -> raise (LlvmgenError "drop_vec vec with dim 0")
  | TMIRVec (1, inner_mirtyp) -> (
    let vec_ptr = Llvm.build_extractvalue origvec 0 "vecptr" builder in
    let vec_len = Llvm.build_extractvalue origvec 1 "veclen" builder in

    let free_bb = append_block ctx.llcontext "drop_vec_free_bb" llfunc in
    let merge_bb = append_block ctx.llcontext "drop_vec_merge_bb" llfunc in

    let cond = build_icmp Icmp.Ne vec_len ((Llvm.const_int ctx.i32_t 0)) "drop_vec_len0_cond" builder in
    ignore (build_cond_br cond free_bb merge_bb builder);

    position_at_end free_bb builder;
    ignore (build_call ctx.free_t ctx.free_func [| vec_ptr |] "" builder);
    ignore (build_br merge_bb builder);

    position_at_end merge_bb builder 
  )
  | TMIRVec (n, inner_mirtyp) -> (
    let vec_ptr = Llvm.build_extractvalue origvec 0 "vecptr" builder in
    let vec_len = Llvm.build_extractvalue origvec 1 "veclen" builder in
    let len_i64 = Llvm.build_zext vec_len ctx.i64_t "len_i64" builder in

    let copy_elm (idx : llvalue) : unit =
      let vec_elm_ptr = build_in_bounds_gep ctx.vec_t vec_ptr [| idx |] "drop_vec_elm_ptr" builder in
      let elm = build_load ctx.vec_t vec_elm_ptr "drop_vec_elm" builder in
      drop_vec ctx builder llfunc (TMIRVec (n-1, inner_mirtyp)) elm
    in
    gen_loop ctx builder llfunc copy_elm len_i64
  )

let drop_clos (ctx : proggen_ctx) (builder : llbuilder) (llfunc : llvalue) (mirtyp : mirtyp) ( clos : llvalue) : unit =
  match mirtyp with
  | TMIRUnit | TMIRI32 | TMIRI8 | TMIRVec _ | TMIRTup _ -> raise (LlvmgenError "drop_clos non vec mirtyp passed")
  | TMIRClos _ -> (
    let drop_llfunc = build_extractvalue clos 3 "clos_drop_fptr" builder in
    let data_ptr = build_extractvalue clos 4 "clos_data_ptr" builder in
    let off = build_extractvalue clos 5 "clos_off" builder in
    let drop_func_t = Llvm.function_type (ctx.void_t) [| ctx.ptr_t; ctx.i64_t |] in
    ignore (build_call drop_func_t drop_llfunc [| data_ptr; off |] "clos_drop" builder)
  )

let rec drop_tup (ctx : proggen_ctx) (builder : llbuilder) (llfunc : llvalue) (mirtyp : mirtyp) ( tup : llvalue) : unit =
  match mirtyp with
  | TMIRUnit | TMIRI32 | TMIRI8 | TMIRClos _ | TMIRVec _ -> raise (LlvmgenError "drop_tup non vec mirtyp passed")
  | TMIRTup elms -> (
    List.iteri (fun i elm_mirtyp ->
      let elm = build_extractvalue tup i ("tup_elm_" ^ string_of_int i) builder in
      match elm_mirtyp with
      | TMIRUnit | TMIRI32 | TMIRI8 -> ()
      | TMIRTup _ -> drop_tup ctx builder llfunc elm_mirtyp elm
      | TMIRClos _ -> drop_clos ctx builder llfunc elm_mirtyp elm
      | TMIRVec _ -> drop_vec ctx builder llfunc elm_mirtyp elm
    ) elms
  )

let drop (ctx : proggen_ctx) (builder : llbuilder) (llfunc : llvalue) (mirtyp : mirtyp) ( llval : llvalue) : unit =
  match mirtyp with
  | TMIRUnit | TMIRI32 | TMIRI8 -> ()
  | TMIRTup _ -> drop_tup ctx builder llfunc mirtyp llval
  | TMIRClos _ -> drop_clos ctx builder llfunc mirtyp llval
  | TMIRVec _ -> drop_vec ctx builder llfunc mirtyp llval



(* ========================================================================= *)
(* Create Closures                                                           *)
(* ========================================================================= *)

let get_clos_layout (ctx : proggen_ctx) (args_lltyps : lltype array) : int array * int =

  (*
    simulates the offsets that the pack op is going to use
  *)

  let lltarget_layout = Llvm_target.DataLayout.of_string (Llvm.data_layout ctx.llmodule) in
  let curr_off = ref 0 in (*current offset in bytes*)

  Array.map (fun arg_lltyp ->
    let arg_size = Int64.to_int @@ Llvm_target.DataLayout.abi_size arg_lltyp lltarget_layout in
    let arg_align = Llvm_target.DataLayout.abi_align arg_lltyp lltarget_layout in
    let off = (!curr_off + arg_align - 1) land (lnot (arg_align - 1)) in
    curr_off := off + arg_size;
    off
  ) args_lltyps, !curr_off


let get_clos_wrapper (ctx : proggen_ctx) (mirfuncid : funcid) : llvalue =
  let llfunc_info = find_llfunc_info ctx mirfuncid in
  match llfunc_info.closwrpr with
  | Some closwrpr -> closwrpr
  | None -> (
    let closwrpr_func_t = function_type (return_type llfunc_info.func_t) [| ctx.clos_t |] in
    let closwrpr_func = declare_function "closwrpr_func" closwrpr_func_t ctx.llmodule in
    let bb = append_block ctx.llcontext "entry" closwrpr_func in
    let builder = builder ctx.llcontext in
    position_at_end bb builder;
    let clos_dataptr = param closwrpr_func 0 in
    let args_lltyps = param_types llfunc_info.func_t in
    let clos_data_offsets, _ = get_clos_layout ctx args_lltyps in
    let args_llvalues = 
      Array.init (Array.length args_lltyps) (fun i ->
      let argptr = build_gep ctx.i8_t clos_dataptr [| const_int ctx.i32_t clos_data_offsets.(i) |] ("argptr_" ^ string_of_int i) builder in
      build_load args_lltyps.(i) argptr ("arg_" ^ string_of_int i) builder ) 
    in
    let ret_val = build_call llfunc_info.func_t llfunc_info.func args_llvalues "ret_val" builder in
    ignore (build_ret ret_val builder);
    llfunc_info.closwrpr <- Some closwrpr_func;
    closwrpr_func
  )

let get_clos_helpers (ctx : proggen_ctx) (args_mirtyp : mirtyp list) : clos_helper_info =
    (*
    gen_ladder creates the following llvm code

    bbcurr:
      ...
      branch bb0

    bb0:
      cond0 = i64cmp gt off 0
      cond_branch cond0 bb1 bbend

    bb1:
      effects of (fgen builder 0)

    bb1':                                 if the effects do create new bbs this happens
      ...
      cond1 = i64cmp gt off 1
      cond_branch cond1 bb2 bbend

    ...

    bbmaxoff:
      effects of (fgen builder (maxoff-1))
    
    bbmaxoff':
      ...
      branch bbend

    bbend:
  *)
  let gen_ladder (builder : llbuilder) (llfunc : llvalue) (fgen : llbuilder -> int -> unit) (off : llvalue) (maxoff : int) : unit =
    assert (maxoff > 0); (* empty closures should not exist *)

    (* Phase 1: create bbs and call fgen *)
    let bbcurr = insertion_block builder in

    let bb0 = append_block ctx.llcontext "bb0" llfunc in

    let i = ref 1 in
    let bblst = ref [[bb0]] in
    while !i <= maxoff do
      let bbi = append_block ctx.llcontext ("bb" ^ string_of_int !i) llfunc in
      position_at_end bbi builder;
      fgen builder (!i - 1);
      (*fgen can put the curser in a new bb*)
      let bbi' = insertion_block builder in
      i := !i + 1;
      bblst := [bbi; bbi'] :: !bblst;
    done;

    let bbend = append_block ctx.llcontext "bbend" llfunc in

    (* Phase 2: add the terminators *)
    let rec pair_bbs (flatbblst : llbasicblock list) (acc : (llbasicblock * llbasicblock) list) : ((llbasicblock * llbasicblock) list * llbasicblock) =
      match flatbblst with
      | [] -> raise (LlvmgenError "pair_bbs: empty bblst (internal should not happen)")
      | [h] -> List.rev acc, h
      | h1 :: h2 :: tl -> pair_bbs tl ((h1, h2) :: acc)
    in
    let bbpairs, bbmaxoff = pair_bbs (List.flatten @@ List.rev !bblst) [] in

    position_at_end bbcurr builder;
    ignore (build_br bb0 builder);

    List.iteri (fun i (bb, next_bb) ->
      position_at_end bb builder;
      let cond = build_icmp Icmp.Sgt off (const_int ctx.i64_t i) ("cond_" ^ string_of_int i) builder in
      ignore (build_cond_br cond next_bb bbend builder)
    ) bbpairs;

    position_at_end bbmaxoff builder;
    ignore (build_br bbend builder);

    position_at_end bbend builder;
  in

  match Hashtbl.find_opt ctx.closhelper_env args_mirtyp with
  | Some clos_helpers -> clos_helpers
  | None -> (

    (*layout*)
    let args_mirtyp_arr = Array.of_list args_mirtyp in
    let args_lltype_arr = Array.map (mirtyp_get_lltyp ctx) args_mirtyp_arr in
    let clos_data_offsets, clos_data_size = get_clos_layout ctx args_lltype_arr in


    (*COPY FUNC*)
    (*declare copy func*)
    let copy_func_t = function_type ctx.ptr_t [| ctx.ptr_t ; ctx.i64_t |] in
    let copy_func = declare_function "clos_copy_func" copy_func_t ctx.llmodule in
    let builder = Llvm.builder ctx.llcontext in
    let entry_bb = append_block ctx.llcontext "entry" copy_func in
    position_at_end entry_bb builder;
    let clos_data_ptr = param copy_func 0 in
    let off = param copy_func 1 in

    (*alloc new data memory*)
    let clos_data_copy_ptr = build_call ctx.malloc_t ctx.malloc_func [| const_int ctx.i64_t clos_data_size |] "clos_data_copy_ptr" builder in

    (*copy closure contents*)
    let copy_arg_gen (builder : llbuilder) (i : int) =
      let arg_orig_ptr = build_gep ctx.i8_t clos_data_ptr [| const_int ctx.i32_t clos_data_offsets.(i) |] ("argptr_" ^ string_of_int i) builder in
      let arg_orig_val = build_load args_lltype_arr.(i) arg_orig_ptr ("arg_" ^ string_of_int i) builder in
      let arg_copy_val = copy ctx builder copy_func args_mirtyp_arr.(i) arg_orig_val in
      let arg_copy_ptr = build_gep ctx.i8_t clos_data_copy_ptr [| const_int ctx.i32_t clos_data_offsets.(i) |] ("argcopyptr_" ^ string_of_int i) builder in
      ignore (build_store arg_copy_val arg_copy_ptr builder)
    in
    gen_ladder builder copy_func copy_arg_gen off (List.length args_mirtyp);

    ignore (build_ret clos_data_copy_ptr builder);

    (*DROP FUNC*)
    (*declare drop func*)
    let drop_func_t = function_type ctx.ptr_t [| ctx.ptr_t ; ctx.i64_t |] in
    let drop_func = declare_function "clos_drop_func" drop_func_t ctx.llmodule in
    let builder = Llvm.builder ctx.llcontext in
    let entry_bb = append_block ctx.llcontext "entry" drop_func in
    position_at_end entry_bb builder;
    let clos_data_ptr = param drop_func 0 in
    let off = param drop_func 1 in

    (*drop closure contents*)
    let drop_arg_gen (builder : llbuilder) (i : int) =
      let arg_ptr = build_gep ctx.i8_t clos_data_ptr [| const_int ctx.i32_t clos_data_offsets.(i) |] ("argptr_" ^ string_of_int i) builder in
      let arg_val = build_load args_lltype_arr.(i) arg_ptr ("arg_" ^ string_of_int i) builder in
      drop ctx builder drop_func args_mirtyp_arr.(i) arg_val 
    in
    gen_ladder builder drop_func drop_arg_gen off (List.length args_mirtyp);

    (*free closure data memory*)
    ignore (build_call ctx.free_t ctx.free_func [| clos_data_ptr |] "" builder);

    ignore (build_ret_void builder);

    {
      signature = args_mirtyp;
      copy_func;
      drop_func;
    }
  )




(* ========================================================================= *)
(* Lower Mir                                                                 *)
(* ========================================================================= *)

let consume_or_copy (fgen_ctx : fgen_ctx) (sc : ssaconsume) : llvalue =
  let orig_llvalue = get_llssa fgen_ctx sc.ssaid in
  if sc.consume then 
    orig_llvalue 
  else
    let mirtyp = get_mirtyp_func fgen_ctx.mirfunc sc.ssaid in 
    copy fgen_ctx.proggen_ctx fgen_ctx.builder fgen_ctx.llfunc_info.func mirtyp orig_llvalue

let lower_op (fgen_ctx : fgen_ctx) (mirop : Mir.op) : unit =
  let ctx = fgen_ctx.proggen_ctx in
  let mirfunc = fgen_ctx.mirfunc in
  let builder = fgen_ctx.builder in
  match mirop with
  | Func (def_ssaid, borr_funcid_ref, own_funcid_opt_ref) -> (
    
    (*gen on demand / get clos wrappers for funcs*)
    let borr_closwrpr = get_clos_wrapper ctx !borr_funcid_ref in
    if Option.is_none !own_funcid_opt_ref then raise (LlvmgenError "lower_op: own funcid not implemented yet");
    let own_closwrpr = get_clos_wrapper ctx (Option.get !own_funcid_opt_ref) in

    (*layout*)
    let clos_mirtyp = get_mirtyp_func mirfunc def_ssaid in
    let args_mirtyp = 
      match clos_mirtyp with
      | TMIRClos (args, ret) -> args
      | _ -> raise (LlvmgenError "mir ssa def has non clos type after func op")
    in
    let args_lltype_arr = Array.of_list @@ List.map (mirtyp_get_lltyp ctx) args_mirtyp in
    let _, clos_data_size = get_clos_layout ctx args_lltype_arr in

    (*gen copy and drop helpers *)
    let clos_helpers = get_clos_helpers ctx args_mirtyp in

    (*alloc data memory*)
    let clos_data_ptr = build_call ctx.malloc_t ctx.malloc_func [| const_int ctx.i64_t clos_data_size |] "clos_data_ptr" fgen_ctx.builder in

    (* assemble closure *)
    let clos_val = Llvm.const_struct ctx.llcontext [| borr_closwrpr; own_closwrpr; clos_helpers.copy_func; clos_helpers.drop_func; clos_data_ptr; const_int ctx.i64_t 0 |] in
    set_llssa fgen_ctx def_ssaid clos_val
  )
  | Pack (def_ssaid, clos_consume, args_consume) -> (
      let clos_llval = consume_or_copy fgen_ctx clos_consume in
      let args_llval = List.map (consume_or_copy fgen_ctx) args_consume in
      let args_lltyp = List.map (fun sc -> mirtyp_get_lltyp ctx @@ get_mirtyp_func mirfunc sc.ssaid) args_consume in
      let lltarget_layout = Llvm_target.DataLayout.of_string (Llvm.data_layout ctx.llmodule) in

      let borr_llfunc = build_extractvalue clos_llval 0 "clos_borr_fptr" builder in
      let own_llfunc = build_extractvalue clos_llval 1 "clos_own_fptr" builder in
      let copy_llfunc = build_extractvalue clos_llval 2 "clos_copy_fptr" builder in
      let drop_llfunc = build_extractvalue clos_llval 3 "clos_drop_fptr" builder in
      let clos_data_ptr = build_extractvalue clos_llval 4 "clos_data_ptr" fgen_ctx.builder in
      let clos_data_off_ref = ref (build_extractvalue clos_llval 5 "clos_data_off" fgen_ctx.builder) in

      List.iter2 (fun arg_llval arg_lltyp ->
        let arg_align = Llvm_target.DataLayout.abi_align arg_lltyp lltarget_layout in
        let off_aligned_intermediate = build_add !clos_data_off_ref (const_int ctx.i64_t (arg_align - 1)) "off_aligned_intermediate" fgen_ctx.builder in
        let off_aligned_mask = build_not (const_int ctx.i64_t (arg_align - 1)) "off_aligned_mask" fgen_ctx.builder in
        let off_aligned = build_and off_aligned_intermediate off_aligned_mask "off_aligned" fgen_ctx.builder in
        let arg_ptr = build_gep ctx.i8_t clos_data_ptr [| off_aligned |] "argptr" fgen_ctx.builder in
        ignore (build_store arg_llval arg_ptr fgen_ctx.builder);
        let arg_size = Int64.to_int (Llvm_target.DataLayout.abi_size arg_lltyp lltarget_layout) in
        let off_aligned_plus_argsize = build_add off_aligned (const_int ctx.i64_t arg_size) "off_plus_argsize" fgen_ctx.builder in
        clos_data_off_ref := off_aligned_plus_argsize
      ) args_llval args_lltyp;

      (* assemble closure *)
      let clos_val = Llvm.const_struct ctx.llcontext [| borr_llfunc; own_llfunc; copy_llfunc; drop_llfunc; clos_data_ptr; !clos_data_off_ref |] in
      set_llssa fgen_ctx def_ssaid clos_val
  )
  | _ -> raise (LlvmgenError "lower_op: not implemented yet")

let lower_func (ctx : proggen_ctx) (mirfunc : Mir.func) : unit =
  let fgen_ctx = create_fgen_ctx ctx mirfunc in
  let llfunc_info = fgen_ctx.llfunc_info in
  let llfunc = llfunc_info.func in 

  (*create the entry bb*)
  let entrybb = append_block ctx.llcontext "entry" llfunc in

  (*create all llbbs and lower their ops*)
  let rpo_info = get_rpo_info ctx.miranalysis mirfunc in  
  List.iter (fun bbid ->
    let mirbb = BBMap.find bbid mirfunc.bbs in
    let llbb = append_block ctx.llcontext (string_of_int bbid) llfunc in

    (*add to fgen_ctx*)
    Hashtbl.add fgen_ctx.bb_env bbid llbb;
    
    (*phi node for all bb args*)
    List.iter (fun ssaid ->
      let mirtyp = get_mirtyp_func mirfunc ssaid in
      let lltyp = mirtyp_get_lltyp ctx mirtyp in
      let phi_node = build_empty_phi lltyp (string_of_int ssaid) fgen_ctx.builder in
      set_llssa fgen_ctx ssaid phi_node
    ) mirbb.args;

    (*lower all ops*)
    List.iter (fun mirop ->
      ignore (lower_op fgen_ctx mirop)
    ) (List.rev mirbb.ops);    

  ) rpo_info.rpo_lst;

  (*patch bb branching*)
  BBMap.iter (fun _ mirbb ->
    let llbb = get_llbb fgen_ctx mirbb.bbid in
    position_at_end llbb fgen_ctx.builder;
    match mirbb.term with
    | None -> raise (LlvmgenError "No term in llvmgen lower_func")
    | Some (Br (target_bbid, mir_brargs)) -> (
      (*put br*)
      let target_llbb = get_llbb fgen_ctx target_bbid in
      ignore (build_br target_llbb fgen_ctx.builder);

      (*patch phi nodes*)
      let target_mirbb = find_bb_func mirfunc target_bbid in
      List.iter2 (fun mir_brarg mir_bbarg ->
        let phi_node = get_llssa fgen_ctx mir_bbarg in
        let passed_llval = get_llssa fgen_ctx mir_brarg.ssaid in
        add_incoming (passed_llval, llbb) phi_node
      ) mir_brargs target_mirbb.args 
    )
    | Some (Cbr (cond_ssaid, true_bbid, false_bbid)) -> (
      (* transfor i32 cond into bool cond *)
      let true_llbb = get_llbb fgen_ctx true_bbid in
      let false_llbb = get_llbb fgen_ctx false_bbid in
      let cond_llvalue = get_llssa fgen_ctx cond_ssaid in
      let zero = const_int (ctx.i32_t) 0 in
      let cond_bool = build_icmp Icmp.Ne cond_llvalue zero "cond_bool" fgen_ctx.builder in

      (*put cbr*)
      ignore (build_cond_br cond_bool true_llbb false_llbb fgen_ctx.builder)
    )
    | Some (Ret res_ssaid) -> (
      let res_llvalue = get_llssa fgen_ctx res_ssaid in
      ignore (build_ret res_llvalue fgen_ctx.builder)
    )
  ) mirfunc.bbs;

  (*ll entry bb direct branch to lowered mir entry bb*)
  position_at_end entrybb fgen_ctx.builder;
  match mirfunc.entry_bb with
  | None -> raise (LlvmgenError "No mir entry bb in llvmgen lower_func")
  | Some mirentrybbid -> (
      let mirentry_llbb = get_llbb fgen_ctx mirentrybbid in
      ignore (build_br mirentry_llbb fgen_ctx.builder)
  )

let lower_mir ( p : Mir.program) : llmodule =

  (* Wake up the native code generator *)
  ignore (Llvm_executionengine.initialize ());

  let llcontext = global_context () in
  let llmodule = create_module llcontext "intlang_module" in

  (* Get the host target triple *)
  let triple = Target.default_triple () (* or specify a hardcoded triple string like "x86_64-pc-linux-gnu" *) in
  set_target_triple triple llmodule;

  (* Lookup the target and get its DataLayout *)
  let target = Target.by_triple triple in
  let target_machine = TargetMachine.create ~triple target in
  let data_layout = TargetMachine.data_layout target_machine in
  
  (* Embed the data layout string into the module *)
  set_data_layout (DataLayout.as_string data_layout) llmodule;

  let void_t = void_type llcontext in
  let unit_t = Llvm.struct_type llcontext [||] in
  let i1_t = i1_type llcontext in
  let i8_t = i8_type llcontext in
  let i32_t = i32_type llcontext in
  let i64_t = i64_type llcontext in
  let ptr_t    = Llvm.pointer_type llcontext in
  (* data ptr, len *)
  let vec_t = Llvm.struct_type llcontext [| ptr_t; i32_t |] in
  (* argsborrowed fptr, argsowned fptr, copy fptr, drop fptr, data ptr, data off *)
  let clos_t = Llvm.struct_type llcontext [| ptr_t; ptr_t; ptr_t; ptr_t; ptr_t; i64_t |] in

  let malloc_t   = Llvm.function_type (ptr_t) [| i64_t |] in
  let malloc_func = Llvm.declare_function "malloc" malloc_t llmodule in
  let free_t = Llvm.function_type (void_t) [| ptr_t |] in
  let free_func = Llvm.declare_function "free" free_t llmodule in
  let memcpy_t = Llvm.function_type (void_t) [| ptr_t; ptr_t; i64_t; i1_t |] in
  let memcpy_func = Llvm.declare_function "llvm.memcpy.p0.p0.i64" memcpy_t llmodule in

  let globals_env = Hashtbl.create 32 in
  let func_env = Hashtbl.create 32 in
  let closhelper_env = Hashtbl.create 32 in

  let miranalysis = create_analysis_info () in

  let ctx = {
    llcontext;
    llmodule;
    void_t;
    unit_t;
    i1_t;
    i8_t;
    i32_t;
    i64_t;
    ptr_t;
    vec_t;
    clos_t;
    malloc_t;
    malloc_func;
    free_t;
    globals_env;
    free_func;
    memcpy_t;
    memcpy_func;
    func_env;
    closhelper_env;
    miranalysis;
  } in

  (* Iterate all MIR globals and declare empty llvm equivalents *)
  GlobalMap.iter (fun _ glob -> 
    decl_global ctx glob
  ) p.globals;

  (* Iterate all MIR functions and declare empty llvm equivalents *)
  FuncMap.iter (fun _  func -> 
    decl_func ctx func
  ) p.funcs;

  (* Second pass over MIR functions this time the function bodies are lowered *)
  FuncMap.iter (fun _ func ->
    lower_func ctx func
  ) p.funcs;

  (*call init globals, main and uninit globals in a single new main function*)
  let main_type = function_type i32_t [||] in
  let main_fn = declare_function "main" main_type llmodule in
  let bb = append_block ctx.llcontext "entry" main_fn in
  let main_builder = builder llcontext in
  position_at_end bb main_builder;
  
  let call_unitfunc_opt unitfunc_opt = (
    match unitfunc_opt with
    | Some funcid -> (
      let llfunc_info = find_llfunc_info ctx funcid in
      let llunit = const_struct ctx.llcontext [| |] in
      ignore (build_call (llfunc_info.func_t) llfunc_info.func [| llunit |] "" main_builder)
    )
    | None -> ()
  ) in
  
  call_unitfunc_opt p.init_globals_funcid;
  call_unitfunc_opt p.main_funcid;
  call_unitfunc_opt p.uninit_globals_funcid;

  ignore (build_ret (const_int ctx.i32_t 0) main_builder);
  llmodule


(* ========================================================================= *)
(* Compile Llvm                                                              *)
(* ========================================================================= *)

let llvm_to_bin_clang (llmod : llmodule) (binary_name : string) : int =

  (*
    For now this approach is the simplest as clang will nicely do all the linking :)
  *)

  let llvm_ir = string_of_llmodule llmod in
  let ir_filename = binary_name ^ ".ll" in
  
  let oc = open_out ir_filename in
  output_string oc llvm_ir;
  close_out oc;
  
  let cmd = Printf.sprintf "clang-19 -Wno-override-module %s -o %s" ir_filename binary_name in
  let exit_code = Sys.command cmd in
  Sys.remove ir_filename;
  exit_code