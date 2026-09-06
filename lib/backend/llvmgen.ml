(*

  Lowers MIR to LLVM IR

  The MIR should have drop and all needed copies for terminators (memopt)

*)

open Mir
open Analysis (*process bbs in rpo *)

open Llvm
open Llvm_target
open Llvm_executionengine
open Errors

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
  lldata_layout : DataLayout.t;

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
  trap_t : lltype;
  trap_func : llvalue;
  getchar_t : lltype;
  getchar_func : llvalue;
  putchar_t : lltype;
  putchar_func : llvalue;
  fflush_t : lltype;
  fflush_func : llvalue;

  globals_env : (globalid, (mirtyp * llvalue)) Hashtbl.t;                    (* mir globalid -> llvm glob *)
  func_env : (funcid, llfunc_info) Hashtbl.t;                     (* mir func -> llvm func *)
  closhelper_env : (mirtyp list, clos_helper_info) Hashtbl.t;     (* closure data layout *)

  miranalysis : analysis_info;
}

let ctx_add_llfunc_info (ctx : proggen_ctx) (funcid : funcid) (llfunc_info : llfunc_info) =
  Hashtbl.add ctx.func_env funcid llfunc_info

let find_llfunc_info (ctx : proggen_ctx) (funcid : funcid) : llfunc_info =
  try Hashtbl.find ctx.func_env funcid
  with Not_found -> raise (LlvmgenError ("find_llfunc: function not found in env: " ^ string_of_int funcid))
  
let find_global (ctx : proggen_ctx) (globalid : globalid) : mirtyp * llvalue =
  let global_mirtyp, global_llval = try Hashtbl.find ctx.globals_env globalid
  with Not_found -> raise (LlvmgenError ("find_global: global not found in env: " ^ string_of_int globalid)) in
  (global_mirtyp, global_llval)




(* ========================================================================= *)
(* Function Context                                                          *)
(* ========================================================================= *)

type fgen_ctx = {
  proggen_ctx : proggen_ctx;
  builder : llbuilder;
  mirfunc : func;
  ssa_env : Llvm.llvalue option array;
  bb_start_env  : (bbid, Llvm.llbasicblock) Hashtbl.t; (* mir bb -> llvm bb where the lowering starts *)
  bb_end_env  : (bbid, Llvm.llbasicblock) Hashtbl.t;   (* mir bb -> llvm bb where the lowering ends *)
  llfunc_info : llfunc_info;
}

let create_fgen_ctx (ctx : proggen_ctx) (mirfunc : Mir.func) =
  let builder = builder ctx.llcontext in
  let ssa_env = Array.make mirfunc.next_ssaid None in
  let bb_start_env = Hashtbl.create (BBMap.cardinal mirfunc.bbs) in
  let bb_end_env = Hashtbl.create (BBMap.cardinal mirfunc.bbs) in
  let llfunc_info = find_llfunc_info ctx mirfunc.funcid in
  { proggen_ctx = ctx; builder; mirfunc; ssa_env; bb_start_env; bb_end_env; llfunc_info }

let get_llfunc (fgen_ctx : fgen_ctx) : llvalue =
  fgen_ctx.llfunc_info.func

let get_start_llbb (fgen_ctx : fgen_ctx) (bbid : bbid) : llbasicblock =
  try Hashtbl.find fgen_ctx.bb_start_env bbid
  with Not_found -> raise (LlvmgenError ("get_start_llbb: bb not found in env: " ^ string_of_int bbid))

let get_end_llbb (fgen_ctx : fgen_ctx) (bbid : bbid) : llbasicblock =
  try Hashtbl.find fgen_ctx.bb_end_env bbid
  with Not_found -> raise (LlvmgenError ("get_end_llbb: bb not found in env: " ^ string_of_int bbid))

let set_start_llbb (fgen_ctx : fgen_ctx) (bbid : bbid) (llbb : llbasicblock) : unit =
  if Hashtbl.mem fgen_ctx.bb_start_env bbid then
    raise (LlvmgenError ("set_start_llbb: bb already set in env: " ^ string_of_int bbid))
  else
    Hashtbl.add fgen_ctx.bb_start_env bbid llbb

let set_end_llbb (fgen_ctx : fgen_ctx) (bbid : bbid) (llbb : llbasicblock) : unit =
  if Hashtbl.mem fgen_ctx.bb_end_env bbid then
    raise (LlvmgenError ("set_end_llbb: bb already set in env: " ^ string_of_int bbid))
  else
    Hashtbl.add fgen_ctx.bb_end_env bbid llbb

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

let rec gen_default_llvalue (ctx : proggen_ctx) (mirtyp : mirtyp) : llvalue =
  let zero_i8 = const_int ctx.i8_t 0 in
  let zero_i32 = const_int ctx.i32_t 0 in
  let zero_i64 = const_int ctx.i64_t 0 in
  let null_ptr = const_null ctx.ptr_t in
  match mirtyp with
  | TMIRUnit -> const_struct ctx.llcontext [||]
  | TMIRI32 -> zero_i32
  | TMIRI8 -> zero_i8
  | TMIRClos _ ->
    const_struct ctx.llcontext [| 
            null_ptr; (*borr*)
            null_ptr; (*own*)
            null_ptr; (*copy*)
            null_ptr; (*drop*)
            null_ptr; (*data_ptr*)
            zero_i64; (*off*)
          |]
  | TMIRVec _ -> const_struct ctx.llcontext [| 
            null_ptr; (*vec_ptr*)
            zero_i32; (*vec_len*)
          |]
  | TMIRTup elms_mirtyp -> (
    let elms_def_llvals = Array.of_list @@ List.map (gen_default_llvalue ctx) elms_mirtyp in
    const_struct ctx.llcontext elms_def_llvals
  )



(* ========================================================================= *)
(* Declare Globals and Functions                                             *)
(* ========================================================================= *)

let decl_global (ctx : proggen_ctx) (glob : Mir.global) : unit =
  let glob_default_llval = gen_default_llvalue ctx glob.typ in
  let llglobal = define_global ("global_" ^ string_of_int glob.globalid) glob_default_llval ctx.llmodule in
  Hashtbl.add ctx.globals_env glob.globalid (glob.typ, llglobal)

let decl_func (ctx : proggen_ctx) (builtin_table : (string, lltype * llvalue) Hashtbl.t) (mirfunc : Mir.func) : unit =
  match mirfunc.extern_name with
  | Some extern_name -> (
    let builtin_t, builtin_func = try Hashtbl.find builtin_table extern_name
      with Not_found -> raise (LlvmgenError ("decl_func: builtin function not found in env: " ^ extern_name)) in
    ctx_add_llfunc_info ctx mirfunc.funcid { mir_funcid = mirfunc.funcid; func_t = builtin_t; func = builtin_func; closwrpr = None; }
  )
  | None ->
    let args_mirtyps = List.map (fun (ssaid, _) -> get_mirtyp_func mirfunc ssaid) mirfunc.args in
    let ret_mirtyp = mirfunc.rettyp in
    let args_lltyps = mirtyplst_get_lltyparr ctx args_mirtyps in
    let ret_lltyp = mirtyp_get_lltyp ctx ret_mirtyp in
    let llfunc_t = function_type ret_lltyp args_lltyps in
    let llfunc = declare_function (mirfunc.name ^ "_" ^ string_of_int mirfunc.funcid) llfunc_t ctx.llmodule in
    ctx_add_llfunc_info ctx mirfunc.funcid { mir_funcid = mirfunc.funcid; func_t = llfunc_t; func = llfunc; closwrpr = None; }




(* ========================================================================= *)
(* Llvm struct helpers                                                       *)
(* ========================================================================= *)

let build_vec_struct (ctx : proggen_ctx) (builder : llbuilder) (vec_ptr : llvalue) (vec_len : llvalue) : llvalue =
  let undef = undef ctx.vec_t in
  let s0 = build_insertvalue undef vec_ptr 0 "vec_struct0" builder in
  build_insertvalue s0 vec_len 1 "vec_struct" builder

let build_clos_struct (ctx : proggen_ctx) (builder : llbuilder) 
                      (borr_llfunc : llvalue) 
                      (own_llfunc : llvalue) 
                      (copy_llfunc : llvalue) 
                      (drop_llfunc : llvalue) 
                      (data_ptr : llvalue) 
                      (off : llvalue) : llvalue =
  let undef = undef ctx.clos_t in
  let s0 = build_insertvalue undef borr_llfunc 0 "clos_struct0" builder in
  let s1 = build_insertvalue s0 own_llfunc 1 "clos_struct1" builder in
  let s2 = build_insertvalue s1 copy_llfunc 2 "clos_struct2" builder in
  let s3 = build_insertvalue s2 drop_llfunc 3 "clos_struct3" builder in
  let s4 = build_insertvalue s3 data_ptr 4 "clos_struct4" builder in
  build_insertvalue s4 off 5 "clos_struct" builder

let build_tup_struct (ctx : proggen_ctx) (builder : llbuilder) (mirtyp : mirtyp) (elms_llval : llvalue list) : llvalue =
  let tup_lltyp = mirtyp_get_lltyp ctx mirtyp in
  let tup_struct_acc = ref (undef tup_lltyp) in
  List.iteri (fun i elm_llval ->
    tup_struct_acc := build_insertvalue !tup_struct_acc elm_llval i ("tup_struct_" ^ string_of_int i) builder
  ) elms_llval;
  !tup_struct_acc

(* ========================================================================= *)
(* Copy, Drop and Init Helpers                                               *)
(* ========================================================================= *)

let build_malloc_safe (ctx : proggen_ctx) (builder : llbuilder) (llfunc : llvalue) (bytesz : llvalue) : llvalue =
  (*create and get all bb*)
  let current_bb = insertion_block builder in
  let malloc_bb = append_block ctx.llcontext "malloc_bb" llfunc in
  let malloc_fault_bb = append_block ctx.llcontext "malloc_fault_bb" llfunc in
  let malloc_merge_bb = append_block ctx.llcontext "malloc_merge_bb" llfunc in

  (* check for zero bytes request *)
  let null_ptr = const_null ctx.ptr_t in
  let is_zerobytes = build_icmp Icmp.Eq bytesz (Llvm.const_int ctx.i64_t 0) "build_malloc_safe_zerobytes" builder in
  ignore (build_cond_br is_zerobytes malloc_merge_bb malloc_bb builder);

  (* call malloc and check for null *)
  position_at_end malloc_bb builder;
  let malloc_ptr = build_call ctx.malloc_t ctx.malloc_func [| bytesz |] "build_malloc_safe_malloc_ptr" builder in
  let is_null = build_icmp Icmp.Eq malloc_ptr null_ptr "build_malloc_safe_is_null" builder in
  ignore (build_cond_br is_null malloc_fault_bb malloc_merge_bb builder);

  (* fault on null *)
  position_at_end malloc_fault_bb builder;
  ignore (build_call ctx.trap_t ctx.trap_func [||] "" builder);
  ignore (build_unreachable builder);

  (* merge the malloc retunred ptr or null *)
  position_at_end malloc_merge_bb builder;
  build_phi [(malloc_ptr, malloc_bb); (null_ptr, current_bb)] "build_malloc_safe_malloc_ptr_phi" builder


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
    let vec_bytesz = 
      match inner_mirtyp with
      | TMIRVECI32 -> (
        let i32_bytesz = const_int ctx.i64_t (Int64.to_int @@ DataLayout.abi_size ctx.i32_t ctx.lldata_layout) in
        build_mul len_i64 i32_bytesz "bytesz" builder )
      | TMIRVECI8 -> len_i64 (* is there a machine where a byte is not a byte ??? hope not *)
    in

    (*malloc new vec memory*)
    let copyvec_ptr = build_malloc_safe ctx builder llfunc vec_bytesz in
    
    (*do memcpy*)
    let is_volatile = Llvm.const_int ctx.i1_t 0 in
    ignore (build_call ctx.memcpy_t ctx.memcpy_func [| copyvec_ptr; origvec_ptr; vec_bytesz; is_volatile |] "" builder);

    (*build new vec struct*)
    build_vec_struct ctx builder copyvec_ptr origvec_len
  )
  | TMIRVec (n, inner_mirtyp) -> (
    let origvec_ptr = Llvm.build_extractvalue origvec 0 "vecptr" builder in
    let origvec_len = Llvm.build_extractvalue origvec 1 "veclen" builder in
    let len_i64 = Llvm.build_zext origvec_len ctx.i64_t "len_i64" builder in
    let vec_struct_bytesz = Llvm.size_of ctx.vec_t in
    let vec_bytesz = build_mul len_i64 vec_struct_bytesz "bytesz" builder in

    (*malloc new vec memory*)
    let copyvec_ptr = build_malloc_safe ctx builder llfunc vec_bytesz in

    let copy_elm (idx : llvalue) : unit =
      let origvec_elm_ptr = build_in_bounds_gep ctx.vec_t origvec_ptr [| idx |] "origvec_elm_ptr" builder in
      let orig_elm = build_load ctx.vec_t origvec_elm_ptr "elm" builder in
      let copy_elm = copy_vec ctx builder llfunc (TMIRVec (n-1, inner_mirtyp)) orig_elm in
      let copyvec_elm_ptr = build_in_bounds_gep ctx.vec_t copyvec_ptr [| idx |] "copyvec_elm_ptr" builder in
      ignore (build_store copy_elm copyvec_elm_ptr builder);
    in
    gen_loop ctx builder llfunc copy_elm len_i64;

    build_vec_struct ctx builder copyvec_ptr origvec_len
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
    build_clos_struct ctx builder borr_llfunc own_llfunc copy_llfunc drop_llfunc datacopy_ptr off
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
    build_tup_struct ctx builder mirtyp copy_elms
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

    let free_bb = append_block ctx.llcontext "drop_vec_free_bb1d" llfunc in
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

    let cond = build_icmp Icmp.Ne vec_len ((Llvm.const_int ctx.i32_t 0)) "drop_vec_len0_cond" builder in
    let curr_bb = insertion_block builder in

    let free_bb = append_block ctx.llcontext "drop_vec_free_bb_nd" llfunc in
    position_at_end free_bb builder;

    let drop_elm (idx : llvalue) : unit =
      let vec_elm_ptr = build_in_bounds_gep ctx.vec_t vec_ptr [| idx |] "drop_vec_elm_ptr" builder in
      let elm = build_load ctx.vec_t vec_elm_ptr "drop_vec_elm" builder in
      drop_vec ctx builder llfunc (TMIRVec (n-1, inner_mirtyp)) elm
    in
    gen_loop ctx builder llfunc drop_elm len_i64;
    ignore(build_call ctx.free_t ctx.free_func [| vec_ptr |] "" builder);

    let merge_bb = append_block ctx.llcontext "drop_vec_merge_bb" llfunc in
    ignore(build_br merge_bb builder);

    position_at_end curr_bb builder;
    ignore(build_cond_br cond free_bb merge_bb builder);

    position_at_end merge_bb builder
  )

let drop_clos (ctx : proggen_ctx) (builder : llbuilder) (llfunc : llvalue) (mirtyp : mirtyp) ( clos : llvalue) : unit =
  match mirtyp with
  | TMIRUnit | TMIRI32 | TMIRI8 | TMIRVec _ | TMIRTup _ -> raise (LlvmgenError "drop_clos non vec mirtyp passed")
  | TMIRClos _ -> (
    let drop_llfunc = build_extractvalue clos 3 "clos_drop_fptr" builder in
    let data_ptr = build_extractvalue clos 4 "clos_data_ptr" builder in
    let off = build_extractvalue clos 5 "clos_off" builder in
    let drop_func_t = Llvm.function_type (ctx.void_t) [| ctx.ptr_t; ctx.i64_t |] in
    ignore (build_call drop_func_t drop_llfunc [| data_ptr; off |] "" builder)
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

let rec init_vec (fgen_ctx : fgen_ctx) (defval_ssaid : ssaid) (dim_sizes : ssaid list) : llvalue =
  let ctx = fgen_ctx.proggen_ctx in
  let builder = fgen_ctx.builder in
  let mirfunc = fgen_ctx.mirfunc in
  let defval_mirtyp = get_mirtyp_func mirfunc defval_ssaid in
  match dim_sizes with
  | vec_size_ssaid :: rest_dim_sizes -> (
    (* generic case => use loop over the init_vec of the rest dimensions *)

    (*malloc new vec memory*)
    let vec_size_llval = get_llssa fgen_ctx vec_size_ssaid in
    let vec_size_i64 = build_zext vec_size_llval ctx.i64_t "vec_size_i64" builder in
    let vec_elm_lltyp = 
      match rest_dim_sizes, defval_mirtyp with
      | [], TMIRI32 -> ctx.i32_t
      | [], TMIRI8 -> ctx.i8_t
      | [], TMIRVec _ -> ctx.vec_t
      | h :: tl, _ -> ctx.vec_t 
      | _ -> raise (LlvmgenError "init_vec: defval mirtyp not i32, i8 or vec")
    in
    let vec_elm_size = size_of vec_elm_lltyp in
    let vec_bytesz = build_mul vec_size_i64 vec_elm_size "vec_bytesz" builder in
    let vec_ptr = build_malloc_safe ctx builder fgen_ctx.llfunc_info.func vec_bytesz in

    (*init each element of the vec*)
    let init_elm (idx : llvalue) : unit =
      let elm = init_vec fgen_ctx defval_ssaid rest_dim_sizes in
      let elm_ptr = build_gep vec_elm_lltyp vec_ptr [| idx |] "vec_elm_ptr" builder in
      ignore (build_store elm elm_ptr builder)
    in
    gen_loop ctx builder fgen_ctx.llfunc_info.func init_elm vec_size_i64;

    (*build new vec struct*)
    build_vec_struct ctx builder vec_ptr vec_size_llval
  )
  | [] -> (
    (* base case => copy the defval *)
    copy ctx builder fgen_ctx.llfunc_info.func defval_mirtyp (get_llssa fgen_ctx defval_ssaid)
  )

(* ========================================================================= *)
(* Create Closures                                                           *)
(* ========================================================================= *)

let get_clos_layout (ctx : proggen_ctx) (args_lltyps : lltype array) : int array * int =

  (*
    simulates the offsets that the pack op is going to use
  *)

  let curr_off = ref 0 in (*current offset in bytes*)

  let args_offsets = Array.map (fun arg_lltyp ->
    let arg_size = Int64.to_int @@ Llvm_target.DataLayout.abi_size arg_lltyp ctx.lldata_layout in
    let arg_align = Llvm_target.DataLayout.abi_align arg_lltyp ctx.lldata_layout in
    let off = (!curr_off + arg_align - 1) land (lnot (arg_align - 1)) in
    curr_off := off + arg_size;
    off
  ) args_lltyps in
  (args_offsets, !curr_off)


let get_clos_wrapper (ctx : proggen_ctx) (mirfuncid : funcid) : llvalue =
  let llfunc_info = find_llfunc_info ctx mirfuncid in
  assert (llfunc_info.mir_funcid = mirfuncid);
  match llfunc_info.closwrpr with
  | Some closwrpr -> closwrpr
  | None -> (
    let closwrpr_func_t = function_type (return_type llfunc_info.func_t) [| ctx.ptr_t |] in
    let closwrpr_func = declare_function ("closwrpr_func_" ^ string_of_int mirfuncid) closwrpr_func_t ctx.llmodule in
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
  let gen_ladder (builder : llbuilder) (llfunc : llvalue) (fgen : llbuilder -> int -> unit) (off : llvalue) (clos_data_offsets : int array) : unit =

    let maxoff = Array.length clos_data_offsets in
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
      let cond = build_icmp Icmp.Sgt off (const_int ctx.i64_t clos_data_offsets.(i)) ("cond_" ^ string_of_int i) builder in
      ignore (build_cond_br cond next_bb bbend builder)
    ) bbpairs;

    position_at_end bbmaxoff builder;
    ignore (build_br bbend builder);

    position_at_end bbend builder;
  in

  match Hashtbl.find_opt ctx.closhelper_env args_mirtyp with
  | Some clos_helpers -> clos_helpers
  | None -> (

    (* used to make the llvm function names unique *)
    let hash = Hashtbl.hash args_mirtyp in

    (*layout*)
    let args_mirtyp_arr = Array.of_list args_mirtyp in
    let args_lltype_arr = Array.map (mirtyp_get_lltyp ctx) args_mirtyp_arr in
    let clos_data_offsets, clos_data_size = get_clos_layout ctx args_lltype_arr in


    (*COPY FUNC*)
    (*declare copy func*)
    let copy_func_t = function_type ctx.ptr_t [| ctx.ptr_t ; ctx.i64_t |] in
    let copy_func = declare_function ("clos_copy_func_" ^ string_of_int hash) copy_func_t ctx.llmodule in
    let builder = Llvm.builder ctx.llcontext in
    let entry_bb = append_block ctx.llcontext "entry" copy_func in
    position_at_end entry_bb builder;
    let clos_data_ptr = param copy_func 0 in
    let off = param copy_func 1 in

    (*alloc new data memory
      a closure the contains just unit types will have bytesize 0
      thus a the malloc is checked
    *)
    let clos_data_bytesz = const_int ctx.i64_t clos_data_size in
    let clos_data_copy_ptr = build_malloc_safe ctx builder copy_func clos_data_bytesz in

    (*copy closure contents*)
    let copy_arg_gen (builder : llbuilder) (i : int) =
      let arg_orig_ptr = build_gep ctx.i8_t clos_data_ptr [| const_int ctx.i32_t clos_data_offsets.(i) |] ("argptr_" ^ string_of_int i) builder in
      let arg_orig_val = build_load args_lltype_arr.(i) arg_orig_ptr ("arg_" ^ string_of_int i) builder in
      let arg_copy_val = copy ctx builder copy_func args_mirtyp_arr.(i) arg_orig_val in
      let arg_copy_ptr = build_gep ctx.i8_t clos_data_copy_ptr [| const_int ctx.i32_t clos_data_offsets.(i) |] ("argcopyptr_" ^ string_of_int i) builder in
      ignore (build_store arg_copy_val arg_copy_ptr builder)
    in
    gen_ladder builder copy_func copy_arg_gen off clos_data_offsets;

    ignore (build_ret clos_data_copy_ptr builder);

    (*DROP FUNC*)
    (*declare drop func*)
    let drop_func_t = function_type ctx.void_t [| ctx.ptr_t ; ctx.i64_t |] in
    let drop_func = declare_function ("clos_drop_func_" ^ string_of_int hash) drop_func_t ctx.llmodule in
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
    gen_ladder builder drop_func drop_arg_gen off clos_data_offsets;

    (*free closure data memory
      quick note free(null) is a safe nop so even if the closure data memory
      has size 0 and is thus null the free is safe
    *)
    ignore (build_call ctx.free_t ctx.free_func [| clos_data_ptr |] "" builder);

    ignore (build_ret_void builder);

    let clos_helpers = {
      signature = args_mirtyp;
      copy_func;
      drop_func;
    } in
    Hashtbl.add ctx.closhelper_env args_mirtyp clos_helpers;
    clos_helpers

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

let vec_access_lltyps (fgen_ctx : fgen_ctx) (vec_ssaid : ssaid) : lltype array =
  let ctx = fgen_ctx.proggen_ctx in
  let mirfunc = fgen_ctx.mirfunc in
  let dim, inner_vecmirtyp = 
    match get_mirtyp_func mirfunc vec_ssaid with
    | TMIRVec (dim, inner_vecmirtyp) -> dim, inner_vecmirtyp
    | _ -> raise (LlvmgenError "vec_access_lltyps: vec ssa has non vec type")
  in
  Array.init dim ( fun i -> 
    if i = (dim-1) then 
      match inner_vecmirtyp with | TMIRVECI32 -> ctx.i32_t | TMIRVECI8 -> ctx.i8_t 
    else 
      ctx.vec_t
  )

let fault_on_cond_false (fgen_ctx : fgen_ctx) (cond : llvalue) : unit =
  let ctx = fgen_ctx.proggen_ctx in
  let builder = fgen_ctx.builder in

  let err_bb = append_block ctx.llcontext "fault_on_cond_false_err_bb" fgen_ctx.llfunc_info.func in
  let ok_bb = append_block ctx.llcontext "fault_on_cond_false_ok_bb" fgen_ctx.llfunc_info.func in

  ignore (build_cond_br cond ok_bb err_bb builder);

  position_at_end err_bb builder;
  ignore (build_call ctx.trap_t ctx.trap_func [||] "" builder);
  ignore (build_unreachable builder);

  position_at_end ok_bb builder;
  ()

let vec_checked_access (fgen_ctx : fgen_ctx) (vec : llvalue) (vec_inner_lltype : lltype) (idx : llvalue) : llvalue =
  let builder = fgen_ctx.builder in

  let vec_ptr = build_extractvalue vec 0 "vecptr" builder in
  let vec_len = build_extractvalue vec 1 "veclen" builder in

  let cond = build_icmp Icmp.Slt idx vec_len "vec_access_cond" builder in
  fault_on_cond_false fgen_ctx cond;

  build_gep vec_inner_lltype vec_ptr [| idx |] "vec_elm_ptr" builder
  (*I dont do the load since then the helper can be used for the write ops too*)
  

let lower_op (fgen_ctx : fgen_ctx) (mirop : Mir.op) : unit =
  let ctx = fgen_ctx.proggen_ctx in
  let mirfunc = fgen_ctx.mirfunc in
  let builder = fgen_ctx.builder in
  let get_lltyp_from_ssaid ssaid = mirtyp_get_lltyp ctx @@ get_mirtyp_func mirfunc ssaid in
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
    let clos_data_bytesz = const_int ctx.i64_t clos_data_size in
    let clos_data_ptr = build_malloc_safe ctx builder fgen_ctx.llfunc_info.func clos_data_bytesz in

    (* assemble closure *)
    let clos_val = build_clos_struct ctx builder borr_closwrpr own_closwrpr clos_helpers.copy_func clos_helpers.drop_func clos_data_ptr (const_int ctx.i64_t 0) in
    set_llssa fgen_ctx def_ssaid clos_val
  )
  | Pack (def_ssaid, clos_consume, args_consume) -> (
    let clos_llval = consume_or_copy fgen_ctx clos_consume in
    let args_llval = List.map (consume_or_copy fgen_ctx) args_consume in
    let args_lltyp = List.map (fun sc -> get_lltyp_from_ssaid sc.ssaid) args_consume in

    let borr_llfunc = build_extractvalue clos_llval 0 "clos_borr_fptr" builder in
    let own_llfunc = build_extractvalue clos_llval 1 "clos_own_fptr" builder in
    let copy_llfunc = build_extractvalue clos_llval 2 "clos_copy_fptr" builder in
    let drop_llfunc = build_extractvalue clos_llval 3 "clos_drop_fptr" builder in
    let clos_data_ptr = build_extractvalue clos_llval 4 "clos_data_ptr" builder in
    let clos_data_off_ref = ref (build_extractvalue clos_llval 5 "clos_data_off" builder) in

    List.iter2 (fun arg_llval arg_lltyp ->
      let arg_align = Llvm_target.DataLayout.abi_align arg_lltyp ctx.lldata_layout in
      let off_aligned_intermediate = build_add !clos_data_off_ref (const_int ctx.i64_t (arg_align - 1)) "off_aligned_intermediate" builder in
      let off_aligned_mask = build_not (const_int ctx.i64_t (arg_align - 1)) "off_aligned_mask" builder in
      let off_aligned = build_and off_aligned_intermediate off_aligned_mask "off_aligned" builder in
      let arg_ptr = build_gep ctx.i8_t clos_data_ptr [| off_aligned |] "argptr" builder in
      ignore (build_store arg_llval arg_ptr builder);
      let arg_size = Int64.to_int (Llvm_target.DataLayout.abi_size arg_lltyp ctx.lldata_layout) in
      let off_aligned_plus_argsize = build_add off_aligned (const_int ctx.i64_t arg_size) "off_plus_argsize" builder in
      clos_data_off_ref := off_aligned_plus_argsize
    ) args_llval args_lltyp;

    (* assemble closure *)
    let clos_val = build_clos_struct ctx builder borr_llfunc own_llfunc copy_llfunc drop_llfunc clos_data_ptr !clos_data_off_ref in
    set_llssa fgen_ctx def_ssaid clos_val
  )
  | CallClosure (res_ssaid, clos_sc) -> (
    let clos_llval = get_llssa fgen_ctx clos_sc.ssaid in
    let clos_wrprfunc_t = function_type (get_lltyp_from_ssaid res_ssaid) [| ctx.ptr_t |] in
    let clos_data_ptr = build_extractvalue clos_llval 4 "clos_data_ptr" builder in

    if clos_sc.consume then
      let own_llfunc = build_extractvalue clos_llval 1 "clos_own_fptr" builder in
      let res = build_call clos_wrprfunc_t own_llfunc [| clos_data_ptr |] "call_clos_owned_res" builder in
      set_llssa fgen_ctx res_ssaid res;
      (* if the closure is consumed, the data in the closure is freed or part of the
         return value since the function called assumes all args owned but the memory
         that stores the pointers to the data ie. the closure data is not taken care of
         so one has to free it here *)
      ignore (build_call ctx.free_t ctx.free_func [| clos_data_ptr |] "" builder);
    else
      let borr_llfunc = build_extractvalue clos_llval 0 "clos_borr_fptr" builder in
      let res = build_call clos_wrprfunc_t borr_llfunc [| clos_data_ptr |] "call_clos_borrowed_res" builder in
      set_llssa fgen_ctx res_ssaid res;
  )
  | CallDirect (res_ssaid, funcid_ref, args_consume) -> (
    let llfunc_info = find_llfunc_info ctx !funcid_ref in
    let args_llval_arr = Array.of_list @@ List.map (fun arg -> get_llssa fgen_ctx arg.ssaid) args_consume in
    let res = build_call llfunc_info.func_t llfunc_info.func args_llval_arr "call_direct_res" builder in
    set_llssa fgen_ctx res_ssaid res
  )
  | Copy (def_ssaid, origin_ssaid) -> (
    let origin_llval = get_llssa fgen_ctx origin_ssaid in
    let mirtyp = get_mirtyp_func mirfunc def_ssaid in
    let copy_llval = copy ctx builder fgen_ctx.llfunc_info.func mirtyp origin_llval in
    set_llssa fgen_ctx def_ssaid copy_llval
  )
  | Drop origin_ssaids_lst -> (
    List.iter (fun origin_ssaid ->
      let origin_llval = get_llssa fgen_ctx origin_ssaid in
      let mirtyp = get_mirtyp_func mirfunc origin_ssaid in
      drop ctx builder fgen_ctx.llfunc_info.func mirtyp origin_llval
    ) origin_ssaids_lst
  )
  | StoreGlobal (globalid, origin_consume) -> (
    let origin_llval = consume_or_copy fgen_ctx origin_consume in
    let _, global_llval = find_global ctx globalid in
    ignore (build_store origin_llval global_llval builder)
  )
  | LoadGlobal (def_ssaid, globalid) -> (
    let global_mirtyp, global_llval = find_global ctx globalid in
    let loaded_llval = build_load (mirtyp_get_lltyp ctx global_mirtyp) global_llval "load_global" builder in
    set_llssa fgen_ctx def_ssaid loaded_llval
  )
  | DropGlobal globalid -> (
    let global_mirtyp, global_llval = find_global ctx globalid in
    let loaded_llval = build_load (mirtyp_get_lltyp ctx global_mirtyp) global_llval "load_global" builder in
    drop ctx builder fgen_ctx.llfunc_info.func global_mirtyp loaded_llval;
    (* overwriting this here makes the droped stuff fully unreachable 
       which can avoid bugs and uncover memory leaks with then LeakSanitizer *)
    let glob_default_llval = gen_default_llvalue ctx global_mirtyp in
    ignore (build_store glob_default_llval global_llval builder)
  )
  | Immi32 (def_ssaid, i32) -> (
    let llval = const_int ctx.i32_t (Int32.to_int i32) in
    set_llssa fgen_ctx def_ssaid llval
  )
  | Immi8 (def_ssaid, i8) -> (
    let llval = const_int ctx.i8_t (int_of_char i8) in
    set_llssa fgen_ctx def_ssaid llval
  )
  | ImmUnit def_ssaid -> (
    let llval = const_struct ctx.llcontext [||] in
    set_llssa fgen_ctx def_ssaid llval
  )
  | Uopi32 (def_ssaid, op, origin_ssaid) -> (
    let origin_llval = get_llssa fgen_ctx origin_ssaid in
    let llval =
      match op with
      | Negi32 -> build_neg origin_llval "neg" builder
      | Noti32 -> build_not origin_llval "not" builder
    in
    set_llssa fgen_ctx def_ssaid llval
  )
  | Uopi8 (def_ssaid, op, origin_ssaid) -> (
    let origin_llval = get_llssa fgen_ctx origin_ssaid in
    let llval =
      match op with
      | Negi8 -> build_neg origin_llval "neg" builder
      | Noti8 -> build_not origin_llval "not" builder
    in
    set_llssa fgen_ctx def_ssaid llval
  )
  | Bopi32 (def_ssaid, bop, left_ssaid, right_ssaid) -> (
    let left_llval = get_llssa fgen_ctx left_ssaid in
    let right_llval = get_llssa fgen_ctx right_ssaid in
    let cmp_aux (icmp : Icmp.t) : llvalue =
      let cmp_llval = build_icmp icmp left_llval right_llval "cmp" builder in
      build_zext cmp_llval ctx.i32_t "cmp_i32" builder
    in
    let llval =
      match bop with
      | Eqi32 -> cmp_aux Icmp.Eq
      | Neqi32  -> cmp_aux Icmp.Ne
      | Lti32   -> cmp_aux Icmp.Slt
      | Gti32   -> cmp_aux Icmp.Sgt
      | LtEqi32 -> cmp_aux Icmp.Sle
      | GtEqi32 -> cmp_aux Icmp.Sge

      (* Comparisons (Unsigned) *)
      | ULti32   -> cmp_aux Icmp.Ult
      | UGti32   -> cmp_aux Icmp.Ugt
      | ULtEqi32 -> cmp_aux Icmp.Ule
      | UGtEqi32 -> cmp_aux Icmp.Uge

      (* Arithmetic *)
      | Addi32  -> build_add left_llval right_llval "add" builder
      | Subi32  -> build_sub left_llval right_llval "sub" builder
      | Muli32  -> build_mul left_llval right_llval "mul" builder
      | Divi32  -> build_sdiv left_llval right_llval "sdiv" builder
      | UDivi32 -> build_udiv left_llval right_llval "udiv" builder
      | Modi32  -> build_srem left_llval right_llval "srem" builder
      | UModi32 -> build_urem left_llval right_llval "urem" builder

      (* Bitwise *)
      | Andi32  -> build_and left_llval right_llval "and" builder
      | Ori32   -> build_or left_llval right_llval "or" builder
      | Xori32  -> build_xor left_llval right_llval "xor" builder

      (* Shifts *)
      | Shli32  -> build_shl left_llval right_llval "shl" builder
      | Shri32  -> build_ashr left_llval right_llval "ashr" builder
      | UShri32 -> build_lshr left_llval right_llval "lshr" builder
    in
    set_llssa fgen_ctx def_ssaid llval
  )
  | Bopi8 (def_ssaid, bop, left_ssaid, right_ssaid) -> (
    let left_llval = get_llssa fgen_ctx left_ssaid in
    let right_llval = get_llssa fgen_ctx right_ssaid in
    let cmp_aux (icmp : Icmp.t) : llvalue =
      let cmp_llval = build_icmp icmp left_llval right_llval "cmp" builder in
      build_zext cmp_llval ctx.i32_t "cmp_i32" builder
    in
    let llval =
      match bop with
      (* Comparisons (Unsigned, Yield i32) *)
      | Eqi8   -> cmp_aux Icmp.Eq
      | Neqi8  -> cmp_aux Icmp.Ne
      | Lti8   -> cmp_aux Icmp.Ult
      | Gti8   -> cmp_aux Icmp.Ugt
      | LtEqi8 -> cmp_aux Icmp.Ule
      | GtEqi8 -> cmp_aux Icmp.Uge

      (* Arithmetic (Yield i8) *)
      | Addi8  -> build_add left_llval right_llval "add" builder
      | Subi8  -> build_sub left_llval right_llval "sub" builder

      (* Bitwise (Yield i8) *)
      | Andi8  -> build_and left_llval right_llval "and" builder
      | Ori8   -> build_or left_llval right_llval "or" builder
      | Xori8  -> build_xor left_llval right_llval "xor" builder
    in
    set_llssa fgen_ctx def_ssaid llval
  )
  | Tupwrp (ssa_def, elms_consume) -> (
    let elms_llval = List.map (consume_or_copy fgen_ctx) elms_consume in
    let tup_llval = build_tup_struct ctx builder (get_mirtyp_func mirfunc ssa_def) elms_llval in
    set_llssa fgen_ctx ssa_def tup_llval
  )
  | Tupuwrp (ssa_defs, tup_consume) -> (
    let tup_llval = get_llssa fgen_ctx tup_consume.ssaid in
    List.iteri (fun i ssa_def ->
      let elm_llval = build_extractvalue tup_llval i ("tup_elm_" ^ string_of_int i) builder in
      set_llssa fgen_ctx ssa_def elm_llval
    ) ssa_defs
  ) 
  | Veclit (ssa_def, lits_consume) -> (
    match lits_consume with
    | [] -> (
      let vec_llval = Llvm.const_struct ctx.llcontext [| const_null ctx.ptr_t; const_int ctx.i32_t 0 |] in
      set_llssa fgen_ctx ssa_def vec_llval
    )
    | first_lit :: tl -> (
      let lits_llval = List.map (consume_or_copy fgen_ctx) lits_consume in

      let lit_lltyp = get_lltyp_from_ssaid first_lit.ssaid in
      let lit_size = Int64.to_int (DataLayout.abi_size lit_lltyp ctx.lldata_layout) in
      let vec_bytesz = const_int ctx.i64_t (lit_size * (List.length lits_llval)) in
      let vec_ptr = build_malloc_safe ctx builder fgen_ctx.llfunc_info.func vec_bytesz in
      List.iteri (fun i lit_llval ->
        let lit_ptr = build_gep lit_lltyp vec_ptr [| const_int ctx.i32_t i |] ("litptr_" ^ string_of_int i) builder in
        ignore (build_store lit_llval lit_ptr builder)
      ) lits_llval;
      let vec_len = const_int ctx.i32_t (List.length lits_llval) in
      let vec_llval = build_vec_struct ctx builder vec_ptr vec_len in
      set_llssa fgen_ctx ssa_def vec_llval
    )
  )
  | Vecinit (ssa_def, defval_ssaid, dim_sizes) -> (
    let vec_llval = init_vec fgen_ctx defval_ssaid dim_sizes in
    set_llssa fgen_ctx ssa_def vec_llval
  )
  | Veclen (ssa_def, vec_ssaid) -> (
    let vec_llval = get_llssa fgen_ctx vec_ssaid in
    let vec_len_llval = build_extractvalue vec_llval 1 "vec_len" builder in
    set_llssa fgen_ctx ssa_def vec_len_llval
  )
  | Vecread (ssa_def, vec_ssaid, idxs_ssaids) -> (
    let curr_vec_llval = ref (get_llssa fgen_ctx vec_ssaid) in
    let access_lltyps = vec_access_lltyps fgen_ctx vec_ssaid in
    List.iteri (fun i idx_ssaid ->
      let idx_llval = get_llssa fgen_ctx idx_ssaid in
      let vec_inner_lltyp = access_lltyps.(i) in
      let elm_ptr = vec_checked_access fgen_ctx !curr_vec_llval vec_inner_lltyp idx_llval in
      let elm_llval = build_load vec_inner_lltyp elm_ptr "vec_elm" builder in
      if i = (List.length idxs_ssaids - 1) then
        set_llssa fgen_ctx ssa_def elm_llval
      else
        curr_vec_llval := elm_llval
    ) idxs_ssaids
  )
  | Vecwrite (ssa_def, vec_sc, val_ssaid, idxs_ssaids) -> (
    let vec_llval = consume_or_copy fgen_ctx vec_sc in
    let val_llval = get_llssa fgen_ctx val_ssaid in
    let access_lltyps = vec_access_lltyps fgen_ctx vec_sc.ssaid in
    let curr_vec_llval = ref vec_llval in
    List.iteri (fun i idx_ssaid ->
      let idx_llval = get_llssa fgen_ctx idx_ssaid in
      let vec_inner_lltyp = access_lltyps.(i) in
      let elm_ptr = vec_checked_access fgen_ctx !curr_vec_llval vec_inner_lltyp idx_llval in
      if i = (List.length idxs_ssaids - 1) then (
        (*vecwrite is supposed to store an i32 or i8 so nothing has to be dropped*)
        ignore (build_store val_llval elm_ptr builder);
        set_llssa fgen_ctx ssa_def vec_llval)
      else
        let elm_llval = build_load vec_inner_lltyp elm_ptr "vec_elm" builder in
        curr_vec_llval := elm_llval
    ) idxs_ssaids
  )
  | Vecinsert (ssa_def, vec_sc, val_sc, idxs_ssaids) -> (
    let vec_llval = consume_or_copy fgen_ctx vec_sc in
    let val_llval = consume_or_copy fgen_ctx val_sc in
    let access_lltyps = vec_access_lltyps fgen_ctx vec_sc.ssaid in
    let curr_vec_llval = ref vec_llval in
    List.iteri (fun i idx_ssaid ->
      let idx_llval = get_llssa fgen_ctx idx_ssaid in
      let vec_inner_lltyp = access_lltyps.(i) in
      let elm_ptr = vec_checked_access fgen_ctx !curr_vec_llval vec_inner_lltyp idx_llval in
      let elm_llval = build_load vec_inner_lltyp elm_ptr "vec_elm" builder in
      if i = (List.length idxs_ssaids - 1) then (
        (*vecinsert is supposed to store a vector into another vector*)
        (*a bit hacky to use the mirtyp of the inseted value for the old value but should be fine*)
        drop ctx builder fgen_ctx.llfunc_info.func (get_mirtyp_func mirfunc val_sc.ssaid) elm_llval;
        ignore (build_store val_llval elm_ptr builder);
        set_llssa fgen_ctx ssa_def vec_llval)
      else
        curr_vec_llval := elm_llval
    ) idxs_ssaids
  )
  | Vecslice (ssa_def, vec_ssaid, start_ssaid, len_ssaid) -> (
    let vec_llval = get_llssa fgen_ctx vec_ssaid in
    let start_llval = get_llssa fgen_ctx start_ssaid in
    let len_llval = get_llssa fgen_ctx len_ssaid in

    let vec_ptr = build_extractvalue vec_llval 0 "vecptr" builder in
    let vec_len = build_extractvalue vec_llval 1 "veclen" builder in

    let cond1 = build_icmp Icmp.Slt start_llval vec_len "vecslice_cond1" builder in
    fault_on_cond_false fgen_ctx cond1;

    let end_idx = build_add start_llval len_llval "vecslice_endidx" builder in
    let cond2 = build_icmp Icmp.Sle end_idx vec_len "vecslice_cond2" builder in
    fault_on_cond_false fgen_ctx cond2;

    let access_lltyps = vec_access_lltyps fgen_ctx vec_ssaid in
    let slice_ptr = build_gep access_lltyps.(0) vec_ptr [| start_llval |] "vecslice_ptr" builder in
    let slice_vec_llval = build_vec_struct ctx builder slice_ptr len_llval in
    set_llssa fgen_ctx ssa_def slice_vec_llval
  )
  | Vecextend (ssa_def, old_vec_ssaid, lit_ssaid, off_ssaid) -> (
    let old_vec_llval = get_llssa fgen_ctx old_vec_ssaid in
    let lit_llval = get_llssa fgen_ctx lit_ssaid in
    let off_llval = get_llssa fgen_ctx off_ssaid in

    let old_vec_ptr = build_extractvalue old_vec_llval 0 "vecptr" builder in
    let old_vec_len = build_extractvalue old_vec_llval 1 "veclen" builder in

    let zeroi32 = const_int ctx.i32_t 0 in
    let off_pos = build_icmp Icmp.Sle zeroi32 off_llval "is_gt" builder in
    let prep_len = build_select off_pos zeroi32 (build_neg off_llval "neg_off" builder) "prep_len" builder in
    let prep_len_i64 = build_zext prep_len ctx.i64_t "prep_len_i64" builder in
    let app_len = build_select off_pos off_llval zeroi32 "app_len" builder in
    let prep_len_plus_vec_len = build_add prep_len old_vec_len "new_len_partial" builder in
    let prep_len_plus_vec_len_i64 = build_zext prep_len_plus_vec_len ctx.i64_t "new_len_partial_i64" builder in
    let new_len = build_add prep_len_plus_vec_len app_len "new_len" builder in
    let new_len_i64 = build_zext new_len ctx.i64_t "new_len_i64" builder in

    let access_lltyps = vec_access_lltyps fgen_ctx old_vec_ssaid in
    let vec_elm_size = const_int ctx.i64_t (Int64.to_int @@ DataLayout.abi_size access_lltyps.(0) ctx.lldata_layout) in
    let new_vec_size = build_mul new_len_i64 vec_elm_size "new_vec_size" builder in
    let new_vec_ptr = build_malloc_safe ctx builder fgen_ctx.llfunc_info.func new_vec_size in

    let extend_elm (idx : llvalue) : unit =

      (*calc condition*)
      let prep_cond = build_icmp Icmp.Slt idx prep_len_i64 "prep_cond" builder in
      let app_cond = build_icmp Icmp.Sle prep_len_plus_vec_len_i64 idx "app_cond" builder in
      let lit_cond = build_or prep_cond app_cond "defval_cond" builder in
      let curr_bb = insertion_block builder in

      (*lower lit copy loading*)
      let lit_bb = append_block ctx.llcontext "lit_bb" fgen_ctx.llfunc_info.func in
      position_at_end lit_bb builder;
      let lit_copy = copy ctx builder fgen_ctx.llfunc_info.func (get_mirtyp_func mirfunc lit_ssaid) lit_llval in
      let lit_bb_end = insertion_block builder in

      (*lower old vector loading*)
      let old_vec_bb = append_block ctx.llcontext "old_vec_bb" fgen_ctx.llfunc_info.func in
      position_at_end old_vec_bb builder;
      let old_vec_idx = build_sub idx prep_len_i64 "old_vec_idx" builder in
      let old_vec_elm_ptr = build_gep access_lltyps.(0) old_vec_ptr [| old_vec_idx |] "vec_elm_ptr" builder in
      let old_vec_elm = build_load access_lltyps.(0) old_vec_elm_ptr "vec_elm" builder in
      let old_vec_elm_copy = copy ctx builder fgen_ctx.llfunc_info.func (get_mirtyp_func mirfunc lit_ssaid) old_vec_elm in
      let old_vec_bb_end = insertion_block builder in

      (*cond branch to the lit or old vec bbs*)
      position_at_end curr_bb builder;
      ignore (build_cond_br lit_cond lit_bb old_vec_bb builder);

      (*create merge bb*)
      let merge_bb = append_block ctx.llcontext "merge_bb" fgen_ctx.llfunc_info.func in

      (*put br to merge bb*)
      position_at_end lit_bb_end builder;
      ignore (build_br merge_bb builder);
      position_at_end old_vec_bb_end builder;
      ignore (build_br merge_bb builder);

      (*put phi node and store in new vec*)
      position_at_end merge_bb builder;
      let new_vec_elm = build_phi [(lit_copy, lit_bb); (old_vec_elm_copy, old_vec_bb)] "phi_node" builder in
      let new_vec_elm_ptr = build_gep access_lltyps.(0) new_vec_ptr [| idx |] "new_vec_elm_ptr" builder in
      ignore (build_store new_vec_elm new_vec_elm_ptr builder)
    in
    gen_loop ctx builder fgen_ctx.llfunc_info.func extend_elm new_len_i64;

    let new_vec_llval = build_vec_struct ctx builder new_vec_ptr new_len in
    set_llssa fgen_ctx ssa_def new_vec_llval
  )

let lower_func (ctx : proggen_ctx) (mirfunc : Mir.func) : unit =

  if Option.is_some mirfunc.extern_name then () else

  let fgen_ctx = create_fgen_ctx ctx mirfunc in
  let llfunc_info = fgen_ctx.llfunc_info in
  let llfunc = llfunc_info.func in 

  (*create the entry bb*)
  let entrybb = append_block ctx.llcontext "entry" llfunc in

  (*put all function args the env*)
  List.iteri (fun i (ssaid, _) ->
    let arg_llval = param llfunc i in
    set_llssa fgen_ctx ssaid arg_llval
  ) mirfunc.args;

  (*create all llbbs and lower their ops*)
  let rpo_info = get_rpo_info ctx.miranalysis mirfunc in  
  List.iter (fun bbid ->
    let mirbb = BBMap.find bbid mirfunc.bbs in
    let llbb = append_block ctx.llcontext (string_of_int bbid) llfunc in
    set_start_llbb fgen_ctx bbid llbb;
    ignore (position_at_end llbb fgen_ctx.builder);
    
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
    let curr_bb = insertion_block fgen_ctx.builder in
    set_end_llbb fgen_ctx bbid curr_bb

  ) rpo_info.rpo_lst;

  (*patch bb branching*)
  BBMap.iter (fun _ mirbb ->
    let end_llbb = get_end_llbb fgen_ctx mirbb.bbid in
    position_at_end end_llbb fgen_ctx.builder;
    match mirbb.term with
    | None -> raise (LlvmgenError "No term in llvmgen lower_func")
    | Some (Br (target_bbid, mir_brargs)) -> (
      (*put br*)
      let target_llbb = get_start_llbb fgen_ctx target_bbid in
      ignore (build_br target_llbb fgen_ctx.builder);

      (*patch phi nodes*)
      let target_mirbb = find_bb_func mirfunc target_bbid in
      List.iter2 (fun mir_brarg mir_bbarg ->
        let phi_node = get_llssa fgen_ctx mir_bbarg in
        let passed_llval = get_llssa fgen_ctx mir_brarg.ssaid in
        add_incoming (passed_llval, end_llbb) phi_node
      ) mir_brargs target_mirbb.args 
    )
    | Some (Cbr (cond_ssaid, true_bbid, false_bbid)) -> (
      (* transfor i32 cond into bool cond *)
      let true_llbb = get_start_llbb fgen_ctx true_bbid in
      let false_llbb = get_start_llbb fgen_ctx false_bbid in
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
      let mirentry_llbb = get_start_llbb fgen_ctx mirentrybbid in
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
  let lldata_layout = TargetMachine.data_layout target_machine in
  
  (* Embed the data layout string into the module *)
  set_data_layout (DataLayout.as_string lldata_layout) llmodule;


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
  let trap_t = Llvm.function_type (void_t) [||] in
  let trap_func = Llvm.declare_function "llvm.trap" trap_t llmodule in
  let getchar_t = function_type i32_t [||] in
  let getchar_func = declare_function "getchar" getchar_t llmodule in
  let putchar_t = function_type i32_t [| i32_t |] in
  let putchar_func = declare_function "putchar" putchar_t llmodule in
  let fflush_t = function_type i32_t [| ptr_t |] in
  let fflush_func = declare_function "fflush" fflush_t llmodule in

  let globals_env = Hashtbl.create 32 in
  let func_env = Hashtbl.create 32 in
  let closhelper_env = Hashtbl.create 32 in

  let miranalysis = create_analysis_info () in

  let ctx = {
    llcontext;
    llmodule;
    lldata_layout;
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
    trap_t;
    trap_func;
    getchar_t;
    getchar_func;
    putchar_t;
    putchar_func;
    fflush_t;
    fflush_func;
    miranalysis;
  } in

  (* all the builtins are lowered directly here and put in the builtin_table 
     that gets passed to the function declaration pass*)
  let builtin_table = Hashtbl.create 32 in

  let readi8_t = function_type i8_t [| unit_t |] in
  let readi8_func = declare_function "readi8" readi8_t llmodule in
  let builder = Llvm.builder llcontext in
  let entry_bb = append_block llcontext "entry" readi8_func in
  position_at_end entry_bb builder;
  let read_val_i32 = build_call getchar_t getchar_func [||] "read_val_i32" builder in
  let read_val_i8 = build_trunc read_val_i32 i8_t "read_val_i8" builder in
  ignore (build_ret read_val_i8 builder);
  Hashtbl.add builtin_table "readi8" (readi8_t, readi8_func);

  let writei8_t = function_type unit_t [| i8_t |] in
  let writei8_func = declare_function "writei8" writei8_t llmodule in
  let builder = Llvm.builder llcontext in
  let entry_bb = append_block llcontext "entry" writei8_func in
  position_at_end entry_bb builder;
  let write_val_i8 = param writei8_func 0 in
  let write_val_i32 = build_zext write_val_i8 i32_t "write_val_i32" builder in
  ignore (build_call putchar_t putchar_func [| write_val_i32 |] "" builder);
  ignore (build_ret (const_struct llcontext [||]) builder);
  Hashtbl.add builtin_table "writei8" (writei8_t, writei8_func);

  let flush_t = function_type unit_t [| unit_t |] in
  let flush_func = declare_function "flush" flush_t llmodule in
  let builder = Llvm.builder llcontext in
  let entry_bb = append_block llcontext "entry" flush_func in
  position_at_end entry_bb builder;
  ignore (build_call fflush_t fflush_func [| const_null ptr_t |] "" builder);
  ignore (build_ret (const_struct llcontext [||]) builder);
  Hashtbl.add builtin_table "flush" (flush_t, flush_func);

  let i32_to_i8_t = function_type i8_t [| i32_t |] in
  let i32_to_i8_func = declare_function "i32_to_i8" i32_to_i8_t llmodule in
  let builder = Llvm.builder llcontext in
  let entry_bb = append_block llcontext "entry" i32_to_i8_func in
  position_at_end entry_bb builder;
  let i32_val = param i32_to_i8_func 0 in
  let i8_val = build_trunc i32_val i8_t "i32_to_i8" builder in
  ignore (build_ret i8_val builder);
  Hashtbl.add builtin_table "i32_to_i8" (i32_to_i8_t, i32_to_i8_func);

  let i8_to_i32_t = function_type i32_t [| i8_t |] in
  let i8_to_i32_func = declare_function "i8_to_i32" i8_to_i32_t llmodule in
  let builder = Llvm.builder llcontext in
  let entry_bb = append_block llcontext "entry" i8_to_i32_func in
  position_at_end entry_bb builder;
  let i8_val = param i8_to_i32_func 0 in
  let i32_val = build_zext i8_val i32_t "i8_to_i32" builder in
  ignore (build_ret i32_val builder);
  Hashtbl.add builtin_table "i8_to_i32" (i8_to_i32_t, i8_to_i32_func);
  

  (* Iterate all MIR globals and declare empty llvm equivalents *)
  GlobalMap.iter (fun _ glob -> 
    decl_global ctx glob
  ) p.globals;

  (* Iterate all MIR functions and declare empty llvm equivalents *)
  FuncMap.iter (fun _  func -> 
    decl_func ctx builtin_table func
  ) p.funcs;

  (* Second pass over MIR functions this time the function bodies are lowered *)
  FuncMap.iter (fun _ func ->
    lower_func ctx func
  ) p.funcs;

  (*call init globals, main and uninit globals in a single new main function*)
  let main_type = function_type i32_t [||] in
  let main_fn = declare_function "main" main_type llmodule in
  let bb = append_block ctx.llcontext "entry" main_fn in
  let main_builder = Llvm.builder llcontext in
  position_at_end bb main_builder;
  
  let call_unitfunc_opt unitfunc_opt = (
    match unitfunc_opt with
    | Some funcid -> (
      let llfunc_info = find_llfunc_info ctx funcid in
      let llunit = const_struct ctx.llcontext [||] in
      ignore (build_call (llfunc_info.func_t) llfunc_info.func [| llunit |] "" main_builder)
    )
    | None -> ()
  ) in
  
  call_unitfunc_opt p.init_globals_funcid;
  call_unitfunc_opt p.main_funcid;
  call_unitfunc_opt p.uninit_globals_funcid;

  ignore (build_ret (const_int ctx.i32_t 0) main_builder);
  llmodule