open Mir
open Analysis (*process bbs in rpo *)

open Llvm
open Errors

(*
  TODO:
    - impl lower mir op
    - copy, init, drop codegen
    - uninit globals in mir impl (just load global and then drop if mem object)
*)

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

type fgen_ctx = {
  proggen_ctx : proggen_ctx;
  builder : llbuilder;
  ssa_env : Llvm.llvalue option array;
  bb_env  : (bbid, Llvm.llbasicblock) Hashtbl.t;
  llfunc_info : llfunc_info;
}

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

let find_llfunc_info (ctx : proggen_ctx) (funcid : funcid) : llfunc_info =
  try Hashtbl.find ctx.func_env funcid
  with Not_found -> raise (LlvmgenError ("find_llfunc: function not found in env: " ^ string_of_int funcid))

let create_fgen_ctx (ctx : proggen_ctx) (func : Mir.func) =
  let builder = builder ctx.llcontext in
  let ssa_env = Array.make func.next_ssaid None in
  let bb_env = Hashtbl.create (BBMap.cardinal func.bbs) in
  let llfunc_info = find_llfunc_info ctx func.funcid in
  { proggen_ctx = ctx; builder; ssa_env; bb_env ; llfunc_info }

let get_llfunc (fgen_ctx : fgen_ctx) : llvalue =
  fgen_ctx.llfunc_info.func

let get_llbb (fgen_ctx : fgen_ctx) (bbid : bbid) : llbasicblock =
  try Hashtbl.find fgen_ctx.bb_env bbid
  with Not_found -> raise (LlvmgenError ("get_llbb: bb not found in env: " ^ string_of_int bbid))

let get_llssa (fgen_ctx : fgen_ctx) (ssaid : ssaid) : llvalue =
  match fgen_ctx.ssa_env.(ssaid) with
  | Some llval -> llval
  | None -> raise (LlvmgenError ("get_llssa: ssa not found in env: " ^ string_of_int ssaid))

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

let rec copy_vec (ctx : proggen_ctx) (builder : llbuilder) (llfunc : llvalue) (mirtyp : mirtyp) ( origvec : llvalue) : llvalue =
  match mirtyp with
  | TMIRUnit | TMIRI32 | TMIRI8 | TMIRClos _ | TMIRTup _ -> raise (LlvmgenError "copy_vec non vec mirtyp passed")
  | TMIRVec (0, _) -> raise (LlvmgenError "copy_vec vec with dim 0")
  | TMIRVec (1, inner_mirtyp) -> (
    let origvec_ptr = Llvm.build_extractvalue origvec 0 "vellen" builder in
    let origvec_len = Llvm.build_extractvalue origvec 1 "vellen" builder in
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
    let origvec_ptr = Llvm.build_extractvalue origvec 0 "vellen" builder in
    let origvec_len = Llvm.build_extractvalue origvec 1 "vellen" builder in
    let len_i64 = Llvm.build_zext origvec_len ctx.i64_t "len_i64" builder in
    let vec_bytesz = Llvm.size_of ctx.vec_t in
    let bytesz = build_mul len_i64 vec_bytesz "bytesz" builder in

    (*malloc new vec memory*)
    let copyvec_ptr = build_call ctx.malloc_t ctx.malloc_func [| bytesz |] "copyvec_ptr" builder in

    (*decl loop bbs*)
    (* TO DO Make nice placement *)
    let header_bb = append_block ctx.llcontext ("copy_vec_header_dim_" ^ string_of_int n) llfunc in
    let loop_bb = append_block ctx.llcontext ("copy_vec_loop_dim_" ^ string_of_int n) llfunc in
    let exit_bb = append_block ctx.llcontext ("copy_vec_exit_dim_" ^ string_of_int n) llfunc in
    let current_bb = insertion_block builder in

    (*branch to loop header*)
    ignore (build_br header_bb builder);

    (*loop header*)
    position_at_end header_bb builder;
    let i64_zero = Llvm.const_int ctx.i64_t 0 in
    let phi_index = build_phi [(i64_zero, current_bb)] "copy_vec_index" builder in
    let cond = build_icmp Icmp.Slt phi_index len_i64 "copy_vec_cond" builder in
    ignore (build_cond_br cond loop_bb exit_bb builder);

    (*loop body*)
    position_at_end loop_bb builder;
    let origvec_elm_ptr = build_in_bounds_gep ctx.vec_t origvec_ptr [| phi_index |] "origvec_elm_ptr" builder in
    let orig_elm = build_load ctx.vec_t origvec_elm_ptr "elm" builder in
    let copy_elm = copy_vec ctx builder llfunc (TMIRVec (n-1, inner_mirtyp)) orig_elm in
    let copyvec_elm_ptr = build_in_bounds_gep ctx.vec_t copyvec_ptr [| phi_index |] "copyvec_elm_ptr" builder in
    ignore (build_store copy_elm copyvec_elm_ptr builder);
    let next_index = build_add phi_index (Llvm.const_int ctx.i64_t 1) "next_index" builder in
    ignore (add_incoming (next_index, loop_bb) phi_index);
    ignore (build_br header_bb builder);

    (*loop exit*)
    position_at_end exit_bb builder;
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

let rec copy_tup (ctx : proggen_ctx) (builder : llbuilder) (llfunc : llvalue) (mirtyp : mirtyp) ( origclos : llvalue) : llvalue =
  match mirtyp with
  | TMIRUnit | TMIRI32 | TMIRI8 | TMIRClos _ | TMIRVec _ -> raise (LlvmgenError "copy_tup non vec mirtyp passed")
  | TMIRTup elms -> (
    let copy_elms = List.mapi (fun i elm_mirtyp ->
      let orig_elm = build_extractvalue origclos i ("tup_elm_" ^ string_of_int i) builder in
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
    let clos_data_struct = Llvm.struct_type ctx.llcontext args_lltyps in
    let args_llvalues = 
      Array.init (Array.length args_lltyps) (fun i ->
      let argptr = build_gep clos_data_struct clos_dataptr [| const_int ctx.i32_t 0; const_int ctx.i32_t i |] ("argptr_" ^ string_of_int i) builder in
      build_load args_lltyps.(i) argptr ("arg_" ^ string_of_int i) builder ) 
    in
    let ret_val = build_call llfunc_info.func_t llfunc_info.func args_llvalues "ret_val" builder in
    ignore (build_ret ret_val builder);
    llfunc_info.closwrpr <- Some closwrpr_func;
    closwrpr_func
  )

let get_clos_helpers (ctx : proggen_ctx) (args_mirtyp : mirtyp list) : clos_helper_info =
  
    (*
    creates the following llvm code

    bbcurr:
      ...
      branch bb0

    bb0:
      cond0 = i64cmp gt off 0
      cond_branch cond0 bb1 bbend

    bb1:
      effects of (fgen builder 0)
      cond1 = i64cmp gt off 1
      cond_branch cond1 bb2 bbend

    ...

    bbmaxoff:
      effects of (fgen builder (maxoff-1))
      branch bbend

    bbend:

  *)
  let gen_ladder (builder : llbuilder) (llfunc : llvalue) (fgen : llbuilder -> int -> unit) (off : llvalue) (maxoff : int) =
    assert (maxoff > 0); (* empty closures should not exits *)

    (* Phase 1: create bbs and call f *)
    let bbcurr = insertion_block builder in

    let bb0 = append_block ctx.llcontext "bb0" llfunc in

    let i = ref 1 in
    let bblst = ref [bb0] in
    while !i <= maxoff do
      let bbi = append_block ctx.llcontext ("bb" ^ string_of_int !i) llfunc in
      position_at_end bbi builder;
      fgen builder (!i - 1);
      i := !i + 1;
      bblst := bbi :: !bblst;
    done;

    let bbmaxoff, bbs_wcondbranch = 
      match !bblst with
      | h :: tl -> h, List.rev tl
      | [] -> raise (LlvmgenError "aux: empty bblst (internal should not happen)")
    in

    let condbranch_bbpairs = 
      match List.rev !bblst with
      | h :: tl -> List.combine bbs_wcondbranch tl
      | [] -> raise (LlvmgenError "aux: empty bblst (internal should not happen)")
    in

    let bbend = append_block ctx.llcontext "bbend" llfunc in

    (* Phase 2: add the terminators *)
    position_at_end bbcurr builder;
    ignore (build_br bb0 builder);

    List.iteri (fun i (bb, next_bb) ->
      position_at_end bb builder;
      let cond = build_icmp Icmp.Sgt off (const_int ctx.i64_t i) ("cond_" ^ string_of_int i) builder in
      ignore (build_cond_br cond next_bb bbend builder)
    ) condbranch_bbpairs;

    position_at_end bbmaxoff builder;
    ignore (build_br bbend builder);

    position_at_end bbend builder;
    
  in

  match Hashtbl.find_opt ctx.closhelper_env args_mirtyp with
  | Some clos_helpers -> clos_helpers
  | None -> (

    (*COPY FUNC*)
    (*declare copy func*)
    let copy_func_t = function_type ctx.ptr_t [| ctx.ptr_t ; ctx.i64_t |] in
    let copy_func = declare_function "clos_copy_func" copy_func_t ctx.llmodule in
    let builder = builder ctx.llcontext in
    let entry_bb = append_block ctx.llcontext "entry" copy_func in
    position_at_end entry_bb builder;

    (*alloc new data memory*)
    let args_mirtyp_arr = Array.of_list args_mirtyp in
    let args_lltype_arr = Array.of_list @@ List.map (mirtyp_get_lltyp ctx) args_mirtyp in
    let clos_data_struct = Llvm.struct_type ctx.llcontext args_lltype_arr in
    let clos_data_size = Llvm.size_of clos_data_struct in
    let clos_data_copy_ptr = build_call ctx.malloc_t ctx.malloc_func [| clos_data_size |] "clos_data_copy_ptr" builder in
    let clos_data_ptr = param copy_func 0 in

    let copy_arg_gen (builder : llbuilder) (i : int) =
      let arg_orig_ptr = build_gep clos_data_struct clos_data_ptr [| const_int ctx.i32_t 0; const_int ctx.i32_t i |] ("argptr_" ^ string_of_int i) builder in
      let arg_orig_val = build_load args_lltype_arr.(i) arg_orig_ptr ("arg_" ^ string_of_int i) builder in
      let arg_copy_val = copy ctx builder copy_func args_mirtyp_arr.(i) arg_orig_val in
      let arg_copy_ptr = build_gep clos_data_struct clos_data_copy_ptr [| const_int ctx.i32_t 0; const_int ctx.i32_t i |] ("argcopyptr_" ^ string_of_int i) builder in
      ignore (build_store arg_copy_val arg_copy_ptr builder)
    in
    gen_ladder builder copy_func copy_arg_gen (param copy_func 1) (List.length args_mirtyp);

    ignore (build_ret clos_data_copy_ptr builder);

    (*DROP FUNC*)
    (*declare drop func*)
    {
      signature = args_mirtyp;
      copy_func;
      drop_func = copy_func;
    }
  )

let lower_op (ctx : proggen_ctx) (fgen_ctx : fgen_ctx) (mirfunc : func) (mirop : Mir.op) : unit =
  match mirop with
  | Func (def_ssaid, borr_funcid_ref, own_funcid_opt_ref) -> (
    (*gen on demand / get clos wrappers for funcs*)
    let borr_closwrpr = get_clos_wrapper ctx !borr_funcid_ref in
    if Option.is_none !own_funcid_opt_ref then raise (LlvmgenError "lower_op: own funcid not implemented yet");
    let own_closwrpr = get_clos_wrapper ctx (Option.get !own_funcid_opt_ref) in

    (*build closure struct*)
    let clos_mirtyp = get_mirtyp_func mirfunc def_ssaid in
    let args_mirtyp = 
      match clos_mirtyp with
      | TMIRClos (args, ret) -> args
      | _ -> raise (LlvmgenError "mir ssa def has non clos type after func op")
    in

    (*gen copy and drop helpers *)
    let clos_helpers = get_clos_helpers ctx args_mirtyp in
    ignore (clos_helpers); ignore (borr_closwrpr); ignore (own_closwrpr);

    (*alloc data memory*)

    (* assemble closure *)
    ()
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
      fgen_ctx.ssa_env.(ssaid) <- Some phi_node
    ) mirbb.args;

    (*lower all ops*)
    List.iter (fun mirop ->
      ignore (lower_op ctx fgen_ctx mirfunc mirop)
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

  (*setup the codegen context*)
  let llcontext = global_context () in
  let llmodule = create_module llcontext "intlang_module" in

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