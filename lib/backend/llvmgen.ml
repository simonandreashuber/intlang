open Mir
open Analysis (*process bbs in rpo *)

open Llvm
open Errors

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

  func_env : (funcid, llfunc_info) Hashtbl.t;                     (* mir func -> llvm func *)
  closhelper_env : (mirtyp list, clos_helper_info) Hashtbl.t;     (* closure data layout *)
}

let ctx_add_llfunc_info (ctx : proggen_ctx) (funcid : funcid) (llfunc_info : llfunc_info) =
  Hashtbl.add ctx.func_env funcid llfunc_info

type funcgen_ctx = {
  proggen_ctx : proggen_ctx;
  builder : llbuilder;
  ssa_env : Llvm.llvalue option array;
  bb_env  : (bbid, Llvm.llbasicblock) Hashtbl.t;
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

let create_funcgen_ctx (ctx : proggen_ctx) (func : Mir.func) =
  let builder = builder ctx.llcontext in
  let ssa_env = Array.make func.next_ssaid None in
  let bb_env = Hashtbl.create (BBMap.cardinal func.bbs) in
  { proggen_ctx = ctx; builder; ssa_env; bb_env }


let decl_func (ctx : proggen_ctx) (mirfunc : Mir.func) : unit =
  let args_mirtyps = List.map (fun (ssaid, _) -> get_mirtyp_func mirfunc ssaid) mirfunc.args in
  let ret_mirtyp = mirfunc.rettyp in
  let args_lltyps = mirtyplst_get_lltyparr ctx args_mirtyps in
  let ret_lltyp = mirtyp_get_lltyp ctx ret_mirtyp in
  let llfunc_t = function_type ret_lltyp args_lltyps in
  let llfunc = declare_function (string_of_int mirfunc.funcid) llfunc_t ctx.llmodule in
  ctx_add_llfunc_info ctx mirfunc.funcid { mir_funcid = mirfunc.funcid; func_t = llfunc_t; func = llfunc; closwrpr = None; }

let lower_func (ctx : proggen_ctx) (mirfunc : Mir.func) : unit =
  let funcgen_ctx = create_funcgen_ctx ctx mirfunc in
  let llfunc_info = find_llfunc_info ctx mirfunc.funcid in
  let llfunc = llfunc_info.func in 

  ignore (funcgen_ctx); ignore (llfunc); ()

let lower_mir ( p : Mir.program) : llmodule =

  (*setup the codegen context*)
  let llcontext = global_context () in
  let llmodule = create_module llcontext "intlang_module" in

  let void_t = void_type llcontext in
  let unit_t = Llvm.struct_type llcontext [||] in
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

  let func_env = Hashtbl.create 32 in
  let closhelper_env = Hashtbl.create 32 in

  let ctx = {
    llcontext;
    llmodule;
    void_t;
    unit_t;
    i8_t;
    i32_t;
    i64_t;
    ptr_t;
    vec_t;
    clos_t;
    malloc_t;
    malloc_func;
    free_t;
    free_func;
    func_env;
    closhelper_env;
  } in

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