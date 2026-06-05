open Ast
open Llvm
open Errors
open PrintIntlang

type codegen_ctx = {
  llcontext : llcontext;
  llmodule  : llmodule;
  llbuilder : llbuilder;
  i32_t  : lltype;
  i64_t : lltype;
  vec_struct_t : lltype;
  ptr_t : lltype;
  malloc_t : lltype;
  malloc_func : llvalue;
}

module StringMap = Map.Make(String)
type env = Llvm.llvalue StringMap.t (*mb rename to something clearer that env*)

let rec lower_lexp_to_llvm (ctx : codegen_ctx) (env : env) (e : Ast.lexp) : llvalue =
  match e with
  | Ast.Int n -> 
    const_int ctx.i32_t n
  | Ast.Bop (op, e1, e2) ->
    let lhs = lower_lexp_to_llvm ctx env e1 in
    let rhs = lower_lexp_to_llvm ctx env e2 in
    (
      match op with
      | Ast.Add -> build_add lhs rhs "add_tmp" ctx.llbuilder
      | Ast.Sub -> build_sub lhs rhs "sub_tmp" ctx.llbuilder
      | Ast.Mul -> build_mul lhs rhs "mul_tmp" ctx.llbuilder
      | Ast.Div -> build_sdiv lhs rhs "div_tmp" ctx.llbuilder
      | Ast.Eq  -> 
        (*in intlang Eq and Lt are bops in ints like + or - so we zero extend to i32*)
        let val_i1 = build_icmp Icmp.Eq lhs rhs "eq_tmp" ctx.llbuilder in 
        build_zext val_i1 ctx.i32_t "eq_ext_tmp" ctx.llbuilder
      | Ast.Lt ->  
        let val_i1 = build_icmp Icmp.Slt lhs rhs "lt_tmp" ctx.llbuilder in
        build_zext val_i1 ctx.i32_t "lt_ext_tmp" ctx.llbuilder
    )
  | Ast.Letin (x, exp, body) -> 
    let v1 = lower_lexp_to_llvm ctx env exp in
    let ptr_x = build_alloca ctx.ptr_t x ctx.llbuilder in
    ignore (build_store v1 ptr_x ctx.llbuilder);
    let new_env = StringMap.add x ptr_x env in
    lower_lexp_to_llvm ctx new_env body
  | Ast.Var x ->
    let ptr_x = StringMap.find x env in
    build_load ctx.ptr_t ptr_x ("load_tmp_" ^ x)  ctx.llbuilder
  | Ast.If (cond, ifblk, elseblk) ->

    (*create new blocks*)
    let caller = block_parent (insertion_block ctx.llbuilder) in
    let then_block = append_block ctx.llcontext "then" caller in
    let else_block = append_block ctx.llcontext "else" caller in
    let merge_block = append_block ctx.llcontext "merge" caller in

    (*eval cond and branch*)
    let cond_val = lower_lexp_to_llvm ctx env cond in
    let zero = Llvm.const_int ctx.i32_t 0 in
    let cond_val_i1 = Llvm.build_icmp Llvm.Icmp.Ne cond_val zero "is_nonzero" ctx.llbuilder in
    ignore (build_cond_br cond_val_i1 then_block else_block ctx.llbuilder);

    (*then block*)
    position_at_end then_block ctx.llbuilder;
    let then_val = lower_lexp_to_llvm ctx env ifblk in
    ignore (build_br merge_block ctx.llbuilder);
    let incoming_then_blk = insertion_block ctx.llbuilder in

    (*else block*)
    position_at_end else_block ctx.llbuilder;
    let else_val = lower_lexp_to_llvm ctx env elseblk in
    ignore (build_br merge_block ctx.llbuilder);
    let incoming_else_blk = insertion_block ctx.llbuilder in

    (*merge block*)
    position_at_end merge_block ctx.llbuilder;
    let phi = build_phi [(then_val, incoming_then_blk); (else_val, incoming_else_blk)] "iftmp" ctx.llbuilder in

    (*reorder blocks*)
    Llvm.move_block_after incoming_then_blk else_block;
    Llvm.move_block_after incoming_else_blk merge_block;
    phi

  | Ast.Vecmk (defval, cnt) ->

    (*eval defval and cnt*)
    let defval_llvm = lower_lexp_to_llvm ctx env defval in
    let cnt_i32 = lower_lexp_to_llvm ctx env cnt in
    let cnt_i64 = build_sext cnt_i32 ctx.i64_t "cnt_i64" ctx.llbuilder in

    (*malloc for vec struct and data array*)
    let vec_struct_size = size_of ctx.vec_struct_t in (*size of vec struct in bytes*)
    let vec_size = build_mul (size_of ctx.i32_t) cnt_i64 "vec_total_size" ctx.llbuilder in (*for now just assume defval is an int*)
    let vec_struct_ptr = build_call ctx.malloc_t ctx.malloc_func [| vec_struct_size |] "vec_struct_malloc" ctx.llbuilder in
    let vec_ptr = build_call ctx.malloc_t ctx.malloc_func [| vec_size |] "vec_malloc" ctx.llbuilder in

    (*store length*)
    let len_field_ptr = build_gep ctx.vec_struct_t vec_struct_ptr [| const_int ctx.i64_t 0; const_int ctx.i32_t 0 |] "len_ptr" ctx.llbuilder in
    ignore (build_store cnt_i32 len_field_ptr ctx.llbuilder);

    (*store data pointer*)
    let data_field_ptr = build_gep ctx.vec_struct_t vec_struct_ptr [| const_int ctx.i64_t 0; const_int ctx.i32_t 1 |] "data_ptr" ctx.llbuilder in
    ignore (build_store vec_ptr data_field_ptr ctx.llbuilder);

    (*set defval*)
    let caller = block_parent (insertion_block ctx.llbuilder) in
    let vecmk_loop_body = append_block ctx.llcontext "vecmk_loop_body" caller in
    let vecmk_merge = append_block ctx.llcontext "vecmk_merge" caller in
    let idx_entry = const_int ctx.i64_t 0 in
    ignore (build_br vecmk_loop_body ctx.llbuilder);
    let incoming_blk = insertion_block ctx.llbuilder in
    position_at_end vecmk_loop_body ctx.llbuilder;
    let idx_phi = build_phi [(idx_entry, incoming_blk)] "vecmk_idx_phi" ctx.llbuilder in
    let elem_ptr = build_gep ctx.i32_t vec_ptr [| idx_phi |] "elem_ptr" ctx.llbuilder in
    ignore (build_store defval_llvm elem_ptr ctx.llbuilder);
    let idx_next = build_add idx_phi (const_int ctx.i64_t 1) "idx_next" ctx.llbuilder in
    let cond = build_icmp Icmp.Slt idx_next cnt_i64 "loop_cond" ctx.llbuilder in
    ignore (build_cond_br cond vecmk_loop_body vecmk_merge ctx.llbuilder);
    add_incoming (idx_next, vecmk_loop_body) idx_phi;
    position_at_end vecmk_merge ctx.llbuilder;

    vec_struct_ptr

  | Ast.Veclen vec_exp ->
    let vec_llvm = lower_lexp_to_llvm ctx env vec_exp in
    let len_field_ptr = build_gep ctx.vec_struct_t vec_llvm [| const_int ctx.i64_t 0; const_int ctx.i32_t 0 |] "len_ptr" ctx.llbuilder in
    build_load ctx.i32_t len_field_ptr "vec_len" ctx.llbuilder

  | Ast.Vecget (vec_exp, idx_exp) ->
    let vec_llvm = lower_lexp_to_llvm ctx env vec_exp in
    let idx_llvm = lower_lexp_to_llvm ctx env idx_exp in
    let data_field_ptr = build_gep ctx.vec_struct_t vec_llvm [| const_int ctx.i64_t 0; const_int ctx.i32_t 1 |] "data_ptr" ctx.llbuilder in
    let data_ptr = build_load ctx.ptr_t data_field_ptr "data_ptr" ctx.llbuilder in
    let elem_ptr = build_gep ctx.i32_t data_ptr [| build_sext idx_llvm ctx.i64_t "idx_i64" ctx.llbuilder |] "elem_ptr" ctx.llbuilder in
    build_load ctx.i32_t elem_ptr "vec_elem" ctx.llbuilder

  | _ -> raise (CodegenError "Expression type not yet supported in codegen")

let lower_prog_to_llvm ( (_ , final_exp_opt) : prog) : llmodule =

  (*get things for the codegen context*)
  let llcontext = global_context () in
  let llmodule = create_module llcontext "intlang_module" in
  let llbuilder = builder llcontext in
  let i32_t = i32_type llcontext in
  let i64_t = i64_type llcontext in
  let ptr_t    = Llvm.pointer_type llcontext in
  let vec_struct_t = Llvm.struct_type llcontext [| i32_t; ptr_t |] in
  let malloc_t   = Llvm.function_type (ptr_t) [| i64_t |] in
  let malloc_func = Llvm.declare_function "malloc" malloc_t llmodule in

  let ctx = {
    llcontext;
    llmodule;
    llbuilder;
    i32_t;
    i64_t;
    ptr_t;
    vec_struct_t;
    malloc_t;
    malloc_func;
  } in

  (*create main and set curser at entry*)
  let main_type = function_type i32_t [||] in
  let main_fn = declare_function "main" main_type llmodule in
  let bb = append_block ctx.llcontext "entry" main_fn in
  position_at_end bb ctx.llbuilder;

  match final_exp_opt with
  | None -> raise (CodegenError "No final expression to generate code for")
  | Some final_exp ->
    let result = lower_lexp_to_llvm ctx StringMap.empty final_exp in
    ignore (build_ret result ctx.llbuilder);
    llmodule

let sprint_lower_prog_to_llvm (p : prog) : string =
  let llvm_module = lower_prog_to_llvm p in
  string_of_llmodule llvm_module

let lower_llvm_to_bin_clang (llvm_ir : string) (binary_name : string) : int =
  let ir_filename = binary_name ^ ".ll" in
  
  let oc = open_out ir_filename in
  output_string oc llvm_ir;
  close_out oc;
  
  let cmd = Printf.sprintf "clang-19 %s -o %s" ir_filename binary_name in
  let exit_code = Sys.command cmd in
  Sys.remove ir_filename;
  exit_code