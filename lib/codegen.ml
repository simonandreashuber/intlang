open Ast
open Llvm
open Errors
open PrintIntlang

type codegen_ctx = {
  llcontext : llcontext;
  llmodule  : llmodule;
  llbuilder : llbuilder;
  i32_type  : lltype;
}

module StringMap = Map.Make(String)
type env = Llvm.llvalue StringMap.t (*mb rename to something clearer that env*)

let rec lower_lexp_to_llvm (ctx : codegen_ctx) (env : env) (e : Ast.lexp) : llvalue =
  match e with
  | Ast.Int n -> 
    const_int ctx.i32_type n
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
        build_zext val_i1 ctx.i32_type "eq_ext_tmp" ctx.llbuilder
      | Ast.Lt ->  
        let val_i1 = build_icmp Icmp.Slt lhs rhs "lt_tmp" ctx.llbuilder in
        build_zext val_i1 ctx.i32_type "lt_ext_tmp" ctx.llbuilder
    )
  | Ast.Letin (x, exp, body) -> 
    let v1 = lower_lexp_to_llvm ctx env exp in
    let ptr_x = build_alloca ctx.i32_type x ctx.llbuilder in
    ignore (build_store v1 ptr_x ctx.llbuilder);
    let new_env = StringMap.add x ptr_x env in
    lower_lexp_to_llvm ctx new_env body
  | Ast.Var x ->
    let ptr_x = StringMap.find x env in
    build_load ctx.i32_type ptr_x ("load_tmp_" ^ x)  ctx.llbuilder
  | Ast.If (cond, ifblk, elseblk) ->
    (*create new blocks*)
    let caller = block_parent (insertion_block ctx.llbuilder) in
    let then_block = append_block ctx.llcontext "then" caller in
    let else_block = append_block ctx.llcontext "else" caller in
    let merge_block = append_block ctx.llcontext "merge" caller in
    (*eval cond and branch*)
    let cond_val = lower_lexp_to_llvm ctx env cond in
    let zero = Llvm.const_int ctx.i32_type 0 in
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
  | _ -> raise (CodegenError "Expression type not yet supported in codegen")

let lower_prog_to_llvm ( (_ , final_exp_opt) : prog) : llmodule =
  let context = global_context () in
  let the_module = create_module context "intlang_module" in
  let builder = builder context in
  let i32_t = i32_type context in

  let ctx = {
    llcontext = context;
    llmodule = the_module;
    llbuilder = builder;
    i32_type = i32_t;
  } in

  let main_type = function_type i32_t [||] in
  let main_fn = declare_function "main" main_type the_module in
  
  let bb = append_block context "entry" main_fn in
  position_at_end bb builder;

  match final_exp_opt with
  | None -> raise (CodegenError "No final expression to generate code for")
  | Some final_exp ->
    let result = lower_lexp_to_llvm ctx StringMap.empty final_exp in
    ignore (build_ret result builder);
    the_module

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