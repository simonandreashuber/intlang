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
  closure_struct_t : lltype;
  ptr_t : lltype;
  malloc_t : lltype;
  malloc_func : llvalue;
}

module UUIDMap = Map.Make(Int)
type env = Llvm.llvalue UUIDMap.t (*mb rename to something clearer that env*)
type capturedvars = (uuid * (lltype * llvalue)) list

let get_lltype_of_typ (ctx : codegen_ctx) (t : typ) : lltype =
  match repr t with
  | TInt -> ctx.i32_t
  | TVec _ -> ctx.ptr_t
  | TFun _ -> ctx.ptr_t
  | _ -> raise (CodegenError "Unsupported type in codegen")

let get_llfuntype_of_funtyp (ctx : codegen_ctx) (ft : typ) : lltype =
  match repr ft with
  | TFun (intyp,outtyp) -> function_type (get_lltype_of_typ ctx outtyp) [| ctx.ptr_t ; get_lltype_of_typ ctx intyp |]
  | _ -> raise (CodegenError "Unsupported function type in codegen")

let capture_scan (ctx : codegen_ctx) (env : env) (e : Ast.lexpt) : capturedvars =
  let capvars = ref UUIDMap.empty in
  let rec capture_scan_aux (e : Ast.lexpt) : unit =
    match e with
    | Ast.VarT (_, uuidref, typ) ->
        let id = !uuidref in
        if not (UUIDMap.mem id !capvars) then (
          match UUIDMap.find_opt id env with
          | Some llval -> 
              capvars := UUIDMap.add id ((get_lltype_of_typ ctx typ), llval) !capvars
          | None -> ()
        )
    | Ast.LamT (_, _, e, _) -> capture_scan_aux e
    | Ast.AppT (e1, e2, _) -> 
        capture_scan_aux e1;
        capture_scan_aux e2
    | Ast.IntT (_, _) -> ()
    | Ast.BopT (_, e1, e2, _) ->
        capture_scan_aux e1;
        capture_scan_aux e2
    | Ast.IfT (e1, e2, e3, _) ->
        capture_scan_aux e1;
        capture_scan_aux e2;
        capture_scan_aux e3
    | Ast.LetinT (_, _, e1, e2, _) ->
        capture_scan_aux e1;
        capture_scan_aux e2
    | Ast.VeclitT (es, _) ->
        List.iter capture_scan_aux es
    | Ast.VecmkT (e1, e2, _) ->
        capture_scan_aux e1;
        capture_scan_aux e2
    | Ast.VeclenT (e, _) -> capture_scan_aux e
    | Ast.VecgetT (e1, e2, _) ->
        capture_scan_aux e1;
        capture_scan_aux e2
    | Ast.VecsetT (e1, e2, e3, _) ->
        capture_scan_aux e1;
        capture_scan_aux e2;
        capture_scan_aux e3
    in
  capture_scan_aux e;
  UUIDMap.bindings !capvars
  

let rec lower_lexpt_to_llvm (ctx : codegen_ctx) (env : env) (e : Ast.lexpt) : llvalue =
  match e with
  | Ast.IntT (n, _) -> 
    const_int ctx.i32_t n
  | Ast.BopT (op, e1, e2, _) ->
    let lhs = lower_lexpt_to_llvm ctx env e1 in
    let rhs = lower_lexpt_to_llvm ctx env e2 in
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
  | Ast.LetinT (name, uuid, exp, body, _) -> 
    let v1lltyp = get_lltype_of_typ ctx (lexpt_get_type exp) in
    let v1 = lower_lexpt_to_llvm ctx env exp in
    (*store v1 on the stack*)
    let ptr_x = build_alloca v1lltyp name ctx.llbuilder in
    ignore (build_store v1 ptr_x ctx.llbuilder);
    (*keep track where v1 is stored in the compiler*)
    let new_env = UUIDMap.add uuid ptr_x env in
    lower_lexpt_to_llvm ctx new_env body

  | Ast.VarT (nameref, uuidref, vartyp) ->
    (*lookup where var was stored and load it*)
    let varlltyp = get_lltype_of_typ ctx vartyp in
    let ptr_x = UUIDMap.find !uuidref env in
    build_load varlltyp ptr_x ("load_tmp_" ^ !nameref)  ctx.llbuilder
  | Ast.IfT (cond, ifblk, elseblk, _) ->

    (*create new blocks*)
    let caller = block_parent (insertion_block ctx.llbuilder) in
    let then_block = append_block ctx.llcontext "then" caller in
    let else_block = append_block ctx.llcontext "else" caller in
    let merge_block = append_block ctx.llcontext "merge" caller in

    (*eval cond and branch*)
    let cond_val = lower_lexpt_to_llvm ctx env cond in
    let zero = Llvm.const_int ctx.i32_t 0 in
    let cond_val_i1 = Llvm.build_icmp Llvm.Icmp.Ne cond_val zero "is_nonzero" ctx.llbuilder in
    ignore (build_cond_br cond_val_i1 then_block else_block ctx.llbuilder);

    (*then block*)
    position_at_end then_block ctx.llbuilder;
    let then_val = lower_lexpt_to_llvm ctx env ifblk in
    ignore (build_br merge_block ctx.llbuilder);
    let incoming_then_blk = insertion_block ctx.llbuilder in

    (*else block*)
    position_at_end else_block ctx.llbuilder;
    let else_val = lower_lexpt_to_llvm ctx env elseblk in
    ignore (build_br merge_block ctx.llbuilder);
    let incoming_else_blk = insertion_block ctx.llbuilder in

    (*merge block*)
    position_at_end merge_block ctx.llbuilder;
    let phi = build_phi [(then_val, incoming_then_blk); (else_val, incoming_else_blk)] "iftmp" ctx.llbuilder in

    (*reorder blocks*)
    Llvm.move_block_after incoming_then_blk else_block;
    Llvm.move_block_after incoming_else_blk merge_block;
    phi

  | Ast.VecmkT (defval, cnt, _) ->

    (*eval defval and cnt*)
    let defvallltyp = get_lltype_of_typ ctx (lexpt_get_type defval) in
    let defval_llvm = lower_lexpt_to_llvm ctx env defval in
    let cnt_i32 = lower_lexpt_to_llvm ctx env cnt in
    let cnt_i64 = build_sext cnt_i32 ctx.i64_t "cnt_i64" ctx.llbuilder in

    (*malloc for vec struct and data array*)
    let vec_struct_size = size_of ctx.vec_struct_t in (*size of vec struct in bytes*)
    let vec_size = build_mul (size_of defvallltyp) cnt_i64 "vec_total_size" ctx.llbuilder in (*for now just assume defval is an int*)
    let vec_struct_ptr = build_call ctx.malloc_t ctx.malloc_func [| vec_struct_size |] "vec_struct_malloc" ctx.llbuilder in
    let vec_ptr = build_call ctx.malloc_t ctx.malloc_func [| vec_size |] "vec_malloc" ctx.llbuilder in

    (*store length*)
    let len_field_ptr = build_gep ctx.vec_struct_t vec_struct_ptr [| const_int ctx.i64_t 0; const_int ctx.i32_t 0 |] "len_ptr" ctx.llbuilder in
    ignore (build_store cnt_i32 len_field_ptr ctx.llbuilder);

    (*store data pointer*)
    let data_field_ptr = build_gep ctx.vec_struct_t vec_struct_ptr [| const_int ctx.i64_t 0; const_int ctx.i32_t 1 |] "data_ptr" ctx.llbuilder in
    ignore (build_store vec_ptr data_field_ptr ctx.llbuilder);

    (*set defval*)
    (*create loop blocks*)
    let caller = block_parent (insertion_block ctx.llbuilder) in
    let vecmk_loop_body = append_block ctx.llcontext "vecmk_loop_body" caller in
    let vecmk_merge = append_block ctx.llcontext "vecmk_merge" caller in
    (*loopentry*)
    let idx_entry = const_int ctx.i64_t 0 in
    ignore (build_br vecmk_loop_body ctx.llbuilder);
    let incoming_blk = insertion_block ctx.llbuilder in
    (*loop body*)
    position_at_end vecmk_loop_body ctx.llbuilder;
    let idx_phi = build_phi [(idx_entry, incoming_blk)] "vecmk_idx_phi" ctx.llbuilder in
    let elem_ptr = build_gep defvallltyp vec_ptr [| idx_phi |] "elem_ptr" ctx.llbuilder in
    ignore (build_store defval_llvm elem_ptr ctx.llbuilder);
    let idx_next = build_add idx_phi (const_int ctx.i64_t 1) "idx_next" ctx.llbuilder in
    let cond = build_icmp Icmp.Slt idx_next cnt_i64 "loop_cond" ctx.llbuilder in
    ignore (build_cond_br cond vecmk_loop_body vecmk_merge ctx.llbuilder);
    add_incoming (idx_next, vecmk_loop_body) idx_phi;
    position_at_end vecmk_merge ctx.llbuilder;

    vec_struct_ptr

  | Ast.VeclenT (vec_exp, _) ->
    let vec_llvm = lower_lexpt_to_llvm ctx env vec_exp in
    let len_field_ptr = build_gep ctx.vec_struct_t vec_llvm [| const_int ctx.i64_t 0; const_int ctx.i32_t 0 |] "len_ptr" ctx.llbuilder in
    build_load ctx.i32_t len_field_ptr "vec_len" ctx.llbuilder

  | Ast.VecgetT (vec_exp, idx_exp, vecoftyp) ->
    let vecoflltyp = get_lltype_of_typ ctx vecoftyp in
    let vec_llvm = lower_lexpt_to_llvm ctx env vec_exp in
    let idx_llvm = lower_lexpt_to_llvm ctx env idx_exp in
    let data_field_ptr = build_gep ctx.vec_struct_t vec_llvm [| const_int ctx.i64_t 0; const_int ctx.i32_t 1 |] "data_ptr" ctx.llbuilder in
    let data_ptr = build_load ctx.ptr_t data_field_ptr "data_ptr" ctx.llbuilder in
    let elem_ptr = build_gep vecoflltyp data_ptr [| build_sext idx_llvm ctx.i64_t "idx_i64" ctx.llbuilder |] "elem_ptr" ctx.llbuilder in
    build_load vecoflltyp elem_ptr "vec_elem" ctx.llbuilder

  | Ast.VeclitT (lst, _) ->
    let lst_llvals = List.map (lower_lexpt_to_llvm ctx env) lst in

    let vecoflltyp = get_lltype_of_typ ctx (lexpt_get_type (List.hd lst)) in
    let cnt_i32 = const_int ctx.i32_t (List.length lst) in
    let cnt_i64 = const_int ctx.i64_t (List.length lst) in

    (*malloc for vec struct and data array*)
    let vec_struct_size = size_of ctx.vec_struct_t in (*size of vec struct in bytes*)
    let vec_size = build_mul (size_of vecoflltyp) cnt_i64 "vec_total_size" ctx.llbuilder in (*for now just assume defval is an int*)
    let vec_struct_ptr = build_call ctx.malloc_t ctx.malloc_func [| vec_struct_size |] "vec_struct_malloc" ctx.llbuilder in
    let vec_ptr = build_call ctx.malloc_t ctx.malloc_func [| vec_size |] "vec_malloc" ctx.llbuilder in

    (*store length*)
    let len_field_ptr = build_gep ctx.vec_struct_t vec_struct_ptr [| const_int ctx.i64_t 0; const_int ctx.i32_t 0 |] "len_ptr" ctx.llbuilder in
    ignore (build_store cnt_i32 len_field_ptr ctx.llbuilder);

    (*store data pointer*)
    let data_field_ptr = build_gep ctx.vec_struct_t vec_struct_ptr [| const_int ctx.i64_t 0; const_int ctx.i32_t 1 |] "data_ptr" ctx.llbuilder in
    ignore (build_store vec_ptr data_field_ptr ctx.llbuilder);

    (*store elements*)
    List.iteri (fun i elem ->
      let elem_ptr = build_gep vecoflltyp vec_ptr [| const_int ctx.i64_t i |] ("elem_ptr_" ^ string_of_int i) ctx.llbuilder in
      ignore (build_store elem elem_ptr ctx.llbuilder);
    ) lst_llvals;

    vec_struct_ptr

| Ast.VecsetT (vec_exp, idx_exp, newval_exp, vectyp) ->
    (*Note on the strategy:
        - Always Copy
        - Shallow Copy, this is valid since all vectors are immutable or in other
          words for every mutation we create a new vector in memory so there should be no problems with this strategy.
    *)

    (*eval defval and cnt*)
    let newvallltyp = get_lltype_of_typ ctx (lexpt_get_type newval_exp) in

    (*calc src vector*)
    let src_vec_ptr = lower_lexpt_to_llvm ctx env vec_exp in

    (*load length*)
    let src_len_field_ptr = build_gep ctx.vec_struct_t src_vec_ptr [| const_int ctx.i64_t 0; const_int ctx.i32_t 0 |] "src_len_ptr" ctx.llbuilder in
    let cnt_i32 = build_load ctx.i32_t src_len_field_ptr "src_cnt_i32" ctx.llbuilder in
    let cnt_i64 = build_sext cnt_i32 ctx.i64_t "src_cnt_i64" ctx.llbuilder in
    let src_data_field_ptr = build_gep ctx.vec_struct_t src_vec_ptr [| const_int ctx.i64_t 0; const_int ctx.i32_t 1 |] "src_data_ptr" ctx.llbuilder in
    let src_data_ptr = build_load ctx.ptr_t src_data_field_ptr "src_data_ptr" ctx.llbuilder in

    (*malloc for vec struct and data array*)
    let vec_struct_size = size_of ctx.vec_struct_t in (*size of vec struct in bytes*)
    let vec_size = build_mul (size_of newvallltyp) cnt_i64 "vec_total_size" ctx.llbuilder in (*for now just assume defval is an int*)
    let vec_struct_ptr = build_call ctx.malloc_t ctx.malloc_func [| vec_struct_size |] "vec_struct_malloc" ctx.llbuilder in
    let vec_ptr = build_call ctx.malloc_t ctx.malloc_func [| vec_size |] "vec_malloc" ctx.llbuilder in

    (*store length*)
    let len_field_ptr = build_gep ctx.vec_struct_t vec_struct_ptr [| const_int ctx.i64_t 0; const_int ctx.i32_t 0 |] "copy_len_ptr" ctx.llbuilder in
    ignore (build_store cnt_i32 len_field_ptr ctx.llbuilder);

    (*store data pointer*)
    let data_field_ptr = build_gep ctx.vec_struct_t vec_struct_ptr [| const_int ctx.i64_t 0; const_int ctx.i32_t 1 |] "copy_data_ptr" ctx.llbuilder in
    ignore (build_store vec_ptr data_field_ptr ctx.llbuilder);

    (*copy elements*)
    (*create loop blocks*)
    let caller = block_parent (insertion_block ctx.llbuilder) in
    let vecset_loop_body = append_block ctx.llcontext "vecset_copy_loop_body" caller in
    let vecset_merge = append_block ctx.llcontext "vecset_copy_merge" caller in
    (*loopentry*)
    let idx_entry = const_int ctx.i64_t 0 in
    ignore (build_br vecset_loop_body ctx.llbuilder);
    let incoming_blk = insertion_block ctx.llbuilder in
    (*loop body*)
    position_at_end vecset_loop_body ctx.llbuilder;
    let idx_phi = build_phi [(idx_entry, incoming_blk)] "vecset_idx_phi" ctx.llbuilder in
    (*load from src vector*)
    let src_elem_ptr = build_gep newvallltyp src_data_ptr [| idx_phi |] "src_elem_ptr" ctx.llbuilder in
    let elem = build_load newvallltyp src_elem_ptr "vec_elem" ctx.llbuilder in
    (*store in new vector copy*)
    let elem_ptr = build_gep newvallltyp vec_ptr [| idx_phi |] "vecset_elem_ptr" ctx.llbuilder in
    ignore (build_store elem elem_ptr ctx.llbuilder);
    let idx_next = build_add idx_phi (const_int ctx.i64_t 1) "vecset_idx_next" ctx.llbuilder in
    let cond = build_icmp Icmp.Slt idx_next cnt_i64 "loop_cond" ctx.llbuilder in
    ignore (build_cond_br cond vecset_loop_body vecset_merge ctx.llbuilder);
    add_incoming (idx_next, vecset_loop_body) idx_phi;
    position_at_end vecset_merge ctx.llbuilder;

    (*store newval*)(*store in new vector copy*)
    let idx_llval = lower_lexpt_to_llvm ctx env idx_exp in
    let newval_llval = lower_lexpt_to_llvm ctx env newval_exp in
    let newval_elem_ptr = build_gep newvallltyp vec_ptr [| idx_llval |] "vecset_elem_ptr" ctx.llbuilder in
    ignore (build_store newval_llval newval_elem_ptr ctx.llbuilder);

    vec_struct_ptr
  
  | Ast.LamT (name, uuid, body, typ) ->

    (*alloc the closure heap struct*)
    let closure_struct_size = size_of ctx.closure_struct_t in (*size of closure struct in bytes*)
    let closure_struct_ptr = build_call ctx.malloc_t ctx.malloc_func [| closure_struct_size |] "closure_struct_malloc" ctx.llbuilder in
    
    (*find captured vars and create an llvm struct type for them*)
    let capvars = capture_scan ctx env body in
    let capvars_lltyps = Array.of_list (List.map (fun (_, (llty, _)) -> llty) capvars) in
    let capvars_struct_t = Llvm.struct_type ctx.llcontext capvars_lltyps in

    (*alloc the captured vars heap struct*)
    let capvars_struct_size = size_of capvars_struct_t in (*size of closure struct in bytes*)
    let capvars_struct_ptr = build_call ctx.malloc_t ctx.malloc_func [| capvars_struct_size |] "capvars_struct_malloc" ctx.llbuilder in
    
    (*store captured vars in captured vars heap struct*)
    List.iteri (fun i (uuid, (llty, llval_ptr)) ->
      let llval = build_load llty llval_ptr ("capvar_val_" ^ string_of_int i) ctx.llbuilder in
      let capvar_ptr = build_gep capvars_struct_t capvars_struct_ptr [| const_int ctx.i64_t 0; const_int ctx.i32_t i |] ("capvar_ptr_" ^ string_of_int i) ctx.llbuilder in
      ignore (build_store llval capvar_ptr ctx.llbuilder);
    ) capvars;

    (*store pointer to captured vars struct in closure struct*)
    let capvars_field_ptr = build_gep ctx.closure_struct_t closure_struct_ptr [| const_int ctx.i64_t 0; const_int ctx.i32_t 1 |] "capvars_field_ptr" ctx.llbuilder in
    ignore (build_store capvars_struct_ptr capvars_field_ptr ctx.llbuilder);
    
    (*get llvm function type*)
    let fun_lltyp = get_llfuntype_of_funtyp ctx typ in

    (*declare llvm function and an entry block for it*)
    let lambda_fn = declare_function ("lambda_" ^ string_of_int uuid) fun_lltyp ctx.llmodule in
    let lambda_bb = append_block ctx.llcontext ("entry_lambda_" ^ string_of_int uuid) lambda_fn in

    (*keep track of old basic block*)
    let old_bb = insertion_block ctx.llbuilder in

    (*switch to new llvm functions entry basic block*)
    position_at_end lambda_bb ctx.llbuilder;

    (*access the captured vars and the lambda param with param*)
    let capvars_ptr = param lambda_fn 0 in
    let lambda_param = param lambda_fn 1 in
    (*hacky: put lambda param on the stack so the Vars compilations stays uniform*)
    let lambda_param_ptr = build_alloca (type_of lambda_param) "lambda_param_alloca_ptr" ctx.llbuilder in
    ignore (build_store lambda_param lambda_param_ptr ctx.llbuilder);


    (*load all things from the captured vars struct*)
    let capvars_loaded = List.mapi (fun i (uuid, (llty, _)) ->
      let capvar_ptr = build_gep capvars_struct_t capvars_ptr [| const_int ctx.i64_t 0; const_int ctx.i32_t i |] ("capvar_ptr_" ^ string_of_int i) ctx.llbuilder in
      (*let capvar_val = build_load llty capvar_ptr ("capvar_val_" ^ string_of_int i) ctx.llbuilder in*)
      (uuid, capvar_ptr)
    ) capvars in

    (*extend the env with the captured vars and lambda param*)
    let newfunenv = List.fold_left (fun acc (uuid, llval) -> UUIDMap.add uuid llval acc) 
                                       (UUIDMap.add uuid lambda_param_ptr env) capvars_loaded in

    (*recursively lower the lambda body*)
    let lambda_body_val = lower_lexpt_to_llvm ctx newfunenv body in

    (*put ret for the bodys return value*)
    ignore (build_ret lambda_body_val ctx.llbuilder);

    (*switch back to the old function and basic block*)
    position_at_end old_bb ctx.llbuilder;

    (*store function pointer in closure struct*)
    let fun_field_ptr = build_gep ctx.closure_struct_t closure_struct_ptr [| const_int ctx.i64_t 0; const_int ctx.i32_t 0 |] "fun_field_ptr" ctx.llbuilder in
    ignore (build_store lambda_fn fun_field_ptr ctx.llbuilder);
    
    closure_struct_ptr
  | Ast.AppT (e1, e2, _) ->
    let fun_llvm = lower_lexpt_to_llvm ctx env e1 in
    let arg_llvm = lower_lexpt_to_llvm ctx env e2 in

    (*extract function pointer and captured vars pointer from closure struct*)
    let fun_field_ptr = build_gep ctx.closure_struct_t fun_llvm [| const_int ctx.i64_t 0; const_int ctx.i32_t 0 |] "fun_field_ptr" ctx.llbuilder in
    let capvars_field_ptr = build_gep ctx.closure_struct_t fun_llvm [| const_int ctx.i64_t 0; const_int ctx.i32_t 1 |] "capvars_field_ptr" ctx.llbuilder in
    let fun_ptr = build_load ctx.ptr_t fun_field_ptr "fun_ptr" ctx.llbuilder in
    let capvars_ptr = build_load ctx.ptr_t capvars_field_ptr "capvars_ptr" ctx.llbuilder in

    (*get fun type*)
    let fun_typ = lexpt_get_type e1 in
    let fun_lltyp = get_llfuntype_of_funtyp ctx fun_typ in

    (*call the function pointer with the captured vars pointer and the argument*)
    build_call fun_lltyp fun_ptr [| capvars_ptr; arg_llvm |] "call_tmp" ctx.llbuilder
  
let lower_prog_to_llvm ( lb : letblkmonot) : llmodule =

  match List.find_opt (fun (name,_,_) -> name = "@main") lb with
  | None -> raise (CodegenError "No @main function found in program")
  | Some (_, _, final_exp) ->

  (*get things for the codegen context*)
  let llcontext = global_context () in
  let llmodule = create_module llcontext "intlang_module" in
  let llbuilder = builder llcontext in
  let i32_t = i32_type llcontext in
  let i64_t = i64_type llcontext in
  let ptr_t    = Llvm.pointer_type llcontext in
  let vec_struct_t = Llvm.struct_type llcontext [| i32_t; ptr_t |] in
  let closure_struct_t = Llvm.struct_type llcontext [| ptr_t; ptr_t |] in
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
    closure_struct_t;
    malloc_t;
    malloc_func;
  } in

  (*create main and set curser at entry*)
  let main_type = function_type i32_t [||] in
  let main_fn = declare_function "main" main_type llmodule in
  let bb = append_block ctx.llcontext "entry" main_fn in
  position_at_end bb ctx.llbuilder;

  let result = lower_lexpt_to_llvm ctx UUIDMap.empty final_exp in
  ignore (build_ret result ctx.llbuilder);
  llmodule

let sprint_lower_prog_to_llvm (p : letblkmonot) : string =
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