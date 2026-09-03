open Intlang_lib

let headerline = "-------------------------------------------------------------------\n"

let main () =
  (* Define mutable references *)
  let repeat_count = ref 1 in
  let testast_flag_passed = ref false in
  let testmir_flag_passed = ref false in
  let testbin_flag_passed = ref false in
  let print_ast = ref false in
  let filename = ref "" in
  
  (* Initialize with your current path as the default value *)
  let default_path = (Sys.getcwd ()) ^ "/test/intlangstdlib/" in
  let stdlib_path = ref default_path in

  (* Map command-line flags *)
  let speclist = [
    ("--testast", Arg.Int (fun i ->
       if i <= 0 then
         raise (Arg.Bad "must be greater than 0")
       else begin
         repeat_count := i;
         testast_flag_passed := true
       end
     ), "<int> Number of times to execute the AST interpreter (default: 1)");
    ("--testmir", Arg.Int (fun i ->
       if i <= 0 then
         raise (Arg.Bad "must be greater than 0")
       else begin
         repeat_count := i;
         testmir_flag_passed := true
       end
     ), "<int> Number of times to execute the MIR Simulator (default: 1)");
    ("--testbin", Arg.Int (fun i ->
       if i <= 0 then
         raise (Arg.Bad "must be greater than 0")
       else begin
         repeat_count := i;
         testbin_flag_passed := true
       end
     ), "<int> Number of times to execute the compiled binary (default: 1)");
    ("--print", Arg.Set print_ast, "Print AST and MIR to stdout");
    ("--stdlibpath", Arg.Set_string stdlib_path, "<path> Custom path to the standard library");
  ] in

  (* --help *)
  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " [--test <int>] [--print] [--stdlibpath <path>] <filename>" in

  (* Parse the arguments *)
  Arg.parse speclist (fun anon -> filename := anon) usage_msg;

  (* Guard against missing filename errors *)
  if !filename = "" then begin
    prerr_endline "Error: No input file specified.";
    Arg.usage speclist usage_msg;
    exit 1
  end;

  (* Guard against incompatible flags (need a cleaner way to handle this ...)*)
  if !testast_flag_passed && !print_ast then begin
    prerr_endline "Error: --testast and --print are incompatible. (printing breaks the test protocol)";
    Arg.usage speclist usage_msg;
    exit 1
  end;
  
  if !testmir_flag_passed && !print_ast then begin
    prerr_endline "Error: --testmir and --print are incompatible. (printing breaks the test protocol)";
    Arg.usage speclist usage_msg;
    exit 1
  end;

  if !testbin_flag_passed && !print_ast then begin
    prerr_endline "Error: --testbin and --print are incompatible. (printing breaks the test protocol)";
    Arg.usage speclist usage_msg;
    exit 1
  end;

  if !testast_flag_passed && !testmir_flag_passed then begin
    prerr_endline "Error: --testast and --testmir are incompatible. (printing breaks the test protocol)";
    Arg.usage speclist usage_msg;
    exit 1
  end;

  if !testast_flag_passed && !testbin_flag_passed then begin
    prerr_endline "Error: --testast and --testbin are incompatible. (printing breaks the test protocol)";
    Arg.usage speclist usage_msg;
    exit 1
  end;

  if !testmir_flag_passed && !testbin_flag_passed then begin
    prerr_endline "Error: --testmir and --testbin are incompatible. (printing breaks the test protocol)";
    Arg.usage speclist usage_msg;
    exit 1
  end;

  (* Ensure the path ends with a trailing slash so file concatenation doesn't break *)
  let intlang_std_lib_path = 
    let p = !stdlib_path in
    if String.length p > 0 && p.[String.length p - 1] = '/' then p else p ^ "/"
  in

  try
    (* Lex, Parse and Include Pass *)
    let ast = Include.lex_parse_include intlang_std_lib_path !filename in
    if !print_ast then begin
      Printf.printf "%sPARSED AST:\n%s" headerline (PrintIntlang.sprint_ast ast); flush stdout;
    end;

    (* Recursive Check Pass *)
    Reccheck.reccheck ast;

    (* Type Check Pass *)
    let polytast = Typecheck.typecheck ast in
    if !print_ast then begin
      Printf.printf "%sPOLYTAST:\n%s" headerline (PrintIntlang.sprint_polytast polytast); flush stdout;
    end;

    (* Monomorphization Pass *)
    let monotast = Monomorph.monomorph polytast in
    if !print_ast then begin
      Printf.printf "%sMONOTAST:\n%s" headerline (PrintIntlang.sprint_monotast monotast); flush stdout;
    end;

    Veccheck.veccheck_monotast monotast;

    (* Execute the interpreter *)
    if !testast_flag_passed then (
      for _ = 1 to !repeat_count do
        Interp.interp_monotast monotast;
        flush stdout;
      done
    );


    let b = Mirgen.lower_monotast monotast in
    Mirpipe.run_pipeline b;

    (* Execute Mir Simulator *)
    if !testmir_flag_passed then (
      for _ = 1 to !repeat_count do
        Simmir.simmir_program b.program;
        flush stdout;
      done
    );

    if !print_ast then begin
      Printf.printf "%sMIR:\n%s" headerline (Printmir.string_of_program b.program); flush stdout;
    end;

    let llmod = Llvmgen.lower_mir b.program in

    if !print_ast then begin
      Printf.printf "%sLLVM IR:\n%s" headerline (Llvm.string_of_llmodule llmod); flush stdout;
    end;

    if !testbin_flag_passed then begin
      let clang_exit_code = Llvmgen.llvm_to_bin_clang llmod "out" in
      if clang_exit_code <> 0 then failwith (Printf.sprintf "Clang failed with exit code %d" clang_exit_code)
    end;
    
    exit 0

  with exn ->
    (* Print the exception message to stderr *)
    prerr_endline ("Error: " ^ Printexc.to_string exn);
    exit 1

let () = main ()
