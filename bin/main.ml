open Intlang_lib

let headerline = "-------------------------------------------------------------------\n"

let main () =
  (* Define mutable references *)
  let repeat_count = ref 1 in
  let interpast_flag_passed = ref false in
  let interpmir_flag_passed = ref false in
  let print_ast = ref false in
  let print_monotast = ref false in
  let print_mir = ref false in
  let print_llvm = ref false in
  let outputllvm_name = ref "" in
  let address_sanitizer = ref false in
  let opt_level = ref 0 in
  let prohibit_relative_include = ref false in
  let add_analysis_printmir = ref false in
  let outputmir_name = ref "" in
  let outputfile_passed = ref false in
  let outputfilename = ref "" in
  let inputfilename = ref "" in

  (* Initialize with your current path as the default value *)
  let default_path = (Sys.getcwd ()) ^ "/test/intlangstdlib/" in
  let stdlib_path = ref default_path in

  (* Map command-line flags *)
  let speclist = [
    ("-o", Arg.String (fun s -> outputfilename := s; outputfile_passed := true), "<filename> Specify output filename (default: out)");
    ("--outputllvmir", Arg.Set_string outputllvm_name, "<filename> name for llvm ir output file");
    ("--outputmir", Arg.Set_string outputmir_name, "<filename> name for mir output file");
    ("--stdlibpath", Arg.Set_string stdlib_path, "<path> Custom path to the standard library");
    ("-O0", Arg.Unit (fun () -> opt_level := 0), "No optimizations");
    ("-O1", Arg.Unit (fun () -> opt_level := 1), "Basic optimizations");
    ("-O2", Arg.Unit (fun () -> opt_level := 2), "Moderate optimizations");
    ("-O3", Arg.Unit (fun () -> opt_level := 3), "Heavy optimizations");
    ("--asan", Arg.Set address_sanitizer, "Enable AddressSanitizer for the generated binary");
    ("--printast", Arg.Set print_ast, "Print AST to stdout");
    ("--printmonotast", Arg.Set print_monotast, "Print Monomorphized TAST to stdout");
    ("--printmir", Arg.Set print_mir, "Print MIR to stdout");
    ("--printllvm", Arg.Set print_llvm, "Print LLVM IR to stdout");
    ("--printall", Arg.Unit (fun () -> print_ast := true; print_monotast := true; print_mir := true; print_llvm := true), "Print all intermediate representations to stdout");
    ("--prohibitrelativeinclude", Arg.Set prohibit_relative_include, "Prohibit relative includes");
    ("--addanalysisprintmir", Arg.Set add_analysis_printmir, "In MIR output or print: Adds analysis information to the MIR output");
    ("--interpast", Arg.Int (fun i ->
       if i <= 0 then
         raise (Arg.Bad "must be greater than 0")
       else begin
         repeat_count := i;
         interpast_flag_passed := true
       end
     ), "<int> Number of times to execute the AST interpreter (default: 1)");
    ("--interpmir", Arg.Int (fun i ->
       if i <= 0 then
         raise (Arg.Bad "must be greater than 0")
       else begin
         repeat_count := i;
         interpmir_flag_passed := true
       end
     ), "<int> Number of times to execute the MIR Simulator (default: 1)");
  ] in

  (* --help *)
  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " [options] <inputfile>" in

  (* Parse the arguments *)
  Arg.parse speclist (fun anon -> inputfilename := anon) usage_msg;

  (* Guard against missing filename errors *)
  if !inputfilename = "" then begin
    prerr_endline "Error: No input file specified.";
    Arg.usage speclist usage_msg;
    exit 1
  end;

  (* Only one interpreter can be used at a time (both are of course theoretically possible but I just think its bad practice)*)
  if !interpast_flag_passed && !interpmir_flag_passed then begin
    prerr_endline "Error: --testast and --testmir are incompatible.";
    Arg.usage speclist usage_msg;
    exit 1
  end;

  (* when the interpreter is run the compiler should not emit anything *)
  if (!interpast_flag_passed || !interpmir_flag_passed) && not (!outputllvm_name = "" && !outputmir_name = "" && not !outputfile_passed) then begin
    prerr_endline "Error: --testast / --testmir are incompatible with outputting files (-o / --outputllvmir / --outputmir)";
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
    let ast = Include.lex_parse_include intlang_std_lib_path !inputfilename !prohibit_relative_include in
    if !print_ast then begin
      Printf.printf "%sPARSED AST:\n%s" headerline (PrintIntlang.sprint_ast ast); flush stdout;
    end;

    (* Recursive Check Pass *)
    Reccheck.reccheck ast;

    (* Type Check Pass *)
    let polytast = Typecheck.typecheck ast in

    if !print_ast then begin
      Printf.printf "%sPOLYTAST:\n%s" headerline (PrintIntlang.sprint_polytast polytast); flush stdout end;

    (* Monomorphization Pass *)
    let monotast = Monomorph.monomorph polytast in

    (* Top Level Eta-Expansions *)
    let monotast = Toplvletaexpand.toplvl_eta_expand monotast in

    if !print_monotast then begin
      Printf.printf "%sMONOTAST:\n%s" headerline (PrintIntlang.sprint_monotast monotast); flush stdout end;

    (* Vector Check Pass *)
    Veccheck.veccheck_monotast monotast;

    (* Execute the Mono TAST interpreter *)
    if !interpast_flag_passed then (
      for _ = 1 to !repeat_count do
        Interp.interp_monotast monotast;
        flush stdout;
      done;
      exit 0
    );

    (* Lower Monomorphized TAST to MIR *)
    let mir_builder = Mirgen.lower_monotast monotast in

    (* Run the MIR optimization pipeline *)
    Mirpipe.run_pipeline mir_builder (!opt_level > 0);
    let mir = mir_builder.program in

    if !print_mir || !outputmir_name <> "" then begin
      let mir_str =  Printmir.string_of_program mir !add_analysis_printmir in
      if !print_mir then begin
        Printf.printf "%sMIR:\n%s" headerline mir_str; flush stdout
      end;
      if !outputmir_name <> "" then begin
        try
          let oc = open_out !outputmir_name in
          output_string oc mir_str;
          close_out oc
        with Sys_error msg -> (
          prerr_endline ("Error writing MIR to file: " ^ msg);
          exit 1
        )
      end
    end;


    (* Execute Mir Simulator *)
    if !interpmir_flag_passed then (
      for _ = 1 to !repeat_count do
        Simmir.simmir_program mir;
        flush stdout;
      done;
      exit 0
    );

    (* Lower MIR to LLVM IR *)
    let llmod = Llvmgen.lower_mir mir in

    (* Print and/or output LLVM IR *)
    if !print_llvm || !outputllvm_name <> "" then begin
      let llvm_str = Llvm.string_of_llmodule llmod in
      if !print_llvm then begin
        Printf.printf "%sLLVM IR:\n%s" headerline llvm_str; flush stdout
      end;
      if !outputllvm_name <> "" then begin
        try
          let oc = open_out !outputllvm_name in
          output_string oc llvm_str;
          close_out oc
        with Sys_error msg -> (
          prerr_endline ("Error writing LLVM to file: " ^ msg);
          exit 1
        )
      end
    end;

    (*Lower LLVM IR with clang*)
    (
      let ll_name = "temp.ll" in
      let bin_name = if !outputfile_passed then !outputfilename else "out" in
      let clang_flags = ref "" in
      clang_flags := !clang_flags ^ "-O" ^ string_of_int !opt_level ^ " ";
      clang_flags := !clang_flags ^ if !address_sanitizer then "-fsanitize=address" else "";

      let llvm_ir = Llvm.string_of_llmodule llmod in

      (
      try
        let oc = open_out ll_name in
        output_string oc llvm_ir;
        close_out oc;
      with Sys_error msg -> (
        prerr_endline ("Error writing LLVM IR to file: " ^ msg);
        exit 1
      )
      );

      (* Compile the LLVM IR to a binary using clang *)
      let clang_cmd = Printf.sprintf "clang-19 %s %s -fverify-intermediate-code -o %s" ll_name !clang_flags bin_name in
      let exit_code = Sys.command clang_cmd in
      if exit_code <> 0 then (
        prerr_endline ("Error: clang failed to compile LLVM IR to binary. Exit code: " ^ string_of_int exit_code);
        exit 1
      );
      Sys.remove ll_name
    );

    exit 0

  with exn ->
    (* Print the exception message to stderr *)
    prerr_endline ("Error: " ^ Printexc.to_string exn);
    exit 1

let () = main ()
