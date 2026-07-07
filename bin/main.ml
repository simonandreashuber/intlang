open Intlang_lib

let headerline = "-------------------------------------------------------------------\n"

let main () =
  (* Define mutable references *)
  let repeat_count = ref 1 in
  let test_flag_passed = ref false in
  let print_ast = ref false in
  let filename = ref "" in
  
  (* Initialize with your current path as the default value *)
  let default_path = (Sys.getcwd ()) ^ "/test/intlangstdlib/" in
  let stdlib_path = ref default_path in

  (* Map command-line flags *)
  let speclist = [
    ("--test", Arg.Int (fun i ->
       if i <= 0 then
         raise (Arg.Bad "must be greater than 0")
       else begin
         repeat_count := i;
         test_flag_passed := true
       end
     ), "<int> Number of times to execute the interpreter (default: 1)");
    ("--print", Arg.Set print_ast, "Print the parsed AST to stdout");
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

  (* Guard against incompatible flags *)
  if !test_flag_passed && !print_ast then begin
    prerr_endline "Error: --test and --print are incompatible. (printing breaks the test protocol)";
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

    (* Execute the interpreter *)
    if !test_flag_passed then
      for _ = 1 to !repeat_count do
        Interp.interp_monotast monotast;
        flush stdout;
      done
    else
      Interp.interp_monotast monotast;
    
    exit 0

  with exn ->
    (* Print the exception message to stderr *)
    prerr_endline ("Error: " ^ Printexc.to_string exn);
    exit 1

let () = main ()
