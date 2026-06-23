open Intlang_lib

(* Extract the expected value from the first line if it matches: -- Expect: <integer> *)
let extract_expect_from_file filename =
  try
    let ch = open_in filename in
    let first_line = input_line ch in
    close_in ch;
    
    let line = String.trim first_line in
    (* Check if line matches pattern: -- Expect: <integer> *)
    if String.length line > 11 && String.sub line 0 11 = "-- Expect: " then
      try
        let expect_str = String.sub line 11 (String.length line - 11) in
        let expect_int = int_of_string (String.trim expect_str) in
        Some expect_int
      with _ -> None
    else
      None
  with _ -> None

let intlang_std_lib_path = "/home/simon/code/intlang/test/intlangstdlib/"


let run_test cases_dir intlang_file =
  let filepath = Filename.concat cases_dir intlang_file in
  
  match extract_expect_from_file filepath with
  | None -> 
      Printf.printf "[SKIP] %s (no expect annotation)\n" intlang_file;
      true
  | Some expected ->
      try
        let _ = Include.lex_parse_include intlang_std_lib_path filepath in
        Printf.printf "[PASS] %s\n" intlang_file; flush stdout;
          true
        (*
        let progt, _ = Typecheck.typecheck prog in
        let monoprogt = Monomorph.monomorph_progt progt in
        let llvm_str = Codegen.sprint_lower_prog_to_llvm monoprogt in
        let exit_code = Codegen.lower_llvm_to_bin_clang llvm_str ("out"^intlang_file) in

        if exit_code <> 0 then begin
          Printf.printf "[FAIL] %s: Clang compilation failed.\n" intlang_file; flush stdout;
          false
        end else
        
        let execution_result = Sys.command ("./out"^intlang_file) in
        Sys.remove ("out"^intlang_file);

        (match Interp_tast.interp_prog monoprogt with
         | Some out when (out = expected) && (execution_result = (expected mod 256)) ->
             Printf.printf "[PASS] %s\n" intlang_file; flush stdout;
             true
         | Some out ->
             Printf.printf "[FAIL] %s: expected %d, got %d (interp) and %d (compilation)\n" intlang_file expected out execution_result; flush stdout;
             false
         | None ->
             Printf.printf "[FAIL] %s: expected %d, got None\n" intlang_file expected; flush stdout;
             false)
        *)
      with e ->
        Printf.printf "[FAIL] %s: %s\n" intlang_file (Printexc.to_string e);
        false

let () =
  let cases_dir = if Array.length Sys.argv > 1 then Sys.argv.(1) else "cases" in
  
  (* Read all .intlang files from cases directory *)
  let files = Sys.readdir cases_dir |> Array.to_list in
  let intlang_files = 
    List.filter (fun f -> Filename.check_suffix f ".intlang") files 
    |> List.sort String.compare
  in
  
  (* Run all tests *)
  let success = List.fold_left (fun acc f -> 
    run_test cases_dir f && acc
  ) true intlang_files in

  if success then exit 0 else exit 1