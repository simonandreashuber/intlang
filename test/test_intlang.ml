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
        let prog = Include.lex_parse_include intlang_std_lib_path filepath in
        let _ = Typecheck.typecheck prog in
        
        (match Interp.interp_prog prog with
         | Some out when out = expected ->
             Printf.printf "[PASS] %s\n" intlang_file;
             true
         | Some out ->
             Printf.printf "[FAIL] %s: expected %d, got %d\n" intlang_file expected out;
             false
         | None ->
             Printf.printf "[FAIL] %s: expected %d, got None\n" intlang_file expected;
             false)
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