open Intlang_lib

let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  String.trim s

let run_test_with_interp intlang_file interp_name interp_func =
  let expect_file = Filename.chop_extension intlang_file ^ ".expect" in
  if not (Sys.file_exists expect_file) then
    (Printf.printf "[SKIP] %s (%s, No .expect file found)\n" intlang_file interp_name; true)
  else
    let code = read_file intlang_file in
    let expected = int_of_string (read_file expect_file) in
    
    let lexbuf = Lexing.from_string code in
    try
      let prog = Parser.start Lexer.token lexbuf in
      (*Typecheck.typecheck prog;*)
      let result = interp_func prog in
      if result = expected then
        (Printf.printf "[PASS] %s (%s, out: %d)\n" intlang_file interp_name result; true)
      else
        (Printf.printf "[FAIL] %s: Expected '%d', got '%d'\n" intlang_file expected result; false)
    with e -> (
        match e with
          | Parser.Error  -> Printf.printf "[ERR ] %s: %s\n" intlang_file (Errors.sprint_err_lnum  "Parser Error" lexbuf); false
          | Lexer.LexErr msg -> Printf.printf "[ERR ] %s: %s\n" intlang_file (Errors.sprint_err_lnum msg lexbuf); false
          | _ -> Printf.printf "[ERR ] %s: %s\n" intlang_file (Printexc.to_string e); false
        )

let run_test intlang_file =
  let result1 = run_test_with_interp intlang_file "Interp" Interp.interp_prog in
  let result2 = run_test_with_interp intlang_file "Interp_closure" Interp_closure.interp_prog in
  result1 && result2

let () =
  (* Get the samples directory from command line arguments *)
  let dir = if Array.length Sys.argv > 1 then Sys.argv.(1) else "cases" in
  let files = Sys.readdir dir |> Array.to_list in
  let intlang_files = List.filter (fun f -> Filename.check_suffix f ".intlang") files in
  
  let success = List.fold_left (fun acc f -> 
    run_test (Filename.concat dir f) && acc
  ) true intlang_files in

  if success then exit 0 else exit 1
