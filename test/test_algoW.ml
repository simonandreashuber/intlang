open Intlang_lib

let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  String.trim s

let run_type_check intlang_file =
  let code = read_file intlang_file in
  let lexbuf = Lexing.from_string code in
  try
    let prog = Parser.start Lexer.token lexbuf in
    match prog with
    | [Ast.Lexp lexp] ->
        Printf.printf "[ALGOW] %s\n" (Filename.basename intlang_file);
        Printf.printf "  Expr: %s\n" (PrintIntlang.sprint_lexp lexp);
        (try
          let empty_env = AlgoW.TypeEnv (AlgoW.StrMap.empty) in
          let typ, explanation = AlgoW.explain_typecheck empty_env lexp in
          Printf.printf "  Type: %s\n" (AlgoW.sprint_typ typ);
          Printf.printf "  Inference:\n";
          String.split_on_char '\n' explanation
          |> List.filter (fun s -> String.length s > 0)
          |> List.iter (fun s -> Printf.printf "    %s\n" s);
          Printf.printf "\n";
          true
        with AlgoW.TypeError msg ->
          Printf.printf "  Type Error: %s\n\n" msg;
          true)
    | _ -> 
        Printf.printf "[ALGOW] %s - ERROR: Expected single expression\n\n" (Filename.basename intlang_file);
        false
  with
  | Parser.Error -> Printf.printf "[ALGOW] %s - Parser Error\n\n" (Filename.basename intlang_file); false
  | Lexer.LexErr msg -> Printf.printf "[ALGOW] %s - Lexer Error: %s\n\n" (Filename.basename intlang_file) msg; false
  | e -> Printf.printf "[ALGOW] %s - Error: %s\n\n" (Filename.basename intlang_file) (Printexc.to_string e); false

let () =
  let dir = if Array.length Sys.argv > 1 then Sys.argv.(1) else "algoW" in
  if Sys.file_exists dir then (
    let files = Sys.readdir dir |> Array.to_list in
    let intlang_files = List.filter (fun f -> Filename.check_suffix f ".intlang") files in
    let sorted_files = List.sort String.compare intlang_files in
    Printf.printf "\n=== AlgoW Type Inference Tests ===\n\n";
    List.iter (fun f -> let _ = run_type_check (Filename.concat dir f) in ()) sorted_files
  );
  exit 0
