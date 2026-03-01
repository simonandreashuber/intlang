open Intlang_lib

let main () =
  let filename = Sys.argv.(1) in
  try
    In_channel.with_open_text filename (fun ch ->
      let lexbuf = Lexing.from_channel ch in
      let p = Parser.start Lexer.token lexbuf in
      Interp.print_prog p
    )
  with
  | Sys_error msg -> Printf.eprintf "Could not open file: %s\n" msg; exit 1
  | Parser.Error  -> Printf.eprintf "Parser Error\n"; exit 1
  | Lexer.LexErr msg -> Printf.eprintf "%s\n" msg; exit 1

let () = main ()
