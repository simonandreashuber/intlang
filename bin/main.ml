open Intlang_lib

let main () =
  let filename = Sys.argv.(1) in
  try
    In_channel.with_open_text filename (fun ch ->
      let lexbuf = Lexing.from_channel ch in
      try
          let p = Parser.start Lexer.token lexbuf in
          Interp.print_prog p;
          Printf.printf "out: %d\n" (Interp.interp_prog p)
      with
          | Parser.Error  -> Errors.report_err_lnum  "Parser Error" lexbuf; exit 1
          | Lexer.LexErr msg -> Errors.report_err_lnum msg lexbuf; exit 1
    )
  with
  | Sys_error msg -> Printf.eprintf "Could not open file: %s\n" msg; exit 1

let () = main ()
