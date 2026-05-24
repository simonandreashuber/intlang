open Intlang_lib

let main () =
  (*let use_closure = Array.mem "--closure" Sys.argv in*)
  let filename = Sys.argv.(Array.length Sys.argv - 1) in
  try
    In_channel.with_open_text filename (fun ch ->
      let lexbuf = Lexing.from_channel ch in
      let p = Parser.start Lexer.token lexbuf in
      (*
      Typecheck.typecheck p;
      if use_closure then (
        PrintIntlang.print_prog p;
        Printf.printf "out: %d\n" (Interp_closure.interp_prog p)
      ) else (
        PrintIntlang.print_prog p;
        Printf.printf "out: %d\n" (Interp.interp_prog p)
      )
      *)
      PrintIntlang.print_prog p;
    )
  with
  | Sys_error msg -> Printf.eprintf "Could not open file: %s\n" msg; exit 1

let () = main ()
