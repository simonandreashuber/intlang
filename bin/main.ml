open Intlang_lib

let main () =
  let filename = Sys.argv.(Array.length Sys.argv - 1) in
  let prog = Include.lex_parse_include filename in
  PrintIntlang.print_prog prog;

  (*let env = Typecheck.typecheck prog in
  Printf.printf "Type environment:\n%s\n" (Typecheck.sprint_env env);
  let out = Interp_closure.interp_prog prog in
  Printf.printf "out: %d\n" out;*)
  exit 0

let () = main ()
