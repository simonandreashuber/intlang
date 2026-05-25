open Intlang_lib

let main () =
  let filename = Sys.argv.(Array.length Sys.argv - 1) in
  let parseout = Include.lexnparse filename in
  let prog = Include.handle_includes parseout [] filename in
  PrintIntlang.print_prog prog;

  let env = Typecheck.typecheck prog in
  Printf.printf "Type environment:\n%s\n" (Typecheck.sprint_env env);
  let out = Interp_closure.interp_prog prog in
  Printf.printf "out: %d\n" out;
  exit 0

let () = main ()
