open Intlang_lib

let intlang_std_lib_path = "/home/simon/code/intlang/test/intlangstdlib/"

let main () =
  let filename = Sys.argv.(Array.length Sys.argv - 1) in
  let prog = Include.lex_parse_include intlang_std_lib_path filename in
  PrintIntlang.print_prog prog; flush stdout;

  let env = Typecheck.typecheck prog in
  Printf.printf "Type environment:\n%s\n" (Typecheck.sprint_env env); flush stdout;
  
  let out_opt = Interp.interp_prog prog in
  match out_opt with
  | Some out -> Printf.printf "out: %d\n" out
  | None -> Printf.printf "out: No final Expression\n";
  flush stdout;
  exit 0

let () = main ()
