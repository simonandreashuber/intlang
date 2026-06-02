open Intlang_lib

let intlang_std_lib_path = "/home/simon/code/intlang/test/intlangstdlib/"
let headerline = "-------------------------------------------------------------------\n"

let main () =
  let filename = Sys.argv.(Array.length Sys.argv - 1) in
  let prog = Include.lex_parse_include intlang_std_lib_path filename in
  Printf.printf "%sPARSED PROG:\n%s" headerline (PrintIntlang.sprint_prog prog); flush stdout;

  let env = Typecheck.typecheck prog in
  Printf.printf "%sTYPE ENV:\n%s\n%s" headerline (Typecheck.sprint_env env) headerline; flush stdout;
  
  let out_opt = Interp.interp_prog prog in
  match out_opt with
  | Some out -> Printf.printf "out: %d\n%s" out headerline; flush stdout;
  | None -> Printf.printf "out: No final Expression\n%s\n" headerline; flush stdout;
  flush stdout;
  exit 0

let () = main ()
