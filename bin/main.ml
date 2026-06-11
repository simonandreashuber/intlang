open Intlang_lib

let intlang_std_lib_path = "/home/simon/code/intlang/test/intlangstdlib/"
let headerline = "-------------------------------------------------------------------\n"

let main () =
  let filename = Sys.argv.(Array.length Sys.argv - 1) in
  let prog = Include.lex_parse_include intlang_std_lib_path filename in
  Printf.printf "%sPARSED PROG:\n%s" headerline (PrintIntlang.sprint_prog prog); flush stdout;

  let progt, env = Typecheck.typecheck prog in
  Printf.printf "%sTYPE ENV:\n%s\n%s" headerline (PrintIntlang.sprint_env env) headerline; flush stdout;
  Printf.printf "%sTYPECHECKED PROG:\n%s" headerline (PrintIntlang.sprint_progpolyt_wtyp progt); flush stdout;
  (*Printf.printf "%sINSTREG:\n%s" headerline (PrintIntlang.sprint_instreg instreg); flush stdout;*)

  let monoprog = Monomorph.monomorph_progt progt in
  Printf.printf "%sMONOMORPHIZED PROG:\n%s" headerline (PrintIntlang.sprint_progmonot_wtyp monoprog); flush stdout;
  
  let out_opt = Interp.interp_prog prog in
  (match out_opt with
  | Some out -> Printf.printf "out: %d\n%s" out headerline; flush stdout;
  | None -> Printf.printf "out: No final Expression\n%s\n" headerline; flush stdout;);

  (*
  let llvm_str = Codegen.sprint_lower_prog_to_llvm prog in
  Printf.printf "LLVM IR:\n%s" llvm_str; flush stdout;
  let exit_code = Codegen.lower_llvm_to_bin_clang llvm_str "bin/out" in

  if exit_code = 0 then begin
    ignore (Sys.command "./bin/out; echo \"Execution Result: $?\"");
    Sys.remove "bin/out"
  end else
    print_endline "Clang compilation failed.";
  *)
  exit 0

let () = main ()
