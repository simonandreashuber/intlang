let sprint_err_lnum msg lexbuf =
  let line_num = lexbuf.Lexing.lex_curr_p.pos_lnum in
  Printf.sprintf "Error %s at line %d" msg line_num

let report_err_lnum msg lexbuf =
    Printf.eprintf "%s\n" (sprint_err_lnum msg lexbuf);

