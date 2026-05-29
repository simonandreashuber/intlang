open Ast

open Parser
open Lexer
open Errors

let lex_parse (filepath: string) : parseout =
  let ch = 
    try open_in filepath
    with Sys_error msg -> raise (Errors.IncludeError ("Cannot open file: " ^ filepath ^ " (" ^ msg ^ ")"))
  in
  let lexbuf = Lexing.from_channel ch in
  try
    let parseout = Parser.start Lexer.token lexbuf in
    close_in ch;
    parseout
  with e   -> 
    close_in ch;
    raise (Errors.ParseError ("Parse error in file: " ^ filepath ^ " at line " ^ string_of_int lexbuf.lex_curr_p.pos_lnum ^ " (" ^  (Printexc.to_string e) ^ ")"))


let lex_parse_include (filepath: string) : prog =
  (*get the optional final lexp*)
  let final_lexp = List.fold_right (fun stmt lexp_opt -> 
                                        match stmt with
                                          | Lexp e -> Some e
                                          | _ -> lexp_opt
                                      )
    (lex_parse filepath) None in
  (*
    example 
    /src/
      - main.intlang
      - lib/
        - somelib.intlang
    then:
      dirstem = '/src/'
      name = 'main' or 'lib/somelib'                  here I use the relative path since it is then easy to just get the final filename 
      handled_includes = 'main' or 'somelib'          here I use the file name only since this is what we use as the include identifier in the code so it can ever be doubly used; if someone were to include two files with the same name from different dirs they go to intlang jail
  *)
  let rec acc_includes (dirstem: string) (inclname:string) (is_include:bool) (handled_includes: string list) (letacc: (string * lexp) list) : (string list) * ((string * lexp) list) =
    let basename = (Filename.basename inclname) in
    if List.mem basename handled_includes then (handled_includes, letacc)
    else
    let handled_includes' = basename :: handled_includes in
    let parseout = lex_parse (Filename.concat dirstem (inclname ^ ".intlang")) in
    List.fold_left 
      (
        fun (handincl, lacc) stmt -> 
            match stmt with
              | Nlexp (name, e) -> (
                  let mangled_name = if is_include then (basename ^ "." ^ name) else name in
                  if List.mem_assoc mangled_name lacc then raise (Errors.IncludeError ("In file: " ^ inclname ^ " there is a double def with name: " ^ name ))
                  else (handincl, (mangled_name, e) :: lacc)
                )
              | Include newinclname -> (acc_includes dirstem newinclname true handincl lacc)
              | Lexp _ -> (handincl, lacc)
      ) (handled_includes', letacc) parseout 
  in
  let _, letblk = acc_includes (Filename.dirname filepath) (Filename.chop_extension @@ Filename.basename filepath) false [] [] in
  (letblk, final_lexp)