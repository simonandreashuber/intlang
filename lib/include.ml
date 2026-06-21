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

(*
let lex_parse_include (std_lib_path: string) (filepath: string) : prog =
  (*get the optional final lexp*)
  let final_lexp = List.fold_right (fun stmt lexp_opt -> 
                                        match stmt with
                                          | Lexp e -> Some e
                                          | _ -> lexp_opt
                                      )
    (lex_parse filepath) None in
  let rec vars_add_prefix (prefix: string) (locbound : string list) (e: lexp) : lexp =
    match e with
      | Var v -> if (List.mem v locbound) || (String.contains v '.') then Var v else Var (prefix ^ "." ^ v)
      | Lam (v, body) -> Lam (v, vars_add_prefix prefix (v :: locbound) body)
      | App (e1, e2) -> App (vars_add_prefix prefix locbound e1, vars_add_prefix prefix locbound e2)
      | Int n -> Int n
      | Bop (b, e1, e2) -> Bop (b, vars_add_prefix prefix locbound e1, vars_add_prefix prefix locbound e2)
      | If (c, t, f) -> If (vars_add_prefix prefix locbound c, vars_add_prefix prefix locbound t, vars_add_prefix prefix locbound f)
      | Letin (v, e1, e2) -> Letin (v, vars_add_prefix prefix (v :: locbound) e1, vars_add_prefix prefix (v :: locbound) e2)
      | Veclit es -> Veclit (List.map (vars_add_prefix prefix locbound) es)
      | Vecmk (defval, count) -> Vecmk (vars_add_prefix prefix locbound defval, vars_add_prefix prefix locbound count)
      | Veclen v -> Veclen (vars_add_prefix prefix locbound v)
      | Vecget (v, i) -> Vecget (vars_add_prefix prefix locbound v, vars_add_prefix prefix locbound i)
      | Vecset (v, i, value) -> Vecset (vars_add_prefix prefix locbound v, vars_add_prefix prefix locbound i, vars_add_prefix prefix locbound value)
  in
      (*
    example 
    /src/
      - main.intlang
      - lib/
        - somelib.intlang
    then:
      dirstem = '/src/'                               this is changed as the program moves through the includes
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
                  if List.mem_assoc mangled_name lacc 
                    then raise (Errors.IncludeError ("In file: " ^ inclname ^ " there is a double def with name: " ^ name ))
                  else
                    let e' = if is_include then vars_add_prefix basename [] e else e in
                    (handincl, (mangled_name, e') :: lacc)
                )
              | IncludeRelative newinclname -> (
                  let updt_dirstem = Filename.dirname (Filename.concat dirstem inclname) in
                  acc_includes updt_dirstem newinclname true handincl lacc
                )
              | IncludeGlobal newinclname -> (acc_includes std_lib_path newinclname true handincl lacc)
              | Lexp _ -> (handincl, lacc)
      ) (handled_includes', letacc) parseout 
  in
  let _, letblk = acc_includes (Filename.dirname filepath) (Filename.chop_extension @@ Filename.basename filepath) false [] [] in
  (List.rev letblk, final_lexp)
  *)