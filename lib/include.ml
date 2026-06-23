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

let validate_include_relative (path : string) : string =
  let is_valid_id (s : string) : bool =
    String.length s > 0 &&
    (* First character must be alpha or underscore *)
    (match s.[0] with 'a'..'z' | 'A'..'Z' | '_' -> true | _ -> false) &&
    (* Rest must be alpha, digit, or underscore *)
    String.for_all (function 
      | 'a'..'z' | 'A'..'Z' | '_' | '0'..'9' -> true 
      | _ -> false
  ) s in
  (* Enforces that there is a slash, and the final piece is a valid ID *)
  if is_valid_id (Filename.basename path) then
    path
  else
    raise (Errors.IncludeError ("Invalid relative include path: " ^ path ^ ". Must contain a '/' and end with a valid identifier."))

let lex_parse_include (std_lib_path: string) (filepath: string) : parseout =
  let rec vars_add_prefix (prefix: string) (locbound : string list) (e: lexp) : lexp =
    match e with
      | Var v -> if (List.mem v locbound) || (String.contains v '.') then Var v else Var (prefix ^ "." ^ v)
      | Lam (v, inT, outT, body) -> Lam (v, inT, outT, vars_add_prefix prefix (v :: locbound) body)
      | LamUnit body -> LamUnit (vars_add_prefix prefix locbound body)
      | Letin (v, e1, e2) -> Letin (v, vars_add_prefix prefix locbound e1, vars_add_prefix prefix (v :: locbound) e2)
      | Letrecin (v, e1, e2) -> Letrecin (v, vars_add_prefix prefix (v :: locbound) e1, vars_add_prefix prefix (v :: locbound) e2)
      | LetinTuple (vs, e1, e2) -> LetinTuple (vs, vars_add_prefix prefix locbound e1, vars_add_prefix prefix ((List.filter (fun x -> x <> "_") vs) @ locbound) e2)
      | Tuple es -> Tuple (List.map (vars_add_prefix prefix locbound) es)
      | App (e1, e2) -> App (vars_add_prefix prefix locbound e1, vars_add_prefix prefix locbound e2)
      | I32Lit n -> I32Lit n
      | I8Lit c -> I8Lit c
      | UnitLit -> UnitLit
      | UopI32 (u, e) -> UopI32 (u, vars_add_prefix prefix locbound e)
      | BopI32 (b, e1, e2) -> BopI32 (b, vars_add_prefix prefix locbound e1, vars_add_prefix prefix locbound e2)
      | UopI8 (u, e) -> UopI8 (u, vars_add_prefix prefix locbound e)
      | BopI8 (b, e1, e2) -> BopI8 (b, vars_add_prefix prefix locbound e1, vars_add_prefix prefix locbound e2)
      | If (c, t, f) -> If (vars_add_prefix prefix locbound c, vars_add_prefix prefix locbound t, vars_add_prefix prefix locbound f)
      | VecLit es -> VecLit (List.map (vars_add_prefix prefix locbound) es)
      | Vecmk (defval, size_list) -> Vecmk (vars_add_prefix prefix locbound defval, List.map (vars_add_prefix prefix locbound) size_list)
      | Veclen v -> Veclen (vars_add_prefix prefix locbound v)
      | Vecget (v, idx_list) -> Vecget (vars_add_prefix prefix locbound v, List.map (vars_add_prefix prefix locbound) idx_list)
      | Vecset (v, value, idx_list) -> Vecset (vars_add_prefix prefix locbound v, vars_add_prefix prefix locbound value, List.map (vars_add_prefix prefix locbound) idx_list)
      | Vecresz (v, newlen, idx_list) -> Vecresz (vars_add_prefix prefix locbound v, vars_add_prefix prefix locbound newlen, List.map (vars_add_prefix prefix locbound) idx_list)
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
  let handled_includes = ref [] in
  let letacc = ref [] in
  let rec acc_includes (dirstem: string) (inclname:string) (is_include:bool) : unit =
    let basename = (Filename.basename inclname) in
    Printf.printf "before: %s, basename: %s\n" (String.concat ", " !handled_includes) basename; flush stdout;
    if List.mem basename !handled_includes then ()
    else (
    handled_includes := basename :: !handled_includes;
    Printf.printf "%s, basename: %s\n" (String.concat ", " !handled_includes) basename; flush stdout;
    let parseout = lex_parse (Filename.concat dirstem (inclname ^ ".intlang")) in
    List.iter 
      (
        fun stmt -> 
            match stmt with
              | Let (name, e) -> (
                  let nl = if is_include then Let (basename ^ "." ^ name, vars_add_prefix basename builtin_names e) else Let (name, e) in
                  letacc := nl :: !letacc;
                )
              | Letrec lst -> (
                  let ltuplst = List.map (fun (name, e) -> 
                    if is_include then (basename ^ "." ^ name, vars_add_prefix basename builtin_names e) else (name, e)
                  ) lst in
                  letacc := (Letrec ltuplst) :: !letacc;
                )
              | IncludeRelative newinclname -> (
                  let valid_newinclname = validate_include_relative newinclname in
                  let updt_dirstem = Filename.dirname (Filename.concat dirstem inclname) in
                  acc_includes updt_dirstem valid_newinclname true 
                )
              | IncludeGlobal newinclname -> (acc_includes std_lib_path newinclname true)
              | Lexp e -> if is_include then () 
                          else letacc := (Let ("main", e)) :: !letacc;
      ) parseout;
      )
  in
  acc_includes (Filename.dirname filepath) (Filename.chop_extension @@ Filename.basename filepath) false;
  List.rev !letacc
  