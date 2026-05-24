open Ast

open Parser
open Lexer

exception IncludeError of string

let lexnparse (filepath: string) : parseout =
  try
    let ch = open_in filepath in
    let lexbuf = Lexing.from_channel ch in
    let parseout = Parser.start Lexer.token lexbuf in
    close_in ch;
    parseout
  with Sys_error msg -> raise (IncludeError ("Cannot open file: " ^ filepath ^ " (" ^ msg ^ ")"))

let rec handle_includes (p:parseout) (handled_includes: string list) (origin_filename: string) : prog =
  let local_letblk, local_lexp = List.fold_right 
  (fun stmt (letblk, lexp_opt) -> 
    match stmt with
      | Include _ -> (letblk, lexp_opt) (*first pass: ignore includes*)
      | Nlexp (name, e) -> ( 
        match List.assoc_opt name letblk with
        | Some _ -> raise (IncludeError ("Duplicate definition: " ^ name))
        | None -> (name, e) :: letblk, lexp_opt
      )
      | Lexp e -> (letblk, Some e)
    )
  p ([], None) in
  let final_letblk = List.fold_right 
  (fun stmt letblk -> 
    match stmt with
      | Include inclname -> (
        if List.mem inclname handled_includes then letblk (* already handled, skip *)
        else
        let inclpath = Filename.concat (Filename.dirname origin_filename) (inclname ^ ".intlang") in
        let inclparseout = lexnparse inclpath in
        let inclletblk, _ = handle_includes inclparseout (inclname :: handled_includes) origin_filename in
        let combined_letblk =
          List.fold_left (fun acc (name, e) ->
            if List.mem_assoc name acc then acc (* skip if already defined in current letblk *)
            else (name, e) :: acc
          ) letblk inclletblk in
        combined_letblk
      )
      | _ -> letblk (*second pass: ignore local defs*)
  ) p local_letblk 
  in
  
  match local_lexp with
  | Some e -> (final_letblk, e)
  | None -> raise (IncludeError "No main expression found in the program")
