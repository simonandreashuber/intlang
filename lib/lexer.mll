{
open Parser
exception LexErr of string

let next_line lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <-
    { pos with 
      pos_lnum = pos.pos_lnum + 1;  
      pos_bol = lexbuf.lex_curr_p.pos_cnum; (*set begining of line char number to char number*)
    }
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let id = alpha (alpha | digit)*
let whitespace = [' ' '\t' '\r' ]+
let newline = '\n' | "\r\n"

rule token = parse
  | whitespace  { token lexbuf }
  | newline     {next_line lexbuf; token lexbuf}
  | "let"       { LET }
  | "="         { ASS }
  | ";"         { SEM }
  | "\\"        { LAM } (*is actually a \ in the input*)
  | "."         { DOT }
  | "("         { LPAR }
  | ")"         { RPAR }
  | "=="        { EQ }
  | "<"         { LT }
  | "+"         { ADD }
  | "-"         { SUB }
  | "*"         { MUL }
  | digit+ as n { INT (int_of_string n) }
  | id as s     { ID s }
  | eof         { EOF }
  | _ as c      { raise (LexErr (Printf.sprintf "Unexpected char '%c'" c)) }
