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
let id = alpha (alpha | digit )*
let whitespace = [' ' '\t' '\r' ]+
let newline = '\n' | "\r\n"
let comment = "--" [^ '\n' '\r']*

rule token = parse
  | whitespace  { token lexbuf }
  | newline     {next_line lexbuf; token lexbuf}
  | comment     {token lexbuf}
  | "let"       { LET }
  | "in"        { IN }
  | "="         { ASS }
  | ";"         { SEM }
  | "\\"        { LAM } (*is actually a \ in the input*)
  | "."         { DOT }
  | "("         { LPAR }
  | ")"         { RPAR }
  | "["         { LBRACK }
  | "]"         { RBRACK }
  | "=="        { EQ }
  | "<"         { LT }
  | "+"         { ADD }
  | "-"         { SUB }
  | "*"         { MUL }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "end"       { END }
  | "include"   { INCLUDE }
  | "vec"       { VECLIT }
  | "vecmk"     { VECMK }
  | "veclen"    { VECLEN }
  | "vecget"    { VECGET }
  | "vecset"    { VECSET }
  | ","         { COMMA }
  | "/"         { DIV } 
  | digit+ as n { INT (int_of_string n) }
  | id as s     { ID s }
  | eof         { EOF }
  | _ as c      { raise (LexErr (Printf.sprintf "Unexpected char '%c'" c)) }
