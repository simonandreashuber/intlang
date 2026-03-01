{
open Parser
exception LexErr of string
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let id = alpha (alpha | digit)*
let whitespace = [' ' '\t' '\r' '\n']+

rule token = parse
  | whitespace  { token lexbuf }
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
  | _           { raise (LexErr ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
