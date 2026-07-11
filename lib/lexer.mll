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

let parse_i8 s : char =
    (* s still has the outer single quotes, e.g., "'a'" or "'\\x41'" *)
    let inner = String.sub s 1 (String.length s - 2) in
    if String.length inner = 1 then
      inner.[0]
    else if inner.[0] = '\\' then
      match inner.[1] with
      | 'n'  -> '\n'
      | 't'  -> '\t'
      | 'r'  -> '\r'
      | '\\' -> '\\'
      | '\'' -> '\''
      | '"'  -> '"'
      | 'x'  -> 
          (* Extract the two hex digits after '\x' *)
          let hex_digits = String.sub inner 2 2 in
          Char.chr (int_of_string ("0x" ^ hex_digits))
      | _ -> failwith "Lexer error: Unknown escape sequence"
    else
      failwith "Lexer error: Invalid character literal"

let parse_string s =
  let inner = String.sub s 1 (String.length s - 2) in
  Scanf.sscanf ("\"" ^ inner ^ "\"") "%S" (fun decoded -> decoded)
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let id = alpha (alpha | digit )*
let whitespace = [' ' '\t' '\r' ]+
let newline = '\n' | "\r\n"
let comment = "--" [^ '\n' '\r']*
let escape = '\\' ['\\' '\'' '"' 'n' 't' 'r']
let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let hex_escape = '\\' 'x' hex_digit hex_digit

let ascii_char = [^ '\'' '\\']
let i8init = "'" (ascii_char | escape | hex_escape) "'"

let string_char = [^ '"' '\\']
let strinit = '"' (string_char | escape)* '"'

rule token = parse
  | whitespace  { token lexbuf }
  | newline     {next_line lexbuf; token lexbuf}
  | comment     {token lexbuf}
  | "include"   { INCLUDE }
  | "let"       { LET }
  | "rec"       { REC }
  | "and"       { LETAND }
  | "in"        { IN }
  | "="         { ASS }
  | ";"         { SEM }
  | ":"         { COLON }
  | "=>"        { OUTTYP }  
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "end"       { END }
  | "\\"        { LAM } (*is actually a \ in the input*)
  | "."         { DOT }
  | "("         { LPAR }
  | ")"         { RPAR }
  | "["         { LBRACK }
  | "]"         { RBRACK }
  | ","         { COMMA }
  (*type annotations*)
  | "i32"       {I32TYP}
  | "i8"        {I8TYP}
  | "->"        {FUNTYP}
  | "unit"      {UNITTYP}
  (*I32 ops*)
  | "=="        { EQ_I32 }
  | "!="        { NEQ_I32 }
  | "<"         { LT_I32 }
  | ">"         { GT_I32 }
  | "<="        { LTEQ_I32 }
  | ">="        { GTEQ_I32 }
  | "+"         { ADD_I32 }
  | "-"         { SUB_I32 }
  | "*"         { MUL_I32 }
  | "/"         { DIV_I32 }
  | "%"         { MOD_I32 }
  | "&"         { AND_I32 }
  | "|"         { OR_I32 }
  | "^"         { XOR_I32 }
  | "~"         { NOT_I32 }
  | ">>"        { SHR_I32 }
  | "<<"        { SHL_I32 }
  (*Unsigned I32 ops*)
  | "<u"        { ULT_I32 }
  | ">u"        { UGT_I32 }
  | "<=u"       { ULTEQ_I32 }
  | ">=u"       { UGTEQ_I32 }
  | "/u"        { UDIV_I32 }
  | "%u"        { UMOD_I32 }
  | ">>u"       { USHR_I32 }
  (*I8 ops*)
  | "==i8"      { EQ_I8 }
  | "!=i8"      { NEQ_I8 }
  | "<i8"       { LT_I8 }
  | ">i8"       { GT_I8 }
  | "<=i8"      { LTEQ_I8 }
  | ">=i8"      { GTEQ_I8 }
  | "+i8"       { ADD_I8 }
  | "-i8"       { SUB_I8 }
  | "&i8"       { AND_I8 }
  | "|i8"       { OR_I8 }
  | "^i8"       { XOR_I8 }
  | "~i8"       { NOT_I8 }
  (*Vector ops*)
  | "vec"       { VECLIT }
  | "vecmk"     { VECMK }
  | "veclen"    { VECLEN }
  | "vecget"    { VECGET }
  | "vecset"    { VECSET }
  | "vecslice"  { VECSLICE }
  | "vecextend" { VECEXTEND }
  | digit+ as n { I32 (int_of_string n) }
  | i8init as c { I8 (parse_i8 c) }
  | strinit as s{ STR (parse_string s) }
  | id as s     { ID s }
  | eof         { EOF }
  | _ as c      { raise (LexErr (Printf.sprintf "Unexpected char '%c'" c)) }
