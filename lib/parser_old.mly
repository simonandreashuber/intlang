%{
open Ast
%}

%token EOF
%token EQ LT                (* == < *)
%token ADD SUB MUL          (* + - * *)
%token LPAR RPAR            (* ( ) *)
%token LET ASS SEM          (* let = ; *)
%token LAM DOT              (* \ . *)
%token <int>INT             (* int literal *)
%token <string>ID           (* name of some thing *)

%token APP                  (*application "ghost" token to give applicaiton max precedence*)

(*operator strength first in list is weak*)

%nonassoc LAM
%left EQ 
%left LT
%left ADD
%left SUB
%left MUL
%left APP

%start start         
%type <Ast.prog> start
%%

start:
    | p = prog EOF      { p }

prog:
    | nl = nlexp; p = prog      { nl :: p }
    | l = lexp                  { [Lexp l] }

nlexp:
    | LET; id = ID; ASS; l = lexp; SEM   { Nlexp(id, l) }

lexp:
    | id = ID                           { Var id }
    | i = INT                           { Int i }
    | LPAR; l = lexp; RPAR              { l }
    | ll = lexp; lr = lexp %prec APP    { App(ll, lr) }
    | LAM; id = ID; DOT; l = lexp       { Lam(id, l) }
    | ll = lexp; ADD; lr = lexp         { Bop(Add, ll, lr) }
    | ll = lexp; SUB; lr = lexp         { Bop(Sub, ll, lr) }
    | ll = lexp; MUL; lr = lexp         { Bop(Mul, ll, lr) }
    | ll = lexp; EQ; lr = lexp          { Bop(Eq, ll, lr) }
    | ll = lexp; LT; lr = lexp          { Bop(Lt, ll, lr) }

