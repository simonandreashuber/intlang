%{
open Ast
%}

%token EOF
%token EQ LT                (* == < *)
%token ADD SUB MUL          (* + - * *)
%token LPAR RPAR            (* ( ) *)
%token LET ASS SEM          (* let = ; *)
%token LAM DOT              (* \ . *)
%token IF THEN ELSE END     (* if then else end *)
%token INCLUDE              (* include *)
%token LBRACK RBRACK        (* [] *)
%token COMMA
%token <int>INT             (* int literal *)
%token <string>ID           (* name of some thing *)

%start start         
%type <Ast.parseout> start
%%

start:
    | p = prog EOF      { p }

prog:
    | INCLUDE; id = ID; p = prog      { (Include id) :: p }
    | nl = nlexp; p = prog            { nl :: p }
    | l = lexp                        { [Lexp l] }

nlexp:
    | LET; id = ID; ASS; l = lexp; SEM   { Nlexp(id, l) }

(* tried to keep one lexp non terminal with operator precedence 
   but I did not get it to work quickly so switched back to manual :| *)
lexp:
    | IF; c = lexp; THEN; t = lexp; ELSE; e = lexp; END  { If(c, t, e) }
    | LAM; id = ID; DOT; l = lexp                        { Lam(id, l) }
    | lc = lexp_cmp                                      { lc }

lexp_cmp:
    | ll = lexp_cmp; EQ; lr = lexp_add          { Bop(Eq, ll, lr) }
    | ll = lexp_cmp; LT; lr = lexp_add          { Bop(Lt, ll, lr) }
    | la = lexp_add                             { la }

lexp_add:
    | ll = lexp_add; ADD; lr = lexp_mul         { Bop(Add, ll, lr) }
    | ll = lexp_add; SUB; lr = lexp_mul         { Bop(Sub, ll, lr) }
    | lm = lexp_mul                             { lm }

lexp_mul:
    | ll = lexp_mul; MUL; lr = lexp_app         { Bop(Mul, ll, lr) }
    | la = lexp_app                             { la }

lexp_app:
    | ll = lexp_app; lr = lexp_atom     { App(ll, lr) } (*left rec on applicaiton*)
    | la = lexp_atom                    { la }

lexp_atom:
    | LBRACK; ls = lexp_list; RBRACK    { Tuple ls }
    | l = lexp_atom; DOT; i = INT       { Field(l, i) }
    | id = ID                           { Var id }
    | i = INT                           { Int i }
    | LPAR; l = lexp; RPAR              { l }

lexp_list:
    | l = lexp; COMMA; ls = lexp_list  { l :: ls }
    | l = lexp                         { [l] }
