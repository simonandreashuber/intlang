%{
open Ast
%}

%token EOF
%token EQ LT                (* == < *)
%token ADD SUB MUL          (* + - * *)
%token LPAR RPAR            (* ( ) *)
%token LET ASS SEM          (* let = ; *)
%token IN                   
%token LAM DOT              (* \ . *)
%token IF THEN ELSE END     (* if then else end *)
%token INCLUDE              (* include *)
%token LBRACK RBRACK        (* [] *)
%token COMMA DIV
%token VECLEN VECLIT VECMK VECGET VECSET
%token <int>INT             (* int literal *)
%token <string>ID           (* name of some thing *)

%start start         
%type <Ast.parseout> start
%%

start:
    | p = prog EOF      { p }

prog:
    | INCLUDE; pt = path; p = prog    { (Include pt) :: p }
    | nl = nlexp; p = prog            { nl :: p }
    | nl = nlexp;                     { [nl] }
    | l = lexp                        { [Lexp l] }

path:
    | folder = ID; DIV; p = path      { folder ^ "/" ^ p } (*if u put a dot in ur folder name u go to intlang prison, hahaha*)
    | id = ID                         { id }

nlexp:
    | LET; id = ID; ASS; l = lexp; SEM   { Nlexp(id, l) }

(* tried to keep one lexp non terminal with operator precedence 
   but I did not get it to work quickly so switched back to manual :| *)
lexp:
    | LET; id = ID; ASS; e1 = lexp; IN; e2 = lexp           { Letin(id, e1, e2) }
    | IF; c = lexp; THEN; t = lexp; ELSE; e = lexp; END     { If(c, t, e) }
    | LAM; id = ID; DOT; l = lexp                           { Lam(id, l) }
    | lc = lexp_cmp                                         { lc }

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
    | ll = lexp_mul; DIV; lr = lexp_app         { Bop(Div, ll, lr) }
    | la = lexp_app                             { la }

lexp_app:
    | ll = lexp_app; lr = lexp_atom     { App(ll, lr) } (*left rec on applicaiton*)
    | la = lexp_atom                    { la }

lexp_atom:
    | id = ID                                                                   { Var id }
    | incl = ID; DOT; id = ID                                                   { Var (incl ^ "." ^ id) }
    | i = INT                                                                   { Int i }
    | VECLIT; LBRACK; ls = lexp_list; RBRACK                                    { Veclit ls }
    | VECMK; LBRACK; lit = lexp; COMMA; len = lexp; RBRACK                      { Vecmk(lit, len) }
    | VECLEN; LBRACK; v = lexp; RBRACK                                          { Veclen v }
    | VECGET; LBRACK; v = lexp; COMMA; i = lexp; RBRACK                         { Vecget(v, i) }
    | VECSET; LBRACK; v = lexp; COMMA; i = lexp; COMMA; value = lexp; RBRACK    { Vecset(v, i, value) }
    | LPAR; l = lexp; RPAR                                                      { l }

lexp_list:
    | l = lexp; COMMA; ls = lexp_list  { l :: ls }
    | l = lexp                         { [l] }
