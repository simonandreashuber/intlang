%{
open Ast
%}

%token EOF
%token EQ_I32 NEQ_I32 LT_I32 GT_I32 LTEQ_I32 GTEQ_I32
%token ULT_I32 UGT_I32 ULTEQ_I32 UGTEQ_I32
%token ADD_I32 SUB_I32 MUL_I32 DIV_I32 MOD_I32
%token UDIV_I32 UMOD_I32
%token AND_I32 OR_I32 XOR_I32 NOT_I32
%token SHL_I32 SHR_I32 USHR_I32
%token EQ_I8 NEQ_I8 LT_I8 GT_I8 LTEQ_I8 GTEQ_I8
%token ADD_I8 SUB_I8
%token AND_I8 OR_I8 XOR_I8 NOT_I8
%token LPAR RPAR            (* ( ) *)
%token LET REC RECBLK ASS SEM          (* let = ; *)
%token IN                   
%token LAM COLON OUTTYP DOT              (* \ . *)
%token IF THEN ELSE END     (* if then else end *)
%token INCLUDE              (* include *)
%token LBRACK RBRACK        (* [] *)
%token COMMA
%token VECLEN VECLIT VECMK VECGET VECSET
%token FUNTYP I32TYP I8TYP
%token <int>I32             (* int literal *)
%token <char>I8
%token <string> STR
%token <string>ID           (* name of some thing *)

%start start         
%type <Ast.parseout> start
%%

start:
    | p = prog EOF      { p }

prog:
    | INCLUDE; id = ID; p = prog                            { (IncludeGlobal id) :: p }
    | INCLUDE; pt = STR; p = prog                           { (IncludeRelative pt) :: p } (*the final module/.intlang file should be named with an ID as it must be usable as module.id*)
    | nl = lettoplvl; p = prog                              { nl :: p }
    | nl = lettoplvl                                        { [nl] }
    | l = lexp                                              { [Lexp l] }

lettoplvl:
    | LET; id = ID; ASS; l = lexp; SEM                      { Let(id, l) }
    | LET; REC; id = ID; ASS; l = lexp; SEM                 { Letrec(id, l) }
    | LET; RECBLK; id = ID; ASS; l = lexp; SEM              { Letrecblk(id, l) }


(* tried to keep one lexp non terminal with operator precedence 
   but I did not get it to work quickly so switched back to manual :| *)
lexp:
    | LET; id = ID; ASS; e1 = lexp; IN; e2 = lexp                                   { Letin(id, e1, e2) }
    | LET; REC; id = ID; ASS; e1 = lexp; IN; e2 = lexp                              { Letrecin(id, e1, e2) }
    | LET; LPAR; idlst = id_list; RPAR; ASS; e1 = lexp; IN; e2 = lexp               { LetinTuple(idlst, e1, e2) }
    | IF; c = lexp; THEN; t = lexp; ELSE; e = lexp; END                             { If(c, t, e) }
    | LAM; id = ID; DOT; l = lexp                                                   { Lam(id, None, None, l) }
    | LAM; id = ID; COLON; t = typ_anot; DOT; l = lexp                              { Lam(id, Some t, None, l) }
    | LAM; id = ID; COLON; inT = typ_anot; OUTTYP; outT = typ_anot; DOT; l = lexp   { Lam(id, Some inT, Some outT, l) }
    | LAM; LPAR; RPAR; l = lexp                                                     { LamUnit(l) }
    | lc = lexp_cmp                                                                 { lc }

typ_anot:
    | tlst = typ_tuple_list; FUNTYP; rtyp = typ_anot        { TFun (TTup tlst, rtyp) }
    | atom = typ_anot_atom; FUNTYP; rtyp = typ_anot         { TFun (atom, rtyp) }
    | tlst = typ_tuple_list                                 { TTup tlst }
    | atom = typ_anot_atom                                  { atom }

typ_tuple_list:
    | ltyp = typ_anot_atom; MUL_I32; rtyp = typ_tuple_list { ltyp :: rtyp }
    | ltyp = typ_anot_atom; MUL_I32; rtyp = typ_anot_atom  { [ltyp; rtyp] }

typ_anot_atom: 
    | I32TYP                                                { TI32 }
    | I8TYP                                                 { TI8 }
    | LPAR; RPAR                                            { TUnit }
    | LBRACK; ityp = typ_anot; RBRACK                       { TVec ityp }
    | LPAR; ityp = typ_anot; RPAR                           { ityp }


lexp_cmp:
    | ll = lexp_cmp; EQ_I32; lr = lexp_bor_i32              { BopI32(Eqi32, ll, lr) }
    | ll = lexp_cmp; NEQ_I32; lr = lexp_bor_i32             { BopI32(Neqi32, ll, lr) }
    | ll = lexp_cmp; LT_I32; lr = lexp_bor_i32              { BopI32(Lti32, ll, lr) }
    | ll = lexp_cmp; GT_I32; lr = lexp_bor_i32              { BopI32(Gti32, ll, lr) }
    | ll = lexp_cmp; LTEQ_I32; lr = lexp_bor_i32            { BopI32(LtEqi32, ll, lr) }
    | ll = lexp_cmp; GTEQ_I32; lr = lexp_bor_i32            { BopI32(GtEqi32, ll, lr) }
    | ll = lexp_cmp; ULT_I32; lr = lexp_bor_i32             { BopI32(ULti32, ll, lr) }
    | ll = lexp_cmp; UGT_I32; lr = lexp_bor_i32             { BopI32(UGti32, ll, lr) }
    | ll = lexp_cmp; ULTEQ_I32; lr = lexp_bor_i32           { BopI32(ULtEqi32, ll, lr) }
    | ll = lexp_cmp; UGTEQ_I32; lr = lexp_bor_i32           { BopI32(UGtEqi32, ll, lr) }
    | ll = lexp_cmp; EQ_I8; lr = lexp_bor_i32               { BopI8(Eqi8, ll, lr) }
    | ll = lexp_cmp; NEQ_I8; lr = lexp_bor_i32              { BopI8(Neqi8, ll, lr) }
    | ll = lexp_cmp; LT_I8; lr = lexp_bor_i32               { BopI8(Lti8, ll, lr) }
    | ll = lexp_cmp; GT_I8; lr = lexp_bor_i32               { BopI8(Gti8, ll, lr) }
    | ll = lexp_cmp; LTEQ_I8; lr = lexp_bor_i32             { BopI8(LtEqi8, ll, lr) }
    | ll = lexp_cmp; GTEQ_I8; lr = lexp_bor_i32             { BopI8(GtEqi8, ll, lr) }
    | lb = lexp_bor_i32                                     { lb }

lexp_bor_i32:
    | ll = lexp_bor_i32; OR_I32; lr = lexp_bxor_i32         { BopI32(Ori32, ll, lr) }
    | ll = lexp_bor_i32; OR_I8; lr = lexp_bxor_i32          { BopI8(Ori8, ll, lr) }
    | lb = lexp_bxor_i32                                    { lb }

lexp_bxor_i32:
    | ll = lexp_bxor_i32; XOR_I32; lr = lexp_band_i32       { BopI32(Xori32, ll, lr) }
    | ll = lexp_bxor_i32; XOR_I8; lr = lexp_band_i32        { BopI8(Xori8, ll, lr) }
    | lb = lexp_band_i32                                    { lb }

lexp_band_i32:
    | ll = lexp_band_i32; AND_I32; lr = lexp_shift_i32      { BopI32(Andi32, ll, lr) }
    | ll = lexp_band_i32; AND_I8; lr = lexp_shift_i32       { BopI8(Andi8, ll, lr) }
    | lb = lexp_shift_i32                                   { lb }

lexp_shift_i32:
    | ll = lexp_shift_i32; SHL_I32; lr = lexp_add           { BopI32(Shli32, ll, lr) }
    | ll = lexp_shift_i32; SHR_I32; lr = lexp_add           { BopI32(Shri32, ll, lr) }
    | ll = lexp_shift_i32; USHR_I32; lr = lexp_add          { BopI32(UShri32, ll, lr) }
    | la = lexp_add                                         { la }

lexp_add:
    | ll = lexp_add; ADD_I32; lr = lexp_mul                 { BopI32(Addi32, ll, lr) }
    | ll = lexp_add; SUB_I32; lr = lexp_mul                 { BopI32(Subi32, ll, lr) }
    | ll = lexp_add; ADD_I8; lr = lexp_mul                  { BopI8(Addi8, ll, lr) }
    | ll = lexp_add; SUB_I8; lr = lexp_mul                  { BopI8(Subi8, ll, lr) }
    | lm = lexp_mul                                         { lm }

lexp_mul:
    | ll = lexp_mul; MUL_I32; lr = lexp_unary               { BopI32(Muli32, ll, lr) }
    | ll = lexp_mul; DIV_I32; lr = lexp_unary               { BopI32(Divi32, ll, lr) }
    | ll = lexp_mul; MOD_I32; lr = lexp_unary               { BopI32(Modi32, ll, lr) }
    | ll = lexp_mul; UDIV_I32; lr = lexp_unary              { BopI32(UDivi32, ll, lr) }
    | ll = lexp_mul; UMOD_I32; lr = lexp_unary              { BopI32(UModi32, ll, lr) }
    | lu = lexp_unary                                       { lu }

lexp_unary:
    | SUB_I32; l = lexp_unary                               { UopI32(Negi32, l) }
    | NOT_I32; l = lexp_unary                               { UopI32(Noti32, l) }
    | SUB_I8; l = lexp_unary                                { UopI8(Negi8, l) }
    | NOT_I8; l = lexp_unary                                { UopI8(Noti8, l) }
    | la = lexp_app                                         { la }

lexp_app:
    | ll = lexp_app; lr = lexp_atom                         { App(ll, lr) } (*left rec on applicaiton*)
    | la = lexp_atom                                        { la }

lexp_atom:
    | id = ID                                                                   { Var id }
    | incl = ID; DOT; id = ID                                                   { Var (incl ^ "." ^ id) }
    | LPAR; RPAR;                                                               { UnitLit }
    | i32lit = I32                                                              { I32Lit i32lit }
    | i8lit = I8                                                                { I8Lit i8lit }
    | str = STR                                                                 { VecLit (List.map (fun x -> I8Lit x) (List.of_seq (String.to_seq str))) }
    | LPAR; ls = tup_lexp_list; RPAR;                                               { Tuple ls }
    | VECLIT; LBRACK; ls = lexp_list; RBRACK                                    { VecLit ls }
    | VECMK; LBRACK; lit = lexp; COMMA; len = lexp; RBRACK                      { Vecmk(lit, len) }
    | VECLEN; LBRACK; v = lexp; RBRACK                                          { Veclen v }
    | VECGET; LBRACK; v = lexp; COMMA; i = lexp; RBRACK                         { Vecget(v, i) }
    | VECSET; LBRACK; v = lexp; COMMA; i = lexp; COMMA; value = lexp; RBRACK    { Vecset(v, i, value) }
    | LPAR; l = lexp; RPAR                                                      { l }

lexp_list:
    | l = lexp; COMMA; ls = lexp_list                       { l :: ls }
    | l = lexp                                              { [l] }

tup_lexp_list:
    | l = lexp; COMMA; ls = tup_lexp_list                       { l :: ls }
    | l1 = lexp; COMMA; l2 = lexp                               { [l1; l2] }

id_list:
    | id = ID; COMMA; ids = id_list                         { id :: ids }
    | id = ID                                               { [id] }
