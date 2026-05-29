type bop = 
    | Eq | Lt | Mul | Sub | Add | Div

type lexp =                         (*lambda expression*)
    | Var of string                 (*x*)
    | Lam of string * lexp          (*\x.y*)
    | App of lexp * lexp            (*x y*)
    | Int of int                    (*int*)
    | Bop of bop * lexp * lexp      (*x bop y*)
    | If of lexp * lexp * lexp      (*if c then t else e*)
    | Letin of string * lexp * lexp (*let x = e in b*)
    | Veclit of lexp list           (*vec[e1, e2, ..., en]*)
    | Vecmk of lexp * lexp          (*vecmk[defval, count]*)
    | Veclen of lexp                (*veclen[v]*)
    | Vecget of lexp * lexp         (*vecget[v, i]*)
    | Vecset of lexp * lexp * lexp  (*vecset[v, i, val]*)

type stmt = 
    | Include of string
    | Nlexp of string * lexp (*named lambda expression*)
    | Lexp of lexp           (*lambda expression, that will be evaulated*)

type parseout = stmt list    (*what the parser spits out: includes not resolved, final lexp not split of*)

type prog = (string * lexp) list * lexp option (*restructured program, used after the includes are resolved*)