type bop = 
    | Eq | Lt | Mul | Sub | Add

type lexp =                     (*lambda expression*)
    | Var of string             (*x*)
    | Lam of string * lexp      (*\x.y*)
    | App of lexp * lexp        (*x y*)
    | Int of int                (*int*)
    | Bop of bop * lexp * lexp  (*x bop y*)

type nlexp = string * lexp (*named lambda expression*)

type prog = (list nlexp) * lexp (*list of named lambda expression with a final lambda expression as "entry"*)
