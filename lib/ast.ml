type bop = 
    | Eq | Lt | Mul | Sub | Add

type lexp =                     (*lambda expression*)
    | Var of string             (*x*)
    | Lam of string * lexp      (*\x.y*)
    | App of lexp * lexp        (*x y*)
    | Int of int                (*int*)
    | Bop of bop * lexp * lexp  (*x bop y*)

type stmt = 
    | Nlexp of string * lexp (*named lambda expression*)
    | Lexp of lexp           (*lambda expression, that will be evaulated*)

type prog = stmt list
