(*Basic AST*)
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

(*after parser before include resolution*)
type stmt = 
    | IncludeGlobal of string
    | IncludeRelative of string
    | Nlexp of string * lexp 
    | Lexp of lexp           

type parseout = stmt list    (*what the parser spits out: includes not resolved, final lexp not split of*)

(*after include resolution before type checking*)
type letblk = (string * lexp) list

type prog = letblk * lexp option

(*typechecking*)
type typ =
  | TInt
  | TVec of typ
  | TFun of typ * typ
  | TVar of tvar
  
and tvar = {
  id : int;
  mutable link : typ option; (* None = unsolved, Some t = solved *)
}

(*here as it is a basic necessary when working with types*)
let repr (t : typ) : typ =
  let rec repr_aux t visited =
    match t with
    | TVar {id; link = Some t_linked; _ } -> 
        if List.mem id visited then 
          raise (Errors.TypeError ("[repr] Occurs Check Failed: Recursive types are not allowed. Found tvar: t" ^ string_of_int id ^ "again")) 
        else
          repr_aux t_linked (id :: visited)
    | _ -> t
  in repr_aux t [] 

type uuid = int 

type schema = Forall of int list * typ

type typenv = (string * (schema * int)) list

type constraints = (typ * typ) list

(*Instantiation Registry, returned by typechecker used by monomorphization*)
type instreg = (int * ((int*tvar) list)) list

(*after type checking before interp or code generation*)
type lexpt =
    | VarT of string * uuid * typ                        (*x : T*)
    | LamT of string * uuid * lexpt * typ                (*\x.y : T*)
    | AppT of lexpt * lexpt * typ               (*x y : T*)
    | IntT of int * typ                         (*int : T*)
    | BopT of bop * lexpt * lexpt * typ         (*x bop y : T*)
    | IfT of lexpt * lexpt * lexpt * typ        (*if c then t else e : T*)
    | LetinT of string * uuid * lexpt * lexpt * typ      (*let x = e in b : T*)
    | VeclitT of lexpt list * typ               (*vec[e1, e2, ..., en] : T*)
    | VecmkT of lexpt * lexpt * typ             (*vecmk[defval, count] : T*)
    | VeclenT of lexpt * typ                    (*veclen[v] : T*)
    | VecgetT of lexpt * lexpt * typ            (*vecget[v, i] : T*)
    | VecsetT of lexpt * lexpt * lexpt * typ    (*vecset[v, i, val] : T*)
    
type letblkt = (string * uuid * lexpt) list

type progt = letblkt * lexpt option



(*Helpers*)
let lexpt_get_type (e : lexpt) : typ =
  match e with
  | VarT (_, _, t) -> t
  | LamT (_, _, _, t) -> t
  | AppT (_, _, t) -> t
  | IntT (_, t) -> t
  | BopT (_, _, _, t) -> t
  | IfT (_, _, _, t) -> t
  | LetinT (_, _, _, _, t) -> t
  | VeclitT (_, t) -> t
  | VecmkT (_, _, t) -> t
  | VeclenT (_, t) -> t
  | VecgetT (_, _, t) -> t
  | VecsetT (_, _, _, t) -> t

let schema_of_typ (t : typ) : schema = Forall ([], t)
