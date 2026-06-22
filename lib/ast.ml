(*typechecking*)
type typ =
  | TUnit
  | TI32
  | TI8
  | TFun of typ * typ
  | TTup of typ list
  | TVec of typ
  | TVar of tvar
  
and tvar = {
  id : int;
  mutable link : typ option; (* None = unsolved, Some t = solved *)
}

let tvar_counter = ref 0
let uuid_counter = ref 0

let fresh_tvar () : tvar =
  let id = !tvar_counter in
  tvar_counter := id + 1;
  { id; link = None }

let fresh_uuid () : int =
  let id = !uuid_counter in
  uuid_counter := id + 1;
  id

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

type typenv = (string * (schema * uuid)) list

type constraints = (typ * typ) list

(*Basic AST*)
type bopi32 = 
    | Eqi32 | Neqi32 | Lti32 | Gti32 | LtEqi32 | GtEqi32
    | ULti32 | UGti32 | ULtEqi32 | UGtEqi32
    | Muli32 | Subi32 | Addi32 | Divi32 | Modi32
    | UDivi32 | UModi32
    | Andi32 | Ori32 | Xori32
    | Shli32 | Shri32 | UShri32

type uopi32 = 
    | Negi32 | Noti32

type bopi8 = 
    | Eqi8 | Neqi8 | Lti8 | Gti8 | LtEqi8 | GtEqi8
    | Addi8 | Subi8
    | Andi8 | Ori8 | Xori8

type uopi8 = 
    | Negi8 | Noti8
            

type lexp =                                       (*expression*)
    | Var of string                                             (*x*)
    | Lam of string * typ option * typ option * lexp           (*\ x : inTyp => outTyp .y*)
    | LamUnit of lexp                             (*\().e*)
    | Letin of string * lexp * lexp               (*let x = e in b*)
    | Letrecin of string * lexp * lexp            (*letrec x = e in b*)
    | LetinTuple of string list * lexp * lexp     (*let (x1, x2, ..., xn) = e in b*)
    | Tuple of lexp list                          (*(e1, e2, ..., en)*)
    | App of lexp * lexp                          (*x y*)
    | I32Lit of int                                  (*i32*)
    | I8Lit of char                                  (*i8*)
    | UnitLit
    | UopI32 of uopi32 * lexp                     (*uop x*)
    | BopI32 of bopi32 * lexp * lexp              (*x bop y*)
    | UopI8 of uopi8 * lexp                       (*uop x*)
    | BopI8 of bopi8 * lexp * lexp                (*x bop y*)
    | If of lexp * lexp * lexp                    (*if c then t else e*)
    | VecLit of lexp list                         (*vec[e1, e2, ..., en]*)
    | Vecmk of lexp * (lexp list)                 (*vecmk[defval, size list]*)
    | Veclen of lexp                              (*veclen[v]*)
    | Vecget of lexp * (lexp list)                (*vecget[v, idx list]*)
    | Vecset of lexp * lexp * (lexp list)         (*vecset[v, val, idx list]*)
    | Vecresz of lexp * lexp * (lexp list)        (*vecresz[v, newlen, idx list]*)

(*after parser before include resolution*)
type stmt = 
    | IncludeGlobal of string
    | IncludeRelative of string
    | Let of string * lexp 
    | Letrec of string * lexp 
    | Letrecblk of string * lexp
    | Lexp of lexp     

type parseout = stmt list    (*what the parser spits out: includes not resolved, final lexp not split of*)

(*
(*after include resolution before type checking*)
type letblk = (string * lexp) list

type prog = letblk * lexp option

(*after type checking before interp or code generation*)
type lexpt =
    | VarT of (string ref) * (uuid ref) * typ   (*x : T, note: the name and uuid are ref here to so we can just repoint during monomorphization*)
    | LamT of string * uuid * lexpt * typ       (*\x.y : T*)
    | AppT of lexpt * lexpt * typ               (*x y : T*)
    | IntT of int * typ                         (*int : T*)
    | BopT of bop * lexpt * lexpt * typ         (*x bop y : T*)
    | IfT of lexpt * lexpt * lexpt * typ        (*if c then t else e : T*)
    | LetinT of string * uuid * lexpt * lexpt * typ      (*let x = e in b : T*)
    | VecLitT of lexpt list * typ               (*vec[e1, e2, ..., en] : T*)
    | VecmkT of lexpt * lexpt * typ             (*vecmk[defval, count] : T*)
    | VeclenT of lexpt * typ                    (*veclen[v] : T*)
    | VecgetT of lexpt * lexpt * typ            (*vecget[v, i] : T*)
    | VecsetT of lexpt * lexpt * lexpt * typ    (*vecset[v, i, val] : T*)
    
type letbndpolyt = string * uuid * (uuid list) * lexpt

type letblkpolyt = letbndpolyt list

type progpolyt = letblkpolyt * lexpt option

type letbndmonot = string * uuid * lexpt

type letblkmonot = letbndmonot list

type progmonot = letblkmonot * lexpt option


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
  | VecLitT (_, t) -> t
  | VecmkT (_, _, t) -> t
  | VeclenT (_, t) -> t
  | VecgetT (_, _, t) -> t
  | VecsetT (_, _, _, t) -> t

let schema_of_typ (t : typ) : schema = Forall ([], t)
*)