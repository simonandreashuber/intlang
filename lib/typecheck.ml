open Ast
open Errors

(*CONFIGURATION*)
(*
Prints debug to stderr, debug for some program includes:
  - SCC
  - For each letblk and the final lexp
    - All collected constraints with AST annotations
    - A tvar legend noting where in the AST they originated from
    - Every linking occurring during unification
*)
let print_debug = ref false


(*BASIC STRUCTURES USED FOR TYPE CHECKING*)
type typ =
  | TInt
  | TVec of typ
  | TFun of typ * typ
  | TVar of tvar
  
and tvar = {
  id : int;
  mutable level : int;       (*how deep in the let in structure are we*)
  mutable link : typ option; (* None = unsolved, Some t = solved *)
}

type schema = Forall of int list * typ

type constraints = (typ * typ) list

type type_env = (string * schema) list

type letblk = (string * Ast.lexp) list

module SSet = Set.Make(String)


(*ELEMENTARY FUNCTIONS USED DURING *)
let counter = ref 0
let global_level = ref 0
let debug_tvar_notes = ref ""
let debug_constraints_notes = ref ""

let fresh_tvar (note : string option) : tvar =
  let id = !counter in
  if !print_debug then debug_tvar_notes := !debug_tvar_notes ^ "t" ^ string_of_int id ^ ": " ^ (match note with Some n -> n | None -> "") ^ "\n";
  counter := id + 1;
  { id; level = !global_level; link = None }

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

(*PRINT FUNCTIONS*)

(*not sure if its always correct, all observed bugs were fixed*)
let sprint_typ (t : typ) : string =
  let rec aux (t : typ)  (vis : (int * int) list) : string * (int* int) list =
    match t with
    | TInt -> "int", vis
    | TVec t_inner -> 
        let t_inner_str, vis_t_inner = aux t_inner vis in
        "Vec[" ^ t_inner_str ^ "]", vis_t_inner
    | TFun (t1, t2) -> (
        let t1str, vis_t1 = aux t1 vis in
        let left = match t1 with
          | TFun _ -> "(" ^ t1str ^ ")"
          | _ -> t1str
        in
        let t2str, vis_t2 = aux t2 vis_t1 in
        left ^ " -> " ^ t2str, vis_t2
    )
    | TVar {id; link = Some t_linked; _ } -> 
        (
          match List.assoc_opt id vis with
            | Some count -> raise (Errors.TypeError "Type loop detected during printing: Recursive types are not allowed")
            | None -> (
              let t_linked_str, vis_t_linked = aux t_linked ((id, 0) :: vis) in
              match List.assoc_opt id vis_t_linked with
                | Some count when count > 0 -> "(" ^ t_linked_str ^ " as t" ^ string_of_int id ^ ")", List.filter (fun (i, _) -> i <> id) vis_t_linked
                | _ -> t_linked_str, List.filter (fun (i, _) -> i <> id) vis_t_linked
            )
        ) 
    | TVar {id; level; link = None; _ } -> "t" ^ string_of_int id (*^ " (lvl: " ^ string_of_int level ^ ")"*), vis
  in 
  let res_str, _ = aux t [] in
  res_str

let sprint_env (env : type_env) : string =
  let bindings = List.map (fun (name, Forall (vars, t)) -> 
    let vars_str = if vars = [] then "" else "forall " ^ String.concat " " (List.map (fun v -> "t" ^ string_of_int v) vars) ^ ". " in
    name ^ " : " ^ vars_str ^ sprint_typ t
  ) env in
  String.concat "\n" bindings

let sprint_constraints (cs : constraints) : string =
  let cs_strs = List.map (fun (t1, t2) -> sprint_typ t1 ^ " = " ^ sprint_typ t2) cs in
  String.concat "\n" cs_strs

let sprint_scc (topord : letblk list) : string =
  let scc_strs = List.mapi (fun i blk -> 
    let blk_str = String.concat ", " (List.map (fun (name, lexp) -> name) blk) in
    Printf.sprintf "SCC %d:[%s]" i blk_str
  ) topord in
  String.concat "\n" scc_strs


(*UNIFICATION ENGINE*)
let rec unify (t1 : typ) (t2 : typ) : unit =
  (*you put a tvar v into this that has a link => not good*)
  let rec occurscheck_levelupdt (v : tvar) (t : typ) : unit =
      match repr t with
      | TVar v' ->  if v.id = v'.id then 
                      raise (Errors.TypeError ("[occurscheck_levelupdt] Occurs Check Failed: Recursive types are not allowed. Found tvar: t" ^ string_of_int v.id ^ " again")) 
                    else 
                      v'.level <- min v'.level v.level; ()
      | TFun (t1, t2) -> occurscheck_levelupdt v t1; occurscheck_levelupdt v t2
      | TInt -> () 
      | TVec t_inner -> occurscheck_levelupdt v t_inner
  in

  match (repr t1, repr t2) with
  | (TInt, TInt) -> ()
  | (TFun (t1f, t1x), TFun (t2f, t2x)) -> 
      unify t1f t2f;
      unify t1x t2x
  | (TVec t1_inner, TVec t2_inner) -> unify t1_inner t2_inner;
  | (TVar v, t) | (t, TVar v) -> (
      match repr t with
      | TVar v' when v.id = v'.id -> () (*t0 does not occur in t0 but it would in TVec[t0]*)
      | _ -> (
          occurscheck_levelupdt v t;
          if !print_debug then Printf.eprintf "Linking t%d (lvl: %d) to %s\n" v.id v.level (sprint_typ t);
          v.link <- Some t
      )
    )
      
  | _ -> raise (Errors.TypeError ("Type mismatch: Cannot unify " ^ sprint_typ (repr t1) ^ " with " ^ sprint_typ (repr t2)))  

let rec generalize (t : typ) : schema =
  match repr t with
  | TInt -> Forall ([], TInt)
  | TFun (t1, t2) ->
      let Forall (vars1, t1_gen) = generalize t1 in
      let Forall (vars2, t2_gen) = generalize t2 in
      let unique_vars = List.sort_uniq compare (vars1 @ vars2) in
      Forall (unique_vars, TFun (t1_gen, t2_gen))
  | TVec t_inner ->
      let Forall (vars_inner, t_inner_gen) = generalize t_inner in
      Forall (vars_inner, TVec t_inner_gen)
  | TVar v -> if v.level > !global_level then Forall ([v.id], TVar v) else Forall ([], TVar v)

let instantiate (Forall (vars, t) : schema) : typ =
  let unique_vars = List.sort_uniq compare vars in (*sanitization*)
  let fresh_var_map = List.map (fun var -> var, TVar (fresh_tvar (Some (Printf.sprintf "instantiate inplace of %d" var)))) unique_vars in
  let rec instaux (varmap : (int * typ) list) (t: typ) : typ =
    match repr t with
      | TVar v -> (
          match List.assoc_opt v.id varmap with
            | Some fresh_t -> fresh_t
            | None -> t
        )
      | TFun (t1, t2) -> TFun (instaux varmap t1, instaux varmap t2)
      | TVec t_inner -> TVec (instaux varmap t_inner)
      | TInt -> TInt 
  in instaux fresh_var_map t


let rec typecheck_lexp (e : lexp) (env : type_env) : constraints * typ = (*what e not as second arg, I am ocaml noob*)
  match e with
    | Var x -> (
      match List.assoc_opt x env with
      | Some s -> ([], instantiate s) 
      | None -> raise (Errors.TypeError ("Unbound variable: " ^ x))
      )
    | Lam (x, eb) -> (
      let tv = TVar (fresh_tvar (Some (Printf.sprintf "lambda %s" (PrintIntlang.sprint_lexp e)))) in
      let env' = (x, Forall ([], tv)) :: env in (*it is enough to stitch it to the front as assoc_opt just finds the first one*)
      let cs, t_body = typecheck_lexp eb env' in
      (cs, TFun (tv, t_body))
      )
    | App (f, x) -> (
      let t_out = TVar (fresh_tvar (Some (Printf.sprintf "application %s" (PrintIntlang.sprint_lexp e)))) in
      let cs_f, t_f = typecheck_lexp f env in
      let cs_x, t_x = typecheck_lexp x env in
      if !print_debug then debug_constraints_notes := !debug_constraints_notes ^ (Printf.sprintf "Application constraints for %s:\n    %s\n" (PrintIntlang.sprint_lexp e) (sprint_constraints [(t_f, TFun (t_x, t_out))]));
      ((t_f, TFun (t_x, t_out)) :: (cs_f @ cs_x), t_out)
      )
    | Int i -> ([], TInt)
    | Bop (bop, e1, e2) -> (
      let cs1, t1 = typecheck_lexp e1 env in
      let cs2, t2 = typecheck_lexp e2 env in
      if !print_debug then debug_constraints_notes := !debug_constraints_notes ^ (Printf.sprintf "Bop constraints for %s:\n    %s\n    %s\n" (PrintIntlang.sprint_lexp e) (sprint_constraints [(t1, TInt)]) (sprint_constraints [(t2, TInt)]));
      ((t1, TInt) :: (t2, TInt) :: (cs1 @ cs2), TInt) (*decided against a TInt and TIntNonZero type since it does complicated things and I prefer to just user runtime checks anyways*)
      )
    | If (c, t, e) -> (
      let cs_c, t_c = typecheck_lexp c env in
      let cs_t, t_t = typecheck_lexp t env in
      let cs_e, t_e = typecheck_lexp e env in
      if !print_debug then debug_constraints_notes := !debug_constraints_notes ^ (Printf.sprintf "If constraints for %s:\n    %s\n    %s\n" (PrintIntlang.sprint_lexp e) (sprint_constraints [(t_c, TInt)]) (sprint_constraints [(t_t, t_e)]));
      ((t_c, TInt) :: (t_t, t_e) :: (cs_c @ cs_t @ cs_e), t_t)
      )
    | Letin (x, e, b) -> (
      let env' = typecheck_letblk [(x, e)] env (-1) in
      typecheck_lexp b env'
      )
    | Veclit ls -> (
      if List.length ls = 0 then raise (Errors.TypeError "Cannot infer type of empty vector literal, how did this get past the parser?") 
      else
        let cs_ls, t_ls = List.split (List.map (fun lexp_i -> typecheck_lexp lexp_i env) ls) in
        let cs = List.flatten cs_ls in
        (*while a new TVar is not strictly needed here, it does lend itself better to ocaml programming imo*)
        let t_elem = TVar (fresh_tvar (Some (Printf.sprintf "vector literal element type for %s" (PrintIntlang.sprint_lexp e)))) in
        let elem_constraints = List.map (fun t_i -> (t_i, t_elem)) t_ls in
        if !print_debug then debug_constraints_notes := !debug_constraints_notes ^ (Printf.sprintf "Vector literal constraints for %s:\n    %s\n" (PrintIntlang.sprint_lexp e) (sprint_constraints elem_constraints));
        (elem_constraints @ cs, TVec t_elem)
    )
    | Vecmk (defval, count) -> (
      let cs_defval, t_defval = typecheck_lexp defval env in
      let cs_count, t_count = typecheck_lexp count env in
      if !print_debug then debug_constraints_notes := !debug_constraints_notes ^ (Printf.sprintf "Vector make constraints for %s:\n    %s\n" (PrintIntlang.sprint_lexp e) (sprint_constraints [(t_count, TInt)]));
      ((t_count, TInt) :: (cs_defval @ cs_count), TVec t_defval)
    )
    | Veclen v -> (
      let cs_v, t_v = typecheck_lexp v env in
      let t_vec_of = TVec(TVar (fresh_tvar (Some "vector length on this type"))) in
      if !print_debug then debug_constraints_notes := !debug_constraints_notes ^ (Printf.sprintf "Vector length constraints for %s:\n    %s\n" (PrintIntlang.sprint_lexp e) (sprint_constraints [(t_v, t_vec_of)]));
      ((t_v, t_vec_of) :: cs_v, TInt)
    )
    | Vecget (v, i) -> (
      let cs_v, t_v = typecheck_lexp v env in
      let cs_i, t_i = typecheck_lexp i env in
      let t_vec_of = TVar (fresh_tvar (Some "vector get on this type")) in
      let t_vec = TVec t_vec_of in
      if !print_debug then debug_constraints_notes := !debug_constraints_notes ^ (Printf.sprintf "Vector get constraints for %s:\n    %s\n    %s\n" (PrintIntlang.sprint_lexp e) (sprint_constraints [(t_v, t_vec)]) (sprint_constraints [(t_i, TInt)]));
      ((t_v, t_vec) :: (t_i, TInt) :: cs_v @ cs_i, t_vec_of)
    )
    | Vecset (v, i, value) -> (
      let cs_v, t_v = typecheck_lexp v env in
      let cs_i, t_i = typecheck_lexp i env in
      let cs_val, t_val = typecheck_lexp value env in
      let t_vec_of = TVar (fresh_tvar (Some "vector set on a vector of this type")) in
      let t_vec = TVec t_vec_of in
      if !print_debug then debug_constraints_notes := !debug_constraints_notes ^ (Printf.sprintf "Vector set constraints for %s:\n    %s\n    %s\n    %s\n" (PrintIntlang.sprint_lexp e) (sprint_constraints [(t_v, t_vec)]) (sprint_constraints [(t_i, TInt)]) (sprint_constraints [(t_val, t_vec_of)]));
      ((t_v, t_vec) :: (t_i, TInt) :: (t_val, t_vec_of) :: cs_v @ cs_i @ cs_val, t_vec)
    )

and typecheck_letblk (letblk : letblk) (env : type_env) (letblkid : int): type_env =
  (*debug things*)
  let headerline = "---------------------------------------------\n" in
  if !print_debug then 
    begin
     debug_constraints_notes := headerline ^ "CONSTRAINTS:\n" ^ headerline; debug_tvar_notes := headerline ^ "TVARS:\n" ^ headerline; 
    end;

  (*drop global level*)
  let old_global_level = !global_level in
  global_level := old_global_level + 1;

  (*add all let defs to env*)
  let env_with_letdefs = List.fold_left 
    (fun env' (name, lexp) -> (name, Forall ([], TVar (fresh_tvar (Some (Printf.sprintf "letblk %s" name))))) :: env') 
    env letblk in
  
  (*collect all constraints*)
  let constraints = List.fold_left 
    (fun cs (name, lexp) -> 
      let cs', t = typecheck_lexp lexp env_with_letdefs in (*we add this binding in the step before, it must exist so no need to check *)
      let Forall (_, tv) = List.assoc name env_with_letdefs in (*it is impossible to have something generalized here*)
      (tv, t) :: (cs' @ cs)) (*Not in debug, would ruin my cool headerlines :() *)
    [] letblk in
  
  if !print_debug then
    begin
      Printf.eprintf "%sLETBLK %d:\n%s\n%s" headerline letblkid (sprint_env env) headerline;
      Printf.eprintf "%s" !debug_tvar_notes;
      Printf.eprintf "%s" !debug_constraints_notes;
      Printf.eprintf "%sUNIFICATION\n%s" headerline headerline;
    end;

  (*unify all constraints*)
  List.iteri (fun i (t1, t2) -> 
    unify t1 t2;
  ) constraints;

  (*restore global level*)
  global_level := old_global_level;

  (*generalize all types and add to env*)
   let generalized_env = List.fold_left 
    (fun env' (name, lexp) -> 
      let Forall (_, tv) = List.assoc name env_with_letdefs in
      let gen_type = generalize tv in
      (name, gen_type) :: env')
    env letblk in
  if !print_debug then Printf.eprintf "%sGeneralized types for letblk %d:\n%s\n%s" headerline letblkid (sprint_env generalized_env) headerline;
  generalized_env

(*SCC (Strongly Connected Components) analysis for polymorphic types*)
(* Extracts free variables from an expression, ignoring locally bound parameters *)
let rec free_vars (locals : SSet.t) (e : lexp) : SSet.t =
  match e with
  | Int _ -> SSet.empty
  | Var x -> 
      if SSet.mem x locals then SSet.empty else SSet.singleton x
  | Lam (x, body) -> 
      (* Add 'x' to locals so it shadows outer variables inside the body *)
      free_vars (SSet.add x locals) body
  | App (e1, e2) -> 
      SSet.union (free_vars locals e1) (free_vars locals e2)
  | Bop (_, e1, e2) -> 
      SSet.union (free_vars locals e1) (free_vars locals e2)
  | If (c, t, e) ->
      SSet.union (free_vars locals c) (SSet.union (free_vars locals t) (free_vars locals e))
  | Letin (x, e, b) ->
      let free_in_e = free_vars locals e in
      let free_in_b = free_vars (SSet.add x locals) b in
      SSet.union free_in_e free_in_b
  | Veclit ls ->
      List.fold_left (fun acc lexp_i -> SSet.union acc (free_vars locals lexp_i)) SSet.empty ls
  | Vecmk (defval, count) ->
      SSet.union (free_vars locals defval) (free_vars locals count)
  | Veclen v ->
      free_vars locals v
  | Vecget (v, i) ->
      SSet.union (free_vars locals v) (free_vars locals i)
  | Vecset (v, i, value) ->
      SSet.union (free_vars locals v) (SSet.union (free_vars locals i) (free_vars locals value))

let scc_split_letblk (blk : letblk) : letblk list =
  (* 1. Identify the universe of names defined in this specific block *)
  let blk_names = List.fold_left (fun acc (n, _) -> SSet.add n acc) SSet.empty blk in
  
  (* 2. Build the dependency graph using Hash tables *)
  let expr_map = Hashtbl.create (List.length blk) in
  let graph = Hashtbl.create (List.length blk) in
  
  List.iter (fun (name, expr) ->
    Hashtbl.add expr_map name expr;
    let fvs = free_vars SSet.empty expr in
    (* Only care about edges to other variables in THIS letblk *)
    let deps = SSet.inter fvs blk_names in 
    Hashtbl.add graph name (SSet.elements deps)
  ) blk;

  (* 3. Tarjan's Algorithm State *)
  let index = ref 0 in
  let indices = Hashtbl.create 16 in
  let lowlinks = Hashtbl.create 16 in
  let on_stack = Hashtbl.create 16 in
  let stack = Stack.create () in
  let sccs = ref [] in

  (* The core DFS function *)
  let rec strongconnect v =
    Hashtbl.add indices v !index;
    Hashtbl.add lowlinks v !index;
    Hashtbl.add on_stack v true;
    Stack.push v stack;
    index := !index + 1;

    let neighbors = try Hashtbl.find graph v with Not_found -> [] in
    List.iter (fun w ->
      if not (Hashtbl.mem indices w) then begin
        (* Successor w has not yet been visited; recurse *)
        strongconnect w;
        let v_low = Hashtbl.find lowlinks v in
        let w_low = Hashtbl.find lowlinks w in
        Hashtbl.replace lowlinks v (min v_low w_low)
      end else if Hashtbl.mem on_stack w then begin
        (* Successor w is in the current SCC *)
        let v_low = Hashtbl.find lowlinks v in
        let w_idx = Hashtbl.find indices w in
        Hashtbl.replace lowlinks v (min v_low w_idx)
      end
    ) neighbors;

    (* If v is a root node, pop the stack and generate an SCC *)
    if Hashtbl.find lowlinks v = Hashtbl.find indices v then begin
      let current_scc = ref [] in
      let looping = ref true in
      while !looping do
        let w = Stack.pop stack in
        Hashtbl.remove on_stack w;
        let expr = Hashtbl.find expr_map w in
        current_scc := (w, expr) :: !current_scc;
        if w = v then looping := false
      done;
      sccs := !current_scc :: !sccs
    end
  in

  (* 4. Execute Tarjan's on all nodes in the block *)
  List.iter (fun (name, _) ->
    if not (Hashtbl.mem indices name) then strongconnect name
  ) blk;

  (* 5. Tarjan's pushes components backwards, so we reverse the final 
        list to get a perfect topological type-checking order! *)
  List.rev !sccs


(*
  expects: a program (with all Nlexp except for the last one being a lexp)
  returns: unit if type checks, otherwise raises Errors.TypeError with an error message
*)
let typecheck ((global_letblk, mainlexp_opt) : prog) :  type_env =
  counter := 0; (*reset tvar counter for consistency*)
  let headerline = "---------------------------------------------\n" in
  (*split letblk into scc*) 
  let letscc = scc_split_letblk global_letblk in
  if !print_debug then Printf.eprintf "[typecheck] SCC split into %d groups: \n%s\n" (List.length letscc) (sprint_scc letscc);

  (*typecheck each scc*)
  let env, _ = List.fold_left (fun (env,i) letblk -> 
    (typecheck_letblk letblk env i, i+1)
  ) ([], 0) letscc in
  if !print_debug then Printf.eprintf "%sAll SCCs processed, FINAL ENV: \n%s%s\n" headerline headerline (sprint_env env);
  
  (*typecheck the main expression*)
  if mainlexp_opt != None then (
    let mainlexp = Option.get mainlexp_opt in
    let env_main = typecheck_letblk [("@main", mainlexp)] env (-1) in
    match List.assoc_opt "@main" env_main with
      | Some (Forall ([], TInt)) -> if !print_debug then Printf.eprintf "Main expression has type int: OK\n\n"; env
      | Some s -> raise (Errors.TypeError ("Final expression has type " ^ sprint_typ (instantiate s) ^ " but expected int (this is intlang ;))"))
      | None -> raise (Errors.TypeError "Internal Error")
  )
  else env