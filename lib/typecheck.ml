open Ast

(*CONFIGURATION*)
(*
  allow recursive types
  it is on by default but there is the switch here to turn it of for some sanity ;)
*)
let allowrectypes = ref true
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
  | TFun of typ * typ
  | TVar of tvar
  
and tvar = {
  id : int;
  (*mutable level : int;
    A note on levels: If one was to introduce a "let in" concept, then one would need a level system to prevent
                      the generalization of some vars that are fixed by higher scopes, but since Intlang does not have this.....
  *)
  mutable link : typ option; (* None = unsolved, Some t = solved *)
  note : string option; (*note option for debug*)
}

type schema = Forall of int list * typ

type constraints = (typ * typ) list

type type_env = (string * schema) list

type letblk = (string * Ast.lexp) list

module SSet = Set.Make(String)

exception TypeError of string

(*ELEMENTARY FUNCTIONS USED DURING *)
let counter = ref 0
let debug_tvar_notes = ref ""
let debug_constraints_notes = ref ""

let fresh_tvar (note : string option) : tvar =
  let id = !counter in
  if !print_debug then debug_tvar_notes := !debug_tvar_notes ^ "t" ^ string_of_int id ^ ": " ^ (match note with Some n -> n | None -> "") ^ "\n";
  counter := id + 1;
  { id; link = None ; note = note}

let repr (t : typ) : typ =
  let rec repr_aux t visited =
    match t with
    | TVar {id; link = Some t_linked; _ } -> 
        if List.mem id visited then (
          if !allowrectypes then t else raise (TypeError "Recursive types are disabled (you can enable them)")
        )
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
            | Some count ->
                if !allowrectypes then "t" ^ string_of_int id, (id, count+1) :: vis
                else raise (TypeError "Recursive types are disabled (you can enable them)")
            | None -> (
              let t_linked_str, vis_t_linked = aux t_linked ((id, 0) :: vis) in
              match List.assoc_opt id vis_t_linked with
                | Some count when count > 0 -> "(" ^ t_linked_str ^ " as t" ^ string_of_int id ^ ")", List.filter (fun (i, _) -> i <> id) vis_t_linked
                | _ -> t_linked_str, List.filter (fun (i, _) -> i <> id) vis_t_linked
            )
        ) 
    | TVar {id; link = None; _ } -> "t" ^ string_of_int id, vis
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
let unify (t1 : typ) (t2 : typ) : unit =
  (*you put a tvar v into this that has a link => not good*)
  let rec occurs_check (v : tvar) (t : typ) : bool =
    if !allowrectypes then false else
      match repr t with
      | TVar v' -> v.id = v'.id
      | TFun (t1, t2) -> occurs_check v t1 || occurs_check v t2
      | TInt -> false 
  in

  let rec unify_aux (vis : (typ * typ) list) (t1 : typ) (t2 : typ) : unit =
    (*Printf.eprintf "[unify] Attempting to unify types %s and  %s\n" (sprint_typ t1) (sprint_typ t2);*)
    let repr1 = repr t1 in
    let repr2 = repr t2 in
    if repr1 == repr2 then () (* Physical pointer match *)
    else if List.exists (fun (v1, v2) -> (v1 == repr1 && v2 == repr2) || (v1 == repr2 && v2 == repr1)) vis then () (*already visited*)
    else let vis' = (repr1, repr2) :: vis in 

    match (repr1, repr2) with
    | (TInt, TInt) -> ()
    | (TFun (t1f, t1x), TFun (t2f, t2x)) -> 
        unify_aux vis' t1f t2f;
        unify_aux vis' t1x t2x
    | (TVar v, t) | (t, TVar v) ->
        if occurs_check v t then
          raise (TypeError ("Occurs check failed: Recursive types are disabled (you can enable them). Cannot unify " ^ sprint_typ repr1 ^ " with " ^ sprint_typ repr2))
        else (
          if !print_debug then Printf.eprintf "Linking t%d to %s\n" v.id (sprint_typ t);
          v.link <- Some t
        )
    | _ -> raise (TypeError ("Type mismatch: Cannot unify " ^ sprint_typ repr1 ^ " with " ^ sprint_typ repr2))  
  in unify_aux [] t1 t2

let rec generalize (t : typ) : schema =
  match repr t with
  | TInt -> Forall ([], TInt)
  | TFun (t1, t2) ->
      let Forall (vars1, t1_gen) = generalize t1 in
      let Forall (vars2, t2_gen) = generalize t2 in
      let unique_vars = List.sort_uniq compare (vars1 @ vars2) in
      Forall (unique_vars, TFun (t1_gen, t2_gen))
  | TVar v -> Forall ([v.id], TVar v)

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
      | TInt -> TInt 
  in instaux fresh_var_map t


let rec typecheck_lexp (e : lexp) (env : type_env) : constraints * typ =
  match e with
    | Var x -> (
      match List.assoc_opt x env with
      | Some s -> ([], instantiate s) 
      | None -> raise (TypeError ("Unbound variable: " ^ x))
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
      ((t1, TInt) :: (t2, TInt) :: (cs1 @ cs2), TInt)
      )

let typecheck_letblk (letblk : letblk) (env : type_env) (letblkid : int): type_env =
  let headerline = "---------------------------------------------\n" in
  if !print_debug then 
    begin
     debug_constraints_notes := headerline ^ "CONSTRAINTS:\n" ^ headerline; debug_tvar_notes := headerline ^ "TVARS:\n" ^ headerline; 
    end;
  (*add all let defs to env*)
  let env_with_letdefs = List.fold_left 
    (fun env' (name, lexp) -> (name, Forall ([], TVar (fresh_tvar (Some (Printf.sprintf "letblk %s" name))))) :: env') 
    env letblk in

  (*collect all constraints*)
  let constraints = List.fold_left 
    (fun cs (name, lexp) -> 
      (*Printf.eprintf "[typecheck_letblk] Typechecking binding: %s\n" name;*)
      let cs', t = typecheck_lexp lexp env_with_letdefs in (*we add this binding in the step before, it must exist so no need to check *)
      (*Printf.eprintf "[typecheck_letblk] Got %d new constraints for %s\n" (List.length cs') name;*)
      let Forall (_, tv) = List.assoc name env_with_letdefs in (*it is impossible to have something generalized here*)
      (tv, t) :: (cs' @ cs)) 
    [] letblk in
  if !print_debug then
    begin
      Printf.eprintf "%sLETBLK %d:\n%s" headerline letblkid headerline;
      Printf.eprintf "%s" !debug_tvar_notes;
      Printf.eprintf "%s" !debug_constraints_notes;
      Printf.eprintf "%sUNIFICATION\n%s" headerline headerline;
    end;

  (*unify all constraints*)
  List.iteri (fun i (t1, t2) -> 
    unify t1 t2;
  ) constraints;

  (*generalize all types and add to env*)
   let generalized_env = List.fold_left 
    (fun env' (name, lexp) -> 
      let Forall (_, tv) = List.assoc name env_with_letdefs in
      let gen_type = generalize tv in
      (name, gen_type) :: env')
    env letblk in
  
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


(*No checks to prevent wrong Nlexp and Lexp orders*)
let rec split_prog (p:prog) : letblk * (Ast.lexp option) =
  List.fold_right 
  (fun stmt (letblk, lexp_opt) -> 
    match stmt with
      | Nlexp (name, e) -> ( (name, e) :: letblk, lexp_opt)
      | Lexp e -> (letblk, Some e)
    )
  p ([], None)

(*
  expects: a program (with all Nlexp except for the last one being a lexp)
  returns: unit if type checks, otherwise raises TypeError with an error message
*)
let typecheck (p : prog) :  unit =
  counter := 0; (*reset tvar counter for consistency*)
  let headerline = "---------------------------------------------\n" in
  let global_letblk, lexp_opt = split_prog p in
  let letscc = scc_split_letblk global_letblk in
  if !print_debug then Printf.eprintf "[typecheck] SCC split into %d groups: \n%s\n" (List.length letscc) (sprint_scc letscc);
  let env, _ = List.fold_left (fun (env,i) letblk -> 
    (typecheck_letblk letblk env i, i+1)
  ) ([], 0) letscc in
  if !print_debug then Printf.eprintf "%sAll SCCs processed, FINAL ENV: \n%s%s\n" headerline headerline (sprint_env env);
  match lexp_opt with
    | Some lexp -> (
        if !print_debug then 
          begin
          debug_constraints_notes := headerline ^ "CONSTRAINTS:\n" ^ headerline; debug_tvar_notes := headerline ^ "TVARS:\n" ^ headerline; 
          end;
        let constraints, exp_typ = typecheck_lexp lexp env in
        if !print_debug then
          begin
            Printf.eprintf "%sFINAL LEXP:\n%s" headerline  headerline;
            Printf.eprintf "%s" !debug_tvar_notes;
            Printf.eprintf "%s" !debug_constraints_notes;
            Printf.eprintf "%sUNIFICATION\n%s" headerline headerline;
          end;
        List.iteri (fun i (t1, t2) -> 
          unify t1 t2;
        ) constraints;
        let gen_exp_typ = generalize exp_typ in
        if !print_debug then Printf.eprintf "%sFINAL LEXP TYPECHECK RESULT:\n%s" headerline headerline;
        (if gen_exp_typ = Forall ([], TInt) then
          (if !print_debug then Printf.eprintf "Final expression is int: OK\n\n"; ())
        else
          raise (TypeError ("Final expression has type " ^ sprint_typ exp_typ ^ " but expected int (this is intlang ;))")))
    )
    | None -> if !print_debug then Printf.eprintf "No final expression\n"; ()