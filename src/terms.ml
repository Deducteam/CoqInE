(** Translation of Coq terms *)

open Term

let infer_type env t =
  (fst (Typeops.infer env t)).Environ.uj_type

let infer_sort env a = 
  (fst (Typeops.infer_type env a)).Environ.utj_type
  
let coq x = Dedukti.Var(Name.coq x)

let coq_type s = Dedukti.apps (coq "type") [s]
let coq_term s a = Dedukti.apps (coq "term") [s; a]
let coq_sort s = Dedukti.apps (coq "sort") [s]
let coq_prod s1 s2 a b = Dedukti.apps (coq "prod") [s1; s2; a; b]
let coq_cast s1 s2 a b t = Dedukti.apps (coq "cast") [s1; s2; a; b; t]

let translate_sort env s =
  match s with
  | Prop(Null) -> Universes.coq_p
  | Prop(Pos) -> Universes.coq_z
  | Type(i) -> Universes.translate_universe env i

let rec translate_constr env t =
  match Term.kind_of_term t with
  | Rel(i) ->
      let (x, _, _) = Environ.lookup_rel i env in
      Dedukti.var (Name.translate_name x)
  | Var(identifier) -> failwith "Not implemented: Var"
  | Meta(metavariable) -> failwith "Not implemented: Meta"
  | Evar(pexistential) -> failwith "Not implemented: Evar"
  | Sort(s) ->
      let s' = translate_sort env s in
      coq_sort s'
  | Cast(t, _, b) ->
      let a = infer_type env t in
      let s1 = infer_sort env a in
      let s2 = infer_sort env b in
      let s1' = translate_sort env s1 in
      let s2' = translate_sort env s2 in
      let a' = translate_constr env a in
      let b' = translate_constr env b in
      let t' = translate_constr env t in
      coq_cast s1' s2' a' b' t'
  | Prod(x, a, b) ->
      let s1 = infer_sort env a in
      let s2 = infer_sort (Environ.push_rel (x, None, a) env) b in
      let s1' = translate_sort env s1 in
      let s2' = translate_sort (Environ.push_rel (x, None, a) env) s2 in
      let x' = Name.translate_name x in
      let a' = translate_constr env a in
      let a'' = translate_types env a in
      let b' = translate_constr (Environ.push_rel (x, None, a) env) b in
      coq_prod s1' s2' a' (Dedukti.lam (x', a'') b')
  | Lambda(x, a, t) ->
      let x' = Name.translate_name x in
      let a'' = translate_types env a in
      let t' = translate_constr (Environ.push_rel (x, None, a) env) t in
      Dedukti.lam (x', a'') t'
  | LetIn(x, u, a, t) -> failwith "Not implemented: LetIn"
  | App(t, u_list) ->
      let t' = translate_constr env t in
      let u_list' = List.map (translate_constr env) (Array.to_list u_list) in
      Dedukti.apps t' u_list'
  | Const(c) ->
      let c' = Name.translate_constant c in
      Dedukti.Var c'
  | Ind(inductive) -> failwith "Not implemented: Ind"
  | Construct(constructor) -> failwith "Not implemented: Construct"
  | Case(case_info, constr, constr2, constr_array) -> failwith "Not implemented: Cast"
  | Fix(pfixpoint) -> failwith "Not implemented: Fix"
  | CoFix(pcofixpoint) -> failwith "Not implemented: CoFix"

and translate_types env a =
  (* Specialize on the type to get a nicer and more compact translation. *)
  match Term.kind_of_type a with
  | SortType(s) ->
      let s' = translate_sort env s in
      coq_type s'
  | CastType(a, b) ->
      failwith "Not implemented: CastType"
  | ProdType(x, a, b) ->
      let x' = Name.translate_name x in
      let a' = translate_types env a in
      let b' = translate_types (Environ.push_rel (x, None, a) env) b in
      Dedukti.pie (x', a') b'
  | LetInType(x, u, a, b) ->
      failwith "Not implemented: LetInType"
  | AtomicType(_) ->
      (* Fall back on the usual translation of types. *)
      let s = infer_sort env a in
      let s' = translate_sort env s in
      let a' = translate_constr env a in
      coq_term s' a'

