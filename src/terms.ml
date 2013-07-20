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

let translate_sort out env s =
  match s with
  | Prop(Null) -> Universes.coq_p
  | Prop(Pos) -> Universes.coq_z
  | Type(i) -> Universes.translate_universe env i

let apply_rel_context t context =
  let n = List.length context in
  Term.appvectc t (Array.init n (fun i -> Term.mkRel (n - i)))

(** Get the arguments of the inductive type application [a] *)
let inductive_args env a =
  (* Reduce to get the head normal form. *)
  let whd = Reduction.whd_betadeltaiota env a in
  let head, args = Term.destApp whd in
  (* Make sure the head is an inductive. *)
  let _ = Term.destInd head in
  args

let rec translate_constr out env t =
  match Term.kind_of_term t with
  | Rel(i) ->
      let (x, _, _) = Environ.lookup_rel i env in
      Dedukti.var (Name.translate_name x)
  | Var(x) ->
      Dedukti.var (Name.translate_identifier x)
  | Meta(metavariable) -> failwith "Not implemented: Meta"
  | Evar(pexistential) -> failwith "Not implemented: Evar"
  | Sort(s) ->
      let s' = translate_sort out env s in
      coq_sort s'
  | Cast(t, _, b) ->
      let a = infer_type env t in
      let s1 = infer_sort env a in
      let s2 = infer_sort env b in
      let s1' = translate_sort out env s1 in
      let s2' = translate_sort out env s2 in
      let a' = translate_constr out env a in
      let b' = translate_constr out env b in
      let t' = translate_constr out env t in
      coq_cast s1' s2' a' b' t'
  | Prod(x, a, b) ->
      let s1 = infer_sort env a in
      let s2 = infer_sort (Environ.push_rel (x, None, a) env) b in
      let s1' = translate_sort out env s1 in
      let s2' = translate_sort out (Environ.push_rel (x, None, a) env) s2 in
      let x' = Name.translate_name x in
      let a' = translate_constr out env a in
      let a'' = translate_types out env a in
      let b' = translate_constr out (Environ.push_rel (x, None, a) env) b in
      coq_prod s1' s2' a' (Dedukti.lam (x', a'') b')
  | Lambda(x, a, t) ->
      let x' = Name.translate_name x in
      let a'' = translate_types out env a in
      let t' = translate_constr out (Environ.push_rel (x, None, a) env) t in
      Dedukti.lam (x', a'') t'
  | LetIn(x, u, a, t) ->
      let env, t = lift_let out env x u a t in
      translate_constr out env t
  | App(t, u_list) ->
      let t' = translate_constr out env t in
      let u_list' = List.map (translate_constr out env) (Array.to_list u_list) in
      Dedukti.apps t' u_list'
  | Const(c) ->
      Dedukti.var(Name.translate_constant env c)
  | Ind(i) ->
      Dedukti.var(Name.translate_inductive env i)
  | Construct(c) ->
      Dedukti.var(Name.translate_constructor env c)
  | Case(case_info, return_type, matched, branches) ->
      let ind_args = inductive_args env (infer_type env matched) in
      let match_function' = Dedukti.var (Name.translate_match_function env case_info.ci_ind) in
      let return_type' = translate_constr out env return_type in
      let ind_args' = Array.to_list (Array.map (translate_constr out env) ind_args) in
      let matched' = translate_constr out env matched in
      let branches' = Array.to_list (Array.map (translate_constr out env) branches) in
      Dedukti.apps match_function' (return_type' :: ind_args' @ matched' :: branches')
  | Fix(pfixpoint) -> failwith "Not implemented: Fix"
  | CoFix(pcofixpoint) -> failwith "Not implemented: CoFix"

and translate_types out env a =
  (* Specialize on the type to get a nicer and more compact translation. *)
  match Term.kind_of_type a with
  | SortType(s) ->
      let s' = translate_sort out env s in
      coq_type s'
  | CastType(a, b) ->
      failwith "Not implemented: CastType"
  | ProdType(x, a, b) ->
      let x' = Name.translate_name x in
      let a' = translate_types out env a in
      let b' = translate_types out (Environ.push_rel (x, None, a) env) b in
      Dedukti.pie (x', a') b'
  | LetInType(x, u, a, b) ->
      let env, t = lift_let out env x u a b in
      translate_types out env t
  | AtomicType(_) ->
      (* Fall back on the usual translation of types. *)
      let s = infer_sort env a in
      let s' = translate_sort out env s in
      let a' = translate_constr out env a in
      coq_term s' a'

and lift_let out env x u a t =
  let x = Name.fresh_let x in
  let rel_context = Environ.rel_context env in
  let a = Term.it_mkProd_or_LetIn a rel_context in
  let u = Term.it_mkLambda_or_LetIn u rel_context in
  let env = Environ.push_named (x, Some(u), a) env in
  let x' = Name.translate_identifier x in
  let a' = translate_types out (Global.env ()) a in
  let u' = translate_constr out (Global.env ()) u in
  Dedukti.print out (Dedukti.definition false x' a' u');
  let x = apply_rel_context (Term.mkVar x) rel_context in
  let t = Term.subst1 x t in
  env, t

(** Translate the context [x1 : a1, ..., xn : an] into the list
    [x1, ||a1||; ...; x1, ||an||] *)
let translate_rel_context out env context =
  let translate_rel_declaration (x, t, a) (env, translated) =
    match t with
    | None ->
        let x' = Name.translate_name x in
        let a' = translate_types out env a in
        (Environ.push_rel (x, t, a) env, (x', a') :: translated)
    | Some(t) -> failwith "Cannot translate a rel_declaration with a body." in
  (* Reverse the list as the newer declarations are on top. *)
  List.rev (snd (List.fold_right translate_rel_declaration context (env, [])))

