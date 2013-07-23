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

(** Infer and translate the sort of [a].
    Coq fails if we try to type a sort that was already inferred.
    This function uses pattern matching to avoid it. *)
let infer_translate_sort out env a =
   match Term.kind_of_type a with
  | SortType(s) -> Universes.coq_t (translate_sort out env s)
  | _ -> translate_sort out env (infer_sort env a)

(** Abstract over the variables of [context], ignoring let declarations. *)
let abstract_rel_context context t =
  let abstract_rel_declaration t (x, u, a) =
    match u with
    | None -> Term.mkLambda (x, a, t)
    | Some(_) -> t in
  List.fold_left abstract_rel_declaration t context

(** Generalize over the variables of [context], ignoring let declarations. *)
let generalize_rel_context context b =
  let generalize_rel_declaration b (x, u, a) =
    match u with
    | None -> Term.mkProd(x, a, b)
    | Some(_) -> b in
  List.fold_left generalize_rel_declaration b context

(** Apply the variables of [context] to [t], ignoring let declarations. *)
let apply_rel_context t context =
  let apply_rel_declaration (args, i) (x, t, a) =
    match t with
    | None -> (Term.mkRel(i) :: args, i + 1)
    | Some(_) -> (args, i + 1) in
  let args, _ = List.fold_left apply_rel_declaration ([], 1) context in
  Term.applistc t args

(** Get the arguments of the inductive type application [a] *)
let inductive_args env a =
  (* Reduce to get the head normal form. *)
  let a = Reduction.whd_betadeltaiota env a in
  (* Use match instead of [Term.destApp] because that function fails when
     there are no arguments. *)
  match Term.kind_of_term a with
  | Ind _ -> []
  | App(head, args) ->
      (* Make sure the head is an inductive. *)
      let _ = Term.destInd head in
      Array.to_list args
  | _ -> failwith "Inductive type application"

let convertible env a b =
  try let _ = Reduction.conv env a b in true
  with | Assert_failure _| Reduction.NotConvertible | Util.Anomaly _ -> false

(** Translate the Coq term [t] as a Dedukti term. *)
let rec translate_constr ?expected_type out env t =
  (* Check if the expected type coincides, otherwise make an explicit cast. *)
  let t =
    match expected_type with
    | None -> t
    | Some(a) ->
        let b = infer_type env t in
        if convertible env a b then t else Term.mkCast(t, Term.VMcast, a) in
  match Term.kind_of_term t with
  | Rel(i) ->
      (* If it's a let definition, replace by its value. *)
      let (x, u, _) = Environ.lookup_rel i env in
      begin match u with
      | Some(u) -> translate_constr out env (Term.lift i u)
      | None -> Dedukti.var (Name.translate_name ~ensure_name:true x)
      end
  | Var(x) ->
      Dedukti.var (Name.translate_identifier x)
  | Meta(metavariable) -> failwith "Not implemented: Meta"
  | Evar(pexistential) -> failwith "Not implemented: Evar"
  | Sort(s) ->
      let s' = translate_sort out env s in
      coq_sort s'
  | Cast(t, _, b) ->
      let a = infer_type env t in
      let s1' = infer_translate_sort out env a in
      let s2' = infer_translate_sort out env b in
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
      let env = lift_let out env x u a in
      translate_constr out env t
  | App(t, args) ->
      let a = infer_type env t in
      let translate_app (t', a) u =
        let _, c, d = Term.destProd (Reduction.whd_betadeltaiota env a) in
        let u' = translate_constr ~expected_type:c out env u in
        (Dedukti.app t' u', Term.subst1 u d) in
      let t' = translate_constr out env t in
      fst (Array.fold_left translate_app (t', a) args)
  | Const(c) ->
      Dedukti.var(Name.translate_constant env c)
  | Ind(i) ->
      Dedukti.var(Name.translate_inductive env i)
  | Construct(c) ->
      Dedukti.var(Name.translate_constructor env c)
  | Case(case_info, return_type, matched, branches) ->
      let ind = case_info.ci_ind in
      let mind_body, ind_body = Inductive.lookup_mind_specif env case_info.ci_ind in
      let n_params = mind_body.Declarations.mind_nparams in
      let n_reals = ind_body.Declarations.mind_nrealargs in
      let ind_args = inductive_args env (infer_type env matched) in
      let params, reals = Util.list_chop n_params ind_args in
      let context, end_type = Term.decompose_lam_n_assum (n_reals + 1) return_type in
      let return_sort = infer_sort (Environ.push_rel_context context env) end_type in
      let match_function' = Dedukti.var (Name.translate_match_function env ind) in
      let params' = List.map (translate_constr out env) params in
      let reals' = List.map (translate_constr out env) reals in
      let return_sort' = translate_sort out env return_sort in
      let return_type' = translate_constr out env return_type in
      let matched' = translate_constr out env matched in
      let branches' = Array.to_list (Array.map (translate_constr out env) branches) in
      Dedukti.apps match_function' (params' @ return_sort' :: return_type' :: branches' @ reals' @  [matched'])
  | Fix(pfixpoint) -> failwith "Not implemented: Fix"
  | CoFix(pcofixpoint) -> failwith "Not implemented: CoFix"

(** Translate the Coq type [a] as a Dedukti type. *)
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
      let env = lift_let out env x u a in
      translate_types out env b
  | AtomicType(_) ->
      (* Fall back on the usual translation of types. *)
      let s = infer_sort env a in
      let s' = translate_sort out env s in
      let a' = translate_constr out env a in
      coq_term s' a'

and lift_let out env x u a =
(*  Environ.push_rel (x, Some(u), a) env*)
  let y = Name.fresh_let x in
  let rel_context = Environ.rel_context env in
  let a_closed = generalize_rel_context rel_context a in
  let u_closed = abstract_rel_context rel_context u in
  let env = Environ.push_named (y, Some(u_closed), a_closed) env in
  let y' = Name.translate_identifier y in
  let a_closed' = translate_types out env a_closed in
  let u_closed' = translate_constr out env u_closed in
  Dedukti.print out (Dedukti.definition false y' a_closed' u_closed');
  let y_applied = apply_rel_context (Term.mkVar(y)) rel_context in
  Environ.push_rel (x, Some(y_applied), a) env

let translate_args out env ts =
  List.map (translate_constr out env) ts

(** Ensure that the declaration (x, t, a) is not anonymous. *)
let ensure_name prefix (x, t, a) = (Name.ensure_name prefix x, t, a)

(** Translate the context [x1 : a1, ..., xn : an] into the list
    [x1, ||a1||; ...; x1, ||an||], ignoring let declarations. *)
let translate_rel_context out env context =
  let translate_rel_declaration (x, u, a) (env, translated) =
    match u with
    | None ->
        let x' = Name.translate_name x in
        let a' = translate_types out env a in
        (Environ.push_rel (x, u, a) env, (x', a') :: translated)
    | Some(u) ->
        (Environ.push_rel (x, Some(u), a) env, translated) in
  let env, translated = List.fold_right translate_rel_declaration context (env, []) in
  (* Reverse the list as the newer declarations are on top. *)
  (env, List.rev translated)

