(** Translation of Coq terms *)

open Declarations
open Term
open Dedukti
open Info

let infer_type env t = (Typeops.infer      env t).Environ.uj_type
let infer_sort env a = (Typeops.infer_type env a).Environ.utj_type

let translate_sort info env uenv = function
  | Term.Prop Null -> coq_prop
  | Term.Prop Pos  -> coq_set
  | Term.Type i    -> Tsorts.translate_universe info env uenv i

(** Infer and translate the sort of [a].
    Coq fails if we try to type a sort that was already inferred.
    This function uses pattern matching to avoid it. *)
let rec infer_translate_sort info env uenv a =
(*  This is wrong; there is no subject reduction in Coq! *)
(*  let a = Reduction.whd_all env a in*)
  match Term.kind_of_type a with
  | SortType(s) -> coq_axiom (translate_sort info env uenv s)
  | CastType(a, b) -> Error.not_supported "CastType"
  | ProdType(x, a, b) ->
      let x = Name.fresh_name info env ~default:"_" x in
      let s1' = infer_translate_sort info env uenv a in
      let s2' = infer_translate_sort info (Environ.push_rel (Context.Rel.Declaration.of_tuple (x, None, a)) env) uenv b in
      coq_rule s1' s2'
  | LetInType(x, u, a, b) ->
      (* No need to lift the let here. *)
      infer_translate_sort info (Environ.push_rel (Context.Rel.Declaration.of_tuple (x, Some(u), a)) env) uenv b
  | AtomicType(_) -> translate_sort info env uenv (infer_sort env a)

(** Abstract over the variables of [context], eliminating let declarations. *)
let abstract_rel_context context t =
  let abstract_rel_declaration t c =
    let (x, u, a) = (Context.Rel.Declaration.to_tuple c) in
    match u with
    | None -> Term.mkLambda (x, a, t)
    | Some(u) -> Vars.subst1 u t in
  List.fold_left abstract_rel_declaration t context

(** Generalize over the variables of [context], eliminating let declarations. *)
let generalize_rel_context context b =
  let generalize_rel_declaration b c =
    let (x, u, a) = (Context.Rel.Declaration.to_tuple c) in
    match u with
    | None -> Term.mkProd(x, a, b)
    | Some(u) -> Vars.subst1 u b in
  List.fold_left generalize_rel_declaration b context

(** Apply the variables of [context] to [t], ignoring let declarations. *)
let apply_rel_context t context =
  let apply_rel_declaration (args, i) c =
    let (x, t, a) = (Context.Rel.Declaration.to_tuple c) in
    match t with
    | None -> (Term.mkRel(i) :: args, i + 1)
    | Some(_) -> (args, i + 1) in
  let args, _ = List.fold_left apply_rel_declaration ([], 1) context in
  Term.applistc t args

let convertible_sort info env uenv s1 s2 =
  translate_sort info env uenv s1 = translate_sort info env uenv s2

let rec convertible info env uenv a b =
  let a = Reduction.whd_all env a in
  let b = Reduction.whd_all env b in
  match Term.kind_of_type a, Term.kind_of_type b with
  | SortType(s1), SortType(s2) -> convertible_sort info env uenv s1 s2
  | CastType(_), _
  | _, CastType(_) ->
     Error.not_supported "CastType"
  | ProdType(x1, a1, b1), ProdType(x2, a2, b2) ->
     convertible info env uenv a1 a2 &&
       let nenv = Environ.push_rel (Context.Rel.Declaration.LocalAssum (x1, a1)) env in
      convertible info nenv uenv b1 b2
  | LetInType(_), _
  | _, LetInType(_) ->
      assert false
  | AtomicType(_), AtomicType(_) ->
      begin try
        let _ = Reduction.default_conv Reduction.CONV env a b in true
      with
      | Reduction.NotConvertible -> false
      end
  | _ -> false

(** This table holds the translations of fixpoints, so that we avoid
    translating the same definition multiple times (e.g. mutual fixpoints).
    This is very important, otherwise the size of the files will explode. *)
let fixpoint_table = Hashtbl.create 10007

let make_const mp x =
  Names.make_con mp (Names.make_dirpath []) (Names.label_of_id x)

(** Use constant declarations instead of named variable declarations for lifted
    terms because fixpoint declarations should be global and could be referred
    to from other files (think Coq.Arith.Even with Coq.Init.Peano.plus). *)
let push_const_decl env (c, m, const_type) =
  let const_body =
    match m with
    | None -> Undef None
    | Some m -> Def (Mod_subst.from_val m) in
  let const_body_code =
    (** TODO : None does not handle polymorphic types ! *)
    match Cbytegen.compile_constant_body true (Environ.pre_env env) None const_body with
    | Some code -> code
    | None -> Error.error (Pp.str "compile_constant_body failed")
  in
  (** TODO : Double check the following *)
  let const_universes = (Univ.UContext.make (Univ.Instance.empty, Univ.Constraint.empty)) in
  let body = {
    const_hyps = [];
    const_body = const_body;
    const_type = const_type;
    const_body_code = Some (Cemitcodes.from_val const_body_code);
    const_polymorphic = false;
    const_universes = const_universes;
    const_proj = None;
    const_inline_code = false;
    const_typing_flags = { check_guarded = false; check_universes = false }
  } in
  Environ.add_constant c body env

let check_const env kn =
  try
    Debug.debug_string "Check const:";
    let (types, univ_ctxt) = Global.type_of_global_in_context env (Globnames.ConstRef kn) in
    Debug.debug_coq_type types
  with | _ -> ()
let check_ind env kn =
  try
    Debug.debug_string "Check ind:";
    let (types, univ_ctxt) = Global.type_of_global_in_context env (Globnames.IndRef kn) in
    Debug.debug_coq_type types
  with | _ -> ()
let check_construct env kn =
  try
    Debug.debug_string "Check construct:";
    let (types, univ_ctxt) = Global.type_of_global_in_context env (Globnames.ConstructRef kn) in
    Debug.debug_coq_type types
  with | _ -> ()

(** Translate the Coq term [t] as a Dedukti term. *)
let rec translate_constr ?expected_type info env uenv t =
  (* Check if the expected type coincides, otherwise make an explicit cast. *)
  let t =
    match expected_type with
    | None   -> t
    | Some a ->
        let b = infer_type env t in
        if convertible info env uenv a b then t else Term.mkCast(t, Term.VMcast, a) in
  match Term.kind_of_term t with
  | Rel i ->
      (* If it's a let definition, replace by its value. *)
      let (x, u, _) = Context.Rel.Declaration.to_tuple (Environ.lookup_rel i env) in
      begin match u with
      | Some u -> translate_constr info env uenv (Vars.lift i u)
      | None   -> Dedukti.var (Name.translate_name ~ensure_name:true x)
      end
  | Var x -> Dedukti.var (Name.translate_identifier x)
  | Meta metavariable  -> Error.not_supported "Meta"
  | Evar pexistential  -> Error.not_supported "Evar"
  | Sort s -> coq_sort (translate_sort info env uenv s)
  | Cast(t, _, b) ->
      let a = infer_type env t in
      let s1' = infer_translate_sort info env uenv a in
      let s2' = infer_translate_sort info env uenv b in
      let a' = translate_constr info env uenv a in
      let b' = translate_constr info env uenv b in
      let t' = translate_constr info env uenv t in
      coq_cast s1' s2' a' b' t'
  | Prod(x, a, b) ->
      let x = Name.fresh_name ~default:"_" info env x in
      let s1' = infer_translate_sort info env uenv a in
      let s2' = infer_translate_sort info (Environ.push_rel (Context.Rel.Declaration.of_tuple (x, None, a)) env) uenv b in
      let x' = Name.translate_name x in
      let a' = translate_constr info env uenv a in
      let a'' = translate_types info env uenv a in
      let b' = translate_constr info (Environ.push_rel (Context.Rel.Declaration.of_tuple (x, None, a)) env) uenv b in
      coq_prod s1' s2' a' (Dedukti.lam (x', a'') b')
  | Lambda(x, a, t) ->
      let x = Name.fresh_name ~default:"_" info env x in
      let x' = Name.translate_name x in
      let a'' = translate_types info env uenv a in
      let t' = translate_constr info (Environ.push_rel (Context.Rel.Declaration.of_tuple (x, None, a)) env) uenv t in
      Dedukti.lam (x', a'') t'
  | LetIn(x, u, a, t) ->
      let env, u = lift_let info env uenv x u a in
      translate_constr info
        (Environ.push_rel (Context.Rel.Declaration.of_tuple (x, Some(u), a)) env)
        uenv t
  | App(t, args) ->
     let translate_app (t', a) u =
       let _, c, d = Term.destProd (Reduction.whd_all env a) in
       let u' = translate_constr ~expected_type:c info env uenv u in
       (Dedukti.app t' u', Vars.subst1 u d) in
     let a = infer_type env t in
     let t' = translate_constr ~expected_type:a info env uenv t in
     fst (Array.fold_left translate_app (t', a) args)
  | Const     (kn, univ_instance) ->
    begin
      let univs = Univ.Instance.to_array univ_instance in
      if Array.length univs > 0 then
        ( assert false; Debug.debug_string "Const Universe: ";
          Debug.debug_coq_inst univ_instance );
      check_const env kn;
      Dedukti.var (Name.translate_constant info env kn)
    end
  | Ind (kn, univ_instance) ->
    begin
      let univs = Univ.Instance.to_array univ_instance in
      if Array.length univs > 0 then
        ( Debug.debug_string "Const Universe: ";
          Debug.debug_coq_inst univ_instance );
      check_ind env kn;
      Dedukti.var(Name.translate_inductive   info env kn)
    end
  | Construct (kn, univ_instance) ->
    begin
      let univs = Univ.Instance.to_array univ_instance in
      if Array.length univs > 0 then
        ( Debug.debug_string "Construct Universe: ";
          Debug.debug_coq_inst univ_instance );
      check_construct env kn;
      Dedukti.var(Name.translate_constructor info env kn)
    end
  | Fix((rec_indices, i), ((names, types, bodies) as rec_declaration)) ->
     let n = Array.length names in
     let env, fix_declarations =
       try Hashtbl.find fixpoint_table rec_declaration
       with Not_found -> lift_fix info env uenv names types bodies rec_indices in
     let env = Array.fold_left (fun env declaration ->
                   Environ.push_rel declaration env) env fix_declarations in
     translate_constr info env uenv (Term.mkRel (n - i))
  | Case(case_info, return_type, matched, branches) ->
     let match_function_name = Name.translate_match_function info env case_info.ci_ind in
     let mind_body, ind_body = Inductive.lookup_mind_specif env case_info.ci_ind in
     let n_params = mind_body.Declarations.mind_nparams   in
     let n_reals  =  ind_body.Declarations.mind_nrealargs in
     let pind, ind_args = Inductive.find_inductive env (infer_type env matched) in
     let arity = Inductive.type_of_inductive env ( (mind_body, ind_body), snd pind) in
     
     (* TODO:
implement universe instanciation here
  cf kernel/inductive.ml : line 203
      *)
     begin
       match ind_body.mind_arity with
       | RegularArity a ->
         Debug.debug_string "Case: RegularArity";
         (* subst_instance_constr u a.mind_user_arity *)
       | TemplateArity ar ->
         let ctx = List.rev ind_body.mind_arity_ctxt in
         Debug.debug_string "Case: TemplateArity";
         (*
         let args = Array.to_list paramtyps in
         let subst = make_subst env (ctx,ar.template_param_levels,args) in
         let level = Univ.subst_univs_universe (Univ.make_subst subst) ar.template_level in
         let ty =
           (* Singleton type not containing types are interpretable in Prop *)
           if is_type0m_univ level then prop_sort
           (* Non singleton type not containing types are interpretable in Set *)
           else if is_type0_univ level then set_sort
           (* This is a Type with constraints *)
           else Type level
         *)
     end;
     let params, reals = Utils.list_chop n_params ind_args in
     let context, end_type = Term.decompose_lam_n_assum (n_reals + 1) return_type in
     let return_sort = infer_sort (Environ.push_rel_context context env) end_type in
     
     (* Translate params using expected types to make sure we use proper casts. *)
     let translate_param (params', a) param =
       let _, c, d = Term.destProd (Reduction.whd_all env a) in
       let param' = translate_constr ~expected_type:c info env uenv param in
       (param' :: params', Vars.subst1 param d) in
     let match_function' = Dedukti.var match_function_name in
     let return_sort' = translate_sort info env uenv return_sort in
     let params' = List.rev (fst (List.fold_left translate_param ([], arity) params)) in
     let return_type' = translate_constr info env uenv return_type in
     let branches' = Array.to_list (Array.map (translate_constr info env uenv) branches) in
     let reals' = List.map (translate_constr info env uenv) reals in
     let matched' = translate_constr info env uenv matched in
     
     Dedukti.apps match_function'
                  (params' @ return_sort' :: return_type' :: branches' @ reals' @  [matched'])

  | CoFix(pcofixpoint) -> Error.not_supported "CoFix"
  | Proj (_,_) -> Error.not_supported "Proj"

(** Translate the Coq type [a] as a Dedukti type. *)
and translate_types info env uenv a =
  (* Specialize on the type to get a nicer and more compact translation. *)
  match Term.kind_of_type a with
  | SortType(s) -> coq_U (translate_sort info env uenv s)
  | CastType(a, b) -> Error.not_supported "CastType"
  | ProdType(x, a, b) ->
      let x = Name.fresh_name info ~default:"_" env x in
      let x' = Name.translate_name x in
      let a' = translate_types info env uenv a in
      let b' = translate_types info (Environ.push_rel (Context.Rel.Declaration.of_tuple (x, None, a)) env) uenv b in
      Dedukti.pie (x', a') b'
  | LetInType(x, u, a, b) ->
      let env, u = lift_let info env uenv x u a in
      translate_constr info (Environ.push_rel (Context.Rel.Declaration.of_tuple (x, Some(u), a)) env) uenv b
  | AtomicType(_) ->
      (* Fall back on the usual translation of types. *)
      let s' = infer_translate_sort info env uenv a in
      let a' = translate_constr info env uenv a in
      coq_term s' a'

and lift_let info env uenv x u a =
  let y = Name.fresh_of_name ~global:true ~prefix:"let" ~default:"_" info env x in
  let rel_context = Environ.rel_context env in
  let a_closed = generalize_rel_context rel_context a in
  let u_closed =   abstract_rel_context rel_context u in
  let y' = Name.translate_identifier y in
  (* [a_closed] amd [u_closed] are in the global environment but we still
     have to remember the named variables (i.e. from nested let in). *)
  let global_env = Environ.reset_with_named_context (Environ.named_context_val env) env in
  let a_closed' = translate_types  info global_env uenv a_closed in
  let u_closed' = translate_constr info global_env uenv u_closed in
  Dedukti.print info.out (Dedukti.definition false y' a_closed' u_closed');
  let yconst = make_const info.module_path y in
  let env =
    push_const_decl env (yconst, Some(u_closed), Declarations.RegularArity a_closed) in
  env, apply_rel_context (Term.mkConst yconst) rel_context

and lift_fix info env uenv names types bodies rec_indices =
  (* A fixpoint is translated by 3 functions:
     - The first function duplicates the argument and sends it to the second.
     - The second pattern matches on the second arguments, then throws it away
       and passes the first argument to the third function.
     - The third function executes the body of the fixpoint. *)
  (* fix1_f : |G| -> |x1| : ||A1|| -> ... -> |xr| : ||I u1 ... un|| -> ||A||.
     fix2_f : |G| -> |x1| : ||A1|| -> ... -> |xr| : ||I u1 ... un|| -> |y1| : ||B1|| -> ... -> |yn| : ||Bn|| -> ||I y1 ... yn|| -> ||A||.
     fix3_f : |G| -> |x1| : ||A1|| -> ... -> |xr| : ||I u1 ... un|| -> ||A||.
     [...] fix1_f |G| |x1| ... |xr| --> fix2_f |x1| ... |xr| |u1| ... |un| |xr|.
     [...] fix2_f |G| |x1| ... |xr| {|uj1|} ... {|ujn|} (|cj z1 ... zkj|) --> fix3_f |G| |x1| ... |xr|.
     [...] fix3_f |G| |x1| ... |xr| --> |[(fix1_f G)/f]t|. *)
  let n = Array.length names in
  let fix_names1 = Array.map (Name.fresh_of_name info env ~global:true ~prefix:"fix" ~default:"_") names in
  let fix_names2 = Array.map (Name.fresh_of_name info env ~global:true ~prefix:"fix" ~default:"_") names in
  let fix_names3 = Array.map (Name.fresh_of_name info env ~global:true ~prefix:"fix" ~default:"_") names in
  let contexts_return_types = Array.mapi (fun i -> Term.decompose_prod_n_assum (rec_indices.(i) + 1)) types in
  let contexts = Array.map fst contexts_return_types in
  let return_types = Array.map snd contexts_return_types in
  let ind_applieds = Array.map (fun context -> let (_, _, a) = Context.Rel.Declaration.to_tuple (List.hd context) in a) contexts in
  let inds_args = Array.map (Inductive.find_inductive env) ind_applieds in
  let pinds = Array.map fst inds_args in
  let inds = Array.map fst pinds in
  let ind_args = Array.map snd inds_args in
  let ind_specifs = Array.map (Inductive.lookup_mind_specif env) inds in
  let arity_contexts = Array.map (fun ind_specif -> fst (Inductive.mind_arity (snd ind_specif))) ind_specifs in
  let ind_applied_arities = Array.init n (fun i -> apply_rel_context (Term.mkInd inds.(i)) arity_contexts.(i)) in
  let types1 = types in
  let types2 = Array.init n (fun i ->
    generalize_rel_context contexts.(i) (
    generalize_rel_context arity_contexts.(i) (
    Term.mkArrow ind_applied_arities.(i) (Vars.lift (List.length arity_contexts.(i) + 1) return_types.(i))))) in
  let types3 = types in
  let rel_context = Environ.rel_context env in
  let types1_closed = Array.map (generalize_rel_context rel_context) types1 in
  let types2_closed = Array.map (generalize_rel_context rel_context) types2 in
  let types3_closed = Array.map (generalize_rel_context rel_context) types3 in
  let const1 = Array.map (fun name -> make_const info.module_path name) fix_names1 in
  let const2 = Array.map (fun name -> make_const info.module_path name) fix_names2 in
  let const3 = Array.map (fun name -> make_const info.module_path name) fix_names3 in
  let name1_declarations = Array.init n (fun j -> (const1.(j), None, Declarations.RegularArity types1_closed.(j))) in
  let name2_declarations = Array.init n (fun j -> (const2.(j), None, Declarations.RegularArity types2_closed.(j))) in
  let name3_declarations = Array.init n (fun j -> (const3.(j), None, Declarations.RegularArity types3_closed.(j))) in
  let fix_names1' = Array.map Name.translate_identifier fix_names1 in
  let fix_names2' = Array.map Name.translate_identifier fix_names2 in
  let fix_names3' = Array.map Name.translate_identifier fix_names3 in
  (* we still have to remember the named variables (i.e. from nested let in). *)
  let global_env = Environ.reset_with_named_context (Environ.named_context_val env) env in
  (* Check: we can create a fixpoint particular for current uenv. *)
  let types1_closed' = Array.map (translate_types info global_env uenv)
                                 types1_closed in
  let types2_closed' = Array.map (translate_types info global_env uenv)
                                 types2_closed in
  let types3_closed' = Array.map (translate_types info global_env uenv)
                                 types3_closed in
  for i = 0 to n - 1 do
    Dedukti.print info.out (Dedukti.declaration true fix_names1'.(i) types1_closed'.(i));
    Dedukti.print info.out (Dedukti.declaration true fix_names2'.(i) types2_closed'.(i));
    Dedukti.print info.out (Dedukti.declaration true fix_names3'.(i) types3_closed'.(i));
  done;
  let fix_terms1 = Array.init n (fun i -> Term.mkConst (const1.(i))) in
  let fix_terms2 = Array.init n (fun i -> Term.mkConst (const2.(i))) in
  let fix_terms3 = Array.init n (fun i -> Term.mkConst (const3.(i))) in
  let fix_rules1 = Array.init n (fun i ->
    let env, context' = translate_rel_context info (global_env) uenv (contexts.(i) @ rel_context) in
    let fix_term1' = translate_constr info env uenv fix_terms1.(i) in
    let fix_term2' = translate_constr info env uenv fix_terms2.(i) in
    let ind_args' = List.map (translate_constr info env uenv) (List.map (Vars.lift 1) ind_args.(i)) in
    [(context', Dedukti.apply_context fix_term1' context',
      Dedukti.apps (Dedukti.apply_context fix_term2' context')
        (ind_args' @ [Dedukti.var (fst (List.nth context' (List.length context' - 1)))]))]) in
  let fix_rules2 = Array.init n (fun i ->
    let cons_arities = Inductive.arities_of_constructors pinds.(i) ind_specifs.(i) in
    let cons_contexts_types = Array.map Term.decompose_prod_assum cons_arities in
    let cons_contexts = Array.map fst cons_contexts_types in
    let cons_types = Array.map snd cons_contexts_types in
    let cons_ind_args = Array.map (fun cons_type -> snd (Inductive.find_inductive env cons_type)) cons_types in
    let n_cons = Array.length cons_types in
    let cons_rules = Array.init n_cons (fun j ->
      let env, context' = translate_rel_context info (global_env) uenv (contexts.(i) @ rel_context) in
      let env, cons_context' = translate_rel_context info env uenv (cons_contexts.(j)) in
      let fix_term2' = translate_constr info env uenv fix_terms2.(i) in
      let fix_term3' = translate_constr info env uenv fix_terms3.(i) in
      let cons_term' = translate_constr info env uenv (Term.mkConstruct ((inds.(i), j + 1))) in
      let cons_term_applied' = Dedukti.apply_context cons_term' cons_context' in
      let cons_ind_args' = List.map (translate_constr info env uenv) cons_ind_args.(j) in
      (context' @ cons_context',
        Dedukti.apps (Dedukti.apply_context fix_term2' context') (cons_ind_args' @ [cons_term_applied']),
        Dedukti.apply_context fix_term3' context')) in
    Array.to_list cons_rules) in
  let env = Array.fold_left push_const_decl env name1_declarations in
  let env = Array.fold_left push_const_decl env name2_declarations in
  let env = Array.fold_left push_const_decl env name3_declarations in
  let fix_applieds1 = Array.init n (fun i -> apply_rel_context fix_terms1.(i) rel_context) in
  (* The declarations need to be lifted to account for the displacement. *)
  let fix_declarations1 = Array.init n (fun i ->
    Context.Rel.Declaration.of_tuple
      (names.(i), Some(Vars.lift i fix_applieds1.(i)), Vars.lift i types.(i))) in
  let fix_rules3 = Array.init n (fun i ->
    let env, rel_context' = translate_rel_context info (global_env) uenv rel_context in
    let env = Array.fold_left push_const_decl env name1_declarations in
    let fix_term3' = translate_constr info env uenv fix_terms3.(i) in
    let env = Array.fold_left (fun env declaration -> Environ.push_rel declaration env)
                              env fix_declarations1 in
    let body' = translate_constr info env uenv bodies.(i) in
(*    let env , context' = translate_rel_context info env uenv contexts.(i) in*)
    [(rel_context', Dedukti.apply_context fix_term3' rel_context', body')]) in
  for i = 0 to n - 1 do
    List.iter (Dedukti.print info.out) (List.map Dedukti.rewrite fix_rules1.(i));
    List.iter (Dedukti.print info.out) (List.map Dedukti.rewrite fix_rules2.(i));
    List.iter (Dedukti.print info.out) (List.map Dedukti.rewrite fix_rules3.(i));
  done;
  Hashtbl.add fixpoint_table (names, types, bodies) (env, fix_declarations1);
  env, fix_declarations1

(** Translate the context [x1 : a1, ..., xn : an] into the list
    [x1, ||a1||; ...; x1, ||an||], ignoring let declarations. *)
and translate_rel_context info env uenv context =
  let translate_rel_declaration c (env, translated) =
    let (x, u, a) = Context.Rel.Declaration.to_tuple c in
    match u with
    | None ->
        let x = Name.fresh_name ~default:"_" info env x in
        let x' = Name.translate_name x in
        let a' = translate_types info env uenv a in
        (Environ.push_rel (Context.Rel.Declaration.of_tuple (x, u, a)) env, (x', a') :: translated)
    | Some(u) ->
        (Environ.push_rel (Context.Rel.Declaration.of_tuple (x, Some(u), a)) env, translated) in
  let env, translated = List.fold_right translate_rel_declaration context (env, []) in
  (* Reverse the list as the newer declarations are on top. *)
  (env, List.rev translated)

let translate_args info env uenv ts =
  List.map (translate_constr info env uenv) ts
