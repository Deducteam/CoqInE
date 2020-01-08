(** Translation of Coq terms *)

open Debug
open Translator
open Info

open Declarations
open Term

let infer_type env t = (Typeops.infer      env t).Environ.uj_type
let infer_sort env a = (Typeops.infer_type env a).Environ.utj_type

(*
module ISet = Set.Make(
  struct
    type t = int
    let compare = Pervasives.compare
  end
)

(* Fetch all variables free in a term *)
let add_free_vars set te =
  let set = ref set in
  let add i = set := ISet.add i (!set) in
  let rec occur_rec n c = match Constr.kind c with
    | Constr.Rel m -> if m > n then add (m-n)
    | _ -> Constr.iter_with_binders succ occur_rec n c
  in
  occur_rec 0 te;
  !set

let get_free_vars te =
  List.sort Pervasives.compare (ISet.elements (add_free_vars ISet.empty te))

let fixpoint_signature env rec_decl = rec_decl
let fixpoint_signature env rec_decl =
  let (names, types, bodies) = rec_decl in
  let n = Array.length names in
  message "Debugging %i fixpoints:" n;
  for i = 0 to n - 1 do
    message "%i -> %a : %a" i pp_coq_name names.(i) pp_coq_term types.(i);
    message "body:  %a" pp_coq_term bodies.(i);
  done;
  let set = ref ISet.empty in
  Array.iter (fun e -> set := add_free_vars !set e) types;
  Array.iter (fun e -> set := add_free_vars !set e) bodies;
  let fv = List.sort Pervasives.compare (ISet.elements !set) in
  let fv_types =
    List.map
      (fun i ->
         message "Test: %i in %a" i pp_coq_env env;
         let _,_,ty = Context.Rel.Declaration.to_tuple (Environ.lookup_rel i env) in
         message "Type is : %a" pp_coq_type ty;
         ty) fv in
  (fv_types, rec_decl)
*)
let fixpoint_signature env rec_decl = (env, rec_decl)



let translate_rel_decl info env decl =
  match Context.Rel.Declaration.to_tuple decl with
  | (x, None, a) ->
    let x = Cname.fresh_name ~default:"_" info env x in
    let x' = Cname.translate_name x in
    let new_env = Environ.push_rel (Context.Rel.Declaration.LocalAssum(x, a)) env in
    (new_env, Some (x',a))
  | (x, Some u, a) ->
    let new_env = Environ.push_rel (Context.Rel.Declaration.LocalDef(x, u, a)) env in
    (new_env, None)

(** Infer and translate the sort of [a].
    Coq fails if we try to type a sort that was already inferred.
    This function uses pattern matching to avoid it. *)
let rec infer_translate_sort info env uenv a =
(*  This is wrong; there is no subject reduction in Coq! *)
(*  let a = Reduction.whd_all env a in*)
  match Term.kind_of_type a with
  | SortType s -> Translator.Succ ((Tsorts.translate_sort uenv s),1)
  (* FIXME: CastType are probably not correctly translated.
     They are never used in Coq's kernel but occur in SSReflect module *)
  | CastType(a', _) -> infer_translate_sort info env uenv a'
    (* Error.not_supported "CastType" *)
  | ProdType(x, a, b) ->
    let x = Cname.fresh_name info env ~default:"_" x in
    let s1' = infer_translate_sort info env uenv a in
    let new_env = Environ.push_rel (Context.Rel.Declaration.LocalAssum(x, a)) env in
    let s2' = infer_translate_sort info new_env uenv b in
    Translator.Rule (s1',s2')
  | LetInType(x, u, a, b) ->
    (* No need to lift the let here. *)
    let new_env = Environ.push_rel (Context.Rel.Declaration.LocalDef(x, u, a)) env in
    infer_translate_sort info new_env uenv b
  | AtomicType _ -> Tsorts.translate_sort uenv (infer_sort env a)

(** Abstract over the variables of [context], eliminating let declarations. *)
let abstract_rel_context context t =
  let abstract_rel_declaration t c =
    let (x, u, a) = (Context.Rel.Declaration.to_tuple c) in
    match u with
    | None -> Constr.mkLambda (x, a, t)
    | Some(u) -> Vars.subst1 u t in
  List.fold_left abstract_rel_declaration t context

(** Generalize over the variables of [context], eliminating let declarations. *)
let generalize_rel_context context b =
  let generalize_rel_declaration b c =
    let (x, u, a) = (Context.Rel.Declaration.to_tuple c) in
    match u with
    | None -> Constr.mkProd(x, a, b)
    | Some(u) -> Vars.subst1 u b in
  List.fold_left generalize_rel_declaration b context

(** Apply the variables of [context] to [t], ignoring let declarations. *)
let apply_rel_context t context =
  let apply_rel_declaration (args, i) c =
    let (x, t, a) = (Context.Rel.Declaration.to_tuple c) in
    match t with
    | None -> (Constr.mkRel(i) :: args, i + 1)
    | Some(_) -> (args, i + 1) in
  let args, _ = List.fold_left apply_rel_declaration ([], 1) context in
  Term.applistc t args

let rec convertible info env uenv a b =
  let a = Reduction.whd_all env a in
  let b = Reduction.whd_all env b in
  match Term.kind_of_type a, Term.kind_of_type b with
  | SortType(s1), SortType(s2) -> Tsorts.convertible_sort uenv s1 s2
  (* FIXME: CastType are probably not correctly translated.
     They are never used in Coq's kernel but occur in SSReflect module *)
  | CastType(a',_), _ -> convertible info env uenv a' b
  | _, CastType(b',_) -> convertible info env uenv a b'
  (* Error.not_supported "CastType" *)
  | ProdType(x1, a1, b1), ProdType(x2, a2, b2) ->
    convertible info env uenv a1 a2 &&
    let nenv = Environ.push_rel (Context.Rel.Declaration.LocalAssum (x1, a1)) env in
    convertible info nenv uenv b1 b2
  | LetInType(_), _
  | _, LetInType(_) ->
      assert false
  | AtomicType(_), AtomicType(_) ->
    begin
      try let _ = Reduction.default_conv Reduction.CONV env a b in true
      with Reduction.NotConvertible -> false
    end
  | _ -> false

(** This table holds the translations of fixpoints, so that we avoid
    translating the same definition multiple times (e.g. mutual fixpoints).
    This is very important, otherwise the size of the files will explode. *)
let fixpoint_table = Hashtbl.create 10007

let make_const mp x =
  Names.Constant.make3 mp (Names.DirPath.make []) (Names.Label.of_id x)

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
    match Cbytegen.compile_constant_body
            ~fail_on_error:true
            (Environ.pre_env env)
            (Monomorphic_const Univ.ContextSet.empty) const_body with
    | Some code -> code
    | None -> Error.error (Pp.str "compile_constant_body failed")
  in
  (** TODO : Double check the following *)
  let body = {
    const_hyps = [];
    const_body = const_body;
    const_type = const_type;
    const_body_code = Some (Cemitcodes.from_val const_body_code);
    const_universes = Monomorphic_const Univ.ContextSet.empty;
    const_proj = None;
    const_inline_code = false;
    const_typing_flags =
      { check_guarded = false;
        check_universes = false;
        conv_oracle = Conv_oracle.empty
      }
  } in
  Environ.add_constant c body env

(* This is exactly Inductive.cons_subst *)
let cons_subst u su subst =
  try Univ.LMap.add u (Univ.sup (Univ.LMap.find u subst) su) subst
  with Not_found -> Univ.LMap.add u su subst

(* This is exactly Inductive.remember_subst *)
let remember_subst u subst =
  try let su = Univ.Universe.make u in
    Univ.LMap.add u (Univ.sup (Univ.LMap.find u subst) su) subst
  with Not_found -> subst

(* This is exactly Inductive.make_subst *)
let make_subst env =
  let rec make subst = function
    | Context.Rel.Declaration.LocalDef _ :: sign, exp, args ->
      make subst (sign, exp, args)
    | _::sign, None::exp, args ->
      let args = match args with _::args -> args | [] -> [] in
      make subst (sign, exp, args)
    | _::sign, Some u::exp, a::args ->
      let s = Sorts.univ_of_sort (snd (Reduction.dest_arity env (Lazy.force a))) in
      make (cons_subst u s subst) (sign, exp, args)
    | Context.Rel.Declaration.LocalAssum (na,t) :: sign, Some u::exp, [] ->
      make (remember_subst u subst) (sign, exp, [])
    | sign, [], _ -> subst
    | [], _, _ -> assert false
  in make Univ.LMap.empty

(* This is almost exactly Inductive.instantiate_universes *)
let instantiate_universes env ctx ar argsorts =
  let args = Array.to_list argsorts in
  let subst = make_subst env (ctx,ar.template_param_levels,args) in
  let subst_fn = Univ.make_subst subst in
  let safe_subst_fn x =
    try subst_fn x with | Not_found -> Univ.Universe.make x in
  let level = Univ.subst_univs_universe safe_subst_fn ar.template_level in
  let ty =
    if Univ.is_type0m_univ level then Sorts.prop
    else if Univ.is_type0_univ level then Sorts.set
    else Sorts.Type level
  in (ctx,ty, subst, safe_subst_fn)

let infer_template_polymorph_ind_applied info env uenv (ind,u) args =
  debug "Inferring template polymorphic inductive: %a (%i)"
    pp_coq_label (Names.MutInd.label (fst ind)) (Array.length args);
  let (mib, mip) as spec = Inductive.lookup_mind_specif env ind in
  match mip.mind_arity with
  | RegularArity a -> Vars.subst_instance_constr u a.mind_user_arity, []
  | TemplateArity ar ->
    let args_types = Array.map (fun t -> lazy (infer_type env t)) args in
    let ctx = List.rev mip.mind_arity_ctxt in
    let ctx,s, subst, safe_subst = instantiate_universes env ctx ar args_types in
    let arity = Term.mkArity (List.rev ctx,s) in
    (* Do we really need to apply safe_subst to arity ?
    *)
    let arity = Universes.subst_univs_constr subst arity in
    debug "Substituted type: %a" pp_coq_term arity;
    arity,
    if Encoding.is_templ_polymorphism_on ()
    then
      List.map
        (fun lvl ->  T.coq_universe (Tsorts.translate_universe uenv lvl))
        (List.map safe_subst (Utils.filter_some ar.template_param_levels))
    else []

(* This is inspired from Inductive.type_of_constructor  *)
let infer_template_polymorph_construct_applied info env uenv ((ind,i),u) args =
  debug "Inferring template polymorphic constructor: %a (%i)"
    pp_coq_label (Names.MutInd.label (fst ind)) (Array.length args);
  let (mib, mip) as spec = Inductive.lookup_mind_specif env ind in
  assert (i <= Array.length mip.mind_consnames);
  match mip.mind_arity with
  | RegularArity a -> Vars.subst_instance_constr u a.mind_user_arity, []
  | TemplateArity ar ->
    let type_c = Inductive.type_of_constructor ((ind,i),u) spec in
    let args_types = Array.map (fun t -> lazy (infer_type env t)) args in
    let ctx = List.rev mip.mind_arity_ctxt in
    let ctx, s, subst, safe_subst = instantiate_universes env ctx ar args_types in
    debug "Subst: %a" pp_t (Univ.LMap.pr Univ.Universe.pr subst);
    if not (Encoding.is_templ_polymorphism_on ())
    then Universes.subst_univs_constr subst type_c, []
    else if not (Encoding.is_templ_polymorphism_cons_poly ())
    then type_c, []
    else
      (* FIXME: subst_univs_constr fails here when one of the substituted level is Prop
         1)   Universes.level_subst_of
          turns   Sorts.Type Univ.type0m
          into    Sorts.Prop  in the substitution
         2) Then
            Universes.subst_univs_constr
         -> Universes.subst_univs_fn_constr
         -> Univ.Instance.subst_fn
         -> Univ.Instance.of_array   which assumes all substituted level are not Prop
      *)
      Universes.subst_univs_constr subst type_c,
      List.map
        (fun lvl ->  T.coq_universe (Tsorts.translate_universe uenv lvl))
        (List.map safe_subst (Utils.filter_some ar.template_param_levels))

let infer_dest_applied info env uenv (ind,u) args =
  debug "Inferring destructor: %a (%i)"
    pp_coq_label (Names.MutInd.label (fst ind)) (Array.length args);
  let (mib, mip) as spec = Inductive.lookup_mind_specif env ind in
  match mip.mind_arity with
  | RegularArity a -> Vars.subst_instance_constr u a.mind_user_arity, []
  | TemplateArity ar ->
    debug "Inferring template polymorphic destructor: %a (%i)"
      pp_coq_label (Names.MutInd.label (fst ind)) (Array.length args);
    let args_types = Array.map (fun t -> lazy (infer_type env t)) args in
    let ctx = List.rev mip.mind_arity_ctxt in
    let ctx,s, subst, safe_subst = instantiate_universes env ctx ar args_types in
    let arity = Term.mkArity (List.rev ctx,s) in
    if Encoding.is_templ_polymorphism_on () &&
       Encoding.is_templ_polymorphism_cons_poly ()
    then
      (* Do we really need to apply safe_subst to arity ? *)
      let arity = Universes.subst_univs_constr subst arity in
      debug "Substituted type: %a" pp_coq_term arity;
      arity,
      List.map
        (fun lvl -> T.coq_universe (Tsorts.translate_universe uenv lvl))
        (List.map safe_subst (Utils.filter_some ar.template_param_levels))
    else arity, []


(** Translate the Coq term [t] as a Dedukti term. *)
let rec translate_constr ?expected_type info env uenv t =
  (* Check if the expected type coincides, otherwise make an explicit cast. *)
  let t =
    match expected_type with
    | None   -> t
    | Some a ->
      (* TODO: We have an issue here for applied Template Polymorphic Inductives:
         Type inferred of "True List" is Prop.
         However, inductive translation generate a Type1 when ignoring template poly
         -> This should be fixed:
              List (type 1) (lift prop (type 1) True)  -->  List Prop True
      *)
      debug "Inferring type for %a" (pp_coq_term_env env) t;
      debug " - Expecting: %a" (pp_coq_term_env env) a;
      let b = infer_type env t in
      debug " - Inferred:  %a" (pp_coq_term_env env) b;
      if not (Encoding.is_argument_casted ()) && convertible info env uenv a b then t
      else Constr.mkCast(t, Term.VMcast, a) in
  match Constr.kind t with
  | Rel i ->
    let (x, u, _) = Context.Rel.Declaration.to_tuple (Environ.lookup_rel i env) in
    begin match u with
      | Some u ->
        (* If it's a let definition, replace by its value. *)
        translate_constr info env uenv (Vars.lift i u)
      | None   -> Dedukti.var (Cname.translate_name ~ensure_name:true x)
    end
  | Var x -> Dedukti.var (Cname.translate_identifier x)
  | Sort s -> T.coq_sort (Tsorts.translate_sort uenv s)
  | Cast(t, _, b) ->
    let a = infer_type env t in
    let t' = translate_constr info env uenv t in
    translate_cast info uenv t' env a env b

  | Prod(x, a, b) ->
    let x = Cname.fresh_name ~default:"_" info env x in
    let x' = Cname.translate_name x in
    let a_sort = infer_translate_sort info env uenv a in
    let a'  = translate_constr info env uenv a in
    let a'' = translate_types  info env uenv a in
    let new_env = Environ.push_rel (Context.Rel.Declaration.LocalAssum(x, a)) env in
    let b_sort = infer_translate_sort info new_env uenv b in
    (* TODO: Should x really be in the environment when translating b's sort ? *)
    let b' = translate_constr info new_env uenv b in
    T.coq_prod a_sort b_sort a' (Dedukti.lam (x', a'') b')

  | Lambda(x, a, t) ->
    let x = Cname.fresh_name ~default:"_" info env x in
    let x' = Cname.translate_name x in
    let a'' = translate_types info env uenv a in
    let new_env = Environ.push_rel (Context.Rel.Declaration.LocalAssum(x, a)) env in
    let t' = translate_constr info new_env uenv t in
    Dedukti.lam (x', a'') t'

  | LetIn(x, u, a, t) ->
    if Encoding.is_letins_simpl ()
    then
      let id_x = match x with
        | Names.Name id -> id (* Assume letin var is named (otherwise what's the point ?) *)
        | Names.Anonymous -> assert false in
      let fresh_idx = Cname.fresh_identifier info env id_x in
      let x' = Cname.translate_identifier fresh_idx in
      let a' = translate_types  info env uenv a in
      let u' = translate_constr info env uenv u in
      let def = Context.Rel.Declaration.LocalDef
          (Names.Name.mk_name fresh_idx, Constr.mkVar fresh_idx, a) in
      (* TODO: This is unsafe, the constant should also be pushed to the environment
         with its correct type *)
      let new_env = Environ.push_rel def env in
      try
        let t' = translate_constr info new_env uenv t in
        Dedukti.letin (x',u',a') t'
      with Type_errors.TypeError _ | Constr.DestKO ->
        let new_env = lift_let info env uenv x u a in
        translate_constr info new_env uenv t
    else
      let new_env = lift_let info env uenv x u a in
      translate_constr info new_env uenv t

  | App(f, args) ->
    let tmpl_args = if Encoding.is_templ_polymorphism_on () then args else [||] in
    let type_f, univ_params' =
      match Constr.kind f with
      | Ind (ind, u) when Environ.template_polymorphic_pind (ind,u) env ->
        infer_template_polymorph_ind_applied info env uenv (ind,u) tmpl_args
      | Construct ((ind, c), u) when Environ.template_polymorphic_pind (ind,u) env ->
        infer_template_polymorph_construct_applied info env uenv ((ind, c), u) tmpl_args
      | Const (cst,u) when Environ.polymorphic_pconstant (cst,u) env ->
        debug "Instance: %a" pp_coq_inst u;
        Environ.constant_type_in env (cst,u), []
      | _ -> infer_type env f, []
    in
    let f' = Dedukti.apps (translate_constr info env uenv f) univ_params' in
    let translate_app (f', type_f) u =
      let _, c, d = Constr.destProd (Reduction.whd_all env type_f) in
      let u' = translate_constr ~expected_type:c info env uenv u in
      (Dedukti.app f' u', Vars.subst1 u d) in
    fst (Array.fold_left translate_app (f', type_f) args)

  | Const (kn, univ_instance) ->
    let name = Cname.translate_constant info env kn in
    debug "Printing constant: %s@@{%a}" name pp_coq_inst univ_instance;
    if Utils.str_starts_with "Little__fix" name
    then Dedukti.var (Utils.truncate name 8)
    else if Utils.str_starts_with "fix_" name ||
            not (Encoding.is_polymorphism_on ())
    then Dedukti.var name
    else if Encoding.is_polymorphism_on () &&
            Environ.polymorphic_pconstant (kn,univ_instance) env
    then
      let cb = Environ.lookup_constant kn env in
      let univ_ctxt = Declareops.constant_polymorphic_context cb in
      Tsorts.instantiate_poly_univ_params uenv univ_ctxt univ_instance (Dedukti.var name)
    else Dedukti.var name

  | Ind (ind, univ_instance) ->
    let name = Cname.translate_inductive info env ind in
    debug "Printing inductive: %s@@{%a}" name pp_coq_inst univ_instance;
    Tsorts.instantiate_poly_ind_univ_params env uenv ind univ_instance
      (Tsorts.instantiate_template_ind_univ_params env uenv ind univ_instance
         (Dedukti.var name))

  | Construct (kn, univ_instance) ->
    let name = Cname.translate_constructor info env kn in
    debug "Printing constructor: %s@@{%a}" name pp_coq_inst univ_instance;
    let ind = Names.inductive_of_constructor kn in
    Tsorts.instantiate_poly_ind_univ_params env uenv ind univ_instance
      ( if Tsorts.template_constructor_upoly ()
        then Tsorts.instantiate_template_ind_univ_params env uenv ind univ_instance
            (Dedukti.var name)
        else Dedukti.var name )

  | Fix (((rec_indices, i), ((names, types, bodies) as rec_declaration)) as fp) ->
    if Encoding.is_fixpoint_inlined ()
    then translate_fixpoint info env uenv fp (Array.map (infer_type env) types)
    else
      begin
        let n = Array.length names in
        debug "Translating %i fixpoints:" n;
        for i = 0 to n - 1 do
          debug "%i -> %a : %a" i pp_coq_name names.(i) pp_coq_term types.(i);
          debug "body:  %a" pp_coq_term bodies.(i);
        done;
        (* TODO: not the whole environment should be added here, only the relevant part
           i.e. variables that occur in the body of the fixpoint *)
        let env, fix_declarations =
          let fsig = fixpoint_signature env rec_declaration in
          try Hashtbl.find fixpoint_table fsig
          with Not_found ->
            let res = lift_fix info env uenv names types bodies rec_indices in
            Hashtbl.add fixpoint_table fsig res;
            res
        in
        let new_env =
          Array.fold_left
            (fun env declaration -> Environ.push_rel declaration env)
            env fix_declarations in
        translate_constr info new_env uenv (Constr.mkRel (n - i))
      end

  | Case(case_info, return_type, matched, branches) ->
    let match_function_name = Cname.translate_match_function info env case_info.ci_ind in
    let mind_body, ind_body = Inductive.lookup_mind_specif env case_info.ci_ind in
    let n_params = mind_body.Declarations.mind_nparams   in
    let n_reals  =  ind_body.Declarations.mind_nrealargs in
    let pind, ind_args = Inductive.find_rectype env (infer_type env matched) in
    (*
    let arity = Inductive.type_of_inductive env ( (mind_body, ind_body), snd pind) in
    *)
    let params, reals = Utils.list_chop n_params ind_args in
    (* Probably put that back
    let params = List.map (Reduction.whd_all env) params in
    *)
    debug "Template universe params: %a" (pp_list ", " pp_coq_term) params;
    (* FIXME: This is not correct !
       We do not provide universe level allowing substitution
       of universe polymorphic destructions. *)
    let arity, univ_params' =
      infer_dest_applied info env uenv pind (Array.of_list params)
    in
    let context, end_type = Term.decompose_lam_n_assum (n_reals + 1) return_type in
    let return_sort = infer_sort (Environ.push_rel_context context env) end_type in
    (* Translate params using expected types to make sure we use proper casts. *)
    let translate_param (params', a) param =
      let _, c, d = Constr.destProd (Reduction.whd_all env a) in
      let param' = translate_constr ~expected_type:c info env uenv param in
      (param' :: params', Vars.subst1 param d) in
    let match_function' = Dedukti.var match_function_name in
    let return_sort' = Tsorts.translate_sort uenv return_sort in
    let return_sort' = T.coq_universe return_sort' in
    let params' = List.rev (fst (List.fold_left translate_param ([], arity) params)) in
    debug "params': %a" (pp_list ", " Dedukti.pp_term) params';
    let return_type' = translate_constr info env uenv return_type in
    let branches' = Array.to_list (Array.map (translate_constr info env uenv) branches) in
    let reals' = List.map (translate_constr info env uenv) reals in
    let matched' = translate_constr info env uenv matched in
    Dedukti.apps match_function'
      (univ_params' @ return_sort' :: params' @ return_type' :: branches' @ reals' @  [matched'])

  (* Not yet supported cases: *)
  | Proj (p,t) ->
    begin
      let kn = Names.Projection.constant p in
      let cb = Environ.lookup_constant kn env in (* Constant body *)
      let pb = Option.get cb.const_proj       in (* Projection body *)
      let n = pb.proj_arg in (* Index of the projection *)
      T.coq_proj n (translate_constr info env uenv t)
    end
  (* Not supported cases: *)
  | Meta metavariable  -> Error.not_supported "Meta"
  | Evar pexistential  -> Error.not_supported "Evar"
  | CoFix(pcofixpoint) -> Error.not_supported "CoFix"

and translate_cast info uenv t' enva a envb b =
  debug "Casting %a@.from %a@.to %a" Dedukti.pp_term t' pp_coq_term a pp_coq_term b;
  if Encoding.is_cast_on () then
    let s1' = infer_translate_sort info enva uenv a in
    let s2' = infer_translate_sort info envb uenv b in
    let a' = translate_constr info enva uenv a in
    let b' = translate_constr info envb uenv b in
    if Encoding.is_polymorphism_on () && Encoding.is_constraints_on ()
    then
      let constraints =
        if Term.isArity a && Term.isArity b
        then
          let decla, sa = Term.destArity a in (* Extracts s from A1 -> ... -> An -> Us *)
          let declb, sb = Term.destArity b in
          let eq_types = Tsorts.gather_eq_types decla declb in
          debug "Gathering eq types %a = %a |-> { %a }"
            pp_coq_type a pp_coq_type b
            (pp_list ", " (fun fmt (a,b) -> Format.fprintf fmt "%a = %a"
                              pp_coq_type a pp_coq_type b)) eq_types;
          Univ.enforce_leq (Sorts.univ_of_sort sa) (Sorts.univ_of_sort sb)
            (* Careful, this could enforce a bit too many constraints :
                   max(a,b) < max(c,d) =>  a<c, a<d, b<c, b<d
               However this never happens in practice. *)
            (* Maybe: have an encoding that accepts too many constraints *)
            (Tsorts.enforce_eq_types Univ.Constraint.empty eq_types)
        else
          Tsorts.enforce_eq_types Univ.Constraint.empty [(a,b)]
      in
      let var_cstr = Tsorts.translate_constraint_set uenv constraints in
      T.coq_cast s1' s2' a' b' var_cstr t'
    else
      if Term.isArity a && Term.isArity b
      then
        let _,sa = Term.destArity a in (* Extracts s from A1 -> ... -> An -> Us *)
        let _,sb = Term.destArity b in
        if Sorts.equal sa sb then t' else T.coq_cast s1' s2' a' b' [] t'
      else T.coq_cast s1' s2' a' b' [] t'
  else
    match Term.kind_of_type a, Term.kind_of_type b with
    | SortType sa, SortType sb ->
      if Sorts.equal sa sb then t'
      else T.coq_lift
          (Tsorts.translate_sort uenv sa)
          (Tsorts.translate_sort uenv sb) t'

    (* FIXME: CastType are probably not correctly translated.
       They are never used in Coq's kernel but occur in SSReflect module *)
    | CastType(a',_), _ -> translate_cast info uenv t' enva a' envb b
    | _, CastType(b',_) -> translate_cast info uenv t' enva a envb b'
    (* Error.not_supported "CastType" *)

    | ProdType(x1, a1, b1), ProdType(x2, a2, b2) ->
      (* TODO: should we check for useless casts (b1~b2) ? *)
      let x1' = Cname.fresh_name info enva x1 in
      let (x,tA),t =
        match t' with
        | Dedukti.Lam ((x,Some tA),t) -> ((x,tA), t)
        | _ -> (* We eta-expand the translation t' of t here *)
          let tA = translate_types info enva uenv a1 in
          (* We assume a1 and a2 are convertible.
             Otherwise Coq's typecheking would have failed. *)
          let x = Cname.translate_name x1' in
          let t = Dedukti.app t' (Dedukti.var x)  in
          ((x,tA), t)
      in
      let nenva = Environ.push_rel (Context.Rel.Declaration.LocalAssum(x1', a1)) enva in
      let nenvb = Environ.push_rel (Context.Rel.Declaration.LocalAssum(x1', a2)) envb in
      Dedukti.lam (x, tA) (translate_cast info uenv t nenva b1 nenvb b2)
    | _ -> t'

(** Translate the Coq type [a] as a Dedukti type. *)
and translate_types info env uenv a =
  (* Specialize on the type to get a nicer and more compact translation. *)
  match Term.kind_of_type a with
  | SortType(s) -> T.coq_U (Tsorts.translate_sort uenv s)
  (* FIXME: CastType are probably not correctly translated.
     They are never used in Coq's kernel but occur in SSReflect module *)
  | CastType(a', _) -> translate_types info env uenv a'
  (* Error.not_supported "CastType" *)
  | ProdType(x, a, b) ->
    let x = Cname.fresh_name info ~default:"_" env x in
    let x' = Cname.translate_name x in
    let a' = translate_types info env uenv a in
    let new_env = Environ.push_rel (Context.Rel.Declaration.LocalAssum(x, a)) env in
    let b' = translate_types info new_env uenv b in
    Dedukti.pie (x', a') b'

  | LetInType(x, u, a, b) ->
    if Encoding.is_letins_simpl ()
    then
      let id_x = match x with
        | Names.Name id -> id (* Assume letin var is named (otherwise what's the point ?) *)
        | Names.Anonymous -> assert false in
      let fresh_idx = Cname.fresh_identifier info env id_x in
      let x' = Cname.translate_identifier fresh_idx in
      let a' = translate_types  info env uenv a in
      let u' = translate_constr info env uenv u in
      let def = Context.Rel.Declaration.LocalDef
          (Names.Name.mk_name fresh_idx, Constr.mkVar fresh_idx, a) in
      (* TODO: This is unsafe, the constant should also be pushed to the environment
         with its correct type *)
      let new_env = Environ.push_rel def env in
      let b' = translate_types info new_env uenv b in
      Dedukti.letin (x',u',a') b'
    else
      let new_env = lift_let info env uenv x u a in
      translate_types info new_env uenv b

  | AtomicType(_) ->
    (* Fall back on the usual translation of types. *)
    let s' = infer_translate_sort info env uenv a in
    let a' = translate_constr info env uenv a in
    T.coq_term s' a'

and translate_sort uenv t =
  match Term.kind_of_type t with
  | SortType s -> T.coq_universe (Tsorts.translate_sort uenv s)
  | CastType (a', _) -> translate_sort uenv a'
  | _ -> assert false



(** Translation of   Fix fi { f1 / k1 : A1 := t1, ..., fn / kn : An := tn }  *)
and translate_fixpoint info env uenv (fp:(Constr.constr,Constr.types) Constr.pfixpoint) sorts =
  let (rec_indices, i), (names, types, bodies) = fp in
  debug "Translating fixpoint:@.%a" pp_fixpoint fp;
  let n = Array.length names in
  (* List of all (si,ki, Ai') where Ai' is Ai translated in current context *)
  let arities' =
    Array.init n
      (fun i ->
         translate_sort uenv sorts.(i),
         rec_indices.(i),
         translate_constr ~expected_type:sorts.(i) info env uenv types.(i)
      ) in
  (* Fresh names fi' for the fi *)
  let fresh_names = Array.map (Cname.fresh_name ~default:"_" info env) names in
  let names' = Array.map Cname.translate_name fresh_names in
  (* Computing the extended context where all recursive function symbols are added *)
  let ext_env = Environ.push_rec_types (fresh_names,types,types) env in
  (* Lifted Ai to represent the same term in the extended context *)
  let lifted_types = Array.map (fun ty -> Vars.lift n ty) types in
  (* Bodi ti is translated as   f1' => ... fn' => ti'   *)
  let add_lams t =
    Array.fold_right (fun name t -> Dedukti.ulam name t) names' t in
  (* Translating the ti to ti' in the extended context *)
  let bodies' = Array.init n (fun i ->
      add_lams (translate_constr ~expected_type:lifted_types.(i) info ext_env uenv bodies.(i))) in
  T.coq_fixpoint n arities' bodies' i



and lift_let info env uenv x u a =
  let y = Cname.fresh_of_name ~global:true ~prefix:"let" ~default:"_" info env x in
  let yconstant = make_const info.module_path y in
  let y' = Cname.translate_constant info env yconstant in
  let rel_context = Environ.rel_context env in
  let a_closed = generalize_rel_context rel_context a in
  let u_closed =   abstract_rel_context rel_context u in
  (* [a_closed] amd [u_closed] are in the global environment but we still
     have to remember the named variables (i.e. from nested let in). *)
  let global_env = Environ.reset_with_named_context (Environ.named_context_val env) env in
  let a_closed' = translate_types  info global_env uenv a_closed in
  let u_closed' = translate_constr info global_env uenv u_closed in
  Dedukti.print info.fmt (Dedukti.definition false y' a_closed' u_closed');
  let yconstr = apply_rel_context (Constr.mkConst yconstant) rel_context in
  let env = push_const_decl env (yconstant, Some(u_closed), a_closed) in
  let new_env = Environ.push_rel (Context.Rel.Declaration.LocalDef(x, yconstr, a)) env in
  new_env

and lift_fix info env uenv names types bodies rec_indices =
  (* A fixpoint is translated by 3 functions:
     - The first function duplicates the argument and sends it to the second.
     - The second pattern matches on the second arguments, then throws it away
       and passes the first argument to the third function.
     - The third function executes the body of the fixpoint.

     fix1_f : |Context| ->
              |x1| : ||A1|| -> ... -> |xr| : ||I u1 ... un|| -> ||A||.

     fix2_f : |Context| ->
              |x1| : ||A1|| -> ... -> |xr| : ||I u1 ... un|| ->
              |y1| : ||B1|| -> ... -> |yn| : ||Bn|| ->
              ||I y1 ... yn|| -> ||A||.

     fix3_f : |Context| ->
              |x1| : ||A1|| -> ... -> |xr| : ||I u1 ... un|| -> ||A||.

     [...]
       fix1_f |G| |x1| ... |xr|
       -->
       fix2_f |G| |x1| ... |xr| |u1| ... |un| |xr|.

     [...]
       fix2_f |G| |x1| ... |xr| {|uj1|} ... {|ujn|} (|cj z1 ... zkj|)
       -->
       fix3_f |G| |x1| ... |xr|.

     [...]
       fix3_f |G| |x1| ... |xr|
       -->
       |[(fix1_f G)/f]t|.
  *)
  let n = Array.length names in
  let fix_names1 = Array.map (Cname.fresh_of_name info env ~global:true ~prefix:"fix1" ~default:"_") names in
  let fix_names2 = Array.map (Cname.fresh_of_name info env ~global:true ~prefix:"fix2" ~default:"_") names in
  let fix_names3 = Array.map (Cname.fresh_of_name info env ~global:true ~prefix:"fix3" ~default:"_") names in
  let contexts_return_types = Array.mapi (fun i -> Term.decompose_prod_n_assum (rec_indices.(i) + 1)) types in
  let contexts = Array.map fst contexts_return_types in
  let return_types = Array.map snd contexts_return_types in
  let ind_applieds = Array.map (fun context -> Context.Rel.Declaration.get_type (List.hd context)) contexts in
  let inds_args = Array.map (Inductive.find_inductive env) ind_applieds in
  let pinds = Array.map fst inds_args in
  let inds = Array.map fst pinds in
  let ind_args = Array.map snd inds_args in
  let ind_specifs = Array.map (Inductive.lookup_mind_specif env) inds in
  let one_ind_body = Array.map snd ind_specifs in
  let arity_contexts = Array.map (fun body -> fst (Inductive.mind_arity body)) one_ind_body in
  let ind_applied_arities = Array.init n (fun i -> apply_rel_context (Constr.mkInd inds.(i)) arity_contexts.(i)) in
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
  let name1_declarations = Array.init n (fun j -> (const1.(j), None, types1_closed.(j))) in
  let name2_declarations = Array.init n (fun j -> (const2.(j), None, types2_closed.(j))) in
  let name3_declarations = Array.init n (fun j -> (const3.(j), None, types3_closed.(j))) in
  let fix_names1' = Array.map Cname.translate_identifier fix_names1 in
  let fix_names2' = Array.map Cname.translate_identifier fix_names2 in
  let fix_names3' = Array.map Cname.translate_identifier fix_names3 in
  (* we still have to remember the named variables (i.e. from nested let in). *)
  let global_env = Environ.reset_with_named_context (Environ.named_context_val env) env in
  (* Check: we can create a fixpoint particular for current uenv. *)
  let types1_closed' = Array.map (translate_types info global_env uenv) types1_closed in
  let types2_closed' = Array.map (translate_types info global_env uenv) types2_closed in
  let types3_closed' = Array.map (translate_types info global_env uenv) types3_closed in
  for i = 0 to n - 1 do
    Dedukti.print info.fmt (Dedukti.declaration true fix_names1'.(i) types1_closed'.(i));
    Dedukti.print info.fmt (Dedukti.declaration true fix_names2'.(i) types2_closed'.(i));
    Dedukti.print info.fmt (Dedukti.declaration true fix_names3'.(i) types3_closed'.(i));
  done;
  let fix_terms1 = Array.map Constr.mkConst const1 in
  let fix_terms2 = Array.map Constr.mkConst const2 in
  let fix_terms3 = Array.map Constr.mkConst const3 in
  let fix_rules1 = Array.init n (fun i ->
    let env, context' = translate_rel_context info global_env uenv (contexts.(i) @ rel_context) in
    let fix_term1' = translate_constr info env uenv fix_terms1.(i) in
    let fix_term2' = translate_constr info env uenv fix_terms2.(i) in
    let ind_args' = List.map (translate_constr info env uenv) (List.map (Vars.lift 1) ind_args.(i)) in
    [(context',
      Dedukti.apply_context fix_term1' context',
      Dedukti.apps (Dedukti.apply_context fix_term2' context')
        (ind_args' @ [Dedukti.var (fst (List.nth context' (List.length context' - 1)))]))
    ]) in
  let fix_rules2 = Array.init n (fun i ->
      let nb_poly_univs =
        if Encoding.is_polymorphism_on () then Univ.Instance.length (snd pinds.(i)) else 0 in
      debug "Nb poly univs: %i" nb_poly_univs;
      let nb_templ_poly = if Encoding.is_templ_polymorphism_on ()
        then (match one_ind_body.(i).mind_arity with
            | RegularArity a -> 0
            | TemplateArity ar -> Utils.count_some ar.template_param_levels)
        else 0 in
      let cons_arities = Inductive.arities_of_constructors pinds.(i) ind_specifs.(i) in
      let cons_contexts_types = Array.map Term.decompose_prod_assum cons_arities in
      let cons_contexts = Array.map fst cons_contexts_types in
      let cons_types = Array.map snd cons_contexts_types in
      let cons_ind_args = Array.map (fun cons_type -> snd (Inductive.find_inductive env cons_type)) cons_types in
      let n_cons = Array.length cons_types in
      let f j =
        let env, context' = translate_rel_context info (global_env) uenv (contexts.(i) @ rel_context) in
        let env, cons_context' = translate_rel_context info env uenv (cons_contexts.(j)) in
        let fix_term2' = translate_constr info env uenv fix_terms2.(i) in
        let fix_term3' = translate_constr info env uenv fix_terms3.(i) in
        let cons_term' = translate_constr info env uenv
            (Constr.mkConstructUi (((pinds.(i), j + 1)))) in
        (* These variable do not need to be named
        let cons_term_applied' = Dedukti.apps cons_term'
            (List.map (fun v -> Dedukti.var (fst v)) cons_context') in
        *)
        let nb_args = nb_poly_univs + nb_templ_poly + List.length cons_context' in
        let cons_term_applied' = Dedukti.apps cons_term'
            (Utils.list_init Dedukti.wildcard nb_args) in
        (*
        let cons_ind_args' = List.map (translate_constr info env uenv) cons_ind_args.(j) in
        *)
        let cons_ind_args' = Utils.list_init Dedukti.wildcard (List.length cons_ind_args.(j)) in
        Debug.debug "cons_ind_args': %a" (pp_list ", " Dedukti.pp_term) cons_ind_args';
        (context' ,
         Dedukti.apps
           (Dedukti.apply_context fix_term2' context')
           (cons_ind_args' @ [cons_term_applied']),
         Dedukti.apply_context fix_term3' context') in
      let cons_rules = Array.init n_cons f in
      Array.to_list cons_rules
    ) in
  let fix_applieds1 = Array.init n (fun i -> apply_rel_context fix_terms1.(i) rel_context) in
  (* The declarations need to be lifted to account for the displacement. *)
  let fix_declarations1 = Array.init n (fun i ->
    Context.Rel.Declaration.LocalDef
      (names.(i), Vars.lift i fix_applieds1.(i), Vars.lift i types.(i))) in
  let fix_rules3 = Array.init n (fun i ->
    let env, rel_context' = translate_rel_context info (global_env) uenv rel_context in
    let env = Array.fold_left push_const_decl env name1_declarations in
    let fix_term3' = translate_constr info env uenv fix_terms3.(i) in
    let env = Array.fold_left
        (fun env declaration -> Environ.push_rel declaration env)
        env fix_declarations1 in
    let body' = translate_constr info env uenv bodies.(i) in
(*    let env , context' = translate_rel_context info env uenv contexts.(i) in*)
    [(rel_context',
      Dedukti.apply_context fix_term3' rel_context',
      body')
    ]) in
  for i = 0 to n - 1 do
    List.iter (Dedukti.print info.fmt) (List.map Dedukti.typed_rewrite fix_rules1.(i));
    List.iter (Dedukti.print info.fmt) (List.map Dedukti.typed_rewrite fix_rules2.(i));
    List.iter (Dedukti.print info.fmt) (List.map Dedukti.typed_rewrite fix_rules3.(i));
  done;
  let env = Array.fold_left push_const_decl env name1_declarations in
  let env = Array.fold_left push_const_decl env name2_declarations in
  let env = Array.fold_left push_const_decl env name3_declarations in
  env, fix_declarations1

(** Translate the context [x1 : a1, ..., xn : an] into the list
    [x1, ||a1||; ...; x1, ||an||], ignoring let declarations. *)
and translate_rel_context info env uenv context =
  let aux decl (env, translated) =
    match translate_rel_decl info env decl with
    | (new_env, Some (x',a)) ->
      let a' = translate_types info env uenv a in
      (new_env, (x', a') :: translated)
    | (new_env, None) -> (new_env, translated) in
  let env, translated = List.fold_right aux context (env, []) in
  (* Reverse the list as the newer declarations are on top. *)
  (env, List.rev translated)

(** Translating Constrs as Dedukti patterns *)
let translate_args info env uenv ts =
  (* TODO: improve this to have patterns *)
  List.map (translate_constr info env uenv) ts
