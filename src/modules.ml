(** Translation of Coq modules *)

open Declarations

open Info

let get_constant_type const_type =
  match const_type with
  | NonPolymorphicType(a) -> a
  | PolymorphicArity(rel_context, poly_arity) ->
      Term.mkSort (Term.Type(poly_arity.poly_level))

let translate_constant_body info env label const =
  let label' = Name.translate_label label in
  (* TODO: Handle [constant_body.const_hyps] *)
  let const_type = get_constant_type const.const_type in
  let const_type' = Terms.translate_types info env const_type in
  match const.const_body with
  | Undef(inline) ->
      Dedukti.print info.out (Dedukti.declaration label' const_type')
  | Def(constr_substituted) ->
      let constr' = Terms.translate_constr ~expected_type:const_type info env (Declarations.force constr_substituted) in
      Dedukti.print info.out (Dedukti.definition false label' const_type' constr')
  | OpaqueDef(lazy_constr) ->
      let constr' = Terms.translate_constr ~expected_type:const_type info env (Declarations.force_opaque lazy_constr) in
      Dedukti.print info.out (Dedukti.definition true label' const_type' constr')

let get_inductive_arity_sort ind_arity =
  match ind_arity with
  | Monomorphic(mono_ind_arity) -> mono_ind_arity.mind_sort
  | Polymorphic(poly_arity) -> Term.Type(poly_arity.poly_level)

(** Translate the i-th inductive body in [mind_body]. *)
let translate_one_inductive_body info env label mind_body i =
  let ind_body = mind_body.mind_packets.(i) in (* Body of the current inductive type *)
  let n_types = mind_body.mind_ntypes in (* Number of mutual inductive types *)
  let n_params = mind_body.mind_nparams in (* Number of parameters common to all definitions *)
  let n_cons = Array.length ind_body.mind_consnames in (* Number of constructors in the current type *)
  let mind = Names.make_mind info.module_path Names.empty_dirpath label in
  let ind_terms = Array.init n_types (fun i -> Term.mkInd(mind, i)) in
  (* Constructor names start from 1. *)
  let cons_terms = Array.init n_cons (fun j -> Term.mkConstruct((mind, i), j + 1)) in
  
  (* Translate the inductive type. *)
  (* I : ||p1 : P1 -> ... -> pr : Pr -> x1 : A1 -> ... -> xn : An -> s|| *)
  let name = ind_body.mind_typename in
  let arity_context = ind_body.mind_arity_ctxt in
  let arity_sort = get_inductive_arity_sort ind_body.mind_arity in
  let arity = Term.it_mkProd_or_LetIn (Term.mkSort arity_sort) arity_context in
  let name' = Name.translate_identifier name in
  let arity' = Terms.translate_types info env arity in
  Dedukti.print info.out (Dedukti.declaration name' arity');
  
  (* Translate the constructors. *)
  (* cj : ||p1 : P1 -> ... -> pr : Pr -> yj1 : B1 -> ... -> yjkj : Bjkj -> I p1 ... pr uj1 ... ujn|| *)
  let cons_names = ind_body.mind_consnames in
  (* Substitute the inductive types as specified in the Coq code. *)
  let cons_types = Array.map (Term.substl (Array.to_list ind_terms)) ind_body.mind_user_lc in
  let cons_names' = Array.map Name.translate_identifier cons_names in
  let cons_types' = Array.map (Terms.translate_types info env) cons_types in
  for j = 0 to n_cons - 1 do
    Dedukti.print info.out (Dedukti.declaration cons_names'.(j) cons_types'.(j));
  done;
  
  (* Use the normalized types in the rest. *)
  let cons_types = Array.map (Term.substl (Array.to_list ind_terms)) ind_body.mind_nf_lc in
  
  (* Translate the match function. *)
  (* match_I : |p1| : ||P1|| -> ... |pr| : ||Pr|| ->
       s : srt -> P : (|x1| : ||A1|| -> ... -> |xn| : ||An|| -> ||I p1 ... pr x1 ... xn|| -> type s) -> ...
       case_cj : (|yj1| : ||Bj1|| -> ... -> |yjk1| : ||Bjk1|| -> term s (P |uj1| ... |ujn| (|c1 p1 ... pr yj1 ... yjkj|))) -> ...
       |x1| : ||A1|| -> ... -> |xn| : ||An|| -> x : ||I p1 ... pr x1 ... xn|| -> term s (P |x1| ... |xn| x) *)
  let match_function_name = Name.match_function name in
  let params_context = mind_body.mind_params_ctxt in
  let arity_real_context, _ = Util.list_chop ind_body.mind_nrealargs_ctxt arity_context in
  let ind_applied = Terms.apply_rel_context ind_terms.(i) (arity_real_context @ params_context) in
  let cons_context_types = Array.map Term.decompose_prod_assum cons_types in
  let cons_contexts = Array.map fst cons_context_types in
  let cons_types = Array.map snd cons_context_types in
  let cons_real_contexts = Array.init n_cons (fun j ->
    fst (Util.list_chop ind_body.mind_consnrealdecls.(j) cons_contexts.(j))) in 
  let cons_ind_args = Array.map (fun a -> snd (Inductive.find_inductive env a)) cons_types in
  let cons_ind_real_args = Array.init n_cons (fun j ->
    snd (Util.list_chop n_params cons_ind_args.(j))) in
  let cons_applieds = Array.init n_cons (fun j ->
    Terms.apply_rel_context cons_terms.(j) (cons_real_contexts.(j) @ params_context))  in
  let match_function_name' = Name.translate_identifier match_function_name in
  let params_env, params_context' = Terms.translate_rel_context info (Global.env ()) params_context in
  let return_sort_name = Name.fresh_of_string info params_env "s" in
  let return_sort_name' = Name.translate_identifier return_sort_name in
  let params_env = Name.push_identifier return_sort_name params_env in
  let return_type_name = Name.fresh_of_string info params_env "P" in
  let return_type_name' = Name.translate_identifier return_type_name in
  let params_env = Name.push_identifier return_type_name params_env in
  let params_env, case_names' = Array.fold_left (fun (params_env, case_names') cons_name ->
    let case_name = Name.fresh_identifier info params_env ~prefix:"case" cons_name in
    let case_name' = Name.translate_identifier case_name in
    let params_env = Name.push_identifier case_name params_env in
    (params_env, case_name' :: case_names')) (params_env, []) cons_names in
  let case_names' = Array.of_list (List.rev case_names') in
  let arity_real_env, arity_real_context' = Terms.translate_rel_context info params_env arity_real_context in
  let ind_applied' = Terms.translate_types info arity_real_env ind_applied in
  let matched_name = Name.fresh_of_string info arity_real_env "x" in
  let matched_name' = Name.translate_identifier matched_name in
  let params_env = Name.push_identifier matched_name params_env in
  let match_function' = Dedukti.var match_function_name' in
  let return_sort' = Dedukti.var return_sort_name' in
  let return_type' = Dedukti.var return_type_name' in
  let cases' = Array.map Dedukti.var case_names' in
  let matched' = Dedukti.var matched_name' in
  let params' = List.map Dedukti.var (fst (List.split params_context')) in
  let cons_real_env_contexts' = Array.map (Terms.translate_rel_context info params_env) cons_real_contexts in
  let cons_real_envs = Array.map fst cons_real_env_contexts' in
  let cons_real_contexts' = Array.map snd cons_real_env_contexts' in
  let cons_ind_real_args' = Array.mapi (fun j -> Terms.translate_args info cons_real_envs.(j)) cons_ind_real_args in
  let cons_applieds' = Array.mapi (fun j -> Terms.translate_constr info cons_real_envs.(j)) cons_applieds in
  (* Combine the above. *)
  let case_types' = Array.init n_cons (fun j -> Dedukti.pies cons_real_contexts'.(j)
    (Terms.coq_term return_sort' (Dedukti.apps return_type' (cons_ind_real_args'.(j) @ [cons_applieds'.(j)])))) in
  let cases_context' = Array.to_list (Array.init n_cons (fun j -> (case_names'.(j), case_types'.(j)))) in
  let common_context' =
    params_context' @
    (return_sort_name', Universes.coq_srt) ::
    (return_type_name', Dedukti.pies arity_real_context' (Dedukti.arr ind_applied' (Terms.coq_type return_sort'))) ::
    cases_context' in
  let match_function_context' = common_context' @ arity_real_context' @ [matched_name', ind_applied'] in
  let match_function_type' = Terms.coq_term return_sort'
    (Dedukti.app (Dedukti.apply_context return_type' arity_real_context') matched') in
  Dedukti.print info.out (Dedukti.declaration match_function_name' (Dedukti.pies match_function_context' match_function_type'));
  let match_function_applied' =
    Dedukti.apps match_function' (params' @ return_sort' :: return_type' :: Array.to_list cases') in
  let case_rules = Array.init n_cons (fun j ->
    let case_rule_context' = common_context' @ cons_real_contexts'.(j) in
    let case_rule_left' = Dedukti.apps match_function_applied' (cons_ind_real_args'.(j) @ [cons_applieds'.(j)]) in
    let case_rule_right' = Dedukti.apply_context cases'.(j) cons_real_contexts'.(j) in
    (case_rule_context', case_rule_left', case_rule_right')) in
  Dedukti.print info.out (Dedukti.rewrite (Array.to_list case_rules))
  
  (* Translate the fix function. *)

(** Translate the body of mutual inductive definitions [mind]. *)
let translate_mutual_inductive_body info env label mind_body =
  for i = 0 to mind_body.mind_ntypes - 1 do
    translate_one_inductive_body info env label mind_body i
  done

let identifiers_of_mutual_inductive_body mind_body =
  let identifiers_of_inductive_body ind_body =
    ind_body.mind_typename :: Array.to_list ind_body.mind_consnames in
  List.concat (Array.to_list
    (Array.map identifiers_of_inductive_body mind_body.mind_packets))

let identifiers_of_structure_field_body (label, struct_field_body) =
  match struct_field_body with
  | SFBconst(_) -> [Names.id_of_label label]
  | SFBmind(mind_body) -> identifiers_of_mutual_inductive_body mind_body
  | SFBmodule(_) -> []
  | SFBmodtype(_) -> []

let identifiers_of_structure_body structure_body =
  List.concat (List.map identifiers_of_structure_field_body structure_body)

let rec translate_module_body info env mods =
  match mods.mod_expr with
  | Some(struct_expr) -> translate_struct_expr_body info env struct_expr
  | None -> ()

and translate_struct_expr_body info env struct_expr =
  match struct_expr with
  | SEBstruct(structs) -> translate_structure_body info env structs
  | _ -> ()

and translate_structure_body info env structure_body =
  List.iter (translate_structure_field_body info env) structure_body

and translate_structure_field_body info env (label, struct_field_body) =
  match struct_field_body with
  | SFBconst(const_body) -> translate_constant_body info env label const_body
  | SFBmind(mind_body) -> translate_mutual_inductive_body info env label mind_body
  | _ -> ()

