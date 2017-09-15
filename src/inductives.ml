open Declarations

open Info

(** An inductive definition is organised into:
    - [mutual_inductive_body] : a block of (co)inductive type definitions,
      containing a context of common parameter and list of [inductive_body]
    - [inductive_body] : a single inductive type definition,
      containing a name, an arity, and a list of constructor names and types **)

(** Translate the i-th inductive type in [mind_body]. *)
let translate_inductive info env label mind_body i =
  let ind_body = mind_body.mind_packets.(i) in (* Body of the current inductive type *)
  
  (* Translate the inductive type. *)
  (* I : ||p1 : P1 -> ... -> pr : Pr -> x1 : A1 -> ... -> xn : An -> s|| *)
  let name = ind_body.mind_typename in
  let arity_context = ind_body.mind_arity_ctxt in
  let arity_sort = match ind_body.mind_arity with
    | RegularArity  ria -> ria.mind_sort
    | TemplateArity ta  -> Term.Type(ta.template_level) in
  let arity = Term.it_mkProd_or_LetIn (Term.mkSort arity_sort) arity_context in
  let name' = Name.translate_element_name info env (Names.label_of_id name) in
  let arity' = Terms.translate_types info env arity in
  Dedukti.print info.out (Dedukti.declaration false name' arity')

(** Translate the constructors of the i-th inductive type in [mind_body]. *)
let translate_constructors info env label mind_body i =
  let ind_body = mind_body.mind_packets.(i) in (* Body of the current inductive type *)
  let n_types = mind_body.mind_ntypes in (* Number of mutual inductive types *)
  let n_cons = Array.length ind_body.mind_consnames in (* Number of constructors in the current type *)
  let mind = Names.make_mind info.module_path Names.empty_dirpath label in
  let ind_terms = Array.init n_types (fun i -> Term.mkInd(mind, i)) in
  
  (* Translate the constructors. *)
  (* cj : ||p1 : P1 -> ... -> pr : Pr -> yj1 : B1 -> ... -> yjkj : Bjkj -> I p1 ... pr uj1 ... ujn|| *)
  let cons_names = ind_body.mind_consnames in
  (* Substitute the inductive types as specified in the Coq code. *)
  let cons_types = Array.map (Vars.substl (List.rev (Array.to_list ind_terms))) ind_body.mind_user_lc in
  let cons_names' = Array.map (fun cons_name -> Name.translate_element_name info env (Names.label_of_id cons_name)) cons_names in
  let cons_types' = Array.map (Terms.translate_types info env) cons_types in
  for j = 0 to n_cons - 1 do
    Dedukti.print info.out (Dedukti.declaration false cons_names'.(j) cons_types'.(j));
  done

(** Translate the match function for the i-th inductive type in [mind_body]. *)
let translate_match info env label mind_body i =
  let ind_body = mind_body.mind_packets.(i) in (* Body of the current inductive type *)
  let n_types = mind_body.mind_ntypes in (* Number of mutual inductive types *)
  let n_params = mind_body.mind_nparams in (* Number of parameters common to all definitions *)
  let n_cons = Array.length ind_body.mind_consnames in (* Number of constructors in the current type *)
  let mind = Names.make_mind info.module_path Names.empty_dirpath label in
  let ind_terms = Array.init n_types (fun i -> Term.mkInd(mind, i)) in
  (* Constructor names start from 1. *)
  let cons_terms = Array.init n_cons (fun j -> Term.mkConstruct((mind, i), j + 1)) in
  
  let name = ind_body.mind_typename in
  let arity_context = ind_body.mind_arity_ctxt in
  
  let cons_names = ind_body.mind_consnames in
  
  (* Use the normalized types in the rest. *)
  let cons_types = Array.map (Vars.substl (List.rev (Array.to_list ind_terms))) ind_body.mind_nf_lc in
  
  (* Translate the match function. *)
  (* match_I : |p1| : ||P1|| -> ... |pr| : ||Pr|| ->
       s : srt -> P : (|x1| : ||A1|| -> ... -> |xn| : ||An|| -> ||I p1 ... pr x1 ... xn|| -> type s) -> ...
       case_cj : (|yj1| : ||Bj1|| -> ... -> |yjk1| : ||Bjk1|| -> term s (P |uj1| ... |ujn| (|c1 p1 ... pr yj1 ... yjkj|))) -> ...
       |x1| : ||A1|| -> ... -> |xn| : ||An|| -> x : ||I p1 ... pr x1 ... xn|| -> term s (P |x1| ... |xn| x) *)
  let match_function_name = Name.match_function name in
  let params_context = mind_body.mind_params_ctxt in
  let arity_real_context, _ = Utils.list_chop ind_body.mind_nrealdecls arity_context in
  let ind_applied = Terms.apply_rel_context ind_terms.(i) (arity_real_context @ params_context) in
  let cons_context_types = Array.map Term.decompose_prod_assum cons_types in
  let cons_contexts = Array.map fst cons_context_types in
  let cons_types = Array.map snd cons_context_types in
  let cons_real_contexts = Array.init n_cons (fun j ->
    fst (Utils.list_chop ind_body.mind_consnrealdecls.(j) cons_contexts.(j))) in 
  let cons_ind_args = Array.map (fun a -> snd (Inductive.find_inductive env a)) cons_types in
  let cons_ind_real_args = Array.init n_cons (fun j ->
    snd (Utils.list_chop n_params cons_ind_args.(j))) in
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
    (Dedukti.coq_term return_sort' (Dedukti.apps return_type' (cons_ind_real_args'.(j) @ [cons_applieds'.(j)])))) in
  let cases_context' = Array.to_list (Array.init n_cons (fun j -> (case_names'.(j), case_types'.(j)))) in
  let common_context' =
    params_context' @
    (return_sort_name', Dedukti.coq_Sort) ::
    (return_type_name', Dedukti.pies arity_real_context' (Dedukti.arr ind_applied' (Dedukti.coq_U return_sort'))) ::
    cases_context' in
  let match_function_context' = common_context' @ arity_real_context' @ [matched_name', ind_applied'] in
  let match_function_type' = Dedukti.coq_term return_sort'
    (Dedukti.app (Dedukti.apply_context return_type' arity_real_context') matched') in
  Dedukti.print info.out (Dedukti.declaration true match_function_name' (Dedukti.pies match_function_context' match_function_type'));
  let match_function_applied' =
    Dedukti.apps match_function' (params' @ return_sort' :: return_type' :: Array.to_list cases') in
  let case_rules = Array.init n_cons (fun j ->
    let case_rule_context' = common_context' @ cons_real_contexts'.(j) in
    let case_rule_left' = Dedukti.apps match_function_applied' (cons_ind_real_args'.(j) @ [cons_applieds'.(j)]) in
    let case_rule_right' = Dedukti.apply_context cases'.(j) cons_real_contexts'.(j) in
    (case_rule_context', case_rule_left', case_rule_right')) in
  List.iter (Dedukti.print info.out) (List.map Dedukti.rewrite (Array.to_list case_rules))

