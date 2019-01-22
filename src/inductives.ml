open Debug
open Translator
open Info

open Declarations

(** Return a map of template parameters and a sort for given declaration. *)
let dest_ind_body (ind_body:Declarations.one_inductive_body) =
  let arity_context = ind_body.mind_arity_ctxt in
  let arity         = ind_body.mind_arity in
  match arity with
  | RegularArity ria -> ([], ria.mind_sort)
  | TemplateArity ta  -> begin
      debug "Template params levels:";
      List.iter (function   None   -> debug "None"
                          | Some u -> debug "%a" pp_coq_level u) ta.template_param_levels;
      debug "Template level: %a" pp_coq_univ ta.template_level;
      debug "Arity context: %a"  pp_coq_ctxt arity_context;
      Tsorts.translate_template_params ta.template_param_levels,
      Term.Type ta.template_level
    end

let dest_ind_univ universes =
  match universes with
  | Monomorphic_ind univ_ctxt -> Univ.Instance.empty, Univ.Constraint.empty
  | Polymorphic_ind univ_ctxt ->
    let uctxt = Univ.AUContext.repr univ_ctxt in
    Univ.UContext.instance uctxt,
    Univ.UContext.constraints uctxt
  | Cumulative_ind _ -> Error.not_supported "Mutual Cumulative inductive types"



(** An inductive definition is organised into:
    - [mutual_inductive_body] : a block of (co)inductive type definitions,
      containing a context of common parameter and list of [inductive_body]
    - [inductive_body] : a single inductive type definition,
      containing a name, an arity, and a list of constructor names and types **)

(** Translate the i-th inductive type in [mind_body]. *)
let translate_inductive info env label mind_body i =
  (* An inductive definition is organised into:
     - [mutual_inductive_body] : a block of (co)inductive type definitions,
       containing a context of common parameter and list of [inductive_body]
     - [inductive_body] : a single inductive type definition, containing a name,
       an arity, and a list of constructor names and types *)
  
  (* Body of the current inductive type *)
  let ind_body = mind_body.mind_packets.(i) in
  
  let name          = ind_body.mind_typename in
  let arity_context = ind_body.mind_arity_ctxt in
  
  let name' = Cname.translate_element_name info env (Names.Label.of_id name) in
  debug "--- %s ---" name';
  debug "%a" pp_coq_ctxt arity_context;

  let poly_inst, poly_cstr = dest_ind_univ mind_body.mind_universes in
  let (template_map, arity_sort) = dest_ind_body ind_body in
  let template_params = List.map snd template_map in
  let univ_poly_params = Tsorts.translate_univ_poly_params poly_inst in
  let poly_cstr        = Tsorts.translate_univ_poly_constraints poly_cstr in
  let uenv = Info.make template_map (List.length univ_poly_params) poly_cstr  in
  
  debug "Translate the regular inductive type.";
  (* I : ||p1 : P1 -> ... -> pr : Pr -> x1 : A1 -> ... -> xn : An -> s|| *)
  let arity = Term.it_mkProd_or_LetIn (Constr.mkSort arity_sort) arity_context in
  let arity' = Terms.translate_types info env uenv arity in
  let arity' = Tsorts.add_sort_params template_params arity' in
  let arity' = Tsorts.add_sort_params univ_poly_params arity' in
  debug "Arity sort: %a" pp_coq_type (Constr.mkSort arity_sort);
  debug "Arity: %a" pp_coq_type arity;
  debug "Arity': %a" Dedukti.pp_term arity';
  Dedukti.print info.fmt (Dedukti.declaration false name' arity')



(** Translate the constructors of the i-th inductive type in [mind_body].
    cj : ( s1:Sort -> |p1| : Type(s1)   |   |p1| : ||P1||             ) ->
         ( s2:Sort -> |p2| : Type(s2)   |   |p1| : ||P1||(s1)         ) ->
         ( s3:Sort -> |p3| : Type(s3)   |   |p1| : ||P1||(s1,s2)      ) ->
         ... ->
         ( sr:Sort -> |pr| : Type(sr)   |   |pr| : ||Pr||(s1,...sr-1) ) ->
         yj1  : B1(s1,...,sr) ->
         ... ->
         yjkj : Bjkj(s1,...,sr) ->
         I [s1] ... [sr]  p1 ... pr  yj1 ... yjkj
*)
let translate_constructors info env label mind_body i =
  (* Body of the current inductive type *)
  let ind_body = mind_body.mind_packets.(i) in
  
  let univ_instance =
    match mind_body.mind_universes with
    | Monomorphic_ind univ_ctxt ->
      Univ.UContext.instance (Univ.ContextSet.to_context univ_ctxt)
    | Polymorphic_ind univ_ctxt ->
      Univ.AUContext.instance univ_ctxt (* Univ.abstract_universe_context *)
    | Cumulative_ind _ -> Error.not_supported "Mutual Cumulative inductive types" in
  
  (* Compute universe parameters names and corresponding local environnement *)
  let poly_inst, poly_cstr = dest_ind_univ mind_body.mind_universes in
  let (template_map, arity_sort) = dest_ind_body ind_body in
  let template_params = List.map snd template_map in
  let univ_poly_params = Tsorts.translate_univ_poly_params poly_inst in
  let poly_cstr        = Tsorts.translate_univ_poly_constraints poly_cstr in
  let uenv = Info.make template_map (List.length univ_poly_params) poly_cstr  in
  
  let mind = Names.MutInd.make3 info.module_path Names.DirPath.empty label in

  let ind_subst = Inductive.ind_subst mind mind_body univ_instance in
  
  (* Number of constructors in the current type *)
  let n_cons = Array.length ind_body.mind_consnames in
  
  for j = 0 to n_cons - 1 do
    let cons_name = ind_body.mind_consnames.(j) in
    let cons_type = ind_body.mind_user_lc.(j) in
    debug "Cons_type: %a" pp_coq_type cons_type;
    
    
    (* Substitute the inductive types as specified in the Coq code. *)
    let cons_type = Vars.substl ind_subst cons_type in
    debug "Cons_type: %a" pp_coq_type cons_type;
    
    let cons_name = Cname.translate_element_name info env (Names.Label.of_id cons_name) in
    let cons_type = Terms.translate_types info env uenv cons_type in
    let template_type = Tsorts.add_sort_params template_params cons_type in
    let univ_type     = Tsorts.add_sort_params univ_poly_params template_type in
    debug "Cons_type: %a" Dedukti.pp_term univ_type;
    
    Dedukti.print info.fmt (Dedukti.declaration false cons_name univ_type);
  done
  

(** Translate the match function for the i-th inductive type in [mind_body].

    match_I :
    [ s1:Sort -> ] -> p1 : ||P1||(s1)        ->
    [ s2:Sort -> ] -> p2 : ||P2||(s1,s2)     ->
    ... ->
    [ sr:Sort -> ] -> pr : ||Pr||(s1,...,sr) ->
    
    s : Sort ->
    P : (|x1| : ||A1|| -> ... -> |xn| : ||An|| ->
            ||I [s1] p1 ... [sr] pr x1 ... xn|| ->
            type s) ->
    
    case_c1 : (|y11| : ||B11|| -> ... -> |y1k1| : ||B1k1|| ->
               term s (P |u11| ... |u1n| (|c1 [s1] p1 ... [sr] pr y11 ... y1k1|))) -> ...
    ... ->
    case_cj : (|yj1| : ||Bj1|| -> ... -> |yjkj| : ||Bjkj|| ->
               term s (P |uj1| ... |ujn| (|c1 [s1] p1 ... [sr] pr yj1 ... yjkj|))) -> ...
    
    |x1| : ||A1|| -> ... -> |xn| : ||An|| ->
    x : ||I [s1] p1 ... [sr] pr x1 ... xn|| ->
    term s (P |x1| ... |xn| x)

*)
let translate_match info env label mind_body i =
  (* Body of the current inductive type *)
  let ind_body = mind_body.mind_packets.(i) in
  
  (* Number of mutual inductive types *)
  let n_types = mind_body.mind_ntypes in
  
  (* Number of parameters common to all definitions *)
  let n_params = mind_body.mind_nparams in
  
  (* Constructor names *)
  let cons_names = ind_body.mind_consnames in
  
  (* Number of constructors in the current type *)
  let n_cons = Array.length cons_names in

  (* Instance (array of universe levels) corresponding to the mutually inductive
     universe parameters  *)
  let univ_instance =
    match mind_body.mind_universes with
    | Monomorphic_ind univ_ctxt ->
      Univ.UContext.instance (Univ.ContextSet.to_context univ_ctxt)
    | Polymorphic_ind univ_ctxt ->
      Univ.AUContext.instance univ_ctxt
    | Cumulative_ind _ -> Error.not_supported "Mutual Cumulative inductive types" in
  
  (* Compute universe parameters names and corresponding local environnement *)
  let poly_inst, poly_cstr = dest_ind_univ mind_body.mind_universes in
  let (template_map, arity_sort) = dest_ind_body ind_body in
  let template_params = List.map snd template_map in
  let univ_poly_params = Tsorts.translate_univ_poly_params poly_inst in
  let poly_cstr        = Tsorts.translate_univ_poly_constraints poly_cstr in
  let uenv = Info.make template_map (List.length univ_poly_params) poly_cstr  in

  let mind = Names.MutInd.make3 info.module_path Names.DirPath.empty label in
  
  let ind_terms = Array.init n_types (fun i -> Constr.mkIndU((mind, i), univ_instance)) in
  let ind_subst = Inductive.ind_subst mind mind_body univ_instance in
  
  (* Constructor names start from 1. *)
  let cons_terms = Array.init n_cons
      (fun j -> Constr.mkConstructU(((mind, i), j + 1), univ_instance)) in
  
  let indtype_name = ind_body.mind_typename in
  let match_function_name' = Cname.translate_identifier (Cname.match_function indtype_name) in
  let match_function_var'  = Dedukti.var match_function_name' in
  debug "###  %s" match_function_name';
  
  let arity_context = ind_body.mind_arity_ctxt in
  
  (* Use the normalized types in the rest. *)
  let cons_types = Array.map (Vars.substl ind_subst) ind_body.mind_nf_lc in
  
  (* Translate the match function. *)
  (* match_I :
       s1 : Sort -> ... -> sr : Sort ->
       p1 : ||P1|| -> ... -> pr : ||Pr|| ->
       
       s : Sort ->
       P : (x1 : ||A1|| -> ... -> xn : ||An|| ->
            ||I s1 ... sr p1 ... pr x1 ... xn|| ->
            type s) ->
       
       case_c1 : (|y11| : ||B11|| -> ... -> |y1k1| : ||B1k1|| ->
                  term s (P |u11| ... |u1n| (|c1 s1 ... sr p1 ... pr y11 ... y1k1|))) -> ...
       ... ->
       case_cj : (|yj1| : ||Bj1|| -> ... -> |yjkj| : ||Bjkj|| ->
                  term s (P |uj1| ... |ujn| (|c1 s1 ... sr p1 ... pr yj1 ... yjkj|))) -> ...
                  
       |x1| : ||A1|| -> ... -> |xn| : ||An|| ->
       x : ||I s1 ... sr p1 ... pr x1 ... xn|| ->
       Term s (P |x1| ... |xn| x)
   *)
  let params_context = mind_body.mind_params_ctxt in
  let arity_real_context, _ = Utils.list_chop ind_body.mind_nrealdecls arity_context in
  let ind_applied = Terms.apply_rel_context ind_terms.(i) (arity_real_context @ params_context) in
  let cons_context_types = Array.map Term.decompose_prod_assum cons_types in
  let cons_contexts = Array.map fst cons_context_types in
  let cons_types    = Array.map snd cons_context_types in
  let cons_real_contexts = Array.init n_cons (fun j ->
    fst (Utils.list_chop ind_body.mind_consnrealdecls.(j) cons_contexts.(j))) in 
  let cons_ind_args = Array.map (fun a -> snd (Inductive.find_inductive env a)) cons_types in
  let cons_ind_real_args = Array.init n_cons (fun j ->
    snd (Utils.list_chop n_params cons_ind_args.(j))) in
  let cons_applieds = Array.init n_cons (fun j ->
      Terms.apply_rel_context cons_terms.(j) (cons_real_contexts.(j) @ params_context))  in
  
  if n_cons > 0 then debug "Test: %a" pp_coq_term cons_applieds.(0);
  let params_env, params_context' =
    Terms.translate_rel_context info (Global.env ()) uenv params_context in
  
  (* Create a fresh variable s and add it to the environment *)
  let return_sort_name = Cname.fresh_of_string info params_env "s" in
  let return_sort_name' = Cname.translate_identifier return_sort_name in
  let return_sort' = Translator.LocalNamed return_sort_name' in
  let params_env = Cname.push_identifier return_sort_name params_env in
  
  (* Create a fresh variable P and add it to the environment *)
  let return_type_name = Cname.fresh_of_string info params_env "P" in
  let return_type_name' = Cname.translate_identifier return_type_name in
  let return_type_var' = Dedukti.var return_type_name' in
  let params_env = Cname.push_identifier return_type_name params_env in
  
  (* Create a fresh variables for each constructors of the inductive type
     and add them to the environment (why ?) *)
  let params_env, case_names' =
    Array.fold_left
      (fun (params_env, case_names') cons_name ->
         let case_name = Cname.fresh_identifier info params_env ~prefix:"case" cons_name in
         let case_name' = Cname.translate_identifier case_name in
         let params_env = Cname.push_identifier case_name params_env in
         (params_env, case_name' :: case_names'))
      (params_env, [])
      cons_names in
  let case_names' = Array.of_list (List.rev case_names') in
  
  let arity_real_env, arity_real_context' =
    Terms.translate_rel_context info params_env uenv arity_real_context in
  let ind_applied' = Terms.translate_types info arity_real_env uenv ind_applied in
  
  debug "ind_applied: %a / %a" pp_coq_term ind_applied Dedukti.pp_term ind_applied';
  
  (* Create a fresh variable x and add it to the environment *)
  let matched_name = Cname.fresh_of_string info arity_real_env "x" in
  let matched_name' = Cname.translate_identifier matched_name in
  let matched_var' = Dedukti.var matched_name' in
  let params_env = Cname.push_identifier matched_name params_env in
  
  let cases' = Array.map Dedukti.var case_names' in
  let params' = List.map Dedukti.var (fst (List.split params_context')) in
  
  let cons_real_env_contexts' =
    Array.map
      (Terms.translate_rel_context info params_env uenv)
      cons_real_contexts in
  
  let cons_real_envs      = Array.map fst cons_real_env_contexts' in
  let cons_real_contexts' = Array.map snd cons_real_env_contexts' in
  
  debug "Env: %a" pp_coq_env params_env;
  Array.iter (debug "%a" pp_coq_ctxt) cons_real_contexts;
  Array.iter (debug "%a" pp_coq_env ) cons_real_envs;
  
  let cons_ind_real_args' =
    Array.mapi
      (fun j -> Terms.translate_args info cons_real_envs.(j) uenv)
      cons_ind_real_args in
  
  let cons_applieds' =
    Array.mapi
      (fun j -> Terms.translate_constr info cons_real_envs.(j) uenv)
      cons_applieds in
  
  (* Combine the above. *)
  let case_types' =
    Array.init n_cons
      (fun j ->
         Dedukti.pies cons_real_contexts'.(j)
           (T.coq_term return_sort'
              (Dedukti.apps return_type_var'
                 (cons_ind_real_args'.(j) @ [cons_applieds'.(j)])))) in

  if n_cons > 0 then debug "Test: %a" Dedukti.pp_term cons_applieds'.(0);
  
  let cases_context' = Array.to_list (Array.init n_cons (fun j -> (case_names'.(j), case_types'.(j)))) in
  let template_poly_context' =
    if Encoding.is_templ_polymorphism_on ()
    then List.map (fun x -> (x, T.coq_Sort())) template_params else [] in
  let univ_poly_context' =
    if Encoding.is_polymorphism_on ()
    then List.map (fun x -> (x, T.coq_Sort())) univ_poly_params else [] in
  let common_context' =
    (return_sort_name', T.coq_Sort()) ::
    params_context' @   (* Shouldn't this be first ? *)
    (return_type_name',
     Dedukti.pies
       arity_real_context'
       (Dedukti.arr ind_applied' (T.coq_U return_sort'))
    ) ::
    cases_context' in
  let match_function_context' =
    univ_poly_context' @
    template_poly_context' @
    common_context' @
    arity_real_context' @
    [matched_name', ind_applied'] in
  let match_function_type' = T.coq_term return_sort'
    (Dedukti.app
       (Dedukti.apply_context return_type_var' arity_real_context')
       matched_var') in

  (* Printing out the match definition. *)
  Dedukti.print info.fmt
    (Dedukti.declaration true match_function_name'
       (Dedukti.pies match_function_context' match_function_type'));

  let match_function_applied' =
    Dedukti.apps match_function_var'
      (List.map Dedukti.var univ_poly_params @
       List.map Dedukti.var template_params @
       Dedukti.var return_sort_name' ::
       params' @
       return_type_var' ::
       Array.to_list cases') in
  let case_rules = Array.init n_cons
      (fun j ->
         let case_rule_context' =
           univ_poly_context' @ template_poly_context' @ common_context' @ cons_real_contexts'.(j) in
         let case_rule_left' =
           Dedukti.apps
             match_function_applied'
             (cons_ind_real_args'.(j) @ [cons_applieds'.(j)]) in
         let case_rule_right' = Dedukti.apply_context cases'.(j) cons_real_contexts'.(j) in
         (case_rule_context', case_rule_left', case_rule_right')
      ) in
  
  (* Printing out the match rewrite rules. *)
  List.iter (Dedukti.print info.fmt) (List.map Dedukti.rewrite (Array.to_list case_rules))
