open Debug
open Translator
open Info

open Declarations

type ind_infos =
  {
    mind_body : Declarations.mutual_inductive_body;

    mind_univs : Declarations.abstract_inductive_universes;
    
    (* Number of mutual inductive types *)
    nb_mutind : int;
    
    (* Index of the current inductive type *)
    index : int;
    
    (* Body of the current inductive type *)
    body : Declarations.one_inductive_body;
    
    (* Number of parameters common to all definitions *)
    n_params : int;
    
    (* Number of constructors in the current type *)
    n_cons : int;
    
    (* Constructor names array *)
    cons_names : Names.Id.t array;
    
    
    typename : Names.Id.t;
    arity_context : Context.Rel.t;
    arity : Declarations.inductive_arity;
    template_map : (string * Dedukti.var) list;
    arity_sort : Sorts.t;
  }

let get_infos mind_body index =
  let nb_mutind  = mind_body.mind_ntypes in
  let body       = mind_body.mind_packets.(index) in
  let n_params   = mind_body.mind_nparams in
  let mind_univs = mind_body.mind_universes in
  let typename      = body.mind_typename in
  let arity_context = body.mind_arity_ctxt in
  let arity         = body.mind_arity in
  let template_map, arity_sort =
      match arity with
        | RegularArity ria -> ([], ria.mind_sort)
        | TemplateArity ta -> begin
            debug "Template params levels:";
            List.iter (debug "%a" (pp_option "None" pp_coq_level)) ta.template_param_levels;
            debug "Template level: %a" pp_coq_univ ta.template_level;
            debug "Arity context: %a"  pp_coq_ctxt arity_context;
            Tsorts.translate_template_params ta.template_param_levels,
            Term.Type ta.template_level
          end
  in
  let cons_names = body.mind_consnames in
  let n_cons = Array.length cons_names in
  {
    mind_body;
    mind_univs;
    nb_mutind;
    index;
    body;
    typename;
    arity_context;
    arity;
    template_map;
    arity_sort;
    n_params;
    n_cons;
    cons_names;
  }

(** Return a map of template parameters and a sort for given declaration. *)
let dest_ind_body (ind_body:Declarations.one_inductive_body) =
  let arity_context = ind_body.mind_arity_ctxt in
  let arity         = ind_body.mind_arity in
  match arity with
  | RegularArity ria -> ([], ria.mind_sort)
  | TemplateArity ta -> begin
      debug "Template params levels:";
      List.iter (debug "%a" (pp_option "None" pp_coq_level)) ta.template_param_levels;
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


let is_template_parameter uenv (decl:Context.Rel.Declaration.t) = match decl with
  | Context.Rel.Declaration.LocalDef _ -> None
  | Context.Rel.Declaration.LocalAssum (name, tp) ->
    let rec aux acc t = match Constr.kind t with
      | Term.Prod(x,a,t) -> aux (Cname.translate_name x::acc) t
      | Term.Sort (Sorts.Type u) ->
        (match Univ.Universe.level u with
         | None -> None
         | Some lvl ->
           let lvl_name = Univ.Level.to_string lvl in
           if Info.is_template_polymorphic uenv lvl_name
           then Some (name, List.rev acc, lvl_name)
           else None
        )
      | _ -> None
    in
    aux [] tp


(** An inductive definition is organised into:
    - [mutual_inductive_body] : a block of (co)inductive type definitions,
      containing a context of common parameter and list of [inductive_body]
    - [inductive_body] : a single inductive type definition,
      containing a name, an arity, and a list of constructor names and types **)

(** Translate the i-th inductive type in [mind_body].
    I : s1 : Sort -> ... -> sr : Sort ->
        p1 : ||P1|| ->
        ... ->
        pr : ||Pr|| ->
        x1 : A1 -> ... -> xn : An -> s
*)
let translate_inductive info env label ind  =
  let mind_body = ind.mind_body in
  (* An inductive definition is organised into:
     - [mutual_inductive_body] : a block of (co)inductive type definitions,
       containing a context of common parameter and list of [inductive_body]
     - [inductive_body] : a single inductive type definition, containing a name,
       an arity, and a list of constructor names and types *)
  
  (* Body of the current inductive type *)
  let ind_body = ind.body in
  
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
  (*  I : s1 : Sort -> ... -> sr : Sort ->
          p1 : ||P1|| ->
          ... ->
          pr : ||Pr|| ->
          a1 : A1 -> ... -> an : An -> s
  *)
  let arity = Term.it_mkProd_or_LetIn (Constr.mkSort arity_sort) arity_context in
  debug "Arity context: %a" pp_coq_ctxt arity_context;
  let arity' = Terms.translate_types info env uenv arity in
  let arity' = Tsorts.add_sort_params template_params arity' in
  let arity' = Tsorts.add_sort_params univ_poly_params arity' in
  debug "Arity sort: %a" pp_coq_type (Constr.mkSort arity_sort);
  debug "Arity: %a" pp_coq_type arity;
  debug "Arity': %a" Dedukti.pp_term arity';
  (* Printing out the type declaration. *)
  Dedukti.print info.fmt (Dedukti.declaration false name' arity')



(** Subtyping is extended to parameters of template polymorphic inductive types. *)
let translate_inductive_subtyping info env label ind  =
  let mind_body = ind.mind_body in
  let print_param_ST_elim decl = () (*
    (* Translate the rule for lift elimination in j-th parameters template polymorphism *)
    (* I s1 ... si' ... sr
         p1  ... (x1 => ... => xk => lift _ (u si) (pj x1 ... xk)) ... pr
         a1 ...  ... an
       -->
       lift s(s1 ... si ... sr) s(s1 ... si' ... sr)
         (I s1 ... si ... sr
            p1  ... (x1 => ... => xk => pj x1 ... xk) ... pr
            a1 ... an)
    *)
    match is_template_parameter uenv decl with
    | None -> () (* When parameter in not template polymorphic: no rule *)
    | Some (consname,locals,template_var) ->
      begin
        let inductive' = Dedukti.var name' in
        let new_sort = template_var ^ "'" in
        let consname = Cname.fresh_name ~default:"_" info env consname in
        let consname' = Cname.translate_name consname in
        let applied_param =
          Dedukti.apps (Dedukti.var consname') (List.map Dedukti.var locals) in
        let lifted_param_pat = Dedukti.ulams locals
            (T.coq_pattern_lifted_from_sort (Dedukti.var new_sort) applied_param) in
        let replace_pat s =
          if not true then s else
            begin
            end
        in
        let arity_pat = List.map replace_pat arity

        in
        let pattern_match =
          Dedukti.apps inductive'
            (List.map Dedukti.var univ_poly_params @
             List.map Dedukti.var template_params @
             params_pat @
             [lifted_param_pat]) in
        let lifted_param_lhs = Dedukti.ulams locals decl in
        let lhs_match =
          Dedukti.apps match_function_var'
            (List.map Dedukti.var univ_poly_params @
             List.map Dedukti.var template_params @
             List.map (fun x -> Dedukti.var (fst x)) params_context' @
             [Dedukti.ulams local_ctxt_names (Dedukti.apps return_type_var' local_ctxt)]) in

        let context =
          univ_poly_context' @
          template_poly_context' @
          return_sort_binding ::
          params_context' @
          [return_type_binding; (new_sort_name, T.coq_Sort())] in

        (* Printing out the rule for lift elimination *)
        Dedukti.print info.fmt (Dedukti.rewrite (context, pattern_match, lhs_match))
      end *)
  in
  List.iter print_param_ST_elim mind_body.mind_params_ctxt




(** Translate the constructors of the i-th inductive type in [mind_body].
    cj : s1 : Sort -> ... -> sr : Sort ->
         |p1| : ||P1|| ->
         ... -> 
         |pn| : ||Pn|| ->
         yj1  : ||B1||(s1,...,sr) ->
         ... ->
         yjkj : ||Bjkj||(s1,...,sr) ->
         I s1 ... sr  p1 ... pr  x1(yj...)  ... xn(yj...)
*)
let translate_constructors info env label ind =
  let mind_body = ind.mind_body in
  let ind_body  = ind.body in
  
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
let translate_match info env label ind =
  let mind_body = ind.mind_body in
  (* Body of the current inductive type *)
  let ind_body = ind.body in
  
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
  (*
  let ind_terms = Array.init n_types (fun i -> Constr.mkIndU((mind, i), univ_instance)) in
  This is only used for ind_terms.(index), why build the whole array ?
  *)
  let ind_term = Constr.mkIndU((mind, ind.index), univ_instance) in
  
  let ind_subst = Inductive.ind_subst mind mind_body univ_instance in
  
  (* Constructor names start from 1. *)
  let cons_terms = Array.init n_cons
      (fun j -> Constr.mkConstructU(((mind, ind.index), j + 1), univ_instance)) in
  
  let indtype_name = ind_body.mind_typename in
  let match_function_name' = Cname.translate_identifier (Cname.match_function indtype_name) in
  let match_function_var'  = Dedukti.var match_function_name' in
  debug "###  %s" match_function_name';
  
  let arity_context = ind_body.mind_arity_ctxt in
  
  (* Use the normalized types in the rest. *)
  let cons_types = Array.map (Vars.substl ind_subst) ind_body.mind_nf_lc in
  
  (* Translate the match function. *)
  (* match_I :
     s : Sort ->
     
     s1 : Sort -> ... -> sr : Sort ->
     p1 : ||P1|| -> ... -> pr : ||Pr|| ->
     
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
  let ind_applied = Terms.apply_rel_context ind_term (arity_real_context @ params_context) in
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
  let return_sort_binding = (return_sort_name', T.coq_Sort()) in
  let return_type_type = Dedukti.pies
      arity_real_context'
      (Dedukti.arr ind_applied' (T.coq_U return_sort')) in
  let return_type_binding = (return_type_name', return_type_type) in
  let common_context' =
    return_sort_binding ::
    params_context' @   (* Shouldn't this be first ? *)
    return_type_binding ::
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
  (* match_I :
     s : Sort ->
     
     s1 : Sort -> ... -> sr : Sort ->
     p1 : ||P1|| -> ... -> pr : ||Pr|| ->
     
     P : (x1 : ||A1|| -> ... -> xn : ||An|| ->
          ||I s1 ... sr p1 ... pr x1 ... xn|| ->
          Univ s) ->
     
     case_c1 : (|y11| : ||B11|| -> ... -> |y1k1| : ||B1k1|| ->
                term s (P |u11| ... |u1n| (|c1 s1 ... sr p1 ... pr y11 ... y1k1|))) -> ...
     ... ->
     case_cj : (|yj1| : ||Bj1|| -> ... -> |yjkj| : ||Bjkj|| ->
                term s (P |uj1| ... |ujn| (|c1 s1 ... sr p1 ... pr yj1 ... yjkj|))) -> ...
     
     |x1| : ||A1|| -> ... -> |xn| : ||An|| ->
     x : ||I s1 ... sr p1 ... pr x1 ... xn|| ->
     Term s (P |x1| ... |xn| x)
   *)
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
  
  (* Printing out the match rewrite rules. *)
  (* match_I :
       s
       s1 ... sr 
       p1 ... pr
       P
       case_c1
       ...
       case_cj
       a1 .. an
       (ci s1 ... sr p1 ... pr y11 ... y1k1)
       -->
       case_ci y11 ... y1k1
  *)
  for j = 0 to n_cons-1 do
    let case_rule_context' =
      univ_poly_context' @ template_poly_context' @ common_context' @ cons_real_contexts'.(j) in
    (* Note: a1 ... an are patterns in coq (expected return type parameters)
       They should however be translated as brackets patterns. Well-typedness of
       redices matching this left-hand side ensures the terms matched to a1 ... an
       always have the expected shape.
    *)
    let brackets = List.map Dedukti.bracket cons_ind_real_args'.(j) in
    let case_rule_left' =
      Dedukti.apps
        match_function_applied'
        (brackets @ [cons_applieds'.(j)]) in
    let case_rule_right' = Dedukti.apply_context cases'.(j) cons_real_contexts'.(j) in
    let rw_rule = Dedukti.rewrite (case_rule_context', case_rule_left', case_rule_right') in
    Dedukti.print info.fmt rw_rule
  done;
  
  (* Translate the rule for lift elimination in match polymorphism *)
  (* match_I s1 ... sr p1  ... pr _ a1 ... an (x1 => ... => xn => x => lift _ (u s) (P x1 ... xn x))
     -->
     match_I s1 ... sr p1  ... pr s a1 ... an (x1 => ... => xn => x => P x1 ... xn x)
  *)
  let new_sort_name = "s'" in (* TODO: change that to ensure no conflicts *)
  let new_sort = Dedukti.var new_sort_name in
  let local_ctxt_names = List.map fst arity_real_context' @ ["x"]  in
  let local_ctxt       = List.map Dedukti.var local_ctxt_names in
  let pattern = T.coq_pattern_lifted_from_sort new_sort
      (Dedukti.apps return_type_var' local_ctxt) in
  let pattern_match =
    Dedukti.apps match_function_var'
      (List.map Dedukti.var univ_poly_params @
       List.map Dedukti.var template_params @
       Dedukti.var return_sort_name' ::
       List.map (fun x -> Dedukti.var (fst x)) params_context' @
       [Dedukti.ulams local_ctxt_names pattern]) in
  let lhs_match =
    Dedukti.apps match_function_var'
      (List.map Dedukti.var univ_poly_params @
       List.map Dedukti.var template_params @
       Dedukti.var new_sort_name ::
       List.map (fun x -> Dedukti.var (fst x)) params_context' @
       [Dedukti.ulams local_ctxt_names (Dedukti.apps return_type_var' local_ctxt)]) in
  
  let context =
    univ_poly_context' @
    template_poly_context' @
    return_sort_binding ::
    params_context' @
    [return_type_binding; (new_sort_name, T.coq_Sort())] in
  
  (* Printing out the rule for lift elimination *)
  Dedukti.print info.fmt (Dedukti.rewrite (context, pattern_match, lhs_match))
  

