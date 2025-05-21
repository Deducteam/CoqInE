open Debug
open Translator
open Info

open Declarations


type ind_infos =
  {
    (* [mutual_inductive_body] : a block of (co)inductive type definitions,
       containing a context of common parameter and list of [inductive_body] *)
    mind_body : Declarations.mutual_inductive_body;

    (* [inductive_body] : a single inductive type definition,
       containing a name, an arity, and a list of constructor names and types *)
    mind_univs : Declarations.universes;

    (* Total number of mutually inductive types *)
    nb_mutind : int;

    (* Index of the current inductive type in the block *)
    index : int;

    (* Body of the current inductive type *)
    body : Declarations.one_inductive_body;

    (* Types with parameters *)
    nf_full_types : Constr.types array;

    (* Number of parameters common to all definitions *)
    n_params : int;

    (* Number of constructors in the current type *)
    n_cons : int;

    (* Inductive type name *)
    typename : Names.Id.t;

    (* Constructor names array *)
    cons_names : Names.Id.t array;

    (* Full arity context (parameters and real arity together) *)
    arity_context : Constr.rel_context;
    (* Parameters context. Same for all mutual inductive definition. *)
    mind_params_ctxt : Constr.rel_context;
    (* Real context. Specific to each mutual inductive definition. *)
    arity_real_context : Constr.rel_context;

    arity : Declarations.inductive_arity;

    (* Local Coq levels that are template polymorphic (Named level only) *)
    template_levels : Univ.Level.t list;
    (* Names for template polymorphic coq levels *)
    template_names  : Dedukti.var  list;

    arity_sort : Sorts.t;

    (* Universe polymorphic context : instance (array of universe levels) and constraints *)
    apoly_ctxt : Univ.AUContext.t;
    poly_ctxt : Univ.UContext.t;
    poly_inst : Univ.Instance.t;
    poly_cstr : Univ.Constraint.t;

    univ_poly_names : Dedukti.var list;
    univ_poly_cstr : ( Univ.univ_constraint * (Dedukti.var * Dedukti.term * Dedukti.term) ) list;
    univ_poly_nb_params : int;
    inductive_uenv : Info.env;
    constructor_uenv : Info.env;
  }

let get_infos mind_body index =
  let nb_mutind  = mind_body.mind_ntypes in
  let body       = mind_body.mind_packets.(index) in
  let n_params   = mind_body.mind_nparams in
  let mind_univs = mind_body.mind_universes in
  let mind_templ = mind_body.mind_template in
  let mind_params_ctxt = mind_body.mind_params_ctxt in
  debug "mind_params: %a" pp_coq_ctxt mind_params_ctxt;
  let typename      = body.mind_typename in
  let arity_context = body.mind_arity_ctxt in
  let arity         = body.mind_arity in
  let cons_names    = body.mind_consnames in
  let n_cons = Array.length cons_names in

  debug "--- Getting infos for inductive : %a ---" pp_coq_id typename;

  let f (nf_ctxt,nf_type) = Term.it_mkProd_or_LetIn nf_type nf_ctxt in
  let nf_full_types = Array.map f body.mind_nf_lc in

  let arity_real_context, _ = Utils.list_chop body.mind_nrealdecls arity_context in

  (* Compute a map of template parameters and a sort for given declaration. *)
  let (template_levels, template_names), arity_sort =
    match arity, mind_templ with
    | RegularArity ria, None -> ([],[]), ria.mind_sort
    | TemplateArity ta, Some targs ->
      begin
        debug "Template params levels:";
        List.iter (debug "%a" (pp_option "None" pp_coq_level)) targs.template_param_levels;
        debug "Template level: %a" pp_coq_univ ta.template_level;
        debug "Arity context: %a"  pp_coq_ctxt arity_context;
        Tsorts.translate_template_params targs.template_param_levels,
        Sorts.sort_of_univ ta.template_level
      end
    | _ -> assert false (* mixed template/non-template *)
  in

  (* Compute universe polymorphic instance and associated constraints *)
  let apoly_ctxt, poly_ctxt, poly_inst, poly_cstr =
    match mind_univs with
    | Monomorphic univ_ctxt ->
      Univ.AUContext.empty,
      Univ.UContext.empty,
      Univ.Instance.empty,
      Univ.Constraint.empty
      (*
      Univ.UContext.instance (Univ.ContextSet.to_context univ_ctxt), snd univ_ctxt
      *)
    | Polymorphic apoly_ctxt ->
      let poly_ctxt = Univ.AUContext.repr apoly_ctxt in
      apoly_ctxt,
      poly_ctxt,
      Univ.UContext.instance    poly_ctxt,
      Univ.UContext.constraints poly_ctxt
  in

  let univ_poly_names = Tsorts.translate_univ_poly_params poly_inst in
  let univ_poly_cstr   = Tsorts.translate_univ_poly_constraints poly_cstr in
  let univ_poly_nb_params = List.length univ_poly_names in
  let univ_poly_env f =
    Info.make
      template_levels
      (List.map f template_names)
      apoly_ctxt
      univ_poly_nb_params
      univ_poly_cstr  in
  let inductive_uenv =
    univ_poly_env (fun x -> Translator.NamedSort x) in
  let constructor_uenv =
    univ_poly_env
      (if Tsorts.template_constructor_upoly ()
       then fun x -> Translator.NamedSort x
       else fun x -> Type (Translator.NamedLevel x)) in

  {
    mind_body;
    mind_univs;
    mind_params_ctxt;
    nb_mutind;
    index;
    body;
    nf_full_types;
    typename;
    arity_context;
    arity_real_context;
    arity;
    template_levels;
    template_names;
    arity_sort;
    n_params;
    n_cons;
    cons_names;
    apoly_ctxt;
    poly_ctxt;
    poly_inst;
    poly_cstr;
    univ_poly_names;
    univ_poly_cstr;
    univ_poly_nb_params;
    inductive_uenv;
    constructor_uenv;
  }


let is_template_parameter ind = function
  | Context.Rel.Declaration.LocalDef _ -> None
  | Context.Rel.Declaration.LocalAssum (x, tp) ->
    let rec aux acc t = match Constr.kind t with
      | Constr.Prod(binda,a,t) ->
        let name = Context.binder_name binda in
        aux (Cname.translate_name name::acc) t
      | Constr.Sort (Sorts.Type u) ->
        (match Univ.Universe.level u with
         | None -> None
         | Some lvl ->
           try
             let _ = Info.translate_template_arg ind.inductive_uenv lvl in
             let name = Context.binder_name x in
             Some (name, List.rev acc, lvl, Tsorts.translate_template_name lvl)
           with Not_found -> None
        )
      | _ -> None
    in
    aux [] tp

(** Extract the list of LocalAssums in a context in reverse order. *)
let extract_rel_context info env =
  let rec aux env acc = function
    | [] -> (env, acc)
    | decl :: l ->
      match Context.Rel.Declaration.to_tuple decl with
      | (x, None, a) ->
        let decl = Context.Rel.Declaration.LocalAssum(x, a) in
        let name = Context.binder_name x in
        let new_env = Environ.push_rel decl env in
        aux new_env (name::acc) l
      | (x, Some u, a) ->
        let decl = Context.Rel.Declaration.LocalDef(x, u, a) in
        let new_env = Environ.push_rel decl env in
        aux new_env acc l
  in
  aux env []

(* Inductive can either be
      "True" Polymorphic (Level quantified with constraints)
      Template Polymorphic (Sort quantified, no constraints)
  I :     s1 : Sort -> ... -> sk : Sort ->
       <
          s1 : Nat -> ... -> sk : Nat -> c1 : eps |C1| -> ... -> cc : eps |Cc| ->
       p1 : ||P1|| ->
       ... ->
       pr : ||Pr|| ->
       x1 : A1 -> ... -> xn : An -> s
*)
let translate_inductive info env label ind  =
  (* An inductive definition is organised into:
     - [mutual_inductive_body] : a block of (co)inductive type definitions,
       containing a context of common parameter and list of [inductive_body]
     - [inductive_body] : a single inductive type definition, containing a name,
       an arity, and a list of constructor names and types *)

  (* Body of the current inductive type *)
  let name' = Cname.translate_element_name info env (Names.Label.of_id ind.typename) in
  debug "--- Translating inductive type: %s ---" name';

  (*  I :    s1 : Sort -> ... -> sk : Sort ->
          <
             s1 : Nat -> ... -> sk : Nat -> c1 : eps |C1| -> ... -> cc : eps |Cc| ->
          p1 : ||P1|| ->
          ... ->
          pr : ||Pr|| ->
          a1 : A1 -> ... -> an : An -> s
  *)
  let arity = Term.it_mkProd_or_LetIn (Constr.mkSort ind.arity_sort) ind.arity_context in
  debug "Arity: %a" pp_coq_term arity;
  let poly_env = Environ.push_context ind.poly_ctxt env in
  let arity' = Terms.translate_types info poly_env ind.inductive_uenv arity in
  let arity' = Tsorts.add_inductive_params ind.template_names
      ind.univ_poly_names ind.univ_poly_cstr arity' in
  (* Printing out the type declaration. *)
  debug "ok";
  let definable =
    Encoding.is_templ_polymorphism_ind_code() ||
    List.exists (fun p -> Option.has_some (is_template_parameter ind p)) ind.mind_params_ctxt
  in
  Dedukti.print info.fmt (Dedukti.declaration definable name' arity')

(* Template polymorphic inductives types are "sort-irrelevant" in some of their arguments
   This means that instances where the arguments are lifted should be
   convertible with instances of lower sorts with non-lifted arguments.

   I s1 ... si' ... sk
     p1
     ...
     (x1 => ... => xl => lift (u si) _ (pj x1 ... xl))
     ...
     pr
     a1 ...  ... an
   -->
   lift l[s1 ... si ..., sk]  l[s1 ... si' ... sk]
     (I s1 ... si ... sk
        p1  ... (x1 => ... => xl => pj x1 ... xl) ... pr
        a1 ... an)

   This trick only works when we can guarantee minimal subtyping.
   Otherwise, we use the other trick below.

   code (univ _)
     (I s1 ... sk
        p1
        ...
        pr
        a1 ...  ... an)
   -->
   I'
     p1
     ...
     (x1 => ... => xni => code si (pi x1 ... xni))
     ...
     pr
     a1 ... an
  with I' a "private version" of I
  I' :
    Arity_1 ->                      (; "Normal" parameters are left the same ;)
    ...
    (Ai -> ... -> Ani -> Code) ->   (; Polymorphic parameters are now codes ;)
    ...
    Arity_r ->
    Code                            (; Return type is a code as well ;)
*)
let translate_template_inductive_subtyping info env label ind =
  if not (Encoding.is_templ_polymorphism_on ()) then () else
  match ind.arity with
  | RegularArity _ -> ()    (* Ignore non-template inductives *)
  | TemplateArity _ ->
    assert (ind.univ_poly_names = []);
    let name' = Cname.translate_element_name info env (Names.Label.of_id ind.typename) in
    debug "--- Translating sort-irrelevance in template inductive type: %s ---" name';
    let inductive' = Dedukti.var name' in
    let _, arity_ctxt_names = extract_rel_context info env ind.arity_context in
    let translate_name name =
      let name = Cname.fresh_name ~default:"_" info env name in
      Cname.translate_name name in

    if not (Encoding.is_templ_polymorphism_ind_code ())
    then
begin
  (* Translate the rule for lift elimination in j-th parameters if polymorphic *)
  let print_param_ST_elim decl =
    match is_template_parameter ind decl with
    | None -> () (* When parameter in not template polymorphic: no rule *)
    | Some (tparam_name,locals,level,level_name') ->
      begin
        debug "Printing lift extraction for param decl %a of level %a"
          pp_coq_decl decl pp_coq_level level;
        let new_level_name' = level_name' ^ "'" in
        let tparam_name' = Cname.fresh_name ~default:"_" info env tparam_name in
        let tparam_name' = Cname.translate_name tparam_name' in
        let applied_param =  (* pj x1 ... xl *)
          Dedukti.apps (Dedukti.var tparam_name') (List.map Dedukti.var locals) in
        let lifted_param_pat = Dedukti.ulams locals
            (T.coq_pattern_lifted_from_sort (Dedukti.var new_level_name') applied_param) in
        let translate_name_with_lifted name =
          if name = tparam_name
          then lifted_param_pat
          else Dedukti.var (translate_name name)
        in
        let translate_replace_sort s =
          Dedukti.var (if s = level_name' then new_level_name' else s)
        in
        let lhs =
          Dedukti.apps inductive'
            (List.map Dedukti.var ind.template_names @
             List.map translate_name_with_lifted arity_ctxt_names
            ) in
        let origin_sort =
          Tsorts.translate_sort ind.inductive_uenv ind.arity_sort in
        let new_uenv = (* Env remapping sort to new_level_name *)
          Info.replace_template_name ind.inductive_uenv level
            (Translator.NamedSort new_level_name') in
        let small_sort =
          Tsorts.translate_sort new_uenv ind.arity_sort in
        let rhs =
          Dedukti.apps inductive'
            (List.map translate_replace_sort ind.template_names @
             List.map (fun x -> Dedukti.var (translate_name x)) arity_ctxt_names
            ) in
        let rhs = T.coq_pcast
            (Translator.Succ (small_sort , 1))
            (Translator.Succ (origin_sort, 1))
            (T.coq_sort small_sort)
            (T.coq_sort origin_sort) rhs in
        let context =
          new_level_name' ::
          ind.template_names @
          List.map translate_name arity_ctxt_names in
        (* Printing out the rule for lift elimination *)
        Dedukti.print info.fmt (Dedukti.rewrite (context, lhs, rhs))
      end
  in
  List.iter print_param_ST_elim ind.mind_params_ctxt
end

    else
begin
  let lhs =
    T.coq_coded Dedukti.wildcard
      ( Dedukti.apps inductive'
          (List.map Dedukti.var ind.template_names @
           List.map (fun x -> Dedukti.var (translate_name x)) arity_ctxt_names
          )
      ) in
  let priv_inductive' = name' ^ "'" in
  let priv_levels = List.map (fun _ -> Translator.SInf) ind.template_levels in
  let priv_uenv =
    Info.make ind.template_levels priv_levels ind.apoly_ctxt 0 [] in
  let arity = Term.it_mkProd_or_LetIn (Constr.mkSort ind.arity_sort) ind.arity_context in
  let arity' = Terms.translate_types info env priv_uenv arity in
  let rec replace_return_sort = function
    | Dedukti.Pie (decl, b) -> Dedukti.pie decl (replace_return_sort b)
    | Dedukti.App _ -> T.coq_U Translator.SInf
    | _ -> assert false
  in
  let arity' = replace_return_sort arity' in
  Dedukti.print info.fmt (Dedukti.declaration false priv_inductive' arity');
  let rec process_decl acc = function
    | [] -> acc
    | decl :: tl ->
      process_decl
        (match is_template_parameter ind decl with
         | Some (tparam_name,locals,level,level_name') ->
           (tparam_name, (locals,level,level_name')) :: acc
         | None -> acc ) tl
  in
  let templ_params = process_decl [] ind.mind_params_ctxt in
  (* Build the assoc list of [  (param_name, param_infos) :: ... ]  *)
  let translate_replace_sort param =
    let param' = translate_name param in
    let vp = Dedukti.var param' in
    try
      let locals,level,level_name' = List.assoc param templ_params in
      (*   x1 => ... => xl => code s (pj x1 ... xl)   *)
      Dedukti.ulams
        locals
        (T.coq_coded
           (T.coq_universe (Tsorts.level_as_universe ind.inductive_uenv level))
           (Dedukti.apps vp (List.map Dedukti.var locals)))
    with Not_found -> vp
  in
  let rhs =
    Dedukti.apps (Dedukti.var priv_inductive')
      (List.map translate_replace_sort arity_ctxt_names) in
  let context =
    ind.template_names @
    List.map translate_name arity_ctxt_names in
  (* Printing out the rule for lift elimination *)
  Dedukti.print info.fmt (Dedukti.rewrite (context, lhs, rhs))
end

(* Prints all template global universes.
     s1 : Lvl
     ...
     sk : Lvl
   These levels are needed for the monomorphic template constructors.
*)
let translate_template_inductive_levels info env label ind =
  match ind.mind_body.mind_template with
  | Some targs when not (Tsorts.template_constructor_upoly ()) ->
    List.iter (Dedukti.print info.fmt)
      (Tsorts.translate_template_global_level_decl targs.template_param_levels)
  | _ -> ()


(* cj : |p1| : ||P1|| ->
        ... ->
        |pr| : ||Pr|| ->
        y1  : ||Bj1||(s1,...,sr) ->
        ... ->
        ykj : ||Bjkj||(s1,...,sr) ->
        I s1 ... sk  p1 ... pr  aj1(y1...ykj)  ... ajn(y1...ykj)
*)
let translate_constructors info env label ind =
  let mind = Names.MutInd.make2 info.module_path label in
  (* Substitute the inductive types as specified in the Coq code. *)
  let ind_subst = Inductive.ind_subst mind ind.mind_body ind.poly_inst in
  for j = 0 to ind.n_cons - 1 do
    let cons_name = ind.body.mind_consnames.(j) in
    let nf_full_types = Vars.substl ind_subst ind.nf_full_types.(j) in
    debug "Translating inductive constructor: %a" pp_coq_id cons_name;
    debug "Cons_full_type: %a" pp_coq_type nf_full_types;

    let cons_name' = Cname.translate_element_name info env (Names.Label.of_id cons_name) in
    let poly_env = Environ.push_context ind.poly_ctxt env in
    let cons_type' = Terms.translate_types info poly_env ind.constructor_uenv nf_full_types in
    let cons_type' = Tsorts.add_constructor_params ind.template_names
        ind.univ_poly_names ind.univ_poly_cstr cons_type' in
    debug "Cons_type: %a" Dedukti.pp_term cons_type';
    Dedukti.print info.fmt (Dedukti.declaration true cons_name' cons_type');
  done


(* cl s1 ... si' ... sk
      p1 ... (x1 => ... => xl => lift (u si) _ (pj x1 ... xl)) ... pr
   -->
   cl s1 ... si ... sk
      p1 ... (x1 => ... => xl => pj x1 ... xl) ... pr
*)
let translate_template_constructors_subtyping info env label ind =
  match ind.arity with
  | TemplateArity _ when Tsorts.template_constructor_upoly () ->
begin
  let env = Environ.push_context ind.poly_ctxt env in
  for consid = 0 to ind.n_cons - 1 do
    let cons_name = ind.body.mind_consnames.(consid) in
    let cons_name' = Cname.translate_element_name info env (Names.Label.of_id cons_name) in
    let cons' = Dedukti.var cons_name' in
    let _, arity_ctxt_names = extract_rel_context info env ind.mind_params_ctxt in
    let translate_name name =
      let name = Cname.fresh_name ~default:"_" info env name in
      Cname.translate_name name in
    let arity_ctxt_names' = List.map translate_name arity_ctxt_names in
    (* Translate the rule for lift elimination in j-th parameters if template polymorphic *)
    let print_param_ST_elim decl =
      match is_template_parameter ind decl with
      | None -> () (* When parameter in not template polymorphic: no rule *)
      | Some (tparam_name,locals,level,level_name') ->
        begin
          let new_level_name' = level_name' ^ "'" in
          let tparam_name' = Cname.fresh_name ~default:"_" info env tparam_name in
          let tparam_name' = Cname.translate_name tparam_name' in
          let applied_param =  (* pj x1 ... xl *)
          Dedukti.apps (Dedukti.var tparam_name') (List.map Dedukti.var locals) in
          let lifted_param_pat = Dedukti.ulams locals
              (T.coq_pattern_lifted_from_sort (Dedukti.var new_level_name') applied_param) in
          let translate_name_with_lifted name =
            if name = tparam_name
            then lifted_param_pat
            else Dedukti.var (translate_name name)
          in
          let translate_replace_sort s =
            Dedukti.var (if s = level_name' then new_level_name' else s)
          in
          let lhs =
            Dedukti.apps cons'
              (List.map Dedukti.var ind.univ_poly_names @
               List.map Dedukti.var ind.template_names @
               List.map translate_name_with_lifted arity_ctxt_names
              ) in
          let rhs =
            Dedukti.apps cons'
              (List.map Dedukti.var ind.univ_poly_names @
               List.map translate_replace_sort ind.template_names @
               List.map Dedukti.var arity_ctxt_names'
              ) in
          let context =
            new_level_name' ::
            ind.univ_poly_names @
            ind.template_names @
            arity_ctxt_names' in

          (* Printing out the rule for lift elimination *)
          Dedukti.print info.fmt (Dedukti.rewrite (context, lhs, rhs))
        end
    in
    List.iter print_param_ST_elim ind.mind_params_ctxt
  done
end
  | _ -> ()

(* cl s1 ... si' ... sk
      p1 ... (x1 => ... => xl => lift (u si) _ (pj x1 ... xl)) ... pr
   -->
   cl s1 ... si ... sk
      p1 ... (x1 => ... => xl => pj x1 ... xl) ... pr
  Constructor subtyping is only required for Irrelevant arguments of
  Cumulative Inductive types.
*)
let translate_cumulative_constructors_subtyping info env label ind =
  match ind.mind_univs with
  | Monomorphic _
  | Polymorphic _ -> ()
  (* Constructor subtyping is only required for Cumulative Inductive types. *)



(* match_I :
     s1 : Sort -> ... -> sk : Sort ->
     |p1| : ||P1|| ->
     ... ->
     |pr| : ||Pr|| ->

     s : Sort ->
     P : (x1 : ||A1|| -> ... -> xn : ||An|| ->
          ||I [s1] ... [sk] p1 ... pr x1 ... xn|| ->
          Univ s) ->

    case_c1 : (y11 : ||B11|| -> ... -> y1k1 : ||B1k1|| ->
               Term s (P |u11| ... |u1n| (|c1 s1 ... sk p1 ... pr y11 ... y1k1|))) -> ...
    ... ->
    case_cj : (yj1 : ||Bj1|| -> ... -> yjkj : ||Bjkj|| ->
               Term s (P |uj1| ... |ujn| (|cj s1 ... sk p1 ... pr yj1 ... yjkj|))) -> ...

    x1 : ||A1|| -> ... -> xn : ||An|| ->
    x : ||I s1 ... sk p1 ... pr x1 ... xn|| ->
    Term s (P x1 ... xn x)
*)
let translate_match info env label ind =
  let env = Environ.push_context ind.poly_ctxt env in
  let uenv = ind.constructor_uenv in
  let univ_poly_names = ind.univ_poly_names in
  let univ_poly_params = List.map Dedukti.var univ_poly_names in
  let mind_body = ind.mind_body in
  let univ_instance = ind.poly_inst in

  let mind = Names.MutInd.make2 info.module_path label in
  let ind_term = Constr.mkIndU((mind, ind.index), ind.poly_inst) in
  let ind_subst = Inductive.ind_subst mind mind_body univ_instance in

  (* Constructor names start from 1. *)
  let cons_terms = Array.init ind.n_cons
      (fun j -> Constr.mkConstructU(((mind, ind.index), j + 1), univ_instance)) in

  let indtype_name = ind.body.mind_typename in
  let match_function_name' = Cname.match_function info env info.module_path indtype_name in

  let match_function_var'  = Dedukti.var match_function_name' in
  debug "###  %s" match_function_name';

  (* Use the normalized types in the rest.
  let sub (ctx,c) =
    Context.Rel.map (Vars.substl ind_subst) ctx, Vars.substl ind_subst c in
  let cons_nf_lc = Array.map sub ind.body.mind_nf_lc in
   *)

  (* This is the applied inductive built by the constructor : Pair A B
  let cons_types = Array.map snd cons_nf_lc in
   *)

  (* This is the constructors full contexts (params + real arguments) :
     [ (A:Prop) , (B:Prop) , (_:A) , (_:B) ]
  let cons_contexts = Array.map fst cons_nf_lc in
   *)

  (* This is the constructors full type (with params + real arguments) :
     (A:Prop) -> (B:Prop) -> A -> B -> Pair A B
  let cons_full_types = ind.nf_full_types in
   *)

  (* Translate the match function: match_I *)
  let params_context = ind.mind_params_ctxt in
  let arity_real_context = ind.arity_real_context in
  let ind_applied = Terms.apply_rel_context ind_term
                      (arity_real_context @ params_context) in

  let subst_nf_lc =
    Array.init ind.n_cons (fun i ->
        let (ctx, cty) = ind.body.mind_nf_lc.(i) in
        let cty = Term.it_mkProd_or_LetIn cty ctx in
        Term.decompose_prod_assum (Vars.substl ind_subst cty))
  in

  let cons_real_contexts =
    Array.init ind.n_cons
      (fun i -> fst (Utils.list_chop ind.body.mind_consnrealdecls.(i)
                       (fst subst_nf_lc.(i))))
  in
  debug "Params ctxt: %a" pp_coq_ctxt params_context;
  debug "Real ctxt: %a" (pp_array ", " pp_coq_ctxt) cons_real_contexts;

  let dec a = snd (Constr.decompose_app (Reduction.whd_all env (snd a))) in
  let cons_ind_args = Array.map dec subst_nf_lc in

  let cons_ind_real_args = Array.init ind.n_cons (fun j ->
    snd (Utils.list_chop ind.n_params cons_ind_args.(j))) in
  let cons_applieds = Array.init ind.n_cons (fun j ->
      Terms.apply_rel_context cons_terms.(j) (cons_real_contexts.(j) @ params_context)) in
  debug "Cons applieds: %a " (pp_array ", " pp_coq_term) cons_applieds;

  let params_env, params_context' =
    let global_env_with_poly = Environ.push_context ind.poly_ctxt (Global.env ()) in
    Terms.translate_rel_context info global_env_with_poly uenv params_context in

  (* Create a fresh variable s and add it to the environment *)
  let return_sort_name = Cname.fresh_of_string info params_env "s" in
  let return_sort_name' = Cname.translate_identifier return_sort_name in
  let return_sort' = Translator.NamedSort return_sort_name' in
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
      ind.cons_names in
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

  debug "Test: %a " (pp_array " / " (pp_list ", " pp_coq_decl)) cons_real_contexts;
  let cons_real_env_contexts' =
    Array.map
      (Terms.translate_rel_context info params_env uenv)
      cons_real_contexts in

  let cons_real_envs      = Array.map fst cons_real_env_contexts' in
  let cons_real_contexts' = Array.map snd cons_real_env_contexts' in

  debug "Env: %a" pp_coq_env params_env;
  Array.iter (debug "%a" pp_coq_ctxt) cons_real_contexts;
  Array.iter (debug "%a" pp_coq_env ) cons_real_envs;
  debug "ok";

  let cons_ind_real_args' =
    Array.mapi
      (fun j -> Terms.translate_args info cons_real_envs.(j) uenv)
      cons_ind_real_args in

  debug "Real envs : %a" (pp_array "\n" pp_coq_env) cons_real_envs;

  let cons_applieds' =
    Array.mapi
      (fun j -> Terms.translate_constr info cons_real_envs.(j) uenv)
      cons_applieds
  in
  debug "ok";

  let aux = Encoding.flag "cast_arguments" in
  Encoding.set_flag "cast_arguments" false;
  let cons_pat_applieds' =
    Array.mapi
      (fun j -> Terms.translate_constr info cons_real_envs.(j) uenv)
      cons_applieds in
  Encoding.set_flag "cast_arguments" aux;
  let cons_pat_applieds' =
    if Encoding.flag "cast_arguments"
    then Array.map (T.coq_pattern_lifted_from_sort Dedukti.wildcard) cons_pat_applieds'
    else cons_pat_applieds'
  in

  (* Combine the above. *)
  let case_types' =
    Array.init ind.n_cons
      (fun j ->
         Dedukti.pies cons_real_contexts'.(j)
           (T.coq_term return_sort'
              (Dedukti.apps return_type_var'
                 (cons_ind_real_args'.(j) @ [cons_applieds'.(j)])))) in

  if ind.n_cons > 0 then debug "Test: %a" Dedukti.pp_term cons_applieds'.(0);

  let cases_context' = Array.to_list (Array.init ind.n_cons (fun j -> (case_names'.(j), case_types'.(j)))) in
  let univ_poly_context' = Tsorts.get_constructor_params
      ind.template_names
      ind.univ_poly_names
      ind.univ_poly_cstr
  in
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

     s1 : Sort -> ... -> sk : Sort ->
     p1 : ||P1|| -> ... -> pr : ||Pr|| ->

     P : (x1 : ||A1|| -> ... -> xn : ||An|| ->
          ||I s1 ... sk p1 ... pr x1 ... xn|| ->
          Univ s) ->

     case_c1 : (|y11| : ||B11|| -> ... -> |y1k1| : ||B1k1|| ->
                term s (P |u11| ... |u1n| (|c1 s1 ... sk p1 ... pr y11 ... y1k1|))) -> ...
     ... ->
     case_cj : (|yj1| : ||Bj1|| -> ... -> |yjkj| : ||Bjkj|| ->
                term s (P |uj1| ... |ujn| (|cj s1 ... sk p1 ... pr yj1 ... yjkj|))) -> ...

     |x1| : ||A1|| -> ... -> |xn| : ||An|| ->
     x : ||I s1 ... sk p1 ... pr x1 ... xn|| ->
     Term s (P |x1| ... |xn| x)
   *)
  Dedukti.print info.fmt
    (Dedukti.declaration true match_function_name'
       (Dedukti.pies match_function_context' match_function_type'));

  let match_function_applied' =
    Dedukti.apps match_function_var'
      (univ_poly_params @
       ( if Tsorts.template_constructor_upoly ()
         then List.map Dedukti.var ind.template_names
         else [] ) @
       Dedukti.var return_sort_name' ::
       params' @
       return_type_var' ::
       Array.to_list cases') in

  (* Printing out the match rewrite rules. *)
  (* match_I :
       s1 ... sr
       s
       p1 ... pr
       P
       case_c1
       ...
       case_cj
       {ai1(y1...ykj)}
       ...
       {ain(y1...yki)}
       (ci s1 ... sr p1 ... pr y11 ... y1ki)
       -->
       case_ci y11 ... y1k1
  *)
  (* TODO: We could probably remove the non-linearity of si and pi here since it is
     guaranteed by typing *)
  for j = 0 to ind.n_cons-1 do
    let case_rule_context' =
      univ_poly_context' @ common_context' @ cons_real_contexts'.(j) in
    (* Note: a1 ... an are patterns in coq (expected return type parameters)
       They should however be translated as brackets patterns. Well-typedness of
       redices matching this left-hand side ensures the terms matched to a1 ... an
       always have the expected shape.
    *)
    let brackets = List.map Dedukti.bracket cons_ind_real_args'.(j) in
    let case_rule_left' =
      Dedukti.apps
        match_function_applied'
        (brackets @ [cons_pat_applieds'.(j)]) in
    let case_rule_right' = Dedukti.apply_context cases'.(j) cons_real_contexts'.(j) in
    let rw_rule = Dedukti.typed_rewrite (case_rule_context', case_rule_left', case_rule_right') in
    Dedukti.print info.fmt rw_rule
  done;

  (* Translate the rule for lift elimination (sort irrelevance) in match polymorphism *)
  (* match_I
       s1 ... sr
       p1  ... pr _ a1 ... an
       (x1 => ... => xn => x => lift _ (u s) (P x1 ... xn x))
     -->
     match_I
       s1 ... sr
       p1  ... pr s a1 ... an
       (x1 => ... => xn => x => P x1 ... xn x)
  *)
  let new_sort_name = "s'" in (* TODO: change that to ensure no conflicts *)
  let local_ctxt_names = List.map fst arity_real_context' @ ["x"]  in
  let local_ctxt       = List.map Dedukti.var local_ctxt_names in
  let pattern = T.coq_pattern_lifted_from_sort (Dedukti.var new_sort_name)
      (Dedukti.apps return_type_var' local_ctxt) in
  let pattern_match =
    Dedukti.apps match_function_var'
      (univ_poly_params @
       (if Tsorts.template_constructor_upoly ()
        then List.map Dedukti.var ind.template_names
        else [] ) @
       Dedukti.var return_sort_name' ::
       List.map (fun x -> Dedukti.var (fst x)) params_context' @
       [Dedukti.ulams local_ctxt_names pattern]) in
  let rhs_match =
    Dedukti.apps match_function_var'
      (univ_poly_params @
       (if Tsorts.template_constructor_upoly ()
        then List.map Dedukti.var ind.template_names
        else [] ) @
       Dedukti.var new_sort_name ::
       List.map (fun x -> Dedukti.var (fst x)) params_context' @
       [Dedukti.ulams local_ctxt_names (Dedukti.apps return_type_var' local_ctxt)]) in
  let context =
    univ_poly_context' @
    return_sort_binding ::
    params_context' @
    [return_type_binding; (new_sort_name, T.coq_Sort())] in

  (* Printing out the rule for lift elimination *)
  Dedukti.print info.fmt (Dedukti.typed_rewrite (context, pattern_match, rhs_match));


  (* TODO: add match equivalences:
      match_I s1 ... si' ... sk
           p1
           ...
           (x1 => ... => xl => lift (u si) _ (pj x1 ... xl))
           ...
           pr
       -->
       match_I s1 ... si ... sk
           p1
           ...
           (x1 => ... => xl => pj x1 ... xl)
           ...
           pr
  when pj : A1 -> ... -> An -> Type@{s}  with s an irrelevant universe argument
  Only for a Template Inductive when codes are off
  *)
    match ind.arity with
    | TemplateArity _ when Tsorts.template_constructor_upoly () ->
begin
  (* FIXME: this was originally mistakingly written for template polymorphic universe arguments
     use the same idea for cumulative irrelevant universe arguments *)
  let _, arity_ctxt_names = extract_rel_context info env ind.mind_params_ctxt in
  let translate_name name =
    let name = Cname.fresh_name ~default:"_" info env name in
    Cname.translate_name name in
  let arity_ctxt_names' = List.map translate_name arity_ctxt_names in
  (* Translate the rule for lift elimination in j-th parameters if polymorphic *)
  let print_param_ST_elim decl =
    match is_template_parameter ind decl with
    | None -> () (* When parameter in not template polymorphic: no rule *)
    | Some (tparam_name,locals,level,level_name') ->
      begin
        let new_level_name' = level_name' ^ "'" in
        let tparam_name' = Cname.fresh_name ~default:"_" info env tparam_name in
        let tparam_name' = Cname.translate_name tparam_name' in
        let applied_param =  (* pj x1 ... xl *)
          Dedukti.apps (Dedukti.var tparam_name') (List.map Dedukti.var locals) in
        let lifted_param_pat = Dedukti.ulams locals
            (T.coq_pattern_lifted_from_sort (Dedukti.var new_level_name') applied_param) in
        let translate_name_with_lifted name =
          if name = tparam_name
          then lifted_param_pat
          else Dedukti.var (translate_name name)
        in
        let translate_replace_sort s =
          Dedukti.var (if s = level_name' then new_level_name' else s)
        in
        let lhs =
          Dedukti.apps match_function_var'
            (List.map Dedukti.var ind.univ_poly_names @
             List.map Dedukti.var ind.template_names @
             Dedukti.var return_sort_name' ::
             List.map translate_name_with_lifted arity_ctxt_names
            ) in
        let rhs =
          Dedukti.apps match_function_var'
            (List.map translate_replace_sort ind.univ_poly_names @
             List.map translate_replace_sort ind.template_names @
             Dedukti.var return_sort_name' ::
             List.map Dedukti.var arity_ctxt_names'
            ) in
        let context =
          new_level_name' :: return_sort_name' ::
          ind.univ_poly_names @
          ind.template_names @
          arity_ctxt_names' in

        (* Printing out the rule for lift elimination *)
        Dedukti.print info.fmt (Dedukti.rewrite (context, lhs, rhs))
      end
  in
  List.iter print_param_ST_elim ind.mind_params_ctxt
end
    | _ -> ();

(* TODO: add match equivalences:
      match_I s1 ... si' ... sk
           p1
           ...
           (x1 => ... => xl => lift (u si) _ (pj x1 ... xl))
           ...
           pr
       -->
       match_I s1 ... si ... sk
           p1
           ...
           (x1 => ... => xl => pj x1 ... xl)
           ...
           pr
    when  pj : A1 -> ... -> An -> Type@{s}  with s an irrelevant universe argument
    Only for a Cumulative Inductive Type with invariant sorts  (WIP)
  *)
   match ind.mind_univs with
  | Monomorphic _
  | Polymorphic _ -> ()


let translate_guarded info env label ind =
  if Encoding.is_fixpoint_inlined ()
  then
    let nb_params =
      ind.n_params
      + (List.length ind.univ_poly_names)
      + (if Tsorts.template_constructor_upoly ()
         then List.length ind.template_names
         else 0)
    in
    let nb_args = ind.body.mind_consnrealargs in
    (* Number of expected proper arguments of the constructors (w/o params) *)
    for consid = 0 to ind.n_cons - 1 do
      let cons_name = ind.body.mind_consnames.(consid) in
      let cons_name' = Cname.translate_element_name info env (Names.Label.of_id cons_name) in
      let args = nb_args.(consid) + nb_params in
      Dedukti.printc info.fmt (T.coq_guarded cons_name' args);
      Format.pp_force_newline info.fmt ()
    done




(* match_I s
     s1 ... si' ... sk
     p1
     ...
     (x1 => ... => xl => lift (u si) _ (pj x1 ... xl))
     ...
     pr
   -->
   match_I s
     s1 ... si ... sk
     p1
     ...
     (x1 => ... => xl => pj x1 ... xl)
     ...
     pr
*)
let translate_match_subtyping info env label ind = ()
(* TODO This is currently implemented above. Maybe move it here... *)


let print_all_alias_symbols info env label alias =
  (*
  let label' = Cname.translate_element_name info env label in
  let alias' = Cname.translate_kernel_name info env alias in
  let print (label', alias') =
    Dedukti.print info.fmt (Dedukti.alias label' alias') in
  *)
  ()
