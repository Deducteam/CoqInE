(** Translation of Coq modules *)

open Declarations
open Debug
open Info


let fail_on_issue = ref true

let enable_failproofmode () =
  message "Failproof mode enabled.";
  fail_on_issue := false

let disable_failproofmode () =
  message "Failproof mode disabled.";
  fail_on_issue := true

let symb_filter = ref []
let filter_out symb = symb_filter := symb :: !symb_filter
let not_filtered symb = not (List.mem symb !symb_filter)

let isalias resolver kername =
  let mind = Mod_subst.mind_of_delta_kn resolver kername in
  let can = Names.MutInd.canonical mind in
  if Names.KerName.compare (Names.MutInd.user mind) can = 0
  then None
  else Some can

let translate_alias_constant_body info env alias label const =
  let label' = Cname.translate_element_name info env label in
  debug "Translating constant alias %a -> %a"
    pp_coq_label label pp_coq_kername alias;
  let alias' = Cname.translate_kernel_name info env alias in
  Dedukti.print info.fmt (Dedukti.alias label' alias')

(* Constant definitions have a type and a body.
   - The type can be non-polymorphic (normal type) or
     a polymorphic arity (universe polymorphism).
   - The body can be empty (an axiom), a normal definition, or
     an opaque definition (a theorem). *)
let translate_constant_body info env isalias label const =
  match isalias with
  | Some alias -> translate_alias_constant_body info env alias label const
  | None -> begin
  let label' = Cname.translate_element_name info env label in
  let name = Names.Label.to_string label in

  (* There should be no section hypotheses at this stage. *)
  assert (List.length const.const_hyps = 0);
  let poly_ctxt, poly_inst, poly_cstr, env =
    match const.const_universes with
    | Monomorphic uctxt ->
      debug "Translating monomorphic constant body: %s" name;
      let env' = Environ.push_context_set ~strict:true uctxt env in
      Univ.AUContext.empty, Univ.Instance.empty, Univ.Constraint.empty, env'
    | Polymorphic univ_ctxt ->
      let uctxt = Univ.AUContext.repr univ_ctxt in
      let env' = Environ.push_context ~strict:false uctxt env in
      let instance = Univ.UContext.instance uctxt in
      let constraints = Univ.UContext.constraints uctxt in
      debug "Translating polymorphic [%a] constant body: %s"
        pp_coq_inst instance name;
      univ_ctxt, instance, constraints, env'
  in

  let univ_poly_params = Tsorts.translate_univ_poly_params poly_inst in
  let univ_poly_cstr   = Tsorts.translate_univ_poly_constraints poly_cstr in
  let uenv = Info.make [] [] poly_ctxt (List.length univ_poly_params) univ_poly_cstr in

  let const_type = Vars.subst_instance_constr poly_inst const.const_type in
  let const_type' = Terms.translate_types info env uenv const_type in
  let const_type' = Tsorts.add_poly_params_type univ_poly_params univ_poly_cstr const_type' in

  match const.const_body with
  | Undef inline ->
    (* For now assume inline is None. *)
    assert (inline = None);
    Dedukti.print info.fmt (Dedukti.declaration false label' const_type')
  | Def constr ->
    let constr =
      if Encoding.flag "unfold_letin"
      then
        let open CClosure in
        let flags = RedFlags.mkflags [RedFlags.fZETA] in
        norm_val (create_clos_infos flags env) (create_tab ()) (inject constr)
      else constr in
    let constr' = Terms.translate_constr ~expected_type:const_type info env uenv constr in
    let constr' = Tsorts.add_poly_params_def univ_poly_params univ_poly_cstr constr' in
    Dedukti.print info.fmt (Dedukti.definition false label' const_type' constr')
  | OpaqueDef lazy_constr ->
    let constr, _ = Opaqueproof.force_proof Library.indirect_accessor Opaqueproof.empty_opaquetab lazy_constr in
    let constr' = Terms.translate_constr ~expected_type:const_type info env uenv constr in
    let constr' = Tsorts.add_poly_params_def univ_poly_params univ_poly_cstr constr' in
    Dedukti.print info.fmt (Dedukti.definition true label' const_type' constr')
  | Primitive _ -> assert false
end

(** Translate the body of mutual inductive definitions [mind]. *)
let translate_mutual_inductive_body info env isalias label mind_body =
  match isalias with
  | Some alias ->
    debug "Translating inductive alias %a -> %a"
      pp_coq_label label pp_coq_kername alias;
    Inductives.print_all_alias_symbols info env label alias
  | None ->
    debug "Translating inductive: %s" (Names.Label.to_string label);
    let ntypes = mind_body.mind_ntypes in
    let inds = Array.init ntypes (Inductives.get_infos mind_body) in
    let iter f = Array.iter (f info env label) inds in
    (* First declare all the inductive types. Constructors of one inductive type
       can refer to other inductive types in the same block. *)
    iter Inductives.translate_inductive;
    (* Then encode sort irrelevance for template polymorphism (if needed) *)
    iter Inductives.translate_template_inductive_subtyping;
    (* Then encode global universes for template polymorphism (if needed) *)
    iter Inductives.translate_template_inductive_levels;
    (* Then declare all the constructors.
       Template constructors are usually universe monomorphic. *)
    iter Inductives.translate_constructors;
    (* Then declare all the guard definitions for fixpoint compatibility. *)
    iter Inductives.translate_guarded;
    (* Then extend subtyping to template inductive constructors *)
    iter Inductives.translate_template_constructors_subtyping;
    (* Then extend subtyping to cumulative inductive constructors *)
    iter Inductives.translate_cumulative_constructors_subtyping;
    (* Then declare all the match functions and their rewrite rules *)
    iter Inductives.translate_match;
    (* Then extend subtyping to inductive destructors *)
    iter Inductives.translate_match_subtyping

(** Pseudo-translate the body of mutual coinductive definitions [mind_body]. *)
let translate_mutual_coinductive_body info env isalias label mind_body =
  Error.warning "Translating coinductive %a" pp_coq_label label;
  let ntypes = mind_body.mind_ntypes in
  let inds = Array.init ntypes (Inductives.get_infos mind_body) in
  let iter f =
    for i = 0 to pred ntypes do f info env label inds.(i) done in
  debug "Translating co-inductive body: %s" (Names.Label.to_string label);
  (* First declare all the inductive types. Constructors of one inductive type
     may refer to other inductive types in the same block. *)
  iter Inductives.translate_inductive;
  (* Then encode sort irrelevance for template polymorphism (if needed) *)
  iter Inductives.translate_template_inductive_subtyping;
  (* Then encode global universes for template polymorphism (if needed) *)
  iter Inductives.translate_template_inductive_levels;
  (* Then declare all the constructors *)
  iter Inductives.translate_constructors
  (* No match function defined so we are done *)

(** Translate the body of non-recursive definitions when it's a record. *)
let translate_record_body info env isalias label mind_body =
  match mind_body.mind_record with
  | NotRecord
  | FakeRecord
  | PrimRecord _ ->
    debug "Translating record: %a" pp_coq_label label;
    translate_mutual_inductive_body info env isalias label mind_body


(*----------------------  Dead code  ----------------------

let identifiers_of_mutual_inductive_body mind_body =
  let identifiers_of_inductive_body ind_body =
    ind_body.mind_typename :: Array.to_list ind_body.mind_consnames in
  List.concat (Array.to_list
    (Array.map identifiers_of_inductive_body mind_body.mind_packets))

let identifiers_of_structure_field_body (label, struct_field_body) =
  match struct_field_body with
  | SFBconst(_) -> [Names.Label.to_id label]
  | SFBmind(mind_body) -> identifiers_of_mutual_inductive_body mind_body
  | SFBmodule(_) -> []
  | SFBmodtype(_) -> []

let identifiers_of_structure_body structure_body =
  List.concat (List.map identifiers_of_structure_field_body structure_body)
*)


(** Modules are organised into:
    - [module_body] (mb): a wrapper around a struct expression
    - [struct_expr_body] (seb): a struct expression, e.g. functor,
      application, ...
    - [structure_body] (sb): a concrete struct, i.e. a list of fields
    - [structure_field_body] (sfb): a single field declaration, e.g.
      definition, inductive, ... **)
let rec translate_module_body info env mb =
  let mod_name = Names.ModPath.to_string mb.mod_mp in
  debug "";
  debug "=============================================";
  debug "      Module body: %s" mod_name;
  debug "=============================================";
  if not_filtered mod_name
  then
    match mb.mod_expr with
    | Abstract          -> Error.not_supported "Abstract"
    | Algebraic mod_exp ->
      translate_module_expression info env mb.mod_delta mb.mod_mp mod_exp
    | Struct mod_sig    ->
      translate_module_signature info env mb.mod_delta mod_sig
    | FullStruct        ->
      translate_module_signature info env mb.mod_delta mb.mod_type
  else debug "Filtered out"

and translate_module_expression info env resolver modpath = function
  | NoFunctor alg_exp ->
    let modsig, _, resolver, ctxt =
      Mod_typing.translate_mse env (Some modpath) (Some 1000) alg_exp in
    translate_module_signature info env resolver modsig
  | MoreFunctor _ -> ()
  (* Functors definitions are simply ignored.
     Whenever functors are applied to define algebraic modules, their
     definition is expanded.
    Error.not_supported
      (Format.sprintf "Functor (%s)" (Names.ModPath.to_string info.module_path))
*)

and translate_module_signature info env resolver = function
  | NoFunctor struct_body ->
    List.iter (translate_structure_field_body info env resolver) struct_body
  | MoreFunctor _ -> ()
  (* Functors definitions are simply ignored.
     Whenever functors are applied to define algebraic modules, their
     definition is expanded.
    Error.not_supported
      (Format.sprintf "Functor (%s)" (Names.ModPath.to_string info.module_path))
*)

and translate_structure_field_body info env resolver (label, sfb) =
  let full_name = (Names.ModPath.to_string info.module_path) ^ "." ^
                  (Names.Label.to_string label) in
  if Debug.is_debug_smb full_name then debug_start ();
  debug "";
  debug "---------------------------------------------";
  debug "    Structure field body: %s" full_name;
  debug "---------------------------------------------";
  if not_filtered full_name
  then
    begin
      try
        verbose "-> %s" full_name;
        let kername = Names.KerName.make info.module_path label in
        match sfb with
        | SFBconst cb ->
          translate_constant_body info env (isalias resolver kername) label cb
        | SFBmind mib ->
          (match mib.mind_finite with
           | Declarations.Finite   -> translate_mutual_inductive_body
           | Declarations.CoFinite -> translate_mutual_coinductive_body
           | Declarations.BiFinite -> translate_record_body
          ) info env (isalias resolver kername) label mib;
        | SFBmodule  mb -> translate_module_body (Info.update info label) env mb
        | SFBmodtype _  -> ()
      with e ->
        if !fail_on_issue
        then (Info.close info; raise e)
        else verbose "[Error] On symbol %s: %s" full_name (Printexc.to_string e)
    end
  else
    begin
      debug "Filtered out";
      verbose "-> %s (ignored)" full_name
    end;
  if Debug.is_debug_smb full_name then debug_stop ();
