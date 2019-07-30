(** Translation of Coq modules *)

open Declarations
open Debug
open Info


let symb_filter = ref []
let filter_out symb = symb_filter := symb :: !symb_filter
let not_filtered symb = not (List.mem symb !symb_filter)

let dest_const_univ universes =
  match universes with
  | Monomorphic_const univ_ctxt -> Univ.Instance.empty, Univ.Constraint.empty
  | Polymorphic_const univ_ctxt ->
    let uctxt = Univ.AUContext.repr univ_ctxt in
    Univ.UContext.instance uctxt, Univ.UContext.constraints uctxt


(** Constant definitions have a type and a body.
    - The type can be non-polymorphic (normal type) or
      a polymorphic arity (universe polymorphism).
    - The body can be empty (an axiom), a normal definition, or
      an opaque definition (a theorem). **)

let translate_constant_body info env label const =
  let name = Names.Label.to_string label in

  (* There should be no section hypotheses at this stage. *)
  assert (List.length const.const_hyps = 0);
  let poly_inst, poly_cstr, env =
    match const.const_universes with
    | Monomorphic_const uctxt ->
      debug "Translating monomorphic constant body: %s" name;
      let env' = Environ.push_context_set ~strict:true uctxt env in
      Univ.Instance.empty, Univ.Constraint.empty, env'
    | Polymorphic_const univ_ctxt ->
      let uctxt = Univ.AUContext.repr univ_ctxt in
      let env' = Environ.push_context ~strict:false uctxt env in
      let instance = Univ.UContext.instance uctxt in
      let constraints = Univ.UContext.constraints uctxt in
      debug "Translating polymorphic [%a] constant body: %s" pp_coq_inst instance name;
      instance, constraints, env'
  in

  let poly_inst, poly_cstr = dest_const_univ const.const_universes in
  let univ_poly_params = Tsorts.translate_univ_poly_params poly_inst in
  let poly_cstr        = Tsorts.translate_univ_poly_constraints poly_cstr in
  let uenv = Info.make [] [] (List.length univ_poly_params) poly_cstr  in

  let const_type = Vars.subst_instance_constr poly_inst const.const_type in
  let const_type' = Terms.translate_types info env uenv const_type in
  let const_type' = Tsorts.add_sort_params univ_poly_params const_type' in

  let label' = Cname.translate_element_name info env label in

  match const.const_body with
  | Undef inline ->
    (* For now assume inline is None. *)
    assert (inline = None);
    Dedukti.print info.fmt (Dedukti.declaration false label' const_type')
  | Def constr_substituted ->
    let constr = Mod_subst.force_constr constr_substituted in
    let constr' = Terms.translate_constr ~expected_type:const_type info env uenv constr in
    Dedukti.print info.fmt (Dedukti.definition false label' const_type' constr')
  | OpaqueDef lazy_constr ->
    let constr = Opaqueproof.force_proof Opaqueproof.empty_opaquetab lazy_constr in
    let constr' = Terms.translate_constr ~expected_type:const_type info env uenv constr in
    Dedukti.print info.fmt (Dedukti.definition true label' const_type' constr')

(** Translate the body of mutual inductive definitions [mind]. *)
let translate_mutual_inductive_body info env label mind_body =
  debug "Translating inductive: %s" (Names.Label.to_string label);

  let ntypes = mind_body.mind_ntypes in
  let inds = Array.init mind_body.mind_ntypes (Inductives.get_infos mind_body) in
  (* First declare all the inductive types. Constructors of one inductive type
     can refer to other inductive types in the same block. *)
  for i = 0 to pred ntypes do
    Inductives.translate_inductive info env label inds.(i)
  done;
  (* Then extend subtyping to inductive types *)
  for i = 0 to pred ntypes do
    Inductives.translate_inductive_subtyping info env label inds.(i)
  done;
  (* Then declare all the constructors *)
  for i = 0 to pred ntypes do
    Inductives.translate_constructors info env label inds.(i)
  done;
  (* Then declare all the guard definitions *)
  if Encoding.is_fixpoint_inlined ()
  then
    for i = 0 to pred ntypes do
      Inductives.translate_guarded info env label inds.(i)
    done;
  (* Then extend subtyping to inductive constructors *)
  for i = 0 to pred ntypes do
    Inductives.translate_constructors_subtyping info env label inds.(i)
  done;
  (* Then declare all the match functions *)
  for i = 0 to pred ntypes do
    Inductives.translate_match info env label inds.(i)
  done;
  (* Then extend subtyping to inductive destructors *)
  for i = 0 to pred ntypes do
    Inductives.translate_match_subtyping info env label inds.(i)
  done

(** Pseudo-translate the body of mutual coinductive definitions [mind]. *)
let translate_mutual_coinductive_body info env label mind_body =
  let ntypes = mind_body.mind_ntypes in
  Error.warning "Translating coinductive %a" pp_coq_label label;
  let inds = Array.init mind_body.mind_ntypes (Inductives.get_infos mind_body) in
  debug "Translating co-inductive body: %s" (Names.Label.to_string label);
  (* First declare all the inductive types. Constructors of one inductive type
     may refer to other inductive types in the same block. *)
  for i = 0 to pred ntypes do
    Inductives.translate_inductive info env label inds.(i)
  done;
  (* Then extend subtyping if we have template polymorphic inductives *)
  if Encoding.is_templ_polymorphism_on () then
    for i = 0 to pred ntypes do
      Inductives.translate_inductive_subtyping info env label inds.(i)
    done;
  (* Then declare all the constructors *)
  for i = 0 to pred ntypes do
    Inductives.translate_constructors info env label inds.(i)
  done;
  (* No match function defined so we are done *)
  (* Then declare all the match functions *)
  for i = 0 to pred ntypes do
    Inductives.translate_match info env label inds.(i)
  done


(** Translate the body of non-recursive definitions when it's a record. *)
let translate_record_body info env label mind_body =
  match mind_body.mind_record with
  | None   -> Error.not_supported "Non-recursive translation"
  | Some _ ->
    debug "Translating record: %a" pp_coq_label label;
    translate_mutual_inductive_body info env label mind_body


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
    | Abstract       -> Error.not_supported "Abstract"
    | Algebraic _    -> Error.not_supported "Algebraic"
    | Struct mod_sig -> translate_module_signature info env mod_sig
    | FullStruct     -> translate_module_signature info env mb.mod_type
  else debug "Filtered out"

and translate_module_signature info env  = function
  | NoFunctor struct_body -> translate_structure_body info env struct_body
  | MoreFunctor _ ->
    Error.not_supported
      (Format.sprintf "Functor (%s)" (Names.ModPath.to_string info.module_path))

and translate_structure_body info env sb =
  List.iter (translate_structure_field_body info env) sb

and translate_structure_field_body info env (label, sfb) =
  let full_name = (Names.ModPath.to_string info.module_path) ^ "." ^
                  (Names.Label.to_string label) in
  debug "";
  debug "---------------------------------------------";
  debug "    Structure field body: %s" full_name;
  debug "---------------------------------------------";
  if not_filtered full_name
  then
    begin
      verbose "-> %s" full_name;
      match sfb with
      | SFBconst cb -> translate_constant_body info env label cb
      | SFBmind mib ->
        (match mib.mind_finite with
         | Declarations.Finite   -> translate_mutual_inductive_body
         | Declarations.CoFinite -> translate_mutual_coinductive_body
         | Declarations.BiFinite -> translate_record_body
        ) info env label mib
      | SFBmodule  mb -> translate_module_body (Info.update info label) env mb
      | SFBmodtype _  -> ()
    end
  else
    begin
      debug "Filtered out";
      verbose "-> %s (ignored)" full_name
    end
