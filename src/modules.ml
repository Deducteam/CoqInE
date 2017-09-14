(** Translation of Coq modules *)

open Pp
open Declarations

open Info

(** Constant definitions have a type and a body.
    - The type can be non-polymorphic (normal type) or
      a polymorphic arity (universe polymorphism).
    - The body can be empty (an axiom), a normal definition, or
      an opaque definition (a theorem). **)

let translate_constant_body info env label const =
  let label' = Name.translate_element_name info env label in
  (* There should be no section hypotheses at this stage. *)
  assert (List.length const.const_hyps = 0);
  let const_type = match const.const_type with
    | RegularArity a -> a
    | TemplateArity(rel_context, poly_arity) ->
       Terms.generalize_rel_context rel_context (Term.mkSort (Term.Type poly_arity.template_level)) in
  let const_type' = Terms.translate_types info env const_type in
  match const.const_body with
  | Undef inline ->
      (* For now assume inline is None. *)
      assert (inline = None);
      Dedukti.print info.out (Dedukti.declaration false label' const_type')
  | Def constr_substituted ->
      let constr = Mod_subst.force_constr constr_substituted in
      let constr' = Terms.translate_constr ~expected_type:const_type info env constr in
      Dedukti.print info.out (Dedukti.definition false label' const_type' constr')
  | OpaqueDef lazy_constr ->
      let constr = Opaqueproof.force_proof Opaqueproof.empty_opaquetab lazy_constr in
      let constr' = Terms.translate_constr ~expected_type:const_type info env constr in
      Dedukti.print info.out (Dedukti.definition true label' const_type' constr')
  
(** Translate the body of mutual inductive definitions [mind]. *)
let translate_mutual_inductive_body info env label mind_body =
  (* First declare all the inductive types. Constructors of one inductive type
     can refer to other inductive types in the same block. *)
  for i = 0 to mind_body.mind_ntypes - 1 do
    Inductives.translate_inductive info env label mind_body i
  done;
  (* Then declare all the constructors. *)
  for i = 0 to mind_body.mind_ntypes - 1 do
    Inductives.translate_constructors info env label mind_body i
  done;
  (* Then declare all the match functions. *)
  for i = 0 to mind_body.mind_ntypes - 1 do
    Inductives.translate_match info env label mind_body i
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


              
(** Modules are organised into:
    - [module_body] (mb): a wrapper around a struct expression
    - [struct_expr_body] (seb): a struct expression, e.g. functor,
      application, ...
    - [structure_body] (sb): a concrete struct, i.e. a list of fields
    - [structure_field_body] (sfb): a single field declaration, e.g.
      definition, inductive, ... **)
let rec translate_module_body info env mb =
  match mb.mod_expr with
  | Abstract   -> Error.not_supported "Abstract"
  | Algebraic _ -> Error.not_supported "Algebraic"
  | Struct (NoFunctor struct_body) ->  translate_structure_body info env struct_body
  | Struct (MoreFunctor _) -> Error.not_supported "Functor"
  | FullStruct -> Error.not_supported "FullStruct"
(**
and translate_struct_expr_body info env seb =
  match seb with
  | SEBstruct  sb -> translate_structure_body info env sb
  | SEBident   _  -> Error.not_supported "SEBident"
  | SEBfunctor _  ->
    (* For now we ignore unapplied functors. *)
    ()
  | SEBapply(_) -> Error.not_supported "SEBapply"
  | SEBwith(_) -> Error.not_supported "SEBwith"
*)
and translate_structure_body info env sb =
  List.iter (translate_structure_field_body info env) sb

and translate_structure_field_body info env (label, sfb) =
  match sfb with
  | SFBconst cb  -> translate_constant_body info env label cb
  | SFBmind mib ->
     (
     match mib.mind_finite with
     | Finite (** = inductive *)  -> translate_mutual_inductive_body info env label mib
     | CoFinite (** = coinductive   *) -> Error.warning (str "Ignoring coinductive " ++ Names.pr_label label)
     | BiFinite (** = non-recursive *) -> Error.warning (str "Ignoring non-recursive " ++ Names.pr_label label)
     )
  | SFBmodule mb ->
      let info = {info with module_path = Names.MPdot(info.module_path, label)} in
      translate_module_body info env mb
  | SFBmodtype(_) -> ()

