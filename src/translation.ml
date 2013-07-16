open Declarations

(** Translation of terms *)

let translate_constr constr =
  Dedukti.Type

let translate_types types =
  Dedukti.Type

(** Translation of declarations *)

let translate_constant_type constant_type =
  match constant_type with
  | NonPolymorphicType(types) ->
      translate_types types
  | PolymorphicArity(rel_context, polymorphic_arity) ->
      failwith "Polymorphic arity"

let translate_constant_body label constant_body =
  let name = Names.string_of_label label in
  (* TODO: Handle [constant_body.const_hyps] *)
  let const_type' = translate_constant_type constant_body.const_type in
  match constant_body.const_body with
  | Undef(inline) ->
      [Dedukti.declaration name const_type']
  | Def(constr_substituted) ->
      let constr' = translate_constr (Declarations.force constr_substituted) in
      [Dedukti.definition false name const_type' constr']
  | OpaqueDef(lazy_constr) ->
      let constr' = translate_constr (Declarations.force_opaque lazy_constr) in
      [Dedukti.definition true name const_type' Dedukti.Type]

let rec translate_module_body module_body =
  match module_body.mod_expr with
  | Some(struct_expr_body) -> translate_struct_expr_body struct_expr_body
  | None -> failwith "Empty module body"

and translate_struct_expr_body struct_expr_body =
  match struct_expr_body with
  | SEBstruct(structure_body) -> translate_structure_body structure_body
  | _ -> []

and translate_structure_body structure_body =
  List.concat (List.map translate_structure_field_body structure_body)

and translate_structure_field_body (label, structure_field_body) =
  match structure_field_body with
  | SFBconst(constant_body) -> translate_constant_body label constant_body
  | _ -> []

