(** Translation of Coq modules *)

open Declarations

let translate_constant_type env constant_type =
  match constant_type with
  | NonPolymorphicType(a) ->
      Terms.translate_types Environ.empty_env a
  | PolymorphicArity(rel_context, polymorphic_arity) ->
      failwith "Polymorphic arity"

let translate_constant_body env label constant_body =
  let name = Name.translate_label label in
  (* TODO: Handle [constant_body.const_hyps] *)
  let const_type' = translate_constant_type env constant_body.const_type in
  match constant_body.const_body with
  | Undef(inline) ->
      [Dedukti.declaration name const_type']
  | Def(constr_substituted) ->
      let constr' = Terms.translate_constr env (Declarations.force constr_substituted) in
      [Dedukti.definition false name const_type' constr']
  | OpaqueDef(lazy_constr) ->
      let constr' = Terms.translate_constr env (Declarations.force_opaque lazy_constr) in
      [Dedukti.definition true name const_type' constr']

let rec translate_module_body env module_body =
  match module_body.mod_expr with
  | Some(struct_expr_body) -> translate_struct_expr_body env struct_expr_body
  | None -> failwith "Empty module body"

and translate_struct_expr_body env struct_expr_body =
  match struct_expr_body with
  | SEBstruct(structure_body) -> translate_structure_body env structure_body
  | _ -> []

and translate_structure_body env structure_body =
  List.concat (List.map (translate_structure_field_body env) structure_body)

and translate_structure_field_body env (label, structure_field_body) =
  match structure_field_body with
  | SFBconst(constant_body) -> translate_constant_body env label constant_body
  | _ -> []

