(** Translation of Coq modules *)

open Declarations

let translate_constant_type out module_path env const_type =
  match const_type with
  | NonPolymorphicType(a) ->
      Terms.translate_types out env a
  | PolymorphicArity(rel_context, poly_arity) ->
      failwith "Not implemented: PolymorphicArity"

let translate_constant_body out module_path env label const =
  let label' = Name.translate_label label in
  (* TODO: Handle [constant_body.const_hyps] *)
  let const_type' = translate_constant_type out module_path env const.const_type in
  match const.const_body with
  | Undef(inline) ->
      Dedukti.print out (Dedukti.declaration label' const_type')
  | Def(constr_substituted) ->
      let constr' = Terms.translate_constr out env (Declarations.force constr_substituted) in
      Dedukti.print out (Dedukti.definition false label' const_type' constr')
  | OpaqueDef(lazy_constr) ->
      let constr' = Terms.translate_constr out env (Declarations.force_opaque lazy_constr) in
      Dedukti.print out (Dedukti.definition true label' const_type' constr')

let get_inductive_arity_sort ind_arity =
  match ind_arity with
  | Monomorphic(mono_ind_arity) -> mono_ind_arity.mind_sort
  | Polymorphic(poly_arity) -> Term.Type(poly_arity.poly_level)

(** Translate the inductive body [ind] belonging to the mutual inductive
    body [mind]. *)
let translate_one_inductive_body out module_path env ind_body label mind_body =
  (* Translate the inductive type. *)
  let name = ind_body.mind_typename in
  let sort = get_inductive_arity_sort ind_body.mind_arity in
  let arity = Term.it_mkProd_or_LetIn (Term.mkSort sort) ind_body.mind_arity_ctxt in
  let name' = Name.translate_identifier name in
  let arity' = Terms.translate_types out env arity in
  Dedukti.print out (Dedukti.declaration name' arity');
  (* Translate the constructors. *)
  (* Substitute the inductive type names as specified in the Coq code. *)
  let mind = Names.make_mind module_path Names.empty_dirpath label in
  let inds = Array.init mind_body.mind_ntypes (fun i -> Term.mkInd(mind, i)) in
  for j = 0 to Array.length ind_body.mind_consnames - 1 do
    let cname = ind_body.mind_consnames.(j) in
    let ctype = Term.substl (Array.to_list inds) ind_body.mind_user_lc.(j) in
    let cname' = Name.translate_identifier cname in
    let ctype' = Terms.translate_types out env ctype in
    Dedukti.print out (Dedukti.declaration cname' ctype');
  done
  (* Generate the match function. *)
  (* Generate the fix function. *)

(** Translate the body of mutual inductive definitions [mind]. *)
let translate_mutual_inductive_body out module_path env label mind_body =
  for i = 0 to mind_body.mind_ntypes - 1 do
    translate_one_inductive_body out module_path env mind_body.mind_packets.(i) label mind_body
  done

let rec translate_module_body out module_path env mods =
  match mods.mod_expr with
  | Some(struct_expr) -> translate_struct_expr_body out module_path env struct_expr
  | None -> ()

and translate_struct_expr_body out module_path env struct_expr =
  match struct_expr with
  | SEBstruct(structs) -> translate_structure_body out module_path env structs
  | _ -> ()

and translate_structure_body out module_path env structure_body =
  List.iter (translate_structure_field_body out module_path env) structure_body

and translate_structure_field_body out module_path env (label, struct_field_body) =
  match struct_field_body with
  | SFBconst(const_body) -> translate_constant_body out module_path env label const_body
  | SFBmind(mind_body) -> translate_mutual_inductive_body out module_path env label mind_body
  | _ -> ()

