open Term
open Declarations

(** Translation of names *)

let translate_name name =
  match name with
  | Names.Name(identifier) -> Names.string_of_id identifier
  | Names.Anonymous -> ""

let translate_constant constant =
  Names.string_of_con constant

(** Translation of terms *)

let coq name = Dedukti.Var(Printf.sprintf "Coq.%s" name)

let coq_srt = coq "srt"

let coq_p = coq "p"

let coq_t = coq "t"

let coq_type = coq "type"

let coq_term = coq "term"

let coq_sort = coq "sort"

let coq_prod = coq "prod"

let translate_sort s =
  match s with
  | Prop(Null) -> coq_p
  | Prop(Pos) -> coq_p
  | Type(universe) -> coq_t

let rec translate_constr context t =
  match Term.kind_of_term t with
  | Rel(i) ->
      let x = List.nth context (i - 1) in
      Dedukti.var (translate_name x)
  | Var(identifier) -> failwith "Not implemented: Var"
  | Meta(metavariable) -> failwith "Not implemented: Meta"
  | Evar(pexistential) -> failwith "Not implemented: Evar"
  | Sort(s) ->
      let s' = translate_sort s in
      Dedukti.app coq_sort s'
  | Cast(constr, cast_kind, types) -> failwith "Not implemented: Cast"
  | Prod(x, a, b) ->
      (* TODO: Compute the correct sorts *)
      let s1' = coq_t in
      let s2' = coq_t in
      let x' = translate_name x in
      let a' = translate_constr context a in
      let a'' = translate_types context a in
      let b' = translate_constr (x :: context) b in
      Dedukti.apps coq_prod [s1'; s2'; a'; Dedukti.lam (x', a'') b']
  | Lambda(x, a, t) ->
      let x' = translate_name x in
      let a'' = translate_types context a in
      let t' = translate_constr (x :: context) t in
      Dedukti.lam (x', a'') t'
  | LetIn(x, u, a, t) -> failwith "Not implemented: LetIn"
  | App(t, u_list) ->
      let t' = translate_constr context t in
      let u_list' = List.map (translate_constr context) (Array.to_list u_list) in
      Dedukti.apps t' u_list'
  | Const(c) ->
      let c' = translate_constant c in
      Dedukti.Var c'
  | Ind(inductive) -> failwith "Not implemented: Ind"
  | Construct(constructor) -> failwith "Not implemented: Construct"
  | Case(case_info, constr, constr2, constr_array) -> failwith "Not implemented: Cast"
  | Fix(pfixpoint) -> failwith "Not implemented: Fix"
  | CoFix(pcofixpoint) -> failwith "Not implemented: CoFix"

and translate_types context a =
  match Term.kind_of_type a with
  | SortType(s) ->
      let s' = translate_sort s in
      Dedukti.app coq_type s'
  | CastType(a, b) ->
      (* TODO: Fix type cast *)
      translate_types context a
  | ProdType(x, a, b) ->
      let x' = translate_name x in
      let a' = translate_types context a in
      let b' = translate_types (x :: context) b in
      Dedukti.pie (x', a') b'
  | LetInType(x, u, a, b) ->
      failwith "Not implemented"
  | AtomicType(_) ->
      (* TODO: compute the correct sort *)
      let a' = translate_constr context a in
      Dedukti.apps coq_term [coq_t; a']

(** Translation of declarations *)

let translate_constant_type constant_type =
  match constant_type with
  | NonPolymorphicType(a) ->
      translate_types [] a
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
      let constr' = translate_constr [] (Declarations.force constr_substituted) in
      [Dedukti.definition false name const_type' constr']
  | OpaqueDef(lazy_constr) ->
      let constr' = translate_constr [] (Declarations.force_opaque lazy_constr) in
      [Dedukti.definition true name const_type' constr']

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

