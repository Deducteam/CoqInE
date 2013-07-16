open Term
open Declarations

let infer_type env t =
  (fst (Typeops.infer env t)).Environ.uj_type

let infer_sort env a = 
  (fst (Typeops.infer_type env a)).Environ.utj_type

(** Translation of terms *)

let coq name = Dedukti.Var(Printf.sprintf "Coq.%s" name)

let coq_univ = coq "univ"

let coq_z = coq "z"

let coq_s i = Dedukti.apps (coq "s") [i]

let coq_srt = coq "srt"

let coq_p = coq "p"

let coq_t u = Dedukti.apps (coq "t") [u]

let coq_type s = Dedukti.apps (coq "type") [s]

let coq_term s a = Dedukti.apps (coq "term") [s; a]

let coq_sort s = Dedukti.apps (coq "sort") [s]

let coq_prod s1 s2 a b = Dedukti.apps (coq "prod") [s1; s2; a; b]

(** Translate the Coq universe [i] as a concrete Dedukti universe.
    Since the Coq universes are implemented as an abstract datatype, we cannot
    access the information directly. This function uses a trick that involves
    sorting the universe graph and manipulating the string representation. *)
let translate_universe universes i =
  (* Print the universe [i] to obtain a string representation. *)
  Pp.pp_with Format.str_formatter (Univ.pr_uni i);
  let i = Format.flush_str_formatter () in
(*  Dedukti.var i*)
  (* Sort the universes to solve the constraints. *)
  let universes = Univ.sort_universes universes in
  (* Tarverse the constraints and register when we see [i]. *)
  let solution = ref None in
  let register constraint_type j k =
    if i = j then solution := Some(k) in
  Univ.dump_universes register universes;
  (* Extract the registered universe. *)
  match !solution with
  | None -> coq "z"
  | Some(i) ->
      let rec univ i =
      match i with
      | 0 -> coq_z
      | i -> coq_s (univ (i - 1)) in
      Scanf.sscanf i "Type.%d" univ

let translate_sort env s =
  match s with
  | Prop(Null) -> coq_p
  | Prop(Pos) -> coq_p
  | Type(i) ->
      let universes = Environ.universes env in
      let i' = translate_universe universes i in
      coq_t i'

let rec translate_constr env t =
  match Term.kind_of_term t with
  | Rel(i) ->
      let (x, _, _) = Environ.lookup_rel i env in
      Dedukti.var (Name.translate_name x)
  | Var(identifier) -> failwith "Not implemented: Var"
  | Meta(metavariable) -> failwith "Not implemented: Meta"
  | Evar(pexistential) -> failwith "Not implemented: Evar"
  | Sort(s) ->
      let s' = translate_sort env s in
      coq_sort s'
  | Cast(constr, cast_kind, types) -> failwith "Not implemented: Cast"
  | Prod(x, a, b) ->
      let s1 = infer_sort env a in
      let s2 = infer_sort (Environ.push_rel (x, None, a) env) b in
      let s1' = translate_sort env s1 in
      let s2' = translate_sort (Environ.push_rel (x, None, a) env) s2 in
      let x' = Name.translate_name x in
      let a' = translate_constr env a in
      let a'' = translate_types env a in
      let b' = translate_constr (Environ.push_rel (x, None, a) env) b in
      coq_prod s1' s2' a' (Dedukti.lam (x', a'') b')
  | Lambda(x, a, t) ->
      let x' = Name.translate_name x in
      let a'' = translate_types env a in
      let t' = translate_constr (Environ.push_rel (x, None, a) env) t in
      Dedukti.lam (x', a'') t'
  | LetIn(x, u, a, t) -> failwith "Not implemented: LetIn"
  | App(t, u_list) ->
      let t' = translate_constr env t in
      let u_list' = List.map (translate_constr env) (Array.to_list u_list) in
      Dedukti.apps t' u_list'
  | Const(c) ->
      let c' = Name.translate_constant c in
      Dedukti.Var c'
  | Ind(inductive) -> failwith "Not implemented: Ind"
  | Construct(constructor) -> failwith "Not implemented: Construct"
  | Case(case_info, constr, constr2, constr_array) -> failwith "Not implemented: Cast"
  | Fix(pfixpoint) -> failwith "Not implemented: Fix"
  | CoFix(pcofixpoint) -> failwith "Not implemented: CoFix"

and translate_types env a =
  match Term.kind_of_type a with
  | SortType(s) ->
      let s' = translate_sort env s in
      coq_type s'
  | CastType(a, b) ->
      (* TODO: Fix type cast *)
      translate_types env a
  | ProdType(x, a, b) ->
      let x' = Name.translate_name x in
      let a' = translate_types env a in
      let b' = translate_types (Environ.push_rel (x, None, a) env) b in
      Dedukti.pie (x', a') b'
  | LetInType(x, u, a, b) ->
      failwith "Not implemented"
  | AtomicType(_) ->
      let s = infer_sort env a in
      let s' = translate_sort env s in
      let a' = translate_constr env a in
      coq_term s' a'

(** Translation of declarations *)

let translate_constant_type env constant_type =
  match constant_type with
  | NonPolymorphicType(a) ->
      translate_types Environ.empty_env a
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
      let constr' = translate_constr env (Declarations.force constr_substituted) in
      [Dedukti.definition false name const_type' constr']
  | OpaqueDef(lazy_constr) ->
      let constr' = translate_constr env (Declarations.force_opaque lazy_constr) in
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

