(** Translation of Coq terms *)
val infer_type : Environ.env -> Constr.t -> Constr.types

val infer_sort : Environ.env -> Constr.types -> Sorts.t

val infer_translate_sort :
  Info.info -> Environ.env -> Info.env -> Constr.types -> Translator.universe_expr
(** Infer and translate the sort of [a].
    Coq fails if we try to type a sort that was already inferred.
    This function uses pattern matching to avoid it. *)

val abstract_rel_context :
  Constr.rel_context -> Constr.t -> Constr.t
(** Abstract over the variables of [context], eliminating let declarations. *)

val generalize_rel_context :
  Constr.rel_context -> Constr.types -> Constr.types
(** Generalize over the variables of [context], eliminating let declarations. *)

val apply_rel_context :
  Constr.t -> Constr.rel_context -> Constr.t
(** Apply the variables of [context] to [t], ignoring let declarations. *)

val convertible :
  Info.info -> Environ.env -> Info.env -> Constr.t -> Constr.t -> bool

val make_const : Names.ModPath.t -> Names.Id.t -> Names.Constant.t

val translate_constr :
  ?expected_type:Constr.t ->
  Info.info -> Environ.env -> Info.env -> Constr.t -> Dedukti.term
(** Translate the Coq term [t] as a Dedukti term. *)

val translate_types :
  Info.info -> Environ.env -> Info.env -> Constr.types -> Dedukti.term
(** Translate the Coq type [a] as a Dedukti type. *)

val translate_rel_decl :
  Info.info -> Environ.env ->
  Constr.rel_declaration -> (Environ.env * (Dedukti.var * Constr.types) option)

val translate_rel_context :
  Info.info -> Environ.env -> Info.env ->
  Constr.rel_context -> Environ.env * (Dedukti.var * Dedukti.term) list
(** Translate the context [x1 : a1, ..., xn : an] into the list
    [x1, ||a1||; ...; x1, ||an||], ignoring let declarations. *)

val translate_args :
  Info.info -> Environ.env -> Info.env -> Constr.t list -> Dedukti.term list
(** Translating Constrs as Dedukti patterns *)
