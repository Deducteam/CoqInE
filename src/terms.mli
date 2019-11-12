(** Translation of Coq terms *)

val infer_type : Environ.env -> Constr.t -> Constr.types

val infer_sort : Environ.env -> Constr.types -> Sorts.t

val infer_translate_sort :
  Info.info -> Environ.env -> Info.env -> Constr.types -> Translator.universe_expr

val abstract_rel_context :
  Context.Rel.Declaration.t list -> Constr.t -> Constr.t

val generalize_rel_context :
  Context.Rel.Declaration.t list -> Constr.types -> Constr.types

val apply_rel_context :
  Constr.t -> Context.Rel.Declaration.t list -> Constr.t

val convertible :
  Info.info -> Environ.env -> Info.env -> Constr.t -> Constr.t -> bool

val make_const : Names.ModPath.t -> Names.Id.t -> Names.Constant.t

val push_const_decl :
  Environ.env ->
  Names.Constant.t * Constr.t option * Constr.types ->
  Environ.env

val translate_constr :
  ?expected_type:Constr.t ->
  Info.info -> Environ.env -> Info.env -> Constr.t -> Dedukti.term

val translate_types :
  Info.info -> Environ.env -> Info.env -> Constr.types -> Dedukti.term

val translate_rel_decl :
  Info.info -> Environ.env ->
  Context.Rel.Declaration.t -> (Environ.env * (Dedukti.var * Constr.types) option)

val translate_rel_context :
  Info.info -> Environ.env -> Info.env ->
  Context.Rel.t -> Environ.env * (Dedukti.var * Dedukti.term) list

val translate_args :
  Info.info -> Environ.env -> Info.env -> Constr.t list -> Dedukti.term list
(** Translating Constrs as Dedukti patterns *)
