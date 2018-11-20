(** Translation of Coq terms *)

val infer_type : Environ.env -> Constr.t -> Constr.types

val infer_sort : Environ.env -> Constr.types -> Sorts.t

val translate_sort :
  Info.info -> Environ.env -> Info.env -> Sorts.t -> Translator.cic_universe

val infer_translate_sort :
  Info.info -> Environ.env -> Info.env -> Constr.types -> Translator.cic_universe

val abstract_rel_context :
  Context.Rel.Declaration.t list -> Constr.t -> Constr.t

val generalize_rel_context :
  Context.Rel.Declaration.t list -> Constr.types -> Constr.types

val apply_rel_context :
  Constr.t -> Context.Rel.Declaration.t list -> Constr.t

val convertible_sort :
  Info.info -> Environ.env -> Info.env -> Sorts.t -> Sorts.t -> bool

val convertible :
  Info.info -> Environ.env -> Info.env -> Constr.t -> Constr.t -> bool

val fixpoint_table :
  (Names.Name.t array * Constr.types array * Constr.t array,
   Environ.env * Context.Rel.Declaration.t array)
  Hashtbl.t

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

val lift_let :
  Info.info -> Environ.env -> Info.env ->
  Names.Name.t -> Constr.t -> Constr.types -> Environ.env * Constr.t

val lift_fix :
  Info.info -> Environ.env -> Info.env ->
  Names.Name.t array ->
  Constr.types array ->
  Constr.t array ->
  int array -> Environ.env * Context.Rel.Declaration.t array

val translate_rel_context :
  Info.info -> Environ.env -> Info.env ->
  Context.Rel.t -> Environ.env * (Dedukti.var * Dedukti.term) list

val translate_args :
  Info.info -> Environ.env -> Info.env -> Constr.t list -> Dedukti.term list
