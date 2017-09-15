(** Translation of Coq terms *)

val infer_type : 'a -> Environ.env -> Term.constr -> Term.types

val infer_sort : 'a -> Environ.env -> Term.types -> Term.sorts

val translate_sort : 'a -> 'b -> Term.sorts -> Dedukti.term

val infer_translate_sort :
  Info.info -> Environ.env -> Term.types -> Dedukti.term

val abstract_rel_context :
  Context.Rel.Declaration.t list -> Term.constr -> Term.constr

val generalize_rel_context :
  Context.Rel.Declaration.t list -> Term.types -> Term.types

val apply_rel_context :
  Term.constr -> Context.Rel.Declaration.t list -> Term.constr

val convertible_sort : 'a -> 'b -> Term.sorts -> Term.sorts -> bool

val convertible : 'a -> Environ.env -> Term.constr -> Term.constr -> bool

val fixpoint_table :
  (Names.name array * Term.types array * Term.constr array,
   Environ.env * Context.Rel.Declaration.t array)
  Hashtbl.t

val make_const : Names.ModPath.t -> Names.Id.t -> Names.constant

val push_const_decl :
  Environ.env ->
  Names.constant * Term.constr option * Declarations.constant_type ->
  Environ.env

val translate_constr :
  ?expected_type:Term.constr ->
  Info.info -> Environ.env -> Term.constr -> Dedukti.term

val translate_types : Info.info -> Environ.env -> Term.types -> Dedukti.term

val lift_let :
  Info.info ->
  Environ.env ->
  Names.name -> Term.constr -> Term.types -> Environ.env * Term.constr

val lift_fix :
  Info.info ->
  Environ.env ->
  Names.name array ->
  Term.types array ->
  Term.constr array ->
  int array -> Environ.env * Context.Rel.Declaration.t array

val translate_rel_context :
  Info.info ->
  Environ.env ->
  Context.Rel.t -> Environ.env * (Dedukti.var * Dedukti.term) list

val translate_args :
  Info.info -> Environ.env -> Term.constr list -> Dedukti.term list
