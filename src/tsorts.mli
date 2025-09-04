
val translate_constraint :
  Info.env -> Univ.univ_constraint -> (Dedukti.term*Dedukti.term) option
(** From local environment
    [
      c1  :  a1  =/<  b1,
      ...,
      cn  :  an  =/<  bn
    ]
  builds the constraint cstr inhabitant of the given constraint type
    cstr : u =/< v

  This constraint may be trivial (I), one of the ci of a more complex object
  relying on transitivity construction (cumul_trans ...).
*)

val translate_constraints_as_conjunction :
  Info.env -> Univ.Constraint.t -> (Dedukti.term*Dedukti.term) list
(** From local environment
    [
      c1  :  a1  =/<  b1,
      ...,
      cn  :  an  =/<  bn
    ]
  builds a list of constraint names
    [ cstri ; cstrk ]
  for the given constraint set
    { ui =/< vi, ..., uk =/< vk }

  This will later be translated into
    pair cstri (pair ... (pair cstrj cstrk)...)
  of type
    and (Eq/Cumul ui vi) (and ... (and (Eq/Cumul uj vj) (Eq/Cumul uk vk))...)
*)

val template_constructor_upoly : unit -> bool
(** Returns true if the constructors of template universe polymorphic
    inductive types should have quantified universe parameters. *)


type cstr = Univ.univ_constraint * (Dedukti.var * Dedukti.term * Dedukti.term)
(** A constraints together with
    - a variable name
    - a term representing the constraint
    - a type representing the constraint proof space
*)

val add_poly_params_type : Dedukti.var list -> cstr list -> Dedukti.term -> Dedukti.term
(** Prepend universe parameters (levels and constraints)
    before universe poymorphic type definition *)

val add_poly_params_def  : Dedukti.var list -> cstr list -> Dedukti.term -> Dedukti.term
(** Prepend universe parameters (levels and constraints)
    before universe polymorphic definition *)

val add_poly_env_def  : Info.env -> Dedukti.term -> Dedukti.term
val add_poly_env_type : Info.env -> Dedukti.term -> Dedukti.term

val get_inductive_params :
    Dedukti.var list ->
    Dedukti.var list ->
    cstr list ->
    (Dedukti.var * Dedukti.term) list
val add_inductive_params :
    Dedukti.var list ->
    Dedukti.var list ->
    cstr list ->
    Dedukti.term -> Dedukti.term
(** Prepend universe parameters before inductive types *)

val get_constructor_params :
    Dedukti.var list ->
    Dedukti.var list ->
    cstr list ->
    (Dedukti.var * Dedukti.term) list
val add_constructor_params :
    Dedukti.var list ->
    Dedukti.var list ->
    cstr list ->
    Dedukti.term -> Dedukti.term
(** Prepend universe parameters before inductive constructor *)


val get_poly_univ_params :
  Info.env -> Univ.AUContext.t -> Univ.Instance.t -> Dedukti.term list

val instantiate_poly_univ_constant :
  Environ.env -> Info.env -> Names.Constant.t * Univ.Instance.t -> Dedukti.term -> Dedukti.term

val instantiate_poly_ind_univ_params :
  Environ.env -> Info.env -> Names.inductive -> Univ.Instance.t -> Dedukti.term -> Dedukti.term

val instantiate_template_ind_univ_params :
  Environ.env -> Info.env -> Names.inductive -> Univ.Instance.t -> Dedukti.term -> Dedukti.term

(*
val instantiate_ind_univ_params :
  Environ.env -> Info.env -> Dedukti.var -> Names.inductive -> Univ.Instance.t -> Dedukti.term
*)

val set_universes : UGraph.t -> unit

val translate_template_global_level_decl : Univ.Level.t option list -> Dedukti.instruction list

val level_as_level    : Info.env -> Univ.Level.t    -> Translator.level_expr
val level_as_universe : Info.env -> Univ.Level.t    -> Translator.universe_expr

val translate_universe   : Info.env -> Univ.Universe.t -> Translator.universe_expr
val translate_sort       : Info.env -> Sorts.t         -> Translator.universe_expr

val convertible_sort   : Info.env -> Sorts.t -> Sorts.t -> bool

val translate_template_name : Univ.Level.t -> Dedukti.var

val translate_template_params :
  Univ.Level.t option list -> Univ.Level.t list * Dedukti.var list

val translate_univ_poly_params : Univ.Instance.t -> string list

val translate_univ_poly_constraints : Univ.Constraint.t -> cstr list


val gather_eq_types : Context.Rel.t -> Context.Rel.t -> (Constr.types * Constr.types) list
val enforce_eq_types :
  Univ.Constraint.t -> (Constr.t * Constr.t) list -> Univ.Constraint.t
