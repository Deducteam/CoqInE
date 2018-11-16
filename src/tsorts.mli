
val add_sort_params : Dedukti.var list -> Dedukti.term -> Dedukti.term

val instantiate_univ_params :
  Info.env -> Dedukti.var -> Univ.AUContext.t -> Univ.Instance.t -> Dedukti.term

val set_universes : UGraph.t -> unit

val translate_level    : Info.env -> Univ.Level.t    -> Dedukti.cic_universe
val translate_universe : Info.env -> Univ.Universe.t -> Dedukti.cic_universe

val translate_template_params : Univ.Level.t option list -> (string * Dedukti.var) list

val translate_univ_poly_params : Univ.Instance.t -> string list
