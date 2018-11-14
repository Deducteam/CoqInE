
val translate_level : Univ.Level.t -> Dedukti.term

val add_sort_params : Dedukti.var list -> Dedukti.term -> Dedukti.term

val instantiate_univ_params : Dedukti.var -> Univ.Instance.t -> Dedukti.term

val set_universes : UGraph.t -> unit

val translate_universe : Info.info -> Environ.env -> Info.env ->
                         Univ.Universe.t -> Dedukti.term

val translate_template_params : Univ.Level.t option list -> (string * Dedukti.var) list

val translate_univ_poly_params : Univ.Instance.t -> string list
