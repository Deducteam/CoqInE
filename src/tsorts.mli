
val translate_level : Univ.Level.t -> Dedukti.var

val get_level_vars : Univ.Level.t option list -> Dedukti.var list

val univ_params : Dedukti.var list -> Dedukti.term -> Dedukti.term

val instantiate_univ_params : Dedukti.var -> Univ.Instance.t -> Dedukti.term

val set_universes : UGraph.universes -> unit

val translate_universe : Info.info -> Environ.env -> Info.env ->
                         Univ.universe -> Dedukti.term

