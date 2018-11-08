
val set_universes : UGraph.t -> unit

val translate_universe : Info.info -> Environ.env -> Info.env ->
                         Univ.Universe.t -> Dedukti.term
