
val debug_start : unit -> unit
val debug_stop  : unit -> unit

val debug_to_file : string -> unit

val debug_str       : Pp.std_ppcmds -> unit
val debug_string    : string        -> unit
val debug_dk_term   : Dedukti.term  -> unit

val pt_coq_term  : Term.constr         -> Pp.std_ppcmds
val pt_coq_type  : Term.types          -> Pp.std_ppcmds
val pt_coq_level : Univ.universe_level -> Pp.std_ppcmds
val pt_coq_univ  : Univ.universe       -> Pp.std_ppcmds
val pt_coq_inst  : Univ.universe_instance -> Pp.std_ppcmds
val pt_coq_id    : Names.Id.t          -> Pp.std_ppcmds
val pt_coq_name  : Names.Name.t        -> Pp.std_ppcmds
val pt_coq_sort  : Term.sorts          -> Pp.std_ppcmds
val pt_coq_decl  : Context.Rel.Declaration.t -> Pp.std_ppcmds
val pt_coq_ctxt  : Context.Rel.t       -> Pp.std_ppcmds
val pt_coq_env   : Environ.env         -> Pp.std_ppcmds

val debug_coq_term  : Term.constr         -> unit
val debug_coq_type  : Term.types          -> unit
val debug_coq_level : Univ.universe_level -> unit
val debug_coq_univ  : Univ.universe       -> unit
val debug_coq_inst  : Univ.universe_instance -> unit
val debug_coq_id    : Names.Id.t          -> unit
val debug_coq_name  : Names.Name.t        -> unit
val debug_coq_sort  : Term.sorts          -> unit
val debug_coq_decl  : Context.Rel.Declaration.t -> unit
val debug_coq_ctxt  : Context.Rel.t       -> unit
val debug_coq_env   : Environ.env         -> unit

