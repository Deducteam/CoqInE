
val debug_start : unit -> unit
val debug_stop  : unit -> unit

val debug_to_file : string -> unit

val debug_str       : Pp.t         -> unit
val debug_string    : string       -> unit
val debug_dk_term   : Dedukti.term -> unit

val pt_coq_term  : Constr.t                  -> Pp.t
val pt_coq_type  : Constr.types              -> Pp.t
val pt_coq_level : Univ.Level.t              -> Pp.t
val pt_coq_inst  : Univ.Instance.t           -> Pp.t
val pt_coq_univ  : Univ.Universe.t           -> Pp.t
val pt_coq_id    : Names.Id.t                -> Pp.t
val pt_coq_name  : Names.Name.t              -> Pp.t
val pt_coq_sort  : Sorts.t                   -> Pp.t
val pt_coq_decl  : Context.Rel.Declaration.t -> Pp.t
val pt_coq_ctxt  : Context.Rel.t             -> Pp.t
val pt_coq_env   : Environ.env               -> Pp.t
val debug_coq_term  : Constr.t                  -> unit
val debug_coq_type  : Constr.types              -> unit
val debug_coq_level : Univ.Level.t              -> unit
val debug_coq_inst  : Univ.Instance.t           -> unit
val debug_coq_univ  : Univ.Universe.t           -> unit
val debug_coq_id    : Names.Id.t                -> unit
val debug_coq_name  : Names.Name.t              -> unit
val debug_coq_sort  : Sorts.t                   -> unit
val debug_coq_decl  : Context.Rel.Declaration.t -> unit
val debug_coq_ctxt  : Context.Rel.t             -> unit
val debug_coq_env   : Environ.env               -> unit

