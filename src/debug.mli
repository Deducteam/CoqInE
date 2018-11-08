open Dedukti

val debug_start : unit -> unit
val debug_stop  : unit -> unit

val debug_to_file : string -> unit

val pp_str       : Pp.t                      printer
val pp_coq_term  : Constr.t                  printer
val pp_coq_type  : Constr.types              printer
val pp_coq_level : Univ.Level.t              printer
val pp_coq_inst  : Univ.Instance.t           printer
val pp_coq_univ  : Univ.Universe.t           printer
val pp_coq_id    : Names.Id.t                printer
val pp_coq_name  : Names.Name.t              printer
val pp_coq_sort  : Sorts.t                   printer
val pp_coq_decl  : Context.Rel.Declaration.t printer
val pp_coq_ctxt  : Context.Rel.t             printer
val pp_coq_env   : Environ.env               printer

(* Try not using these. Instead switch to printers. *)

val debug_str       : Pp.t                      -> unit
val debug_string    : string                    -> unit
val debug_dk_term   : Dedukti.term              -> unit
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

