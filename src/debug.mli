
val debug_start : unit -> unit
val debug_stop  : unit -> unit

val debug_to_file : string -> unit

type 'a printer = Format.formatter -> 'a -> unit

val debug : ('a, Format.formatter, unit, unit) format4 -> 'a

val pp_list : string -> 'a printer -> 'a list printer
val pp_std_ppcmds : Pp.std_ppcmds printer

val pp_coq_term  : Term.constr         printer
val pp_coq_type  : Term.types          printer
val pp_coq_level : Univ.universe_level printer
val pp_coq_univ  : Univ.universe       printer
val pp_coq_inst  : Univ.universe_instance printer
val pp_coq_id    : Names.Id.t          printer
val pp_coq_name  : Names.Name.t        printer
val pp_coq_sort  : Term.sorts          printer
val pp_coq_decl  : Context.Rel.Declaration.t printer
val pp_coq_ctxt  : Context.Rel.t       printer
val pp_coq_env   : Environ.env         printer
