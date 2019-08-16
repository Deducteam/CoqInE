
val debug_to_file : string -> unit
(** Sets debugging output to file at given path *)

val  enable_debug : unit -> unit
(** Enables debugging *)

val disable_debug : unit -> unit
(** Disables debugging *)


val  enable_verbose : unit -> unit
(** Enables verbose mode *)

val disable_verbose : unit -> unit
(** Disables verbose mode *)


val add_debug_lib : string -> unit
(** Add library name to debug list *)

val is_debug_lib  : Pp.t -> bool
(** Should debug be enabled for given library ? *)


val debug_start : unit -> unit
(** Starts debugging *)

val debug_stop  : unit -> unit
(** Stops debugging *)

val is_debug_on : unit -> bool
(** Is debugging on ? *)


val errdebug : ('a, Format.formatter, unit, unit) format4 -> 'a
(** Prints to error channel *)

val debug   : ('a, Format.formatter, unit, unit) format4 -> 'a
(** Prints to debug channel *)

val message : ('a, Format.formatter, unit, unit) format4 -> 'a
(** Prints to standard output *)

val verbose : ('a, Format.formatter, unit, unit) format4 -> 'a
(** Prints to standard output only if verbose mode is enabled *)

type 'a printer = Format.formatter -> 'a -> unit

val pp_list   : string -> 'a printer -> 'a list   printer
val pp_array  : string -> 'a printer -> 'a array  printer
val pp_option : string -> 'a printer -> 'a option printer
val pp_t : Pp.t printer

val pp_coq_term    : Constr.t           printer
val pp_coq_type    : Constr.types       printer
val pp_coq_id      : Names.Id.t         printer
val pp_coq_label   : Names.Label.t      printer
val pp_coq_name    : Names.Name.t       printer
val pp_coq_sort    : Sorts.t            printer
val pp_coq_decl       : Context.Rel.Declaration.t printer
val pp_coq_arity_ctxt : Context.Rel.Declaration.t list printer
val pp_coq_ctxt    : Context.Rel.t      printer
val pp_coq_env     : Environ.env        printer
val pp_coq_level   : Univ.Level.t       printer
val pp_coq_univ    : Univ.Universe.t    printer
val pp_coq_lvl_arr : Univ.Level.t array printer
val pp_coq_inst    : Univ.Instance.t    printer
val pp_fixpoint    : (Constr.constr,Constr.types) Constr.pfixpoint printer
val pp_coq_constraint : Univ.univ_constraint printer
val pp_uenv_cstr : (Univ.univ_constraint * (string * 'a) ) list printer
val pp_globname : Globnames.global_reference printer
