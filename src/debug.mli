
(** Sets debugging output to file at given path *)
val debug_to_file : string -> unit

type 'a printer = Format.formatter -> 'a -> unit

(** Prints to debug channel *)
val debug   : ('a, Format.formatter, unit, unit) format4 -> 'a

(** Prints to standard output *)
val message : ('a, Format.formatter, unit, unit) format4 -> 'a

val pp_list  : string -> 'a printer -> 'a list  printer
val pp_array : string -> 'a printer -> 'a array printer
val pp_t : Pp.t printer

val pp_coq_term    : Constr.t                  printer
val pp_coq_type    : Constr.types              printer
val pp_coq_id      : Names.Id.t                printer
val pp_coq_name    : Names.Name.t              printer
val pp_coq_sort    : Sorts.t                   printer
val pp_coq_decl    : Context.Rel.Declaration.t printer
val pp_coq_ctxt    : Context.Rel.t             printer
val pp_coq_env     : Environ.env               printer
val pp_coq_level   : Univ.Level.t              printer
val pp_coq_univ    : Univ.Universe.t           printer
val pp_coq_lvl_arr : Univ.Level.t array        printer
val pp_coq_inst    : Univ.Instance.t           printer

val pp_globname : Globnames.global_reference printer
