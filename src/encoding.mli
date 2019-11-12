val flag : string -> bool
(** Is given flag set ?  (See ml file for flag details). *)

val set_flag  : string -> bool -> unit
(** Set given flag. *)

val symb : string -> string
(** Is given symbol defined ?  (See ml file for symbols details). *)

val set_param  : string -> string -> unit
val set_params : (string*string) list -> unit
(** Sets given parameter to given value *)

val is_polymorphism_on : unit -> bool
(** Is (true) polymorphism translation on ? *)

val is_templ_polymorphism_on : unit -> bool
(** Is template polymorphism translation on ? *)

val is_templ_polymorphism_code_on : unit -> bool
(** Is there a private version of template polymorphic types
    to properly handle template polymorphism sort irrelevance ? *)

val is_float_univ_on : unit -> bool
(** Is floating universe translation on ? *)

val is_constraints_on : unit -> bool
(** Constraints translation ? Only has meaning when float is true. *)

val is_named_univ_on : unit -> bool
(** Should we use universe names or value ? Only has meaning when float is false. *)

val need_universe_file : unit -> bool
(** Do we need a universe file ? *)

val is_readable_on : unit -> bool
(** Is (pseudo-)readable translation mode on ? *)

val is_cast_on : unit -> bool
(** Use casts or lifted lambdas ? *)

val is_letins_simpl : unit -> bool
(** Translate let-in as simpl beta redices or not ? *)

val is_fixpoint_inlined : unit -> bool
(** Translate fixpoints as external body or generic unsafe fixpoint operator ?*)

val is_argument_casted : unit -> bool
(** Always cast arguments ? *)
