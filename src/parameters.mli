
val  enable_debug : unit -> unit
(** Enables debugging *)

val disable_debug : unit -> unit
(** Disables debugging *)


val debug_start : unit -> unit
(** Starts debugging *)

val debug_stop  : unit -> unit
(** Stops debugging *)

val is_debug_on : unit -> bool
(** Is debugging on ? *)


val  enable_polymorphism : unit -> unit
(** Enables (true) polymorphism translation *)

val disable_polymorphism : unit -> unit
(** Disables (true) polymorphism translation *)

val is_polymorphism_on : unit -> bool
(** Is (true) polymorphism translation on ? *)


val  enable_templ_polymorphism : unit -> unit
(** Enables template polymorphism translation *)

val disable_templ_polymorphism : unit -> unit
(** Disables template polymorphism translation *)

val is_templ_polymorphism_on : unit -> bool
(** Is template polymorphism translation on ? *)


val  enable_constraints : unit -> unit
(** Enables constraints translation *)

val disable_constraints : unit -> unit
(** Disables constraints translation *)

val is_constraints_on : unit -> bool
(** Is constraints translation on ? *)


val  enable_float_univ : unit -> unit
(** Enables floating universe translation *)

val disable_float_univ : unit -> unit
(** Disables floating universe translation *)

val is_float_univ_on : unit -> bool
(** Is floating universe translation on ? *)


val set_destination : string -> unit
(** Sets the folder in which generated files should be output *)

val get_destination_path : string -> string
(** Returns path in output folder from relative path *)
