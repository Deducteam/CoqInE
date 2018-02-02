(** Translation of Coq libraries *)


val set_destination : string -> unit

val set_debug : string -> unit

val translate_qualified_library : Libnames.qualid -> unit

val translate_library : Libnames.reference -> unit

val translate_all : unit -> unit

val show_universes_constraints : unit -> unit

val show_sorted_universes_constraints : unit -> unit

val debug : unit -> unit

val test : unit -> unit
