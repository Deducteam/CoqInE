(** Translation of Coq libraries *)

val translate_qualified_library : Libnames.qualid -> unit

(** Translates the given library *)
val translate_library : Libnames.reference -> unit

(** Translate the universe table into the "U.dk" file
  Depending on the parameters, it could be:
  - nothing. Universe levels are precomputed and substituted in the translation
  - a list of named universe levels defined to reduce to their precomputed value
  - a list of named universe levels and constraints inhabitants.
 **)
val translate_universes : unit -> unit

(** Translate all loaded libraries. **)
val translate_all : unit -> unit

(** Translate all loaded libraries. **)
val translate_all_but : Libnames.reference list -> unit

val show_universes_constraints : unit -> unit

val show_sorted_universes_constraints : unit -> unit
