(** Translation of Coq libraries *)

val translate_term : Constr.t CAst.t -> unit
(** Translate a single Coq term to a string
    (in an empty context, global environment).
*)

val translate_library : Libnames.qualid -> unit

val translate_qualified_library : Libnames.qualid -> unit

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
val translate_all_but : Libnames.qualid list -> unit
