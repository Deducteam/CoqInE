(** General information about the translation *)


val set_destination : string -> unit
(** Sets the folder in which generated files should be output *)

type info = private
  {
    out         : out_channel;
    fmt         : Format.formatter;
    library     : Names.DirPath.t;
    module_path : Names.ModPath.t;
  }

(** Creates an info for given module written into given file *)
val init : Names.ModPath.t -> string -> info

(** Updates the info to match a certain label inside the current module *)
val update : info -> Names.Label.t -> info

(** Flushes out the output channel and close it *)
val close : info -> unit


(** Universe and constraints environment.
  Contains information about:
  - locally bounded universe variables from universe polymorphism
  - template polymorphic named variables
  - local constraints
*)
type env

val make :
  Univ.Level.t list -> Dedukti.var list ->
  int -> (Dedukti.term * Dedukti.term * Univ.Constraint.elt) list ->
  env

val is_template_polymorphic    : env -> Univ.Level.t -> bool
val translate_template_arg     : env -> Univ.Level.t -> Dedukti.var
val try_translate_template_arg : env -> Univ.Level.t -> Dedukti.var option

val dummy : env
