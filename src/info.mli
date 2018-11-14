(** Information about the translation *)


(** Universe and constraints environment.
  Contains information about:
  - locally bounded universe variables from universe polymorphism
  - template polymorphic named variables
  - local constraints
*)
type env

val make : Univ.Constraint.t -> (string*Dedukti.var) list -> env
val is_template_polymorphic : env -> string -> bool
val translate_template_arg : env -> string -> Dedukti.var



type info = {
  out : Format.formatter;
  library : Names.DirPath.t;
  module_path : Names.ModPath.t;
}

val init : Format.formatter -> Names.DirPath.t -> info
