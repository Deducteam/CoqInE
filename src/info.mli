(** Information about the translation *)

type env

val empty : unit -> env

val add_poly_univ_str : env -> string       -> env
val add_poly_univ_lbl : env -> Univ.Level.t -> env

val is_poly_univ_str : env -> string       -> bool
val is_poly_univ_lbl : env -> Univ.Level.t -> bool

type info = {
  out : Format.formatter;
  library : Names.DirPath.t;
  module_path : Names.ModPath.t;
}

val init : Format.formatter -> Names.DirPath.t -> info

val universe_env : Univ.Level.t option list -> env
