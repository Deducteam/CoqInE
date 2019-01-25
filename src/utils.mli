

val list_chop : int -> 'a list -> 'a list * 'a list

val str_starts_with : string -> string -> bool
(** [str_starts_with prefix s] returns true iif [s] starts with [prefix]. *)

val truncate : string -> int -> string
(** [truncate str l] returns the remaining part of [str] when removing the [l]
    first characters. *)

val filter_some : 'a option list -> 'a list
(** Filters Nones out of a list  *)

val iterate : int -> ('a -> 'a) -> 'a -> 'a
(** Compose a function n times *)

