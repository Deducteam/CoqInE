

val list_chop : int -> 'a list -> 'a list * 'a list

(** [str_starts_with prefix s] returns true iif [s] starts with [prefix]. *)
val str_starts_with : string -> string -> bool

(** Keep only Somes *)
val filter_some : 'a option list -> 'a list

(** Compose a function n times *)
val iterate : int -> ('a -> 'a) -> 'a -> 'a

