
val list_init : 'a -> int -> 'a list
(** [list_init e n] creates the list of size [n] containing only [e]. *)

val list_chop : int -> 'a list -> 'a list * 'a list
(** [list_chop i l] splits [l] into two lists [(l1,l2)] such that
    [l1++l2=l] and [l1] has length [i].
    It raises [Failure] when [i] is negative or greater than the length of [l]  *)

val str_starts_with : string -> string -> bool
(** [str_starts_with prefix s] returns true iif [s] starts with [prefix]. *)

val truncate : string -> int -> string
(** [truncate str l] returns the remaining part of [str] when removing the [l]
    first characters. *)

val filter_some : 'a option list -> 'a list
(** Filters Nones out of a list  *)

val count_some : 'a option list -> int
(** Counts the number of Somes in a list  *)

val iterate : int -> ('a -> 'a) -> 'a -> 'a
(** Compose a function n times *)

val map_opt : ('a -> 'b) -> 'a option -> 'b option
