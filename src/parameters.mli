
(** Enables debugging *)
val  enable_debug : unit -> unit

(** Disables debugging *)
val disable_debug : unit -> unit


(** Starts debugging *)
val debug_start : unit -> unit

(** Stops debugging *)
val debug_stop  : unit -> unit


(** Is debugging on ? *)
val is_debug_on : unit -> bool


(** Enables polymorphism translation *)
val  enable_polymorphism : unit -> unit

(** Disables polymorphism translation *)
val disable_polymorphism : unit -> unit

(** Is polymorphism translation on ? *)
val is_polymorphism_on : unit -> bool

