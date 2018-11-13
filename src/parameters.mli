
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


(** Enables (true) polymorphism translation *)
val  enable_polymorphism : unit -> unit

(** Disables (true) polymorphism translation *)
val disable_polymorphism : unit -> unit

(** Is (true) polymorphism translation on ? *)
val is_polymorphism_on : unit -> bool


(** Enables template polymorphism translation *)
val  enable_templ_polymorphism : unit -> unit

(** Disables template polymorphism translation *)
val disable_templ_polymorphism : unit -> unit

(** Is template polymorphism translation on ? *)
val is_templ_polymorphism_on : unit -> bool


(** Enables constraints translation *)
val  enable_constraints : unit -> unit

(** Disables constraints translation *)
val disable_constraints : unit -> unit

(** Is constraints translation on ? *)
val is_constraints_on : unit -> bool
