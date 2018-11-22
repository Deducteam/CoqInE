
type t =
  {
    polymorphism_flag : bool;
    (** Is (true) polymorphism translation on ? *)

    templ_polymorphism_flag : bool;
    (** Is template polymorphism translation on ? *)

    float_univ_flag : bool;
    (** Is floating universe translation on ? *)

    constraints_flag : bool;
    (** Constraints translation ? Only has meaning when float is true. *)

    named_univ_flag : bool;
    (** Should we use universe names or value ? Only has meaning when float is false. *)
    
    readable_translation_flag : bool;
    (** Is (pseudo-)readable translation mode on ? *)

    cast_flag : bool;
    (** Use casts or lifted lambdas ? *)
    
    encoding_name : string;

    (* Module names *)
    system_module   : string;
    universe_module : string;

    (* Encoding's symbol names *)
    t_Sort : string;
    t_univ : string;
    t_Univ : string;
    t_Term : string;
  }
  
(* Some encodings *)
val default          : t (** Ali's encoding *)
val readable_default : t (** Ali's encoding with short names *)
val named_univ       : t (** Ali's encoding with named universes *)
val polymorph        : t (** Polymorphism *)

val set : t -> unit
(** Sets current encoding *)
  
val get : unit -> t
(** Gets current encoding *)

val set_encoding : string -> unit
(** Selects a given encoding in the following list:
    - default
*)
  
val is_polymorphism_on : unit -> bool
(** Is (true) polymorphism translation on ? *)
  
val is_templ_polymorphism_on : unit -> bool
(** Is template polymorphism translation on ? *)

val is_float_univ_on : unit -> bool
(** Is floating universe translation on ? *)

val is_constraints_on : unit -> bool
(** Constraints translation ? Only has meaning when float is true. *)

val is_named_univ_on : unit -> bool
(** Should we use universe names or value ? Only has meaning when float is false. *)

val is_readable_on : unit -> bool
(** Is (pseudo-)readable translation mode on ? *)

val is_cast_on : unit -> bool
(** Use casts or lifted lambdas ? *)
