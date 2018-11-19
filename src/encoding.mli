
module Enc :
sig
  type t =
    {
      polymorphism_flag : bool;
      (** Is (true) polymorphism translation on ? *)
      
      templ_polymorphism_flag : bool;
      (** Is template polymorphism translation on ? *)

      constraints_flag : bool;
      (** Constraints translation ? *)
      
      float_univ_flag : bool;
      (** Is floating universe translation on ? *)

      readable_translation_flag : bool;
      (** Is (pseudo-)readable translation mode on ? *)

      (* Module names *)
      system_module   : string;
      universe_module : string;

      (* Translation constants *)
      t_Sort : string;
    }

  (* Some encodings *)
  val default          : t (** Ali's encoding *)
  val readable_default : t (** Ali's encoding with short names *)
  val polymorph        : t (** Polymorphism *)

  val set : t -> unit
  (** Sets current encoding *)

  val get : unit -> t
  (** Gets current encoding *)
end


val set_encoding : string -> unit
(** Selects a given encoding in the following list:
  - default
*)

val is_polymorphism_on : unit -> bool
(** Is (true) polymorphism translation on ? *)

val is_templ_polymorphism_on : unit -> bool
(** Is template polymorphism translation on ? *)

val is_constraints_on : unit -> bool
(** Is constraints translation on ? *)

val is_float_univ_on : unit -> bool
(** Is floating universe translation on ? *)

val is_readable_on : unit -> bool
(** Is (pseudo-)readable translation mode on ? *)
