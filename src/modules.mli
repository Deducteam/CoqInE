(** Translation of Coq modules *)

val  enable_failproofmode : unit -> unit
(** Enables failproof mode *)

val disable_failproofmode : unit -> unit
(** Disables failproof mode *)

val filter_out : string -> unit
(** Disables translation of given symbol. *)

val translate_constant_body :
  Info.info -> Environ.env -> Names.KerName.t option -> Names.Label.t -> Declarations.constant_body -> unit

val translate_mutual_inductive_body :
  Info.info -> Environ.env -> Names.KerName.t option -> Names.Label.t -> Declarations.mutual_inductive_body -> unit

val translate_mutual_coinductive_body :
  Info.info -> Environ.env -> Names.KerName.t option -> Names.Label.t -> Declarations.mutual_inductive_body -> unit

val translate_module_body :
  Info.info -> Environ.env -> Declarations.module_body -> unit

val translate_module_signature :
  Info.info -> Environ.env -> Mod_subst.delta_resolver -> Declarations.module_signature -> unit

val translate_structure_field_body :
  Info.info -> Environ.env -> Mod_subst.delta_resolver -> Names.Label.t * Declarations.structure_field_body -> unit
