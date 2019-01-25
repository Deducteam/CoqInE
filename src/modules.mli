(** Translation of Coq modules *)

(*----------------------  Dead code  ----------------------

val identifiers_of_mutual_inductive_body :
  Declarations.mutual_inductive_body -> Names.Id.t list

val identifiers_of_structure_field_body :
  Names.Label.t * Declarations.structure_field_body -> Names.Id.t list

val identifiers_of_structure_body :
  (Names.Label.t * Declarations.structure_field_body) list -> Names.Id.t list
*)

val filter_out : string -> unit
(** Disable translation of given symbol. *)

val translate_constant_body :
  Info.info -> Environ.env -> Names.Label.t -> Declarations.constant_body -> unit

val translate_mutual_inductive_body :
  Info.info -> Environ.env -> Names.Label.t -> Declarations.mutual_inductive_body -> unit

val translate_mutual_coinductive_body :
  Info.info -> Environ.env -> Names.Label.t -> Declarations.mutual_inductive_body -> unit

val translate_module_body :
  Info.info -> Environ.env -> Declarations.module_body -> unit

val translate_module_signature :
  Info.info -> Environ.env -> Declarations.module_signature -> unit

val translate_structure_body :
  Info.info -> Environ.env -> Declarations.structure_body -> unit

val translate_structure_field_body :
  Info.info -> Environ.env -> Names.Label.t * Declarations.structure_field_body -> unit
