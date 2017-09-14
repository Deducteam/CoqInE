(** Translation of Coq modules *)


val identifiers_of_mutual_inductive_body :
  Declarations.mutual_inductive_body -> Names.Id.t list

val identifiers_of_structure_field_body :
  Names.label * Declarations.structure_field_body -> Names.Id.t list

val identifiers_of_structure_body :
  (Names.label * Declarations.structure_field_body) list -> Names.Id.t list


val translate_constant_body :
  Info.info -> Environ.env -> Names.label -> Declarations.constant_body -> unit

val translate_mutual_inductive_body :
  Info.info -> Environ.env -> Names.Label.t -> Declarations.mutual_inductive_body -> unit

val translate_module_body :
  Info.info -> Environ.env -> Declarations.module_body -> unit

val translate_structure_body :
  Info.info -> Environ.env -> Declarations.structure_body -> unit

val translate_structure_field_body :
  Info.info -> Environ.env -> Names.label * Declarations.structure_field_body -> unit
