val translate_inductive :
  Info.info -> Environ.env ->
  'a -> Declarations.mutual_inductive_body -> int -> unit

val translate_constructors :
  Info.info -> Environ.env ->
  Names.Label.t -> Declarations.mutual_inductive_body -> int -> unit

val translate_match :
  Info.info -> Environ.env ->
  Names.Label.t -> Declarations.mutual_inductive_body -> int -> unit
