(** Translation of Coq names *)

val full_path : Info.info -> Names.Id.t -> Libnames.full_path

val push_global : Info.info -> Names.variable -> unit

val push_identifier : Names.Id.t -> Environ.env -> Environ.env

val fresh_identifier :
  Info.info -> Environ.env ->
  ?global:bool -> ?prefix:string -> Names.identifier -> Names.Id.t

val fresh_of_string :
  Info.info -> Environ.env ->
  ?global:bool -> ?prefix:string -> string -> Names.Id.t

val fresh_of_name :
  Info.info -> Environ.env ->
  ?global:bool -> ?prefix:string -> default:string -> Names.name -> Names.Id.t

val fresh_name :
  Info.info -> Environ.env ->
  ?prefix:string -> ?default:string -> Names.name -> Names.name

val match_function : Names.identifier -> Names.identifier

val is_alpha : char -> bool

val is_numerical : char -> bool

val is_alpha_numerical : char -> bool

val escape : string -> string


val coq : string -> string

val translate_identifier : Names.identifier -> string

val translate_name : ?ensure_name:bool -> Names.name -> string

val translate_dir_path : Names.dir_path -> string

val translate_label : Names.label -> string

val translate_mod_bound_id : Names.mod_bound_id -> string

val dir_path_of_labels : Names.label list -> Names.dir_path

val translate_module_path :
  Info.info -> 'a -> Names.module_path -> Names.label list -> string

val translate_element_name : Info.info -> 'a -> Names.label -> string

val translate_kernel_name : Info.info -> 'a -> Names.kernel_name -> string

val translate_constant : Info.info -> 'a -> Names.constant -> string

val get_inductive_body :
  'a ->
  Environ.env ->
  Names.mutual_inductive -> int -> Declarations.one_inductive_body

val translate_inductive :
  Info.info -> Environ.env -> Names.mutual_inductive * int -> string

val translate_constructor :
  Info.info -> Environ.env -> (Names.mutual_inductive * int) * int -> string

val translate_match_function :
  Info.info -> Environ.env -> Names.mutual_inductive * int -> string

                                                                
