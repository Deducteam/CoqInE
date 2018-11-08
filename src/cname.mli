(** Translation of Coq names *)

val full_path : Info.info -> Names.Id.t -> Libnames.full_path

val push_global : Info.info -> Names.variable -> unit

(** Push a dummy declaration to declare an identifier locally. *)
val push_identifier : Names.Id.t -> Environ.env -> Environ.env

val fresh_identifier :
  Info.info -> Environ.env ->
  ?global:bool -> ?prefix:string -> Names.Id.t -> Names.Id.t

val fresh_of_string :
  Info.info -> Environ.env ->
  ?global:bool -> ?prefix:string -> string -> Names.Id.t

val fresh_of_name :
  Info.info -> Environ.env ->
  ?global:bool -> ?prefix:string -> default:string -> Names.Name.t -> Names.Id.t

val fresh_name :
  Info.info -> Environ.env ->
  ?prefix:string -> ?default:string -> Names.Name.t -> Names.Name.t

val match_function : Names.Id.t -> Names.Id.t

val is_alpha : char -> bool

val is_numerical : char -> bool

val is_alpha_numerical : char -> bool

val escape : string -> string


val translate_identifier : Names.Id.t -> string

val translate_name : ?ensure_name:bool -> Names.Name.t -> string

val translate_dir_path : Names.DirPath.t -> string

val translate_label : Names.Label.t -> string

val translate_mod_bound_id : Names.MBId.t -> string

val dir_path_of_labels : Names.Label.t list -> Names.DirPath.t

val translate_module_path :
  Info.info -> 'a -> Names.ModPath.t -> Names.Label.t list -> string

val translate_element_name : Info.info -> 'a -> Names.Label.t -> string

val translate_kernel_name : Info.info -> 'a -> Names.KerName.t -> string

val translate_constant : Info.info -> 'a -> Names.Constant.t -> string

val get_inductive_body :
  'a ->
  Environ.env ->
  Names.MutInd.t -> int -> Declarations.one_inductive_body

val translate_inductive :
  Info.info -> Environ.env -> Names.MutInd.t * int -> string

val translate_constructor :
  Info.info -> Environ.env -> (Names.MutInd.t * int) * int -> string

val translate_match_function :
  Info.info -> Environ.env -> Names.MutInd.t * int -> string

                                                                
