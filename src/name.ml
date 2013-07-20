(** Translation of Coq names *)

(* The name "names.ml" is already taken by a file in the Coq library. *)

(** There are many different types of names in Coq:
    - identifier: basic identifier (i.e. string)
    - name: name used by binders (i.e. "fun x => ...")
    - dir_path: directory paths (e.g. "A.B.C")
    - label: names of structure elements
    - mod_bound_id: names of functor parameters
    - module_path: module paths (e.g. "<file>.A.B", "<bound_mod_id>.A.B")
    - kernel_name: absolute names of objects
    - constant: refers to a declared constant
    - inductive: refers to an inductive type
    - constructor: refers to a constructor of an inductive type *)

(** Escaping *)

let is_alpha c =
  match c with
  | 'a' .. 'z'
  | 'A' .. 'Z' -> true
  | _ -> false

let is_numerical c =
  match c with
  | '0' .. '9' -> true
  | _ -> false

let is_alpha_numerical c =
  is_alpha c || is_numerical c

(** Escape non-alphanumerical characters using underscores and hexadecimal
    values to be compatible with Dedukti. *)
let escape name =
  (* Use Printf.sprintf for efficiency. *)
  let escape_char () c =
    if is_alpha_numerical c then Printf.sprintf "%c" c else
    match c with
    | '.' -> "_dot_"
    | '_' -> "__"
    | '\'' -> "_prime_"
    | _ -> Printf.sprintf "_%02X_" (Char.code c) in
  let rec escape i () name =
    if i = String.length name
    then Printf.sprintf ""
    else Printf.sprintf "%a%a" escape_char name.[i] (escape (i + 1)) name in
  escape 0 () name

(** Name mangling *)

(** Mangle generated names with multiple parts to avoid clashes with 
    the translated variable names. *)
let mangle name_parts =
  String.concat "_"  ("" :: name_parts)

(** Convert [name] to a string, apply [f] to [prefix; name] and convert back.
    This pattern is used to process the different name types uniformly. *)
let process of_string f prefix to_string x =
  of_string (f (prefix @ [to_string x]))

let mangle_identifier prefix identifier =
  process Names.id_of_string mangle prefix Names.string_of_id identifier

let mangle_label prefix label =
  process Names.mk_label mangle prefix Names.string_of_label label

(** Name generation *)

(** Generate a fresh name from [name_parts] using a unique integer suffix. *)
let fresh =
  let counter = ref 0 in
  fun name_parts ->
    incr counter;
    mangle (name_parts @ [string_of_int !counter])

let fresh_identifier prefix identifier =
  process Names.id_of_string fresh prefix Names.string_of_id identifier

let fresh_label prefix label =
  process Names.mk_label fresh prefix Names.string_of_label label

let identifier_of_name name =
  match name with
  | Names.Name(identifier) -> identifier
  | Names.Anonymous -> Names.id_of_string "_"

(** Name of let constants *)
let fresh_let name =
  fresh_identifier ["let"] (identifier_of_name name)

let get_inductive_body env mind i =
  let mind_body = Environ.lookup_mind mind env in
  mind_body.Declarations.mind_packets.(i)

(** Name of the match function for the inductive [(mind, i)]*)
let match_function env (mind, i) =
  let ind_body = get_inductive_body env mind i in
  let label = Names.label_of_id (ind_body.Declarations.mind_typename) in
  mangle_label ["match"] label

(** Name translation *)

let coq name =
  Printf.sprintf "Coq.%s" name

let translate_identifier identifier =
  escape (Names.string_of_id identifier)

let translate_name name =
  match name with
  | Names.Name(identifier) -> translate_identifier identifier
  | Names.Anonymous -> ""

let translate_dir_path dir_path =
  escape (Names.string_of_dirpath dir_path)

let translate_label label =
  escape (Names.string_of_label label)

let translate_mod_bound_id mod_bound_id =
  escape (Names.string_of_mbid mod_bound_id)

(** Translate the path corresponding to [module_path] followed by [labels]. *)
let translate_module_path module_path labels =
  let rec split module_path labels =
    match module_path with
    | Names.MPfile(dir_path) -> (translate_dir_path dir_path, labels)
    | Names.MPbound(mod_bound_id) -> (translate_mod_bound_id mod_bound_id, labels)
    | Names.MPdot(module_path, label) -> split module_path (label :: labels) in
  let (prefix, labels) = split module_path labels in
  let ids = List.map Names.id_of_label labels in
  (* Reverse the order of ids as dirpath elements are given in reverse order. *)
  let dir_path = Names.make_dirpath (List.rev ids) in
  let suffix = translate_dir_path dir_path in
  Printf.sprintf "%s.%s" prefix suffix

let translate_kernel_name env kernel_name =
  translate_module_path (Names.modpath kernel_name) [Names.label kernel_name]

let translate_constant env constant =
  translate_module_path (Names.con_modpath constant) [Names.con_label constant]

let translate_inductive env (mind, i) =
  let ind_body = get_inductive_body env mind i in
  let module_path = Names.mind_modpath mind in
  let label = Names.label_of_id (ind_body.Declarations.mind_typename) in
  translate_module_path module_path [label]

let translate_constructor env ((mind, i), j) =
  let ind_body = get_inductive_body env mind i in
  let module_path = Names.mind_modpath mind in
  let label = Names.label_of_id (ind_body.Declarations.mind_consnames.(j - 1)) in
  translate_module_path module_path [label]

(** The name of the match function for the inductive type [(mind, i)]. *)
let translate_match_function env (mind, i) =
  let module_path = Names.mind_modpath mind in
  let label = match_function env (mind, i) in
  translate_module_path module_path [label]

