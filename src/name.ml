(** Translation of Coq names *)

(* The name "names.ml" is already taken by a file in the Coq library. *)

open Environment

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

let fresh_identifier ?(global=false) ?(prefix=[]) env identifier =
  let avoid = Termops.ids_of_context env.env @ !(env.globals) in
  let identifier = Names.id_of_string (String.concat "_" (prefix@ [Names.string_of_id identifier])) in
  let identifier = Namegen.next_ident_away identifier avoid in
  if global then Environment.declare_global env identifier;
  identifier

let fresh_identifier_of_string ?(global=false) ?(prefix=[]) env str =
  fresh_identifier ~global ~prefix env (Names.id_of_string str)

let fresh_identifier_of_name ?(global=false) ?(prefix=[]) ~default env name =
  match name with
  | Names.Anonymous -> fresh_identifier ~global ~prefix env (Names.id_of_string default)
  | Names.Name(identifier) -> fresh_identifier ~global ~prefix env identifier

let fresh_name ?(global=false) ?(prefix=[]) ?default env name =
  match name, default with
  | Names.Anonymous, None -> name
  | Names.Anonymous, Some(default) ->
      Names.Name(fresh_identifier ~global ~prefix env (Names.id_of_string default))
  | Names.Name(identifier), _ ->
      Names.Name(fresh_identifier ~global ~prefix env identifier)

(** Name of the match function for the inductive type *)
let match_function identifier =
  Names.id_of_string (String.concat "_" ["match"; Names.string_of_id identifier])

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
  let escape_char () c =
    if is_alpha_numerical c then Printf.sprintf "%c" c else
    match c with
    | '.' -> "_dot_"
    | '_' -> "__"
    | '\'' -> "_prime_"
    | _ -> Printf.sprintf "_%02X_" (Char.code c) in
  let rec escape i () name =
    if i = String.length name
    then ""
    else Printf.sprintf "%a%a" escape_char name.[i] (escape (i + 1)) name in
  escape 0 () name

(** Name translation *)

let coq name =
  Printf.sprintf "Coq.%s" name

let translate_identifier identifier =
  escape (Names.string_of_id identifier)

let translate_name ?(ensure_name = false) name =
  match name with
  | Names.Name(identifier) -> translate_identifier identifier
  | Names.Anonymous -> if ensure_name then failwith "Anonymous name" else ""

let translate_dir_path dir_path =
  escape (Names.string_of_dirpath dir_path)

let translate_label label =
  escape (Names.string_of_label label)

let translate_mod_bound_id mod_bound_id =
  escape (Names.string_of_mbid mod_bound_id)

let dir_path_of_labels labels =
  let identifiers = List.map Names.id_of_label labels in
  (* Reverse the order of ids as dirpath elements are given in reverse order. *)
  Names.make_dirpath (List.rev identifiers)

(** Translate the path corresponding to [module_path] followed by [labels]. *)
let rec translate_module_path env module_path labels =
  match module_path with
  | Names.MPfile(dir_path) ->
      let prefix = translate_dir_path dir_path in
      let suffix = translate_dir_path (dir_path_of_labels (labels)) in
      if dir_path = env.library then suffix
      else String.concat "." [prefix; suffix]
  | Names.MPbound(mod_bound_id) ->
      failwith "Not implemented: MPbound"
  | Names.MPdot(module_path, label) ->
      translate_module_path env module_path (label :: labels)

let translate_kernel_name env kernel_name =
  translate_module_path env (Names.modpath kernel_name) [Names.label kernel_name]

let translate_constant env constant =
  translate_module_path env (Names.con_modpath constant) [Names.con_label constant]

let get_inductive_body env mind i =
  let mind_body = Environ.lookup_mind mind env.env in
  mind_body.Declarations.mind_packets.(i)

let translate_inductive env (mind, i) =
  let ind_body = get_inductive_body env mind i in
  let module_path = Names.mind_modpath mind in
  let label = Names.label_of_id (ind_body.Declarations.mind_typename) in
  translate_module_path env module_path [label]

let translate_constructor env ((mind, i), j) =
  let ind_body = get_inductive_body env mind i in
  let module_path = Names.mind_modpath mind in
  let label = Names.label_of_id (ind_body.Declarations.mind_consnames.(j - 1)) in
  translate_module_path env module_path [label]

(** The name of the match function for the inductive type [(mind, i)]. *)
let translate_match_function env (mind, i) =
  let ind_body = get_inductive_body env mind i in
  let module_path = Names.mind_modpath mind in
  let label = Names.label_of_id (match_function ind_body.Declarations.mind_typename) in
  translate_module_path env module_path [label]


