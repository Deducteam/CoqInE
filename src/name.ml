(** Translation of Coq names *)

(* The name "names.ml" is already taken by a file in the Coq library. *)

(** There are many different types of names in Coq:
    - identifier: basic identifier (i.e. string)
    - name: name used by binders (i.e. "fun x => ...")
    - dir_path: directory paths (e.g. "A.B.C")
    - label: names of structure elements
    - mod_bound_id: names of functor parameters
    - module_path: module paths (e.g. "<file>.A.B", "<bound_mod_id>.A.B")
    - constant: refers to a declared constant
    - inductive: refers to an inductive type
    - constructor: refers to a constructor of an inductive type *)

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
    if is_alpha_numerical c
    then Printf.sprintf "%c" c
    else if c = '_'
    then Printf.sprintf "__"
    else Printf.sprintf "_%02X" (Char.code c) in
  let rec escape i () name =
    if i = String.length name
    then Printf.sprintf ""
    else Printf.sprintf "%a%a" escape_char name.[i] (escape (i + 1)) name in
  escape 0 () name

let coq name =
  Printf.sprintf "Coq.%s" name

let translate_name name =
  match name with
  | Names.Name(identifier) -> Names.string_of_id identifier
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
  let suffix = translate_dir_path (Names.make_dirpath (List.rev ids)) in
  Printf.sprintf "%s.%s" prefix suffix

let translate_constant constant =
  translate_module_path (Names.con_modpath constant) [Names.con_label constant]

