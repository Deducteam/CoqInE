(** Translation of Coq names *)

open Info

(** There are many different types of names in Coq:
    - identifier: basic identifier (i.e. string)
    - name: names used by binders (i.e. "fun x => ...")
    - dir_path: directory paths (e.g. "A.B.C")
    - label: names of structure elements
    - mod_bound_id: names of functor parameters
    - module_path: module paths (e.g. "<file>.A.B", "<bound_mod_id>.A.B")
    - kernel_name: absolute names of objects
    - constant: refers to a declared constant
    - inductive: refers to an inductive type
    - constructor: refers to a constructor of an inductive type *)

(** Fresh names *)

let full_path info identifier =
  Libnames.make_path (Nametab.dirpath_of_module info.module_path) identifier

(** Push a dummy declaration to declare an identifier globally. *)
let push_global info identifier =
  Nametab.push (Nametab.Until 0) (full_path info identifier) (Globnames.VarRef identifier)

(** Push a dummy declaration to declare an identifier locally. *)
let push_identifier identifier env =
  Environ.push_named
    (Context.Named.Declaration.of_tuple (identifier, None, Constr.mkSort (Term.Prop(Term.Null)))) env

(** Generate a fresh identifier that is different from any constant, inductive
    type, or constructor in the current module, and from any identifier in
    the current local environment.
    If [global] is true, also declare the identifier globally. *)
let fresh_identifier info env ?(global=false) ?prefix identifier =
  let identifier =
    match prefix with
    | None -> identifier
    | Some(prefix) -> Names.Id.of_string (String.concat "_" ([prefix; Names.Id.to_string identifier])) in
  let avoid identifier =
    Nametab.exists_cci (full_path info identifier) ||
    List.mem identifier (Termops.ids_of_context env) in
  let identifier = Namegen.next_ident_away_from identifier avoid in
  if global then push_global info identifier;
  identifier

let fresh_of_string info env ?(global=false) ?prefix str =
  fresh_identifier info env ~global ?prefix (Names.Id.of_string str)

let fresh_of_name info env ?(global=false) ?prefix ~default name =
  match name with
  | Names.Anonymous -> fresh_identifier info env ~global ?prefix (Names.Id.of_string default)
  | Names.Name(identifier) -> fresh_identifier info env ~global ?prefix identifier

let fresh_name info env ?prefix ?default name =
  match name, default with
  | Names.Name(identifier), _ -> Names.Name(fresh_identifier info env ?prefix identifier)
  | Names.Anonymous, None     -> name
  | Names.Anonymous, Some(d)  ->
    Names.Name(fresh_identifier info env ?prefix (Names.Id.of_string d))

let match_function identifier =
  Names.Id.of_string (String.concat "_" ["match"; Names.Id.to_string identifier])

(** Name of the match function for the inductive type *)
let constraint_name index = "cstr_" ^ (string_of_int index)


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

let is_alpha_numerical c = is_alpha c || is_numerical c

(** Escape non-alphanumerical characters using underscores and hexadecimal
    values to be compatible with Dedukti. *)
let escape name =
  let escape_char () c =
    if is_alpha_numerical c then Printf.sprintf "%c" c else
    match c with
    | '.' -> "__"
    | '_' -> "__"
(*    | '\'' -> "_prime_"*)
    | _ -> Printf.sprintf "_%02X_" (Char.code c) in
  let rec escape i () name =
    if i = String.length name
    then ""
    else Printf.sprintf "%a%a" escape_char name.[i] (escape (i + 1)) name in
  escape 0 () name

(** Name translation *)

let translate_identifier identifier =
  escape (Names.Id.to_string identifier)

let translate_name ?(ensure_name = false) name =
  match name with
  | Names.Name(identifier) -> translate_identifier identifier
  | Names.Anonymous -> if ensure_name then failwith "Anonymous name" else ""

let translate_dir_path dir_path =
  escape (Names.DirPath.to_string dir_path)

let translate_label label =
  escape (Names.Label.to_string label)

let translate_mod_bound_id mod_bound_id =
  escape (Names.MBId.to_string mod_bound_id)

let dir_path_of_labels labels =
  let identifiers = List.map Names.Label.to_id labels in
  (* Reverse the order of ids as dirpath elements are given in reverse order. *)
  Names.DirPath.make (List.rev identifiers)

(** Translate the path corresponding to [module_path] followed by [labels]. *)
let rec translate_module_path info env module_path labels =
  match module_path with
  | Names.MPfile(dir_path) ->
      let prefix = translate_dir_path dir_path in
      let suffix = translate_dir_path (dir_path_of_labels (labels)) in
      if dir_path = info.library then suffix
      else String.concat "." [prefix; suffix]
  | Names.MPbound(mod_bound_id) ->
      failwith "Not implemented: MPbound"
  | Names.MPdot(module_path, label) ->
      translate_module_path info env module_path (label :: labels)

(** Translate the name of the structure element in the current module path. *)
let translate_element_name info env label =
  translate_module_path info env info.module_path [label]

let translate_kernel_name info env kernel_name =
  translate_module_path info env (Names.KerName.modpath kernel_name) [Names.KerName.label kernel_name]

let translate_constant info env constant =
  translate_module_path info env (Names.Constant.modpath constant) [Names.Constant.label constant]

let get_inductive_body info env mind i =
  let mind_body = Environ.lookup_mind mind env in
  mind_body.Declarations.mind_packets.(i)

let translate_inductive info env (mind, i) =
  let ind_body = get_inductive_body info env mind i in
  let module_path = Names.MutInd.modpath mind in
  let label = Names.Label.of_id (ind_body.Declarations.mind_typename) in
  translate_module_path info env module_path [label]

let translate_constructor info env ((mind, i), j) =
  let ind_body = get_inductive_body info env mind i in
  let module_path = Names.MutInd.modpath mind in
  let label = Names.Label.of_id (ind_body.Declarations.mind_consnames.(j - 1)) in
  translate_module_path info env module_path [label]

(** The name of the match function for the inductive type [(mind, i)]. *)
let translate_match_function info env (mind, i) =
  let ind_body = get_inductive_body info env mind i in
  let module_path = Names.MutInd.modpath mind in
  let label = Names.Label.of_id (match_function ind_body.Declarations.mind_typename) in
  translate_module_path info env module_path [label]
