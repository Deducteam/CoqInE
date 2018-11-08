
module StringSet = Set.Make(String)

type env = StringSet.t

let empty () = StringSet.empty

let add_poly_univ_str env v =
  StringSet.add v env

let is_poly_univ_str env v =
  StringSet.mem v env

let add_poly_univ_lbl env l =
  add_poly_univ_str env (Univ.Level.to_string l)

let is_poly_univ_lbl env l =
  is_poly_univ_str env (Univ.Level.to_string l)

type info = {
  out : Format.formatter;
  library : Names.DirPath.t;
  module_path : Names.ModPath.t;
  }

let init out dir_path = {
  out = out;
  library = dir_path;
  module_path = Names.MPfile(dir_path);
  }

let universe_env =
  let aux acc = function
  | None   -> acc
  | Some u -> add_poly_univ_lbl acc u in
  List.fold_left aux (empty ())
