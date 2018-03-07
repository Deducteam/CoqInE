
open Debug

module StringSet = Set.Make(String)

type env = StringSet.t

let empty () = StringSet.empty

let add_poly_univ_str env v =
  debug "Adding polymorphic universe: %s" v;
  StringSet.add v env

let is_poly_univ_str env v =
  StringSet.mem v env

let add_poly_univ_lvl env lvl =
  add_poly_univ_str env (Univ.Level.to_string lvl)

let rec add_poly_univ_lvl_list env = function
  | [] -> env
  | None   :: tl -> add_poly_univ_lvl_list env                       tl
  | Some u :: tl -> add_poly_univ_lvl_list (add_poly_univ_lvl env u) tl

let is_poly_univ_lvl env l =
  is_poly_univ_str env (Univ.Level.to_string l)

type info = {
  out : Format.formatter;
  library : Names.dir_path;
  module_path : Names.module_path;
  }

let init out dir_path =
  {
    out = out;
    library = dir_path;
    module_path = Names.MPfile(dir_path);
  }
