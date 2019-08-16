
module LevelMap = Map.Make(
  struct
    type t = Univ.Level.t
    let compare = Univ.Level.compare
  end)

type env =
  {
    template_params : Dedukti.var LevelMap.t;
    constraints : ( Univ.univ_constraint * (Dedukti.var * Dedukti.term) ) list
  }

let make
    (template_levels : Univ.Level.t list)
    (template_names  : Dedukti.var list)
    (nb_polymorphic_args : int)
    (constraints_args : ( Univ.univ_constraint * (Dedukti.var * Dedukti.term)) list) =

  let aux map k v = LevelMap.add k v map in
  let template_params = List.fold_left2 aux LevelMap.empty template_levels template_names in
  (* TODO: implement here a mechanism that processes polymorphic constraints *)
  {
    template_params = template_params;
    constraints = constraints_args
  }

let replace_template_name uenv lvl new_name =
  { uenv with template_params = LevelMap.add lvl new_name uenv.template_params }

let is_template_polymorphic (e:env) a = LevelMap.mem a e.template_params

let translate_template_arg (e:env) a = LevelMap.find a e.template_params

let try_translate_template_arg (e:env) a =
  try Some (translate_template_arg e a)
  with Not_found -> None

let fetch_constraint uenv cstr =
  try Some (fst (List.assoc cstr uenv.constraints))
  with Not_found -> None

let dummy = make [] [] 0 []


let destination = ref "."
let set_destination dest = destination := dest
let get_destination_path path = Filename.concat !destination path


type info =
  {
    out         : out_channel;
    fmt         : Format.formatter;
    library     : Names.DirPath.t;
    module_path : Names.ModPath.t;
  }

let init module_path filename =
  let filename = get_destination_path (filename ^ ".dk") in
  let out = open_out filename in
  {
    out = out;
    fmt = Format.formatter_of_out_channel out;
    library =
      if module_path = Names.ModPath.initial then Names.DirPath.initial
      else Nametab.dirpath_of_module module_path;
    module_path = module_path;
  }

let update info label =
  {info with module_path = Names.MPdot(info.module_path, label)}

let close info =
  Format.pp_print_flush info.fmt ();
  close_out info.out
