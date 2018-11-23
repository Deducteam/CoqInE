
module StringMap = Map.Make(
  struct
    type t = string
    let compare = String.compare
  end)

type env =
  {
    template_params : Dedukti.var StringMap.t;
    constraints : (Dedukti.term * Dedukti.term * Univ.Constraint.elt) list
  }

let make
    (template_args    : (string * Dedukti.var) list)
    (nb_polymorphic_args : int)
    (constraints_args : (Dedukti.term * Dedukti.term * Univ.Constraint.elt) list) =
  
  let aux map (k,v) = StringMap.add k v map in
  let template_params = List.fold_left aux StringMap.empty template_args in
  (* TODO: implement here a mechanism that processes polymorphic constraints *)
  {
    template_params = template_params;
    constraints = constraints_args
  }

let is_template_polymorphic (e:env) a = StringMap.mem a e.template_params

let translate_template_arg (e:env) a = StringMap.find a e.template_params


let dummy = make [] 0 []


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
  let filename = get_destination_path filename in
  let out = open_out (filename ^ ".dk") in
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
  
