
module StringMap = Map.Make(
  struct
    type t = string
    let compare = String.compare
  end)

type env =
  {
    template_params : Dedukti.var StringMap.t;
    constraints : Univ.Constraint.t
  }

let make (constraints:Univ.Constraint.t) (params:(string * Dedukti.var) list) =
  let aux map (k,v) = StringMap.add k v map in
  {
    template_params = List.fold_left aux StringMap.empty params;
    constraints = constraints
  }

let is_template_polymorphic (e:env) a = StringMap.mem a e.template_params

let translate_template_arg (e:env) a = StringMap.find a e.template_params



type info = {
  out : Format.formatter;
  library : Names.DirPath.t;
  module_path : Names.ModPath.t;
  }

let init out dir_path =
  {
    out = out;
    library = dir_path;
    module_path = Names.MPfile(dir_path);
  }
