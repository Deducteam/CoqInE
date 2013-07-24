(** Environment of the translation *)

type env = {
  out : out_channel;
  library : Names.dir_path;
  module_path : Names.module_path;
  mutable globals : Names.identifier list;
  env : Environ.env;
  }

let init_env out dir_path = {
  out = out;
  library = dir_path;
  module_path = Names.MPfile(dir_path);
  globals = [];
  env = Global.env ();
  }

let global_env env =
  {env with env = Global.env ()}

(** Declare a global name. *)
let declare_global env identifier =
  env.globals <- identifier :: env.globals

let push_rel declaration env =
  {env with env = Environ.push_rel declaration env.env}

let push_rel_context context env =
  {env with env = Environ.push_rel_context context env.env}

let push_named declaration env =
  {env with env = Environ.push_named declaration env.env}

(** Push a dummy declaration to declare an identifier locally. *)
let push_identifier identifier env =
  {env with env = Environ.push_named (identifier, None, Term.mkSort (Term.Prop(Term.Null))) env.env}

