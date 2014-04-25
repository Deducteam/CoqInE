(** Information about the translation *)

type info = {
  out : Format.formatter;
  library : Names.dir_path;
  module_path : Names.module_path;
  }

let init out dir_path = {
  out = out;
  library = dir_path;
  module_path = Names.MPfile(dir_path);
  }

