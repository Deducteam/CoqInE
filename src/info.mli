type info = {
  out : Format.formatter;
  library : Names.dir_path;
  module_path : Names.module_path;
}
val init : Format.formatter -> Names.dir_path -> info
