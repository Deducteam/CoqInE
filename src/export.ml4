(** Main export commands *)

open Pp

let destination = ref "."

let set_destination dest =
  destination := dest

(** Export the library located at [dir_path]. *)
let export dir_path =
  msgnl (str "Exporting library " ++ Libnames.pr_dirpath dir_path);
  let filename = Filename.concat !destination (Name.translate_dir_path dir_path) in
  let out = open_out (filename ^ ".dk") in
  try (
    Libraries.translate_library out dir_path;
    close_out out)
  with e -> (
    close_out out;
    raise e)

(** Require and export export the library [reference]. *)
let require_and_export reference =
  let (loc, qualid) = Libnames.qualid_of_reference reference in
  Library.require_library [loc, qualid] None;
  let dir_path = Nametab.full_name_module qualid in
  export dir_path

(** Export all loaded libraries. *)
let export_all () =
  List.iter export (Library.loaded_libraries ())

VERNAC COMMAND EXTEND Dedukti
| [ "Dedukti" "Set" "Destination" string(destination) ] -> [ set_destination destination ]
| [ "Dedukti" "Export" global_list(references) ] -> [ List.iter require_and_export references ]
| [ "Dedukti" "All" "Export" ] -> [ export_all () ]
END

