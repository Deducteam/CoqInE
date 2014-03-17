(** Main export commands *)

open Pp

let destination = ref "."

let set_destination dest =
  destination := dest

(** Export the library located at [dir_path]. *)
let export_library dir_path =
  msgnl (str "Exporting library " ++ Libnames.pr_dirpath dir_path);
  let filename = Filename.concat !destination (Name.translate_dir_path dir_path) in
  let out = open_out (filename ^ ".dk") in
  try (
    Libraries.translate_library out dir_path;
    close_out out)
  with e -> (
    close_out out;
    raise e)

(** Export the library [reference]. *)
let export reference =
  let (loc, qualid) = Libnames.qualid_of_reference reference in
  let dir_path, _ = Library.try_locate_qualified_library (loc, qualid) in
  (* Make sure the module is loaded. *)
  if not (Library.library_is_loaded dir_path) then
    Util.errorlabstrm "Dedukti" (str "Library " ++ Libnames.pr_qualid qualid ++ str " is not loaded");
  Sorts.set_universes (Global.universes ());
  export_library dir_path

(** Export all loaded libraries. *)
let export_all () =
  Sorts.set_universes (Global.universes ());
  List.iter export_library (Library.loaded_libraries ())

VERNAC COMMAND EXTEND Dedukti
| [ "Dedukti" "Set" "Destination" string(destination) ] -> [ set_destination destination ]
| [ "Dedukti" "Export" global_list(references) ] -> [ List.iter export references ]
| [ "Dedukti" "All" "Export" ] -> [ export_all () ]
END

