(** Main export commands *)

open Pp

let destination = ref "."

let set_destination dest =
  destination := dest

(** Export the library located at [dir_path]. *)
let export_library dir_path =
  msgnl (str "Exporting library " ++ Libnames.pr_dirpath dir_path);
  let filename = Filename.concat !destination (Name.translate_dir_path dir_path) in
  let out_channel = open_out (filename ^ ".dk") in
  let formatter = Format.formatter_of_out_channel out_channel in
  let flush_and_close () =
    Format.pp_print_flush formatter ();
    close_out out_channel
  in
  begin try Libraries.translate_library formatter dir_path with
  | e ->
    flush_and_close ();
    raise e
  end;
  flush_and_close ()

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

