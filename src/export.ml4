(** Main export commands *)

let export filename =
  Library.require_library_from_file None filename None;
  let dir_path = Libnames.dirpath_of_string filename in
  let full_filename = Library.library_full_filename dir_path in
  let translation = Libraries.translate_library dir_path in
  let out = open_out (Filename.chop_extension full_filename ^ ".dk") in
  List.iter (Dedukti.print out) translation;
  close_out out

VERNAC COMMAND EXTEND Dedukti
| [ "Dedukti" "Export" string_list(filenames) ] -> [ List.iter export filenames ]
END

