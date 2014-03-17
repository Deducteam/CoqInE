(** Commands used in Coq to interact with the Yolk plugin **)

VERNAC COMMAND EXTEND DeduktiSetDestination
| [ "Dedukti" "Set" "Destination" string(dest) ] ->
  [ Libraries.set_destination dest ]
END

VERNAC COMMAND EXTEND DeduktiExportLibrary
| [ "Dedukti" "Export" "Library" global_list(refs) ] ->
  [ List.iter Libraries.translate_library refs ]
END

VERNAC COMMAND EXTEND DeduktiExportAll
| [ "Dedukti" "Export" "All" ] ->
  [ Libraries.translate_all () ]
END

