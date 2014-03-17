(** Commands used in Coq to interact with the Yolk plugin **)

VERNAC COMMAND EXTEND Dedukti
| [ "Dedukti" "Set" "Destination" string(dest) ] -> [ Libraries.set_destination dest ]
| [ "Dedukti" "Export" global_list(refs) ] -> [ List.iter Libraries.translate_library refs ]
| [ "Dedukti" "All" "Export" ] -> [ Libraries.translate_all () ]
END

