let export_library () =
  ()

VERNAC COMMAND EXTEND Coqine
| [ "Dedukti" "Export" ] -> [ export_library () ]
END

