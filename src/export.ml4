let export reference =
  let located_qualid = Libnames.qualid_of_reference reference in
  let dir_path, filename = Library.try_locate_qualified_library located_qualid in
  ()

VERNAC COMMAND EXTEND Coqine
| [ "Dedukti" "Export" global(reference) ] -> [ export reference ]
END

