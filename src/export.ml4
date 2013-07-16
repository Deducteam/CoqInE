let export_universe_constraint out constraint_type u v =
  let sign =
    match constraint_type with
    | Univ.Lt -> "<"
    | Univ.Le -> "<="
    | Univ.Eq -> "=" in
  Dedukti.print_comment out (Printf.sprintf "%s %s %s" u sign v)

let export_library out (loc, qualid) =
  let module_path = Nametab.locate_module qualid in
  let module_name = Names.string_of_mp module_path in
  let module_body = Global.lookup_module module_path in
  let translation = Translation.translate_module_body (Global.env ()) module_body in
  Dedukti.print_comment out "This file was automatically generated by Coqine.";
  Dedukti.print_command out "NAME" [module_name];
  Dedukti.print_command out "IMPORT" ["Coq"];
  List.iter (Dedukti.print_instruction out) translation

let export reference =
  let located_qualid = Libnames.qualid_of_reference reference in
  let dir_path, filename = Library.try_locate_qualified_library located_qualid in
  let out = open_out (Filename.chop_extension filename ^ ".dk") in
  try export_library out located_qualid
  with e -> (close_out out; raise e)

VERNAC COMMAND EXTEND Coqine
| [ "Dedukti" "Export" global(reference) ] -> [ export reference ]
END

