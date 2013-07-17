(** Main export commands *)

let vo_magic_number = 08400

let raw_intern_library =
  snd (System.raw_extern_intern vo_magic_number ".vo")

type library_objects

type compilation_unit_name = Names.dir_path

type library_disk = {
  md_name : compilation_unit_name;
  md_compiled : Safe_typing.LightenLibrary.lightened_compiled_library;
  md_objects : library_objects;
  md_deps : (compilation_unit_name * Digest.t) list;
  md_imports : compilation_unit_name list }

let get_deps dir_path =
  let filename = Library.library_full_filename dir_path in
  let ch = raw_intern_library filename in
  let (md:library_disk) = System.marshal_in filename ch in
  close_in ch;
  fst (List.split md.md_deps)

let translate_dep dep =
  Dedukti.command "IMPORT" [Names.string_of_dirpath dep]

let translate_library dir_path =
  let deps = get_deps dir_path in
  let qualid = Libnames.qualid_of_dirpath dir_path in  
  let module_path = Nametab.locate_module qualid in
  let module_name = Names.string_of_mp module_path in
  let module_body = Global.lookup_module module_path in
  Dedukti.comment "This file was automatically generated by Coqine." ::
  Dedukti.command "NAME" [module_name] ::
  Dedukti.command "IMPORT" ["Coq"] ::
  List.map translate_dep deps @
  Modules.translate_module_body (Global.env ()) module_body

let export filename =
  Library.require_library_from_file None filename None;
  let dir_path = Libnames.dirpath_of_string filename in
  let full_filename = Library.library_full_filename dir_path in
  let translation = translate_library dir_path in
  let out = open_out (Filename.chop_extension full_filename ^ ".dk") in
  List.iter (Dedukti.print_instruction out) translation;
  close_out out

VERNAC COMMAND EXTEND Dedukti
| [ "Dedukti" "Export" string_list(filenames) ] -> [ List.iter export filenames ]
END

