(** Translation of Coq libraries *)

open Parameters
open Pp
open Debug

let destination = ref "."

let set_destination dest =
  message "Setting destination: %s" dest;
  destination := dest

let set_debug dest =
  message "Setting debug to: %s" dest;
  Parameters.enable_debug ();
  debug_to_file dest

(** Translate the library referred to by [qualid].
    A libray is a module that corresponds to a file on disk. **)
let translate_qualified_library qualid =
  let libname = Libnames.pr_qualid qualid in
  message "Exporting %a" pp_t libname;
  if (libname = (str "Top.Debuglib")) then debug_start ();
  debug_start ();
  debug "Exporting %a" pp_t (Libnames.pr_qualid qualid);
  let module_path = Nametab.locate_module qualid in
  let module_body = Global.lookup_module module_path in
  let dir_path = Nametab.dirpath_of_module module_path in
  let filename = Filename.concat !destination (Cname.translate_dir_path dir_path) in
  let out = open_out (filename ^ ".dk") in
  let formatter = Format.formatter_of_out_channel out in
  let info = Info.init formatter dir_path in
  let flush_and_close () =
    Format.pp_print_flush formatter ();
    close_out out
  in
  begin
    (*    try*)
      (Debug.pp_list "" Dedukti.printc) formatter (Dedukti.Coq.coq_header);
      Modules.translate_module_body info (Global.env ()) module_body;
      (Debug.pp_list "" Dedukti.printc) formatter (Dedukti.Coq.coq_footer)
(*  with
  | e ->
    flush_and_close ();
    raise e*)
  end;
  debug_stop ();
  flush_and_close ()


(** Translates the given library *)
let translate_library reference =
  let cast_qualid = Libnames.qualid_of_reference reference in
  let qualid = cast_qualid.CAst.v in
  let lib_loc, lib_path, lib_phys_path = Library.locate_qualified_library qualid in
  Library.require_library_from_dirpath [ (lib_path, Libnames.string_of_qualid qualid) ] None;
  Tsorts.set_universes (Global.universes ());
  translate_qualified_library qualid

(** Translate all loaded libraries. **)
let translate_all () =
  let dirpaths = Library.loaded_libraries () in
  let qualids = List.map Libnames.qualid_of_dirpath dirpaths in
  Tsorts.set_universes (Global.universes ());
  List.iter translate_qualified_library qualids


let print_universes_constraints universes =
  let register constraint_type j k =
    match constraint_type with
    | Univ.Lt -> message "%s <  %s" j k
    | Univ.Le -> message "%s <= %s" j k
    | Univ.Eq -> message "%s == %s" j k
  in
  UGraph.dump_universes register universes

let show_universes_constraints () =
  message "";
  message "------------------------------------------------";
  message "|    Printing global universes constraints     |";
  message "------------------------------------------------";
  print_universes_constraints (Global.universes ());
  message "-----------------------------------------------";
  message ""

let show_sorted_universes_constraints () =
  message "";
  message "------------------------------------------------";
  message "| Printing global sorted universes constraints |";
  message "------------------------------------------------";
  print_universes_constraints (UGraph.sort_universes (Global.universes ()));
  message "-----------------------------------------------";
  message ""
