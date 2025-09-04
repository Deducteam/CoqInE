(** Translation of Coq libraries *)

open Debug
open Translator

(** Translate the library referred to by [qualid].
    A libray is a module that corresponds to a file on disk. **)
let translate_qualified_library qualid =
  let libname = Libnames.pr_qualid qualid in
  message "Exporting %a" pp_t libname;
  if is_debug_lib libname then debug_start ();
  debug "Exporting %a" pp_t (Libnames.pr_qualid qualid);
  let module_path = Nametab.locate_module qualid in
  let module_body = Global.lookup_module module_path in
  let dir_path = Nametab.dirpath_of_module module_path in
  let filename = Cname.translate_dir_path dir_path in
  let info = Info.init module_path filename in
  begin
    (pp_list "" Dedukti.printc) info.Info.fmt (T.coq_header ());
    (Modules.translate_module_body info (Global.env ()) module_body);
    (pp_list "" Dedukti.printc) info.Info.fmt (T.coq_footer ())
  end;
  debug_stop ();
  Info.close info

let qualid_of_ref r = (Libnames.qualid_of_reference r).CAst.v

(** Translates the given library *)
let translate_library reference =
  let qualid = qualid_of_ref reference in
  let lib_loc, lib_path, lib_phys_path = Library.locate_qualified_library qualid in
  Library.require_library_from_dirpath [ (lib_path, Libnames.string_of_qualid qualid) ] None;
  Tsorts.set_universes (Global.universes ());
  translate_qualified_library qualid

let translate_universes () =
  if Encoding.need_universe_file ()
  then
    (* "universe_file" is the file for global universe definitions  *)
    let info = Info.init Names.ModPath.initial (Encoding.symb "universe_file") in
    begin
      try
        (pp_list "" Dedukti.printc) info.Info.fmt (T.coq_header ());
        Tunivs.translate_all_universes info (Global.universes ())
      with e -> Info.close info; raise e
    end;
    Info.close info

(** Translate all loaded libraries but expressions. **)
let translate_all_but refs =
  let sep = "\n  Ignoring " in
  let ignore_qualids = List.map qualid_of_ref refs in
  begin
    match ignore_qualids with
    | [] -> message "Translating all libraries"
    | ignore_qualids ->
      message "Translating all libraries except from the following:%s%a"
        sep (pp_list sep pp_t) (List.map Libnames.pr_qualid ignore_qualids)
  end;
  let not_ignored qualid = not (List.exists (Libnames.qualid_eq qualid) ignore_qualids) in
  let dirpaths = Library.loaded_libraries () in
  let qualids = List.map Libnames.qualid_of_dirpath dirpaths in
  Tsorts.set_universes (Global.universes ());
  translate_universes ();
  List.iter translate_qualified_library (List.filter not_ignored qualids)

(** Translate all loaded libraries. **)
let translate_all () = translate_all_but []


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
