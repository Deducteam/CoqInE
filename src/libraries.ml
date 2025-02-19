(** Translation of Coq libraries *)

open Debug
open Translator

let translate_term term =
  (*
  let t = CAst.with_val (fun x -> x) term in
  debug "Exporting %a" pp_coq_term t;
  let module_path = assert false in
  let filename = assert false in
  let empty_env = assert false in
  let info = assert false in
  let term' = Terms.translate_constr info (Global.env ()) empty_env term in
  Info.close info;
*)
  Feedback.msg_notice (Pp.str "Dedukti rep: ")

(* Translate the library referred to by [qualid].
   A libray is a module that corresponds to a file on disk. *)
let translate_qualified_library qualid =
  let libname = Pp.string_of_ppcmds (Libnames.pr_qualid qualid) in
  message "Exporting %s" libname;
  if is_debug_lib libname then debug_start ();
  debug "Exporting %s" libname;
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

(* Translates the given library *)
let translate_library qualid =
  match Loadpath.locate_qualified_library qualid with
  | Ok(lib_path, lib_phys_path) ->
    let lib_resolver = Loadpath.try_locate_absolute_library in
    Library.require_library_from_dirpath (Library.require_library_syntax_from_dirpath [ lib_path ] ~lib_resolver);
    Tsorts.set_universes (Global.universes ());
    translate_qualified_library qualid
  | Error _ -> assert false

let translate_universes () =
  if Encoding.need_universe_file ()
  then
    (* "universe_file" is the file for global universe definitions  *)
    let info = Info.init Names.ModPath.dummy (Encoding.symb "universe_file") in
    begin
      try
        (pp_list "" Dedukti.printc) info.Info.fmt (T.coq_header ());
        Tunivs.translate_all_universes info (Global.universes ())
      with e -> Info.close info; raise e
    end;
    Info.close info

(* Translate all loaded libraries but expressions. *)
let translate_all_but ignore_qualids =
  let sep = "\n  Ignoring " in
  begin
    match ignore_qualids with
    | [] -> message "Translating all libraries"
    | ignore_qualids ->
      message "Translating all libraries except from the following:%s%a"
        sep (pp_list sep pp_t) (List.map Libnames.pr_qualid ignore_qualids)
  end;
  let not_ignored qualid =
    not (List.exists (Libnames.qualid_eq qualid) ignore_qualids) in
  let dirpaths = Library.loaded_libraries () in
  let qualids = List.map Libnames.qualid_of_dirpath dirpaths in
  Tsorts.set_universes (Global.universes ());
  translate_universes ();
  List.iter translate_qualified_library (List.filter not_ignored qualids)

(* Translate all loaded libraries. *)
let translate_all () = translate_all_but []
