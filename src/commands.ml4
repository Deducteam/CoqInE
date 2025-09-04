(*i camlp4deps: "grammar/grammar.cma" i*)

DECLARE PLUGIN "coqine_plugin"

open Libraries
open Modules
open Stdarg


(** Exporting to Dedukti **)

VERNAC COMMAND EXTEND DeduktiExportLibrary CLASSIFIED AS QUERY
| [ "Dedukti" "Export" "Library" global_list(refs) ] ->
  [ List.iter translate_library refs ]
END

VERNAC COMMAND EXTEND DeduktiExportUniverses CLASSIFIED AS QUERY
| [ "Dedukti" "Export" "Universes" ] ->
  [ translate_universes () ]
END

VERNAC COMMAND EXTEND DeduktiExportAll CLASSIFIED AS QUERY
| [ "Dedukti" "Export" "All" ] ->
  [ translate_all () ]
END

VERNAC COMMAND EXTEND DeduktiExportAllBut CLASSIFIED AS QUERY
| [ "Dedukti" "Export" "All" "But" global_list(refs) ] ->
  [ translate_all_but refs ]
END


(** Printing universes **)

VERNAC COMMAND EXTEND DeduktiShowUniverses CLASSIFIED AS QUERY
| [ "Dedukti" "Show" "Universes" ] ->
  [ show_universes_constraints () ]
END

VERNAC COMMAND EXTEND DeduktiShowSortedUniverses CLASSIFIED AS QUERY
| [ "Dedukti" "Show" "Sorted" "Universes" ] ->
  [ show_sorted_universes_constraints () ]
END


(** Setting parameters **)

VERNAC COMMAND EXTEND EnableFailProofMode CLASSIFIED AS QUERY
| [ "Dedukti" "Enable" "Failproofmode" ] ->
  [ enable_failproofmode () ]
END

VERNAC COMMAND EXTEND DisableFailProofMode CLASSIFIED AS QUERY
| [ "Dedukti" "Disable" "Failproofmode" ] ->
  [ disable_failproofmode () ]
END

VERNAC COMMAND EXTEND DeduktiSetDestination CLASSIFIED AS QUERY
| [ "Dedukti" "Set" "Destination" string(dest) ] ->
  [ Info.set_destination dest ]
END

VERNAC COMMAND EXTEND DeduktiSetDebug CLASSIFIED AS QUERY
| [ "Dedukti" "Set" "Debug" string(dest) ] ->
  [ Debug.debug_to_file dest ]
END

VERNAC COMMAND EXTEND DeduktiAddDebug CLASSIFIED AS QUERY
| [ "Dedukti" "Add" "Debug" string(lib) ] ->
  [ Debug.add_debug_lib lib ]
END

VERNAC COMMAND EXTEND DeduktiFilterSymb CLASSIFIED AS QUERY
| [ "Dedukti" "Filter" "Out" string(symb) ] ->
  [ Modules.filter_out symb ]
END

VERNAC COMMAND EXTEND DeduktiEnableVerbose CLASSIFIED AS QUERY
| [ "Dedukti" "Enable" "Verbose" ] ->
  [ Debug.enable_verbose () ]
END

VERNAC COMMAND EXTEND DeduktiDisableVerbose CLASSIFIED AS QUERY
| [ "Dedukti" "Disable" "Verbose" ] ->
  [ Debug.disable_verbose () ]
END

VERNAC COMMAND EXTEND DeduktiEnableDebug CLASSIFIED AS QUERY
| [ "Dedukti" "Enable" "Debug" ] ->
  [ Debug.enable_debug () ]
END

VERNAC COMMAND EXTEND DeduktiDisableDebug CLASSIFIED AS QUERY
| [ "Dedukti" "Disable" "Debug" ] ->
  [ Debug.disable_debug () ]
END

VERNAC COMMAND EXTEND DeduktiSetParam CLASSIFIED AS QUERY
| [ "Dedukti" "Set" "Param" string(key) string(value) ] ->
  [ Encoding.set_param key value ]
END
