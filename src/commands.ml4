(*i camlp4deps: "grammar/grammar.cma" i*)

DECLARE PLUGIN "coqine_plugin"


open Libraries
open Stdarg


(** Commands used in Coq to interact with the Yolk plugin **)


VERNAC COMMAND EXTEND DeduktiSetDestination CLASSIFIED AS QUERY
| [ "Dedukti" "Set" "Destination" string(dest) ] ->
  [ set_destination dest ]
END

VERNAC COMMAND EXTEND DeduktiSetDebug CLASSIFIED AS QUERY
| [ "Dedukti" "Set" "Debug" string(dest) ] ->
  [ set_debug dest ]
END

VERNAC COMMAND EXTEND DeduktiExportLibrary CLASSIFIED AS QUERY
| [ "Dedukti" "Export" "Library" global_list(refs) ] ->
  [ List.iter translate_library refs ]
END

VERNAC COMMAND EXTEND DeduktiExportAll CLASSIFIED AS QUERY
| [ "Dedukti" "Export" "All" ] ->
  [ translate_all () ]
END

VERNAC COMMAND EXTEND DeduktiShowUniverses CLASSIFIED AS QUERY
| [ "Dedukti" "Show" "Universes" ] ->
  [ show_universes_constraints () ]
END

VERNAC COMMAND EXTEND DeduktiShowSortedUniverses CLASSIFIED AS QUERY
| [ "Dedukti" "Show" "Sorted" "Universes" ] ->
  [ show_sorted_universes_constraints () ]
END

VERNAC COMMAND EXTEND DeduktiEnablePolymorphism CLASSIFIED AS QUERY
| [ "Dedukti" "Enable" "Universes" ] ->
  [ Debug.enable_polymorphism () ]
END

VERNAC COMMAND EXTEND DeduktiDisablePolymorphism CLASSIFIED AS QUERY
| [ "Dedukti" "Disable" "Universes" ] ->
  [ Debug.disable_polymorphism () ]
END
