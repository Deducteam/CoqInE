(*i camlp4deps: "grammar/grammar.cma" i*)

DECLARE PLUGIN "coqine_plugin"


open Libraries
open Stdarg


(** Commands used in Coq to interact with the Yolk plugin **)


VERNAC COMMAND EXTEND DeduktiSetDestination
| [ "Dedukti" "Set" "Destination" string(dest) ] ->
  [ set_destination dest ]
END

VERNAC COMMAND EXTEND DeduktiSetDebug
| [ "Dedukti" "Set" "Debug" string(dest) ] ->
  [ set_debug dest ]
END

VERNAC COMMAND EXTEND DeduktiExportLibrary
| [ "Dedukti" "Export" "Library" global_list(refs) ] ->
  [ List.iter translate_library refs ]
END

VERNAC COMMAND EXTEND DeduktiExportAll
| [ "Dedukti" "Export" "All" ] ->
  [ translate_all () ]
END

VERNAC COMMAND EXTEND DeduktiShowUniverses
| [ "Dedukti" "Show" "Universes" ] ->
  [ show_universes_constraints () ]
END

VERNAC COMMAND EXTEND DeduktiShowSortedUniverses
| [ "Dedukti" "Show" "Sorted" "Universes" ] ->
  [ show_sorted_universes_constraints () ]
END

VERNAC COMMAND EXTEND DeduktiDebug
| [ "Dedukti" "Debug" ] -> [ debug () ]
END

VERNAC COMMAND EXTEND DeduktiTest
| [ "Dedukti" "Test" ] -> [ test () ]
END
