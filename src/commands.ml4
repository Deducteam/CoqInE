
open Libraries
open Stdarg
open Constrarg

(** Commands used in Coq to interact with the Yolk plugin **)


VERNAC COMMAND EXTEND DeduktiSetDestination
| [ "Dedukti" "Set" "Destination" string(dest) ] ->
  [ set_destination dest ]
END

VERNAC COMMAND EXTEND DeduktiExportLibrary
| [ "Dedukti" "Export" "Library" global_list(refs) ] ->
  [ List.iter translate_library refs ]
END

VERNAC COMMAND EXTEND DeduktiExportAll
| [ "Dedukti" "Export" "All" ] ->
  [ translate_all () ]
END

