(* This script tests the Dedukti plugin by exporting test files and parts
   of the Coq standard library. *)

Declare ML Module "coqine_plugin".

Set Printing Universes.

Dedukti Set Destination "out".

Dedukti Enable Debug.
Dedukti Set Debug "debug.out".

Dedukti Debug Lib "Coq.Init.Logic".

Require Import import_debug.

Load config.

Dedukti Export All.
