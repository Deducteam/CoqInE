(* This script tests the Dedukti plugin by exporting test files and parts
   of the Coq standard library. *)

Declare ML Module "coqine_plugin".

Set Printing Universes.

Dedukti Set Destination "out".

Dedukti Enable Debug.
Dedukti Set Debug "debug.out".

Dedukti Add Debug "Coq.Init.Datatypes".
Dedukti Add Debug "Top.Test.UPolymorph".

Dedukti Enable Verbose.

Require Import import_poly.

Load config.

Dedukti Export All.
