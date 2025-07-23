(* This script tests the Dedukti plugin by exporting test files and parts
   of the Coq standard library. *)

Require Import Coqine.

Set Printing Universes.

Dedukti Set Destination "out".

Dedukti Enable Debug.
Dedukti Set Debug "debug.out".

Dedukti Debug Lib "Corelib.Init.Logic".

Require Import import_debug.

Load config.

Dedukti Export All
  But Corelib.Classes.Init
      Corelib.Init.Prelude. (* This library uses universe polymorphism *)
