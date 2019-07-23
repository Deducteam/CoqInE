(* This script tests the Dedukti plugin by exporting test files and parts
  of the Coq standard library. *)

Declare ML Module "coqine_plugin".

Dedukti Set Destination "out".

Dedukti Enable Debug.
Dedukti Set Debug "debug.out".
Dedukti Add Debug "Coq.Init.Peano".

Dedukti Set Encoding "universo".

Require Import import.

Dedukti Export All But
        Coq.Init.Specif
        Coq.Init.Tactics
(*
*)
.
