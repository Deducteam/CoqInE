(* This script tests the Dedukti plugin by exporting test files and parts
  of the Coq standard library. *)

From mathcomp Require Import all_ssreflect.

Require Coqine.

Dedukti Enable Debug.
Dedukti Set Debug "debug.out".
Dedukti Set Destination "out".

Dedukti Set Encoding "readable polymorph".

Dedukti Export All.
