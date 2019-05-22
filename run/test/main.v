(* This script tests the Dedukti plugin by exporting test files and parts
  of the Coq standard library. *)

Require Coqine.

Dedukti Set Destination "out".

Dedukti Set Encoding "readable universo".

Dedukti Export All.
