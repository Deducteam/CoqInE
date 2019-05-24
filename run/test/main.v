(* This script tests the Dedukti plugin by exporting test files and parts
  of the Coq standard library. *)

Require Coqine.

Dedukti Set Destination "out".

Dedukti Set Encoding "original_cast ".

Dedukti Export All But
	    Coq.Init.Specif
	Coq.Init.Tactics
	Coq.Init.Peano
	Coq.Init.Nat
.
