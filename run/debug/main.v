
(* This script tests the Dedukti plugin by exporting test files and parts
  of the Coq standard library. *)

Require Coqine.

Dedukti Enable Debug.
Dedukti Set Debug "debug.out".
Dedukti Set Destination "out".
Dedukti Add Debug "Coq.Init.Specif".
(*
Dedukti Add Debug "Coq.Classes.RelationClasses".
*)

Dedukti Set Encoding "readable universo".

(*
Dedukti Filter Out "Coq.Init.Logic.rew_ex".
Dedukti Filter Out "Coq.Init.Logic.rew_ex2".
*)

Require Import import.

Dedukti Export All.


