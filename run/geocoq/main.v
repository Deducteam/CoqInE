(* This script tests the Dedukti plugin by exporting test files and parts
  of the Coq standard library. *)

Require Coqine.

Dedukti Enable Debug.
Dedukti Set Debug "debug.out".
Dedukti Set Destination "out".

Dedukti Add Debug "Coq.Classes.CRelationClasses".

Dedukti Set Encoding "readable original_cast".

Dedukti Filter Out "Coq.Init.Logic.rew_ex".
Dedukti Filter Out "Coq.Init.Logic.rew_ex2".

Require Import import.

Require Import Tactics.Coinc.tactics_axioms.


Dedukti Export All.
