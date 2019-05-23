(* This script tests the Dedukti plugin by exporting test files and parts
  of the Coq standard library. *)

Require Coqine.

Dedukti Enable Debug.
Dedukti Set Debug "debug.out".
Dedukti Set Destination "out".

Dedukti Add Debug "Coq.Classes.CRelationClasses".
Dedukti Add Debug "Coq.Classes.RelationClasses".
Dedukti Add Debug "Top.GeoCoq.Axioms.euclidean_axioms".

Dedukti Set Encoding "readable universo".

Dedukti Filter Out "Coq.Init.Logic.rew_ex".
Dedukti Filter Out "Coq.Init.Logic.rew_ex2".

Dedukti Filter Out "Coq.Init.Logic.rew_ex2".


Require Import import.


Dedukti Export All.
