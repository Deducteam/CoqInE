(* This script tests the Dedukti plugin by exporting test files and parts
  of the Coq standard library. *)

From mathcomp Require Import all_ssreflect.

Require Coqine.

Dedukti Enable Debug.
Dedukti Set Debug "debug.out".
Dedukti Set Destination "out".

Dedukti Filter Out "Coq.Init.Logic.rew_ex".
Dedukti Filter Out "Coq.Init.Logic.rew_ex2".

Dedukti Filter Out "mathcomp.ssreflect.eqtype.MakeEqTypePred".
Dedukti Filter Out "mathcomp.ssreflect.eqtype.EqTypePred".
Dedukti Filter Out "Coq.Logic.EqdepFacts.EqdepTheory".
Dedukti Filter Out "Coq.Logic.Eqdep_dec.DecidableEqDep".
Dedukti Filter Out "Coq.Logic.Eqdep_dec.DecidableEqDepSet".

Dedukti Add Debug "mathcomp.ssreflect.eqtype".

Dedukti Set Encoding "readable lift_priv".

Dedukti Export All.
