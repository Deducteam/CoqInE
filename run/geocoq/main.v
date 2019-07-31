(* This script tests the Dedukti plugin by exporting test files and parts
  of the Coq standard library. *)

Declare ML Module "coqine_plugin".

Set Printing Universes.

Dedukti Set Destination "out".

(* These generates a huge file...
Dedukti Enable Debug.
Dedukti Set Debug "debug.out".

Dedukti Add Debug "Top.GeoCoq.Meta_theory.Decidability.equivalence_between_decidability_properties_of_basic_relations".
Dedukti Add Debug "Top.GeoCoq.Tarski_dev.Ch08_orthogonality".
 *)

Dedukti Enable Failproofmode.

Dedukti Enable Verbose.

Require Import import.

Load config.

Dedukti Export All.
