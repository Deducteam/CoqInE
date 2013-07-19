(* This script tests the Dedukti plugin. *)

Require Dedukti.

Dedukti Set Destination "test".

Dedukti Export Identity.
Dedukti Export ImportA.
Dedukti Export ImportB.
Dedukti Export ImportC.
Dedukti Export Cast.
Dedukti Export Let.
Dedukti Export NestedModules.
Dedukti Export NestedLibraries.
Dedukti Export Reflexivity.

Dedukti All Export.
