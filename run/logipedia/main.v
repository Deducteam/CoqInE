
Require Coqine.

Dedukti Set Destination "out".

Dedukti Set Param "simpl_letins" "true".

Require Import import.

Load config.

Dedukti Export All
  But Corelib.Init.Prelude. (* This library uses universe polymorphism *)
