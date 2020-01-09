Declare ML Module "coqine_plugin".
Dedukti Set Destination "out".
Load config.
Dedukti Set Param "simpl_letins" "true".

Set Printing All.
Set Printing Universes.
Dedukti Enable Debug.
Dedukti Set Debug "debug.out".
Dedukti Add Debug "Coq.Init.Logic".

(*
Dedukti Enable Failproofmode.
Dedukti Enable Verbose.

Set Printing All.
Set Printing Universes.
Dedukti Enable Debug.
Dedukti Set Debug "debug.out".
Dedukti Add Debug "Coq.Arith.PeanoNat".
*)

Require Import import.
Dedukti Export All.
