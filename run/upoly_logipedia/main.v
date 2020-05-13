Declare ML Module "coqine_plugin".
Dedukti Set Destination "out".
Load config.
Dedukti Set Param "simpl_letins" "false".

(*
Dedukti Enable Failproofmode.
 *)

Set Printing All.
Set Printing Universes.
Dedukti Enable Debug.
Dedukti Set Debug "debug.out".
Dedukti Debug Lib "Coq.Logic.Hurkens.NoRetractToImpredicativeUniverse".
(*
Dedukti Debug Symbol "Coq.Logic.Hurkens.NoRetractToModalProposition.paradox".
 *)

Dedukti Enable Verbose.

Require Import import.
Dedukti Export All.
