Declare ML Module "coqine".
Dedukti Set Destination "out".
Load config.
Dedukti Set Param "simpl_letins" "false".

Require Import import.

Dedukti Export All.

(*
Dedukti Enable Failproofmode.

Set Printing All.
Set Printing Universes.

Dedukti Enable Debug.
Dedukti Set Debug "debug.out".
Dedukti Debug Lib "Corelib.Logic.Hurkens.NoRetractToImpredicativeUniverse".
Dedukti Debug Symbol "Corelib.Logic.Hurkens.NoRetractToModalProposition.paradox".
Dedukti Enable Verbose.

Require Import
  Corelib.PArith.BinPos.
  Corelib.Classes.CMorphisms.

  Corelib.Bool.Zerob
  Corelib.Bool.Bvector (* Requires Corelib.PArith.BinPos which is bugged *)

  Corelib.Logic.ProofIrrelevanceFacts
  Corelib.Logic.Decidable
  Corelib.Logic.Hurkens   (* Bug to fix... *)
  Corelib.Arith.Arith_base

  Corelib.Lists.List (* Requires Corelib.PArith.BinPos which is bugged *)
.

Dedukti Export Library Corelib.Logic.Hurkens.
*)
