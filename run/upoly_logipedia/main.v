Declare ML Module "coqine:coqine.plugin".
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
Dedukti Debug Lib "Coq.Logic.Hurkens.NoRetractToImpredicativeUniverse".
Dedukti Debug Symbol "Coq.Logic.Hurkens.NoRetractToModalProposition.paradox".
Dedukti Enable Verbose.

Require Import
  Coq.PArith.BinPos.
  Coq.Classes.CMorphisms.

  Coq.Bool.Zerob
  Coq.Bool.Bvector (* Requires Coq.PArith.BinPos which is bugged *)

  Coq.Logic.ProofIrrelevanceFacts
  Coq.Logic.Decidable
  Coq.Logic.Hurkens   (* Bug to fix... *)
  Coq.Arith.Arith_base

  Coq.Lists.List (* Requires Coq.PArith.BinPos which is bugged *)
.

Dedukti Export Library Coq.Logic.Hurkens.
*)
