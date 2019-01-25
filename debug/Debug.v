(* This script tests the Dedukti plugin by exporting test files and parts
  of the Coq standard library. *)

Require Coqine.

Dedukti Enable Debug.
Dedukti Set Debug "debug.out".
Dedukti Set Destination "out".

Dedukti Set Encoding "named original_cast".

Require Import GeoCoq.Tarski_dev.Ch02_cong.

(*
Require Import ssr.ssreflect.
 *)

Require Import Debuglib.

Dedukti Export All.
