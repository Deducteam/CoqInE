
(* This script tests the Dedukti plugin by exporting test files and parts
  of the Coq standard library. *)

Declare ML Module "coqine_plugin".

Dedukti Enable Debug.
Dedukti Set Debug "debug.out".
Dedukti Set Destination "out".

(* This generates a huge file...
Dedukti Add Debug "Top.GeoCoq.Tarski_dev.Ch08_orthogonality".
 *)

Dedukti Set Encoding "readable universo".

Dedukti Enable Failproofmode.

Require Import import.

Dedukti Export All.

(*
Dedukti Export Library
 Coq.Init.Logic
  Top.GeoCoq.Utils.general_tactics
  Top.GeoCoq.Axioms.euclidean_axioms
  Top.GeoCoq.Axioms.tarski_axioms
  Top.GeoCoq.Tarski_dev.Definitions
  Top.GeoCoq.Tactics.finish
  Top.GeoCoq.Tarski_dev.Ch02_cong
  Top.GeoCoq.Tarski_dev.Ch03_bet
  Top.GeoCoq.Tarski_dev.Ch04_cong_bet
  Top.GeoCoq.Tarski_dev.Ch04_col
  Top.GeoCoq.Meta_theory.Decidability.equivalence_between_decidability_properties_of_basic_relations
  Top.GeoCoq.Tarski_dev.Ch05_bet_le
  Top.GeoCoq.Tarski_dev.Ch06_out_lines
  Top.GeoCoq.Tarski_dev.Ch07_midpoint
  Top.GeoCoq.Tarski_dev.Ch08_orthogonality
  Top.GeoCoq.Tarski_dev.Annexes.coplanar
  Top.GeoCoq.Tarski_dev.Ch09_plane
  Coq.Program.Basics
  Coq.Classes.Init
  Coq.Program.Tactics
  Coq.Relations.Relation_Definitions
.

Dedukti Export Library All But
  Coq.Classes.RelationClasses
  Coq.Classes.Morphisms
  Coq.Classes.CRelationClasses
  Coq.Classes.CMorphisms
  Coq.Init.Notations
  Coq.Init.Specif
  Coq.Init.Tactics
  Coq.Init.Nat
  Coq.Init.Peano
  Coq.Init.Decimal
  Coq.Init.Wf
  Coq.Init.Logic_Type
  Coq.Init.Prelude
  Coq.Init.Tauto
  Coq.Bool.Bool
  Coq.Structures.Equalities
  Coq.Structures.Orders
  Coq.Structures.OrdersTac
  Coq.Structures.OrdersFacts
.
*)
