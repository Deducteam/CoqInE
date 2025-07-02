Parameter u : Type.

Fixpoint vector (A:Type) (n : nat)  : Type :=
  match n with
  | 0 => u
  | S k => A * (vector A k)
  end.


Fixpoint exp (a b : nat) :=
  match b with
  | 0 => 1
  | S n => a * (exp a n)
  end.

Definition t := exp 10 3.

Definition t' := 1000.

Fixpoint A m := fix A_m n :=
  match m with
    | 0 => n + 1
    | S pm =>
      match n with
        | 0 => A pm 1
        | S pn => A pm (A_m pn)
      end
  end.

Definition t'' := A 2 2.

Set Allow StrictProp.

Require Import
  Test.Test
  Test.Identity
  Test.Polymorph
  Test.ImportA
  Test.ImportB
  Test.ImportC
  Test.Cast
  Test.Let
  Test.Inductives
  Test.Fixpoints
  Test.Sections
  Test.NestedModules
  Test.Functors
  Test.NestedLibraries
  Test.Reflexivity
(*
  Test.Debuglib
*)
.


Require Import
  Corelib.Classes.RelationClasses
(*

  Corelib.Arith.PeanoNat

  Corelib.Logic.Berardi
  Corelib.Logic.ChoiceFacts
  Corelib.Logic.ClassicalChoice
  Corelib.Logic.ClassicalDescription
  Corelib.Logic.ClassicalEpsilon
  Corelib.Logic.ClassicalFacts
  Corelib.Logic.Classical_Pred_Set
  Corelib.Logic.Classical_Pred_Type
  Corelib.Logic.Classical_Prop
  Corelib.Logic.Classical_Type
  Corelib.Logic.ClassicalUniqueChoice
  Corelib.Logic.Classical
  Corelib.Logic.ConstructiveEpsilon
  Corelib.Logic.Decidable
  Corelib.Logic.Description
  Corelib.Logic.Diaconescu
  Corelib.Logic.Epsilon
  Corelib.Logic.Eqdep_dec
  Corelib.Logic.EqdepFacts
  Corelib.Logic.Eqdep
  Corelib.Logic.FunctionalExtensionality
  Corelib.Logic.Hurkens
  Corelib.Logic.IndefiniteDescription
  Corelib.Logic.JMeq
  Corelib.Logic.ProofIrrelevanceFacts
  Corelib.Logic.ProofIrrelevance
  Corelib.Logic.RelationalChoice
  Corelib.Logic.SetIsType
*)
.
