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

Require Import
  Test.Test
  Test.Identity
  Coq.Classes.RelationClasses
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

  Coq.Arith.PeanoNat
  Test.Debuglib

  Coq.Logic.Berardi
  Coq.Logic.ChoiceFacts
  Coq.Logic.ClassicalChoice
  Coq.Logic.ClassicalDescription
  Coq.Logic.ClassicalEpsilon
  Coq.Logic.ClassicalFacts
  Coq.Logic.Classical_Pred_Set
  Coq.Logic.Classical_Pred_Type
  Coq.Logic.Classical_Prop
  Coq.Logic.Classical_Type
  Coq.Logic.ClassicalUniqueChoice
  Coq.Logic.Classical
  Coq.Logic.ConstructiveEpsilon
  Coq.Logic.Decidable
  Coq.Logic.Description
  Coq.Logic.Diaconescu
  Coq.Logic.Epsilon
  Coq.Logic.Eqdep_dec
  Coq.Logic.EqdepFacts
  Coq.Logic.Eqdep
  Coq.Logic.FunctionalExtensionality
  Coq.Logic.Hurkens
  Coq.Logic.IndefiniteDescription
  Coq.Logic.JMeq
  Coq.Logic.ProofIrrelevanceFacts
  Coq.Logic.ProofIrrelevance
  Coq.Logic.RelationalChoice
  Coq.Logic.SetIsType
*)
.
