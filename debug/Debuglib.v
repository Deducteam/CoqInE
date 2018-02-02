
Set Printing All.
Set Printing Universes.


Definition xType0 : Type := Set.
Definition xType1 : Type := xType0.
Inductive  xSet   : Set  :=.
Inductive  xProp  : Prop :=.


Inductive exp (A:Type) (B:Type) : Type :=
  tst : forall f: A -> B, exp A B.

Print exp.


Definition expType0 := @exp xType0 xType0.
Definition expSet   := @exp xSet   xSet.
Definition expProp  := @exp xProp  xProp.

Print exp.
Check exp xType0.
Check exp xType0 xType1.

Print Universes.

(*
Inductive foo
          (A:xSet)
          (B:xSet -> Type) :
  (B A) -> Type := bar : forall x:(B A), foo A B x.
*)