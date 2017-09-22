
Set Printing All.
Set Printing Universes.


Definition xType0 : Type := Set.
Definition xType1 : Type := xType0.
Inductive  xSet   : Set  :=.
Inductive  xProp  : Prop :=.


Inductive exp2 (A:Type -> Type) : Type :=
  foo2 : forall D:(A xType0 -> Type), exp2 A.

Print exp2.

Check exp2 (fun x:Type => x).
Check @foo2.

Inductive exp (A:Type) (B:Type) : Type := foo : forall f: A -> B, exp A B.



Definition expType0 := @exp xType0 xType0.
Definition expSet   := @exp xSet   xSet.
Definition expProp  := @exp xProp  xProp.

Print exp.
Check exp xType0.
Check exp xType0 xType1.


Inductive test (f:nat -> Type) : Type := Test : f 0 -> test f.
Definition a := test (fun (x:nat) => Set).
Definition b := test (fun (x:nat) => nat).
