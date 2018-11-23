

Set Printing Universes.


Inductive TInd (A:Type) : Type :=
| ct : A ->  TInd A.

About TInd.

Definition A : Prop := TInd True.


Polymorphic Inductive Ind (A:Type) : Type :=
| c : A ->  Ind A.

About Ind.

Fail Definition B : Prop := Ind True.

