Section S.

Variable A : Type.

Variable a : A.

Definition x := a.

Variable b : A.

Definition y := b.

Inductive I : A -> Type :=
| c : forall x, I x.

Definition z := c x.

End S.
