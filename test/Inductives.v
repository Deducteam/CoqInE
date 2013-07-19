Inductive eq : forall (A : Type) (x : A), A -> Type :=
| eq_refl : forall (A : Type) (x : A), eq A x x.

Definition f (A : Type) (x : A) (y : A) (p : eq A x y) : A :=
match p with
| eq_refl B z => z
end.

