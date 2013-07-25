Inductive nat : Type :=
| o : nat
| s : nat -> nat.

Inductive eq (A : Type) (x : A) : A -> Type :=
| eq_refl : eq A x x.

Definition f (A : Type) (x : A) (y : A) (p : eq A x y) : A :=
match p with
| eq_refl => x
end.

