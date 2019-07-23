
Inductive nat : Type :=
| Z : nat
| S : nat -> nat.

Fixpoint plus (x:nat) (y:nat) :=
  match x with
  | Z => y
  | S x' => S (plus x' y)
  end.

Fixpoint times (x:nat) (y:nat) :=
  match x with
  | Z => Z
  | S x' => plus y (times x' y)
  end.

Inductive vect : nat -> Type :=
| nil : vect Z
| cons : forall n : nat, nat -> vect n -> vect (S n).

Fixpoint f acc n (v:vect n) {struct v} :=
  match v with
  | nil => acc
  | cons n' e v' => g (plus acc e) n' v'
  end
with g acc n v {struct v} :=
  match v with
  | nil => acc
  | cons n' e v' => f (times acc e) n' v'
  end
.
