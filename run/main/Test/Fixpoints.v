Inductive nat :=
| o : nat
| s : nat -> nat.

Fixpoint id x :=
match x with
| o => o
| s x => s (id x)
end.

Fixpoint id1 a b n :=
match n with
| o => a
| s m => s (id2 a b m)
end

with id2 a b n :=
match n with
| o => b
| s m => s (id1 a b m)
end.

Fixpoint f x :=
(fix g y :=
  match x with
  | o => o
  | s x1 =>
    match y with
    | o => f x1
    | s y1 => g y1
    end
  end) x.



Inductive nat' : Type :=
| Z' : nat'
| S' : nat' -> nat'.

Fixpoint plus (x:nat') (y:nat') :=
  match x with
  | Z' => y
  | S' x' => S' (plus x' y)
  end.

Fixpoint times (x:nat') (y:nat') :=
  match x with
  | Z' => Z'
  | S' x' => plus y (times x' y)
  end.

Inductive vect : nat' -> Type :=
| nil : vect Z'
| cons : forall n : nat', nat' -> vect n -> vect (S' n).

Fixpoint g acc n (v:vect n) {struct v} :=
  match v with
  | nil => acc
  | cons n' e v' => h (plus acc e) n' v'
  end
with h acc n v {struct v} :=
  match v with
  | nil => acc
  | cons n' e v' => g (times acc e) n' v'
  end
.

Inductive list : Type :=
| Nil : list
| Cons : list -> list.

Inductive pair_list : list -> Type :=
| pair_nil  : pair_list Nil
| pair_nons : forall (l : list), pair_list (Cons (Cons l)).
