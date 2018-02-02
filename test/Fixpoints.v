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

