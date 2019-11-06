
Inductive list {A:Type} : Type :=
| Nil : list
| Cons : A -> list -> list.

Definition f : nat -> nat :=
  fun x => (fix aux (n:nat) : nat := 0) x.

(*
Eval compute in (fun x => f x).
 *)

Definition map {A B:Type} (f:A -> B) (l1:list) : list :=
  (fix aux (l:list) : list :=
    match l with
    | Nil => Nil
    | Cons hd tl => Cons (f hd) (aux tl)
    end) l1.

Fixpoint map2 {A B:Type} (f:A -> B) (l:list) : list :=
  match l with
  | Nil => Nil
  | Cons hd tl => Cons (f hd) (map2 f tl)
  end.


Definition aux {A B:Type} (f:A -> B) (map' : (A -> B) -> list -> list) (l:list) : list :=
  match l with
  | Nil => Nil
  | Cons hd tl => Cons (f hd) (map' f tl)
  end.

Fixpoint map3 {A B:Type} (f:A -> B) (l:list) : list := aux f map3 l.

(*
Print map3.
 *)


Fixpoint iter {A:Type} (n:nat) (f:A->A) (x:A) :=
  match n with
  | 0   => x
  | S n => iter n f (f x)
  end.

Definition f4 := iter 4 (fun x : nat => x) 2.
