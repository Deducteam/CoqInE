
Inductive list {A:Type} : Type :=
| Nil : list
| Cons : A -> list -> list.

Fixpoint map (A B:Type) (f:A -> B) (l1:list) : list :=
  (fix map (l:list) : list :=
    match l with
    | Nil => Nil
    | Cons hd tl => Cons (f hd) (map tl)
    end) l1.
