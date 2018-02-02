Definition id1 := fun (A : Type) (x : A) => x.
Definition id2 := fun (A : Type) (x : A) => x.
Definition id3 := fun (A : Type) (x : A) => x.

Definition id4 := id1 (forall (A : Type), A -> A) id2.
