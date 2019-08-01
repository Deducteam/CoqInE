

Definition id (a:Type) : a -> a := fun x => x.

Definition foo := id (forall a:Type, a -> a) id.


Set Universe Polymorphism.

Definition id2 (a:Type) : a -> a := fun x => x.

Definition foo2 := id2 (forall a:Type, a -> a) id2.
