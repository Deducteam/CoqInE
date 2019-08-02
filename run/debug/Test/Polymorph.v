

Definition id (a:Type) : a -> a := fun x => x.

Definition foo := id (forall a:Type, a -> a) id.
