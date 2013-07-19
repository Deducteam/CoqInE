Parameter x : Prop.

Definition y : Type := (x : Type).

Definition f : Prop -> Type := ((fun x : Prop => x) : Prop -> Type).

Definition g : Prop -> Type := fun x : Prop => (x : Type).

Parameter P : (Prop -> Type) -> Type.

Parameter h : forall f : (Prop -> Type), P f.

Definition z : P f := h g.

