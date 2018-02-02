Definition x :=
  let x := Type in
  let x := Prop in
  x.

Definition y := 
  let x := Prop in
  let x := Type in
  x.

Definition f (x : Type) :=
  let y := x in y.

Definition g :=
  fun a : Type => fun b : Type =>
  let g a b := let b := a in b in
  let a := b in
  g a b.

Definition test (P : Type -> Type) (h : forall g, P g) (a : Type) (b : Type) : P a := h a.

