

Definition f :=
  fun a : Type => fun b : Type =>
  let f a b := let b := a in b in
  let a := b in
  f a b.

Definition test (P : Type -> Type) (h : forall f, P f) (a : Type) (b : Type) : P a := h a.

