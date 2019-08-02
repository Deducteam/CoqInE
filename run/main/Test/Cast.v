Parameter x : Prop.

Definition y : Type := (x : Type).

Definition f : Prop -> Type := ((fun x : Prop => x) : Prop -> Type).

Definition g : Prop -> Type := fun x : Prop => x.

Parameter P : (Prop -> Type) -> Type.

Parameter h : forall f : (Prop -> Type), P f.

Definition z : P f := h g.

Definition Type3 : Type := Type.
Definition Type2 : Type3 := Type.
Definition Type1 : Type2 := Type.
Definition Type0 : Type1 := Type.

Definition type_canonicity
  (A : Type0)
  (x : forall (A : Type3), A)
  (y : A -> A)
:=
  y (x A).

(*Definition foo (A : Type) (B : Prop) : Prop := (fun (B : Type0) => forall (x : A), B) B.*)

(* Add a parameter to freeze reduction and force conversion on the term representation. *)

Parameter F : Type0 -> Type0.

(*Definition prop_canonicity*)
(*  (A : Type0)*)
(*  (B : Prop)*)
(*  (x : F ((fun (B : Type0) => forall (x : A), B) B))*)
(*:*)
(*  F ((fun C : Type0 => C) (forall (x : A), B))*)
(*:=*)
(*  x.*)
