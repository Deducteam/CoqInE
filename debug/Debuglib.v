
Set Printing All.
Set Printing Universes.


Definition xType0 : Type := Set.
Definition xType1 : Type := xType0.
Inductive  xSet   : Set  :=.
Inductive  xProp  : Prop :=.

Inductive exp (A:Type) (B:Type) : Type :=
  tst : forall f: A -> B, exp A B.

Definition expType0 := @exp xType0 xType0.
Definition expSet   := @exp xSet   xSet.
Definition expProp  := @exp xProp  xProp.

Print exp.
Check exp xType0.
Check exp xType0 xType1.

Print Universes.

(*
Inductive foo
          (A:xSet)
          (B:xSet -> Type) :
  (B A) -> Type := bar : forall x:(B A), foo A B x.
 *)


(* Universe polymorphism

Definition id {A : Type} (a : A) := a.

Polymorphic Definition pid {A : Type} (a : A) := a.

Definition pid_id      := pid (@id).
Definition pid_id_expl := @pid (forall A:Type,A->A) (@id).

Definition pid_pid      := pid (@pid).
Definition pid_pid_expl := @pid (forall A:Type,A->A) (@pid). (* identical *)

Polymorphic Definition p_pid_id      := pid (@id).
Polymorphic Definition p_pid_id_expl := @pid (forall A:Type,A->A) (@id).

Polymorphic Definition p_pid_pid      := pid (@pid).
Polymorphic Definition p_pid_pid_expl := @pid (forall A:Type,A->A) (@pid).

*)