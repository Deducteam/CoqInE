(* Universe polymorphism *)

Set Printing All.
Set Printing Universes.

Definition id {A : Type} (a : A) := a.

Polymorphic Definition pid {A : Type} (a : A) := a.

Definition pid_id      := pid (@id).
Definition pid_id_expl := @pid (forall A:Type,A->A) (@id).

Definition pid_pid      := pid (@pid).
Definition pid_pid_expl := @pid (forall A:Type,A->A) (@pid). (* identical *)

Polymorphic Definition p_pid_id      := pid (@id).
Polymorphic Definition p_pid_id_expl := @pid (forall A:Type,A->A) (@id).


(*
Print Universes.

Print pid_id.

Print p_pid_id_expl.
Print id.
*)

Polymorphic Definition p_pid_pid      := pid (@pid).
Polymorphic Definition p_pid_pid_expl := @pid (forall A:Type,A->A) (@pid).




Polymorphic Definition templ_poly_f (A:Type) := A.

Inductive templ_poly_g (A:Type) : Type :=
| templ_poly_cons : templ_poly_f A -> templ_poly_g A.

Definition templ_poly_t := @templ_poly_cons True I.
