(* Universe polymorphism *)

Definition id {A : Type} (a : A) := a.

Polymorphic Definition pid {A : Type} (a : A) := a.

Print pid.

Definition pid_id      := pid (@id).
Definition pid_id_expl := @pid (forall A:Type,A->A) (@id).

Definition pid_pid      := pid (@pid).
Definition pid_pid_expl := @pid (forall A:Type,A->A) (@pid). (* identical *)

Polymorphic Definition p_pid_id      := pid (@id).
Polymorphic Definition p_pid_id_expl := @pid (forall A:Type,A->A) (@id).

Polymorphic Definition p_pid_pid      := pid (@pid).
Polymorphic Definition p_pid_pid_expl := @pid (forall A:Type,A->A) (@pid).
