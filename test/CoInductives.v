CoInductive stream : Type :=
| seq : nat -> stream -> stream.

CoFixpoint id (p : stream) :=
match p with
| seq n q => seq n (id p)
end.

Definition hd (p : stream) :=
match p with
| seq n q => n
end.

Definition tl (p : stream) :=
match p with
| seq n q => q
end.

CoInductive eq_stream : stream -> stream -> Type :=
| eq_stream_refl : forall (p : stream) (q : stream),
  eq (hd p) (hd q) -> eq_stream (tl p) (tl q) -> eq_stream p q.

Check eq_refl.

CoFixpoint zeros : stream := seq 0 zeros.

CoFixpoint eq_zeros_id_zeros : eq_stream zeros (id zeros) :=
eq_stream_refl zeros (id zeros) (eq_refl 0) eq_zeros_id_zeros.

Definition match_stream (P : stream -> Type) (p : stream)
  (case_seq : forall (n : nat) (q : stream),  P (seq n q)) : P p :=
match p with
| seq n q => case_seq n q
end.
