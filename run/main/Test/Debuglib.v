Set Printing All.

Set Printing Universes.

Require Import Corelib.Init.Notations.
Require Import Corelib.Init.Logic.
Require Import Corelib.Init.Datatypes.
Require Import Corelib.Init.Peano.


Inductive iter (A : Type) (f : A -> Type) : Type -> Type :=
  | nil  : forall a : A, iter A f (f a).

Definition g (A:Type) : A -> A := fun x => x.

Inductive eq_g (A:Type) (a:A) : A -> Prop :=
| refl_g : eq_g A a (g A a).


Inductive option (A : Type) : Type :=
  Some : forall _ : A, option A | None : option A.


Record myrec := mkMyrec { A : Type; a : A; f : A -> A}.

Definition recex : myrec := mkMyrec nat 0 (fun x => x).

(*
Print Universes.
Print Sorted Universes.
*)

(* Simple types *)
Definition xType0 : Type := Set.
Definition xType1 : Type := xType0.
Inductive  xSet   : Set  := I.
Inductive  xProp  : Prop := T.

(* Simple template polymorphic inductive type *)
Inductive exp (A:Type) (B:Type) : Type := tst : forall f: A -> B, exp A B.

Definition test := fun (P:forall e : exp xSet xSet, Type) => exp_rect xSet xSet P.

Definition exp_i := exp_rect xSet xSet.

(* Applied to various parameters. *)
Definition expType0 := @exp xType0 xType0.
Definition expSet   := @exp xSet   xSet.
Definition expProp  := @exp xProp  xProp.


(* Another template polymorphic inductive type *)
Inductive pair (A:Type) (B:Type) : Type :=
| fst : A -> pair A B
| snd : B -> pair A B.

(*
Print pair.
*)
(* Type@{Top.1} -> Type@{Top.2} -> Type@{max(Set, Top.1, Top.2)} *)

Definition aux := pair xSet.

(*
Print aux.
*)
(* Type@{Top.2} -> Type@{max(Set, Top.2)} *)

Definition aux_e (B:Type) : aux B := fst xSet B I.
(*
Print aux_e.
 *)

Definition aux2 := aux xSet.
(*
Print aux2.
*)
(* Type@{max(Set, Top.2)} *)

Definition aux2_e1 : aux2 := fst xSet xSet I.
Definition aux2_e2 : aux2 := snd xSet xSet I.

Definition aux3 := pair xSet xSet.
(*
Print aux3.
*)
(* Set *)

Definition aux3_e1 : aux3 := fst xSet xSet I.
Definition aux3_e2 : aux3 := snd xSet xSet I.


Definition aux4 := (fun x => x xSet) (pair xSet).
(*
Print aux4. (* (fun x : (Set -> Set) => x xSet) (fun B : Set => pair xSet B) *)
*)
(* Set *)

Definition aux4_e1 : aux4 := fst xSet xSet I.
Definition aux4_e2 : aux4 := snd xSet xSet I.

(*
Inductive foo
          (A:xSet)
          (B:xSet -> Type) :
  (B A) -> Type := bar : forall x:(B A), foo A B x.
 *)
