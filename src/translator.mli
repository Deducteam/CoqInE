open Dedukti

(** Intermediate representation of universes *)
type cic_universe =
  | Prop (** Impredicative Prop *)
  | Set  (** Predicative   Set *)
  | LocalNamed of string
  (** Special variable for match *)
  | Local    of int
  (** Locally bounded universe polymorphic variable. *)
  | Template of string
  (** Locally bounded template polymorphic universe  "Coq.Module.index"  *)
  | Global   of string
  (** Global universe "Coq.Module.index" *)
  | Succ     of cic_universe * int (** l + n *)
  | Max      of cic_universe list  (** sup {u | u \in l} *)
  | Rule     of cic_universe * cic_universe
  (** Universe *)

val mk_type : int -> cic_universe
(** [mk_type i] represents Type_i = Set + (i+1) = Prop + (i+1)  *)

(*
Note: in Coq cumulativity (subtyping) and axioms are not the same:
  Set  : Type_0 : Type_1 : ...
  Prop : Type_0
but
  Prop < Set < Type_0 < Type_1 < ...
! This hierarchy is supposed to change starting v8.10 !
*)


module T :
sig
  val coq_Sort : unit -> term
  (** Term representing the type of sorts. *)

  val coq_var_univ_name : int -> var
  (** Translates a "var" universe level's name  *)

  val coq_univ_name : string -> var
  (** Template (local) Coq universe name translation
      e.g.  Coq.Arith.0 --> Coq__Arith__0  *)

  val coq_global_univ : string -> term
  (** Global universe name translation to Dedukti variable
      e.g.  Coq.Arith.0 --> Var "U.Coq__Arith__0"  *)

  val coq_universe   : cic_universe -> term
  (** Translate a universe level *)

  val coq_pattern_universe : cic_universe -> term
  (** Translate a universe level as a rule rhs pattern *)

  val coq_U    : cic_universe -> term
  val coq_term : cic_universe -> term -> term
  val coq_sort : cic_universe -> term
  val coq_prod : cic_universe -> cic_universe -> term -> term -> term
  val coq_cast : cic_universe -> cic_universe -> term -> term -> term -> term
  val coq_lift : cic_universe -> cic_universe -> term -> term

  val coq_proj : int -> term -> term

  val cstr_le : cic_universe -> cic_universe -> term
  val cstr_lt : cic_universe -> cic_universe -> term

  val coq_header : unit -> instruction list
  val coq_footer : unit -> instruction list
end
