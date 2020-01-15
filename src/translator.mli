open Dedukti


(** Representation of universe level expression *)
type level_expr =
  | Lvl of int (** Concrete integer level *)
  | S of int*level_expr (** [S(lvl,i)]  level is  [lvl] + [i] *)
  | GlobalLevel of string
  | NamedLevel  of string
  | Local of int
  (** Locally bounded universe level polymorphic variable. *)
  | Max of level_expr list   (** max {u | u \in l} *)

(** Representation of universe expression *)
type universe_expr =
  | Prop
  | Set  (** Predicative   Set *)
  | Type of level_expr (** Type of a level *)
  | GlobalSort of string
  (** Global universe "Coq.Module.index" *)
  | NamedSort  of string
  (** Locally bounded polymorphic universe  *)
  | LocalU  of int
  (** Locally bounded universe polymorphic variable. (FIXME: should not be used) *)
  | Succ of universe_expr * int
  (** [Succ u n] = u + n
      Notes:
        Succ(u,0) = u
        Succ(Prop,1) = Axiom(Prop) = Type@{0} = Succ(Set ,1)  *)
  | Rule of universe_expr * universe_expr
  | Sup  of universe_expr list
  | SInf

val mk_level : int -> level_expr
val mk_type  : int -> universe_expr
(** [mk_type i] represents Type@{i} in the hierarchy
        Set < Type@{0} < Type@{1} < ...
    Type@{i} = Set + (i+1) = Prop + (i+1)
*)

val set_level : level_expr
val set_univ  : universe_expr


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
  val coq_Lvl : unit -> term
  (** Term representing the type of Type levels (for universe polymorphism). *)

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

  val coq_level : level_expr -> term
  (** Translate a universe level *)

  val coq_universe : universe_expr -> term
  (** Translate a universe *)

  val coq_pattern_universe : universe_expr -> term
  (** Translate a universe level as a rule rhs pattern *)

  val coq_nat_universe : universe_expr -> term
  (** Nat level of universe *)
  val coq_trans_cstr : term -> (term * term) list -> term

  val coq_U    : universe_expr -> term
  val coq_term : universe_expr -> term -> term
  val coq_sort : universe_expr -> term
  val coq_prod : universe_expr -> universe_expr -> term -> term -> term
  val coq_cast : universe_expr -> universe_expr -> term -> term -> (term*term) list -> term -> term
  (** [coq_cast s1 s2 A B
        [ (c1, proof_o_f_c1);
          ...;
          (cn, proof_of_cn) ] t]
      build the cast representation of t from type A : Us1 to B : Us2
      using the given list of constraints
  *)

  val coq_lift : universe_expr -> universe_expr -> term -> term

  val coq_pcast : universe_expr -> universe_expr -> term -> term -> term -> term
  (** Private cast. Use only in inductive subtyping. *)

  val coq_coded : term -> term -> term
  (** returns [code t a]  where a : D t *)

  val coq_pattern_lifted_from_sort : term -> term -> term
  (** [coq_pattern_lifted_from_sort s t] Returns a pattern matching a term lifted from
      sort pattern [s] (for instance a variable). *)
  val coq_pattern_lifted_from_level : var -> term -> term
  (** [coq_pattern_lifted_from_level lvl t] Returns a pattern matching a term lifted from
      sort [type lvl] . *)

  val coq_proj : int -> term -> term

  val coq_cstr : Univ.constraint_type -> universe_expr -> universe_expr -> term
  val coq_Cstr : Univ.constraint_type -> universe_expr -> universe_expr -> term
  val coq_I : unit -> term
  val coq_cstr_eps : term -> term

  val coq_header : unit -> instruction list
  val coq_footer : unit -> instruction list
  val coq_fixpoint : int -> (term*int*term) array -> term array -> int -> term
  val coq_guarded : string -> int -> instruction
end
