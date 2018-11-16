(** Dedukti syntax and pretty-printing functions *)

open Debug

type var = string

type term =
    Type
  | Var of var
  | Pie of (var * term) * term
  | Lam of (var * term) * term
  | App of term * term
  | Dot of term
  | Cmt of string * term

type instruction =
  | EmptyLine
  | Comment of string
  | Command of string * string list
  | Declaration of bool * var * term
  | Definition of bool * var * term * term
  | UDefinition of bool * var * term
  | Rewrite of (var * term) list * term * term

val var : var -> term
val arr : term -> term -> term
val pie : var * term -> term -> term
val lam : var * term -> term -> term
val app : term -> term -> term
val dot : term -> term
val cmt : string -> term -> term
val vars : var list -> term list
val arrs : term list -> term -> term
val pies : (var * term) list -> term -> term
val lams : (var * term) list -> term -> term
val apps : term -> term list -> term
val comment : string -> instruction
val command : string -> string list -> instruction
val declaration : bool -> var -> term -> instruction
val definition : bool -> var -> term -> term -> instruction
val udefinition : bool -> var -> term -> instruction
val rewrite : (var * term) list * term * term -> instruction

val apply_context : term -> (var * term) list -> term

(** Prints out instructions *)
val print  : instruction printer

(** Prints out instructions in a condensed way
  (for prelude declarations, comments, etc) *)
val printc : instruction printer

val pp_term : term printer

type cic_universe =
  | Prop (** Impredicative Prop *)
  | Set  (** Set *)
  | LocalNamed of string
  (** Special variable for match *)
  | Local    of int
  (** Locally bounded universe polymorphic variable. *)
  | Template of string             (** "Coq.Module.index" *)
  | Global   of string             (** "Coq.Module.index" *)
  | Succ     of cic_universe * int (** l + n *)
  | Max      of cic_universe list  (** sup {u | u \in l} *)
  | Rule     of cic_universe * cic_universe
  (** *)

(** Note: in Coq cumulativity (subtyping) and axioms are not the same:
  Set  : Type_0 : Type_1 : ...
  Prop : Type_0
but
  Prop < Set < Type_0 < Type_1 < ...
! This hierarchy could change starting v8.10 !
*)


module type CoqTranslator =
sig
  val coq_Sort       : term
  val coq_univ_index : int -> term
  val coq_prop       : term
  val coq_set        : term
  val coq_Sort       : term
  val coq_univ       : int -> term

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
  
  val coq_axiom    : term -> term
  val coq_axioms   : term -> int -> term
  val coq_rule     : term -> term -> term
  val coq_sup      : term -> term -> term
  val coq_U        : cic_universe -> term
  val coq_term     : cic_universe -> term -> term
  val coq_sort     : cic_universe -> term
  val coq_prod     : cic_universe -> cic_universe -> term -> term -> term
  val coq_cast     : cic_universe -> cic_universe -> term -> term -> term -> term
    
  val cstr_leq : term -> term -> term
  val cstr_le  : term -> term -> term

  val coq_header : instruction list
  val coq_footer : instruction list
end

module PatternTranslator : CoqTranslator

module Translator : CoqTranslator
