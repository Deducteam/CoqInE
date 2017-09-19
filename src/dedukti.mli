(** Dedukti syntax and pretty-printing functions *)

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
    Comment of string
  | Command of string * string list
  | Declaration of bool * var * term
  | Definition of bool * var * term * term
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
val rewrite : (var * term) list * term * term -> instruction
val apply_context : term -> (var * 'a) list -> term
val print_var : Format.formatter -> string -> unit
val print_term : Format.formatter -> term -> unit
val print_app : Format.formatter -> term -> unit
val print_atomic : Format.formatter -> term -> unit
val print_binding : Format.formatter -> var * term -> unit
val print_binding_context : Format.formatter -> string * 'a -> unit
val print_context : Format.formatter -> (string * 'a) list -> unit
val print : Format.formatter -> instruction -> unit

type coq_universe =
  | Prop
  | Set
  | Atom of string
  | Succ of coq_universe * int
  | Max of coq_universe list

val coqify : string -> string

val coq_var : string -> term

val coq_Sort  : term
val coq_z     : term
val coq_s     : term -> term
val coq_univ_index : int -> term
val coq_prop  : term
val coq_set   : term
val coq_type  : term -> term
val coq_univ  : int -> term
val coq_axiom : term -> term
val coq_axioms: term -> int -> term
val coq_rule  : term -> term -> term
val coq_sup   : term -> term -> term
val coq_U     : term -> term
val coq_term  : term -> term -> term
val coq_sort  : term -> term
val coq_prod  : term -> term -> term -> term -> term
val coq_cast  : term -> term -> term -> term -> term -> term

val start_debug : unit -> unit
val stop_debug : unit -> unit

val debug_dk_term : term -> unit
val debug_coq_term : Term.constr -> unit
val debug_coq_type : Term.types -> unit
val debug_string : string -> unit
