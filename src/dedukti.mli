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
