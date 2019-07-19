(** Dedukti syntax and pretty-printing functions *)

open Debug

type var = string

type term =
    Type
  | Var of var
  | Pie of (var * term) * term
  | Lam of (var * term option) * term
  | LetIn of (var * term * term) * term
  | App of term * term
  | Dot of term
  | Cmt of string * term
  | Bracket of term
  | Wildcard

type context = (var * term) list

type untyped_context = var list

type instruction =
  | EmptyLine
  | Comment of string
  | Command of string * string list
  | Declaration of bool * var * term
  | Definition of bool * var * term * term
  | UDefinition of bool * var * term
  | Rewrite of untyped_context * term * term

val var : var -> term
val arr : term -> term -> term
val pie : var * term -> term -> term
val lam : var * term -> term -> term
val letin : var * term * term -> term -> term
val ulam : var -> term -> term
val app : term -> term -> term
val dot : term -> term
val cmt : string -> term -> term
val wildcard : term
val bracket : term -> term
val vars : var list -> term list
val arrs : term list -> term -> term
val pies : (var * term) list -> term -> term
val lams : (var * term) list -> term -> term
val ulams : var list -> term -> term
val apps : term -> term list -> term
val comment : string -> instruction
val command : string -> string list -> instruction
val declaration : bool -> var -> term -> instruction
val definition : bool -> var -> term -> term -> instruction
val udefinition : bool -> var -> term -> instruction
val rewrite : untyped_context * term * term -> instruction
val typed_rewrite : context * term * term -> instruction

val apply_context : term -> context -> term

(** Prints out instructions *)
val print  : instruction printer

(** Prints out instructions in a condensed way
  (for prelude declarations, comments, etc) *)
val printc : instruction printer

val pp_term : term printer
