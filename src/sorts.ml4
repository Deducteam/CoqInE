(** Translation of Coq universes *)

(** Since the Coq universes are implemented as an abstract datatype, we cannot
    access the information directly. This module uses a trick that involves
    sorting the universe graph and manipulating the string representations. *)

open Genlex

open Pp

open Info

let coq x = Dedukti.Var(Name.coq x)

let coq_srt = coq "Sort"
let coq_p = coq "prop"
let coq_z = Dedukti.app (coq "type") (coq "z")
let coq_t i = Dedukti.apps (coq "axiom") [i]
let coq_r i j = Dedukti.apps (coq "rule") [i; j]
let coq_m i j = Dedukti.apps (coq "sup") [i; j]

let rec make_coq_univ n =
  if n = 0 then coq_z else coq_t (make_coq_univ (n - 1))

type universe =
  | Set
  | Atom of string
  | Succ of universe
  | Max of universe list

(** Maping from the string reresentation of abstract atomic universes to
    concrete levels. *)
let universe_table : (string, int) Hashtbl.t = Hashtbl.create 10007

(** Dump universe graph [universes] in the universe table. *)
let set_universes universes =
  msgnl (str "Sorting universes");
  let universes = Univ.sort_universes universes in
  msgnl (str "Saving universes");
  let register constraint_type j k =
    Scanf.sscanf k "Type.%d" (fun k -> Hashtbl.add universe_table j k) in
  Univ.dump_universes register universes

(** Evaluate a universe according to the solutions in the universe table. *)
let evaluate_universe i =
  let rec evaluate i =
    match i with
    | Set -> coq_z
    | Atom(i) ->
        begin try make_coq_univ (Hashtbl.find universe_table i) with
        | Not_found -> coq_z
        end
    | Succ(i) -> coq_t (evaluate i)
    | Max([]) -> coq_p
    | Max([i]) -> evaluate i
    | Max(i :: j_list) -> coq_m (evaluate i) (evaluate (Max(j_list))) in
  evaluate i

let lexer = Genlex.make_lexer ["."; "+"; "("; ","; ")"]

let rec parse_universe = parser
  | [< 'Ident "Set" >] -> Set
  | [< 'Kwd "("; i = parse_universe; 'Kwd ")"; 'Kwd "+"; 'Int 1 >] -> Succ(i)
  | [< 'Ident "max"; 'Kwd "("; i_list = parse_universes; 'Kwd ")" >] -> Max(i_list)
  | [< a = parse_atom >] -> Atom(a)
and parse_universes = parser
  | [< i = parse_universe; 'Kwd ","; i_list = parse_universes >] -> i :: i_list
  | [< i = parse_universe >] -> [i]
  | [< >] -> []
and parse_atom = parser
  | [< 'Int i >] -> Printf.sprintf "%d" i
  | [< 'Ident p; 'Kwd "."; rest = parse_atom >] ->  Printf.sprintf "%s.%s" p rest

(** Translate the Coq universe [i] as a concrete Dedukti universe. *)
let translate_universe info env i =
  (* Print the universe [i] to obtain a string representation and extract
     the universe by parsing the string representation. *)
  let i_str = (
    Pp.pp_with Format.str_formatter (Univ.pr_uni i);
    Format.flush_str_formatter ()) in
  let i =
    try parse_universe (lexer (Stream.of_string i_str)) with
    | Stream.Failure
    | Stream.Error _ -> failwith (Printf.sprintf "Unable to parse universe %s" i_str) in
  (* Evaluate the universe using the solutions of the universe graph. *)
  evaluate_universe i

