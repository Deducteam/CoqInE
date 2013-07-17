(** Translation of Coq universes *)

open Genlex

let coq x = Dedukti.Var(Name.coq x)

let coq_univ = coq "univ"
let coq_z = coq "z"
let coq_s i = Dedukti.apps (coq "s") [i]
let coq_max i j = Dedukti.apps (coq "max") [i; j]

let rec make_coq_univ n =
  if n = 0 then coq_z else coq_s (make_coq_univ (n - 1))

type universe =
  | Atom of string
  | Succ of universe
  | Max of universe list

let lexer = Genlex.make_lexer ["."; "+"; "max"; "("; ","; ")"]

let rec parse_universe = parser
    [< 'Ident p; 'Kwd "."; 'Int i >] -> Atom(Printf.sprintf "%s.%d" p i)
  | [< 'Kwd "("; i = parse_universe; 'Kwd ")"; 'Kwd "+"; 'Int 1 >] -> Succ(i)
  | [< 'Kwd "max"; 'Kwd "("; i_list = parse_universes; 'Kwd ")" >] -> Max(i_list)
and parse_universes = parser
    [< i = parse_universe; 'Kwd ","; i_list = parse_universes >] -> i :: i_list
  | [< i = parse_universe >] -> [i]
  | [< >] -> []

let rec evaluate_universe solutions i =
  let rec evaluate i =
    match i with
    | Atom(i) ->
        (try make_coq_univ (Hashtbl.find solutions i)
         with Not_found -> coq_z)
    | Succ(i) -> coq_s (evaluate i)
    | Max([]) -> failwith "Empty max"
    | Max([i]) -> evaluate i
    | Max(i :: j_list) -> coq_max (evaluate i) (evaluate (Max(j_list))) in
  evaluate i

(** Translate the Coq universe [i] as a concrete Dedukti universe.
    Since the Coq universes are implemented as an abstract datatype, we cannot
    access the information directly. This function uses a trick that involves
    sorting the universe graph and manipulating the string representation. *)
let translate_universe env i =
  (* Print the universe [i] to obtain a string representation and extract
     the universe by parsing the string representation. *)
  let i_str =
    (Pp.pp_with Format.str_formatter (Univ.pr_uni i);
     Format.flush_str_formatter ()) in
  let i = parse_universe (lexer (Stream.of_string i_str)) in
  (* Sort the universes to solve the constraints and save them in a table. *)
  let universes = Univ.sort_universes (Environ.universes (Global.env ())) in
  let solutions = Hashtbl.create 10007 in
  let register constraint_type j k =
    Scanf.sscanf k "Type.%d" (fun k -> Hashtbl.add solutions j k) in
  Univ.dump_universes register universes;
  (* Evaluate the universe using the solutions of the constraints. *)
  evaluate_universe solutions i

