(** Translation of Coq universes *)

open Genlex

let coq x = Dedukti.Var(Name.coq x)

let coq_srt = coq "srt"
let coq_p = coq "p"
let coq_z = coq "z"
let coq_t i = Dedukti.apps (coq "t") [i]
let coq_r i j = Dedukti.apps (coq "r") [i; j]

let rec make_coq_univ n =
  if n = 0 then coq_z else coq_t (make_coq_univ (n - 1))

type universe =
  | Set
  | Atom of string
  | Succ of universe
  | Max of universe list

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
  
let rec evaluate_universe solutions i =
  let rec evaluate i =
    match i with
    | Set -> coq_z
    | Atom(i) ->
        (try make_coq_univ (Hashtbl.find solutions i)
         with Not_found -> coq_z)
    | Succ(i) -> coq_t (evaluate i)
    | Max([]) -> failwith "Empty max"
    | Max([i]) -> evaluate i
    | Max(i :: j_list) -> coq_r (evaluate i) (evaluate (Max(j_list))) in
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
  let i =
    try parse_universe (lexer (Stream.of_string i_str)) with
    | Stream.Failure
    | Stream.Error _ -> failwith (Printf.sprintf "Unable to parse universe %s" i_str) in
  (* Sort the universes to solve the constraints and save them in a table. *)
  let universes = Univ.sort_universes (Environ.universes env) in
  let solutions = Hashtbl.create 10007 in
  let register constraint_type j k =
    Scanf.sscanf k "Type.%d" (fun k -> Hashtbl.add solutions j k) in
  Univ.dump_universes register universes;
  (* Evaluate the universe using the solutions of the constraints. *)
  evaluate_universe solutions i

