(** Translation of Coq universes *)

(** Since the Coq universes are implemented as an abstract datatype, we cannot
    access the information directly. This module uses a trick that involves
    sorting the universe graph and manipulating the string representations. *)

open Genlex

open Dedukti

let lexer = Genlex.make_lexer ["."; "+"; "("; ","; ")"]

let rec parse_universe = parser
  | [< 'Ident "Set"; 'Kwd "+"; 'Int i >] -> Succ (Set, i)
  | [< 'Ident "Set" >] -> Set
  | [< 'Ident "Prop"; 'Kwd "+"; 'Int i >] -> Succ (Prop, i)
  | [< 'Ident "Prop" >] -> Prop
  | [< 'Kwd "("; u = parse_universe; 'Kwd ")"; 'Kwd "+"; 'Int i >] -> Succ (u,i)
  | [< 'Ident "max"; 'Kwd "("; i_list = parse_universes; 'Kwd ")" >] -> Max(i_list)
  | [< a = parse_atom; 'Kwd "+"; 'Int i >] -> Succ (Atom(a),i)
  | [< a = parse_atom >] -> Atom(a)
and parse_universes = parser
  | [< i = parse_universe; 'Kwd ","; i_list = parse_universes >] -> i :: i_list
  | [< i = parse_universe >] -> [i]
  | [< >] -> []
and parse_atom = parser
  | [< 'Int i >] -> Printf.sprintf "%d" i
  | [< 'Ident p; 'Kwd "."; rest = parse_atom >] ->  Printf.sprintf "%s.%s" p rest


(* Print the universe [i] to obtain a string representation and extract
   the universe by parsing the string representation. *)
let translate_universe i_str =
  try parse_universe (lexer (Stream.of_string i_str)) with
  | Stream.Failure
  | Stream.Error _ -> failwith (Printf.sprintf "Unable to parse universe %s" i_str)
