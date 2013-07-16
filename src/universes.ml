(** Translation of Coq universes *)

let coq x = Dedukti.Var(Name.coq x)

let coq_univ = coq "univ"
let coq_z = coq "z"
let coq_s i = Dedukti.apps (coq "s") [i]

(** Translate the Coq universe [i] as a concrete Dedukti universe.
    Since the Coq universes are implemented as an abstract datatype, we cannot
    access the information directly. This function uses a trick that involves
    sorting the universe graph and manipulating the string representation. *)
let translate_universe env i =
  (* Print the universe [i] to obtain a string representation. *)
  Pp.pp_with Format.str_formatter (Univ.pr_uni i);
  let i = Format.flush_str_formatter () in
(*  Dedukti.var i*)
  (* Sort the universes to solve the constraints. *)
  let universes = Univ.sort_universes (Environ.universes env) in
  (* Tarverse the constraints and register when we see [i]. *)
  let solution = ref None in
  let register constraint_type j k =
    if i = j then solution := Some(k) in
  Univ.dump_universes register universes;
  (* Extract the registered universe. *)
  match !solution with
  | None -> coq "z"
  | Some(i) ->
      let rec univ i =
      match i with
      | 0 -> coq_z
      | i -> coq_s (univ (i - 1)) in
      Scanf.sscanf i "Type.%d" univ

