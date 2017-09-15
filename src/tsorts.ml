(** Translation of Coq universes *)

open Pp
open Dedukti

let rec make_coq_univ n =
  if n = 0 then coq_type0 else coq_axiom (make_coq_univ (n - 1))

(** Maping from the string reresentation of abstract atomic universes to
    concrete levels. *)
let universe_table : (string, int) Hashtbl.t = Hashtbl.create 10007

(** Dump universe graph [universes] in the universe table. *)
let set_universes universes =
  Feedback.msg_info (str "Sorting universes");
  let universes = UGraph.sort_universes universes in
  Feedback.msg_info (str "Saving universes");
  let register constraint_type j k =
    match constraint_type with
      | Univ.Eq -> Scanf.sscanf k "Type.%d" (fun k -> Hashtbl.add universe_table j k)
      | Univ.Lt | Univ.Le -> () in
  UGraph.dump_universes register universes

(** Evaluate a universe according to the solutions in the universe table. *)
let evaluate_universe i =
  let rec evaluate i =
    match i with
    | Set -> coq_type0
    | Prop -> coq_prop
    | Atom(i) ->
        begin try make_coq_univ (Hashtbl.find universe_table i) with
        | Not_found -> coq_type0
        end
    | Succ(i) -> coq_axiom (evaluate i)
    | Max([]) -> coq_prop
    | Max([i]) -> evaluate i
    | Max(i :: j_list) -> coq_sup (evaluate i) (evaluate (Max(j_list))) in
  evaluate i

(** Translate the Coq universe [i] as a concrete Dedukti universe. *)
let translate_universe info env i =
  let i_str = Pp.string_of_ppcmds (Univ.pr_uni i) in
   evaluate_universe (Univparse.translate_universe i_str)


