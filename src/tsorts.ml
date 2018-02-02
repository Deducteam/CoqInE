(** Translation of Coq universes *)

open Pp
open Dedukti


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
let evaluate_universe info env uenv i =
  let rec evaluate i =
    match i with
    | Set -> coq_set
    | Prop -> coq_prop
    | Atom a ->
       if Info.is_poly_univ_str uenv a
       (* If a is a polymorphic universe variable in this context *)
       then coq_univ_var a
       else begin
           try coq_univ (Hashtbl.find universe_table a) with
           | Not_found -> failwith (Printf.sprintf "Unable to parse atom: %s" a)
         end
    | Succ (u,i) -> coq_axioms (evaluate u) i
    | Max []  -> coq_prop
    | Max [i] -> evaluate i
    | Max (i :: j_list) -> coq_sup (evaluate i) (evaluate (Max(j_list))) in
  evaluate i


let translate_universe info env uenv i =
  let i_str = Pp.string_of_ppcmds (Univ.pr_uni i) in
   evaluate_universe info env uenv (Univparse.translate_universe i_str)


