(** Translation of Coq universes *)

open Pp
open Dedukti
open Debug

(** Global Coq universe name translation
  e.g.  Coq.Arith.0 --> Coq__Arith__0  *)
let coq_univ_name  s = String.concat "__" (String.split_on_char '.' s)

(** Translates a "var" universe level *)
let translate_var_level_name n = "s" ^ string_of_int n

(** Translates a "var" or "named" universe level *)
let translate_big_level_name l =
  assert (not (Univ.Level.is_small l));
  match Univ.Level.var_index l with
  | None -> coq_univ_name (Univ.Level.to_string l)
  | Some n -> translate_var_level_name n

(** Translates a universe level *)
let translate_level l =
  if Univ.Level.is_prop l then Dedukti.Translator.coq_prop
  else if Univ.Level.is_set l then Dedukti.Translator.coq_set
  else Dedukti.var (translate_big_level_name l)

(** Prepend template universe parameters before type *)
let add_sort_params params t =
  List.fold_right (function u -> pie (u, Translator.coq_Sort)) params t

let instantiate_univ_params name univ_instance =
  let levels = Univ.Instance.to_array univ_instance in
  if Array.length levels > 0 then debug "Instantiating: %a" pp_coq_inst univ_instance;
  Array.fold_left
    (fun t l -> Dedukti.app t (translate_level l))
    (Dedukti.var name)
    levels

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

(** Translate an atom universe according to the solutions in the universe table. *)
let translate_atom a uenv =
  if Info.is_template_polymorphic uenv a
  then Dedukti.var (Info.translate_template_arg uenv a)
  else
    try Translator.coq_univ (Hashtbl.find universe_table a)
    with Not_found -> failwith (Printf.sprintf "Unable to parse atom: %s" a)

let evaluate_universe info env uenv i =
  let rec evaluate = function
    | Set -> Translator.coq_set
    | Prop -> Translator.coq_prop
    | Atom a -> translate_atom a uenv
    | Succ (u,i) -> Translator.coq_axioms (evaluate u) i
    | Max []  -> Translator.coq_prop
    | Max [i] -> evaluate i
    | Max (i :: j_list) -> Translator.coq_sup (evaluate i) (evaluate (Max(j_list))) in
  evaluate i

let translate_universe info env uenv i =
  debug "Translating universe : %a" pp_coq_univ i;
  let i_str = Pp.string_of_ppcmds (Univ.pr_uni i) in
  evaluate_universe info env uenv (Univparse.translate_universe i_str)

let translate_univ_poly_params (uctxt:Univ.Instance.t) =
  if Parameters.is_polymorphism_on ()
  then
    let params = Array.to_list (Univ.Instance.to_array uctxt) in
    let aux l = assert (not (Univ.Level.is_small l));
      match Univ.Level.var_index l with
      | None -> assert false
      | Some n -> translate_var_level_name n
    in
    List.map aux params
  else []

(** Extract template parameters levels and return an assoc list:
    Coq name -> Dedukti name *)
let translate_template_params (ctxt:Univ.Level.t option list) : (string * Dedukti.var) list =
  if Parameters.is_templ_polymorphism_on ()
  then
    let params = Utils.filter_some ctxt in
    let aux l = assert (not (Univ.Level.is_small l));
      match Univ.Level.var_index l with
      | None -> let coq_name = Univ.Level.to_string l in
        (coq_name, coq_univ_name coq_name)
      | Some n -> assert false
    in
    List.map aux params
  else []
