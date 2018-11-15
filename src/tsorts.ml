(** Translation of Coq sorts *)

open Dedukti
open Debug

(** Prepend template universe parameters before type *)
let add_sort_params params t =
  List.fold_right (function u -> pie (u, Translator.coq_Sort)) params t


(** Maping from the string reresentation of global named universes to
    concrete levels. *)
let universe_table : (string, int) Hashtbl.t = Hashtbl.create 10007

(** Dump universe graph [universes] in the universe table. *)
let set_universes universes =
  message "Saving universes";
  if (not (Parameters.is_float_univ_on ()))
  then begin
    let universes = UGraph.sort_universes universes in
    let register constraint_type j k =
      match constraint_type with
      | Univ.Eq -> Scanf.sscanf k "Type.%d" (fun k -> Hashtbl.add universe_table j k)
      | Univ.Lt | Univ.Le -> () in
    UGraph.dump_universes register universes
  end

(** Translate an atom universe according to the solutions in the universe table. *)
let translate_atom uenv a =
  if Info.is_template_polymorphic uenv a
  then Dedukti.var (Info.translate_template_arg uenv a)
  else if Parameters.is_float_univ_on ()
  then Translator.coq_global_univ a
  else
    try Translator.coq_univ (Hashtbl.find universe_table a)
    with Not_found -> failwith (Printf.sprintf "Unable to parse atom: %s" a)

let evaluate_universe uenv i =
  let rec evaluate = function
    | Set -> Translator.coq_set
    | Prop -> Translator.coq_prop
    | Atom a -> translate_atom uenv a
    | Succ (u,i) -> Translator.coq_axioms (evaluate u) i
    | Max []  -> Translator.coq_prop
    | Max [i] -> evaluate i
    | Max (i :: j_list) -> Translator.coq_sup (evaluate i) (evaluate (Max(j_list))) in
  evaluate i

(** Translates a universe level *)
let translate_level uenv l =
  if Univ.Level.is_prop l then Dedukti.Translator.coq_prop
  else if Univ.Level.is_set l then Dedukti.Translator.coq_set
  else
    match Univ.Level.var_index l with
    | None -> translate_atom uenv (Univ.Level.to_string l)
    | Some n -> Dedukti.var (Translator.coq_var_univ_name n)

let instantiate_univ_params uenv name univ_ctxt univ_instance =
  let nb_params = Univ.AUContext.size univ_ctxt in
  if Univ.Instance.length univ_instance < nb_params
  then debug "Something suspicious is going on with thoses universes...";
  let levels = Univ.Instance.to_array univ_instance in
  let levels = Array.init nb_params (fun i -> levels.(i)) in 
  if Array.length levels > 0 then debug "Instantiating: %a" pp_coq_inst univ_instance;
  Array.fold_left
    (fun t l -> Dedukti.app t (translate_level uenv l))
    (Dedukti.var name)
    levels

let translate_universe uenv i =
  debug "Translating universe : %a" pp_coq_univ i;
  let i_str = Pp.string_of_ppcmds (Univ.pr_uni i) in
  evaluate_universe uenv (Univparse.translate_universe i_str)

let translate_univ_poly_params (uctxt:Univ.Instance.t) =
  if Parameters.is_polymorphism_on ()
  then
    let params = Array.to_list (Univ.Instance.to_array uctxt) in
    let aux l = assert (not (Univ.Level.is_small l));
      match Univ.Level.var_index l with
      | None -> assert false
      | Some n -> Translator.coq_var_univ_name n
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
        (coq_name, Translator.coq_univ_name coq_name)
      | Some n -> assert false
    in
    List.map aux params
  else []
