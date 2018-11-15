(** Translation of Coq universe levels *)

open Pp
open Dedukti
open Debug


(** Get all global universes names together with their concrete levels *)
let get_universes_levels universes =
  let universes = UGraph.sort_universes universes in
  let res = ref [] in
  let register constraint_type j k =
    match constraint_type with
    | Univ.Eq ->
      let closed_univ = Scanf.sscanf k "Type.%d" (fun x -> x) in
      res := (Translator.coq_univ_name j, closed_univ):: !res
    | Univ.Lt | Univ.Le -> () in
  UGraph.dump_universes register universes;
  !res


module StringSet = Set.Make(struct type t = string let compare = String.compare end)

(** Returns all global universes names and all constraints on these universes *)
let get_universes_constraints universes =
  let defined_univs = ref StringSet.empty in
  let reg u =
    if      u = "Set"  then Translator.coq_set
    else if u = "Prop" then Translator.coq_prop
    else if Utils.str_starts_with "Type." u
    then
      let closed_univ = Scanf.sscanf u "Type.%d" (fun x -> x) in
      Translator.coq_univ closed_univ
    else begin
      if not (StringSet.mem u !defined_univs)
      then defined_univs := StringSet.add u !defined_univs;
      Dedukti.var (Translator.coq_univ_name u)
    end
  in
  let res = ref [] in
  let register constraint_type j k =
    let jd = reg j in
    let kd = reg k in
    if j = "Prop" && k = "Set" then ()
    else res := (j, jd, constraint_type, k, kd) :: !res in
  UGraph.dump_universes register universes;
  (StringSet.elements !defined_univs, List.rev !res)


(** Instructions for universe declaration as constant symbols and
  reduction rules on "sup" operator *)
let universe_encoding_float_noconstr universes =
  (* This generates far too many constraints: takes a long time to typecheck. *)
  let unames, cstr = get_universes_constraints universes in
  let register inst (j, jd, constraint_type, k, kd) =
    match constraint_type with
    | Univ.Eq -> (Dedukti.rewrite ([], jd, kd)) :: inst
    | Univ.Le ->
      let pat = PatternTranslator.coq_sup jd kd in
      (Dedukti.rewrite ([], pat, kd)) :: inst
    | Univ.Lt ->
      let pat1 = PatternTranslator.coq_sup jd kd in
      let ax =
        if j = "Set"
        then PatternTranslator.coq_univ 0
        else PatternTranslator.coq_axiom jd in
      let pat2 = PatternTranslator.coq_sup ax kd in
      (Dedukti.rewrite ([], pat1, kd)) ::
      (Dedukti.rewrite ([], pat2, kd)) :: inst in
  let decl_u u = Dedukti.declaration false (Translator.coq_univ_name u) Translator.coq_Sort in
  let inst = List.fold_left register [] cstr in
  List.rev_append (List.map decl_u unames) (EmptyLine :: inst)

(** Instructions for universe declaration as constant symbols and
  constant constraints constructors. *)
let universe_encoding_float_constr universes =
  let unames, cstr = get_universes_constraints universes in
  let counter = ref 0 in
  let decl cstr_type =
    incr counter;
    let fresh_name = "cstr_" ^ (string_of_int !counter) in
    Dedukti.declaration false fresh_name cstr_type
  in
  let register inst (j, jd, constraint_type, k, kd) =
    match constraint_type with
    | Univ.Eq -> (decl (Translator.cstr_leq jd kd)) ::
                 (decl (Translator.cstr_leq kd jd)) :: inst
    | Univ.Le -> (decl (Translator.cstr_leq jd kd)) :: inst
    | Univ.Lt -> (decl (Translator.cstr_le  jd kd)) :: inst in
  let decl_u u = Dedukti.declaration false (Translator.coq_univ_name u) Translator.coq_Sort in
  let inst = List.fold_left register [] cstr in
  List.rev_append (List.map decl_u unames) (EmptyLine :: inst)

(** Instructions for universes declaration as defined symbols
  reducing to their concrete levels. *)
let universe_encoding_nofloat universes =
  let univ_levels = get_universes_levels universes in
  List.map (fun (name, lvl) ->
      Dedukti.definition false name Translator.coq_Sort
        (Translator.coq_univ lvl)) univ_levels


let translate_all_universes info universes =
  message "Translating global universes";
  (pp_list "" Dedukti.printc) info.Info.fmt
    (match Parameters.is_float_univ_on (), Parameters.is_constraints_on () with
     | true , true  -> universe_encoding_float_constr   universes
     | true , false -> universe_encoding_float_noconstr universes
     | false, _     -> universe_encoding_nofloat universes)
