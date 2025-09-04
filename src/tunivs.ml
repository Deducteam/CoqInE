(** Translation of Coq universe levels *)

open Debug
open Translator

(** Get all global universes names together with their concrete levels *)
let get_universes_levels (universes:UGraph.t) =
  let universes = UGraph.sort_universes universes in
  let res = ref [] in
  let register constraint_type j k =
    match constraint_type with
    | Univ.Eq ->
      let closed_univ = Scanf.sscanf k "Type.%d" (fun x -> x) in
      res := (T.coq_univ_name j, Translator.mk_level closed_univ):: !res
    | Univ.Lt | Univ.Le -> () in
  UGraph.dump_universes register universes;
  !res


module StringSet = Set.Make(struct type t = string let compare = String.compare end)

(** Returns all global universe level names and all constraints on these levels *)
let get_universes_constraints (universes:UGraph.t) =
  let defined_univs = ref StringSet.empty in
  let reg u =
    if      u = "Set"  then set_level
    else if u = "Prop" then set_level (* Hack to represent Prop as a level *)
    else if Utils.str_starts_with "Type." u
    then mk_level (Scanf.sscanf u "Type.%d" (fun x -> x))
    else begin
      if not (StringSet.mem u !defined_univs)
      then defined_univs := StringSet.add u !defined_univs;
      Translator.GlobalLevel u
    end
  in
  let res = ref [] in
  let register ct j k =
    match ct, reg j, reg k with
    | Univ.Lt, Translator.Lvl 0, Translator.Lvl 0 -> () (* ignore the Prop < Set constraint *)
    | _ , jd, kd -> res := (j, jd, ct, k, kd) :: !res in
  UGraph.dump_universes register universes;
  (StringSet.elements !defined_univs, List.rev !res)


(** Instructions for universe declaration as constant symbols and
    reduction rules on "max" operator *)
let universe_encoding_float_noconstr (universes:UGraph.t) =
  (* This generates far too many constraints: takes a long time to typecheck. *)
  let unames, cstr = get_universes_constraints universes in
  let rw a b = Dedukti.rewrite ([], T.coq_level a, T.coq_level b) in
  let register inst (j, jd, constraint_type, k, kd) =
    match constraint_type with
    | Univ.Eq -> (rw jd                        kd) :: inst
    | Univ.Le -> (rw (Translator.Max [jd; kd]) kd) :: inst
    | Univ.Lt -> (rw (Translator.Max [jd; kd]) kd) ::
                 (rw (Translator.Max [Translator.S(1,jd); kd]) kd) :: inst in
  let decl_u u = Dedukti.declaration false (T.coq_univ_name u) (T.coq_Lvl ()) in
  (List.map decl_u unames) @ Dedukti.EmptyLine :: (List.fold_left register [] cstr)

(** Instructions for universe declaration as constant symbols and
    constant constraints constructors. *)
let universe_encoding_float_constr (universes:UGraph.t) =
  let unames, cstr = get_universes_constraints universes in
  let counter = ref 0 in
  let decl cstr_type =
    incr counter;
    let fresh_name = "cstr_" ^ (string_of_int !counter) in
    Dedukti.declaration false fresh_name cstr_type
  in
  let register inst (j, jd, constraint_type, k, kd) =
    (decl (T.coq_cstr constraint_type (Type jd) (Type kd))) :: inst in
  let decl_u u = Dedukti.declaration false (T.coq_univ_name u) (T.coq_Sort ()) in
  (List.map decl_u unames) @ Dedukti.EmptyLine :: (List.fold_left register [] cstr)

(** Instructions for universes declaration as defined symbols
  reducing to their concrete levels. *)
let universe_encoding_named (universes:UGraph.t) =
  let get_definition (name, lvl) =
    Dedukti.definition false name (T.coq_Lvl ()) (T.coq_level lvl) in
  List.map get_definition (get_universes_levels universes)

let translate_all_universes (info:Info.info) (universes:UGraph.t) =
  message "Translating global universes";
  (pp_list "" Dedukti.printc) info.Info.fmt
    (
      if Encoding.is_float_univ_on ()
      then if Encoding.is_constraints_on ()
        then universe_encoding_float_constr   universes
        else universe_encoding_float_noconstr universes
      else if Encoding.is_named_univ_on ()
      then universe_encoding_named universes
      else []
    )
