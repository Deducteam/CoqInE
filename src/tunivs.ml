(** Translation of Coq universe levels *)

open Debug
open Translator

(* Copied From Coqv8.14 *)
let dump_universes output g =
  let open Univ in
  let dump_arc u = function
    | UGraph.Node ltle ->
      Univ.Level.Map.iter (fun v strict ->
          let typ = if strict then Lt else Le in
          output typ u v) ltle;
    | UGraph.Alias v ->
      output Eq u v
  in
  Univ.Level.Map.iter dump_arc (UGraph.repr g)

(** Get all global universes names together with their concrete levels *)
let get_universe_levels g =
  let gene = Hashtbl.create 10007 in
  let cano = Hashtbl.create 10007 in
  let open Univ in
  let g = UGraph.repr g in
  let wl = ref Level.Set.empty in
  let rec cano_arc u nd =
    if Hashtbl.mem cano u then Hashtbl.find cano u
    else
      match nd with
      | UGraph.Node _ ->
         Hashtbl.add gene u 0;
         wl := Level.Set.add u !wl;
         Hashtbl.add cano u u; u
      | UGraph.Alias v ->
         let v' = cano_arc v (Level.Map.find v g) in
         Hashtbl.add cano u v'; v' in
  Level.Map.iter (fun u v -> ignore (cano_arc u v)) g;
  let rec proc () =
    if Level.Set.is_empty !wl then ()
    else
      (let u = Level.Set.choose !wl in
       wl := Level.Set.remove u !wl;
       (if not (Level.is_set u)
       then
         let n = Hashtbl.find gene u in
         (match Level.Map.find u g with
         | UGraph.Alias _ -> assert false
         | UGraph.Node ltle ->
            Level.Map.iter (fun v is_lt ->
                let v = Hashtbl.find cano v in
                let m = Hashtbl.find gene v in
                let m' = if is_lt then n+1 else n in
                if m < m' && not (Level.is_set v) then (
                  (*message "Propagate: %s(%d) <= %s(%d) := %d" (Level.to_string u) m  (Level.to_string v) m m';*)
                  wl := Level.Set.add v !wl; Hashtbl.replace gene v m'))
              ltle));
       proc()) in
  proc();
  let res = ref [] in
  Hashtbl.iter (fun u v ->
      let v' = Hashtbl.find gene v in
      res := (u, v') :: !res) cano;
  !res


module StringSet = Set.Make(struct type t = string let compare = String.compare end)

(** Returns all global universe level names and all constraints on these levels *)
let get_universes_constraints (universes:UGraph.t) =
  let defined_univs = ref StringSet.empty in
  let reg u =

    if      Univ.Level.is_set u then set_level
    else if Univ.Level.is_set u then set_level
    (* Hack to represent Prop as a level even though it shouldn't *)
    else
      let u' = Univ.Level.to_string u in
      if Utils.str_starts_with "Type." u'
      then mk_level (Scanf.sscanf u' "Type.%d" (fun x -> x))
      else begin
        if not (StringSet.mem u' !defined_univs)
        then defined_univs := StringSet.add u' !defined_univs;
        Translator.GlobalLevel u'
    end
  in
  let res = ref [] in
  let register ct j k =
    match ct, reg j, reg k with
    | Univ.Lt, Translator.Lvl 0, Translator.Lvl 0 -> () (* ignore the Prop < Set constraint *)
    | _ , jd, kd -> res := (j, jd, ct, k, kd) :: !res in
  dump_universes register universes;
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
    Dedukti.definition
      false (T.coq_univ_name (Univ.Level.to_string name))
      (T.coq_Lvl ()) (T.coq_level (mk_level lvl)) in
  List.map get_definition (get_universe_levels universes)

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
