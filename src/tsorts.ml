(** Translation of Coq sorts *)

open Debug
open Translator

open Declarations

let add_templ_params_type params t =
  if Encoding.is_templ_polymorphism_on ()
  then List.fold_right (function u -> Dedukti.pie (u, (T.coq_Sort ()))) params t
  else t

let add_poly_params_type params cstr t =
  if Encoding.is_polymorphism_on ()
  then List.fold_right (function u -> Dedukti.pie (u, (T.coq_Sort ()))) params
      (List.fold_right (function (cstr,_) -> Dedukti.pie cstr) cstr t)
  else t

let add_poly_params_def params cstr t =
  if Encoding.is_polymorphism_on ()
  then List.fold_right (function u -> Dedukti.lam (u, (T.coq_Sort ()))) params
      (List.fold_right (function (cstr,_) -> Dedukti.lam cstr) cstr t)
  else t

(** Maping from the string representation of global named universes to
    concrete levels. *)
let universe_table : (string, int) Hashtbl.t = Hashtbl.create 10007

(** Dump universe graph [universes] in the universe table. *)
let set_universes universes =
  message "Saving universes";
  if (not (Encoding.is_float_univ_on ()))
  then begin
    let universes = UGraph.sort_universes universes in
    let register constraint_type j k =
      match constraint_type with
      | Univ.Eq -> Scanf.sscanf k "Type.%d" (fun k -> Hashtbl.add universe_table j k)
      | Univ.Lt | Univ.Le -> () in
    UGraph.dump_universes register universes
  end


(** Translates a universe level in a given local universe environment  *)
let translate_level uenv l =
  if Univ.Level.is_prop l then Translator.Prop
  else if Univ.Level.is_set l then Translator.Set
  else
    match Univ.Level.var_index l with
    | Some n ->
      if Encoding.is_polymorphism_on ()
      then Translator.Local n (* Locally bounded universe variable *)
      else failwith "Universe polymorphism no supported by this encoding "
    | None ->
      if Info.is_template_polymorphic uenv l
      then Translator.Template (Info.translate_template_arg uenv l)
      else
        let name = Univ.Level.to_string l in
        if Encoding.is_float_univ_on () || Encoding.is_named_univ_on ()
        then Translator.Global name
        else
          try Translator.mk_type (Hashtbl.find universe_table name)
          with Not_found -> failwith (Format.sprintf "Unable to parse atom: %s" name)

let instantiate_poly_univ_params uenv name univ_ctxt univ_instance =
  let nb_params = Univ.AUContext.size univ_ctxt in
  debug "Instantiating %i polymorphic universes %s: %a" nb_params name pp_coq_inst univ_instance;
  if Univ.Instance.length univ_instance < nb_params
  then debug "Something suspicious is going on with thoses universes...";
  let levels = Univ.Instance.to_array univ_instance in
  let levels = Array.init nb_params (fun i -> levels.(i)) in
  Array.fold_left
    (fun t l -> Dedukti.app t (T.coq_universe (translate_level uenv l)))
    (Dedukti.var name)
    levels

let instantiate_template_univ_params uenv name univ_ctxt univ_instance =
  let nb_instance = Univ.Instance.length univ_instance in
  let nb_params = List.length univ_ctxt in
  if nb_instance < nb_params
  then debug "Something suspicious is going on with thoses universes...";
  debug "Instantiating %i template universes %s: %a" nb_params name pp_coq_inst univ_instance;
  debug "Univ context: %a" (pp_list " " (pp_option "None" pp_coq_level)) univ_ctxt;

  let levels = Univ.Instance.to_array univ_instance in
  let rec aux acc i = function
    | None     :: tl -> aux acc (i+1) tl
    | (Some a) :: tl when i < nb_instance ->
      let lvl = if i < nb_instance then levels.(i) else a in
      aux (lvl::acc) (i+1) tl
    | _             -> List.rev acc
  in
  let levels = aux [] 0 univ_ctxt in
  debug "Univ context: %a" (pp_list " " pp_coq_level) levels;

  List.fold_left
    (fun t l -> Dedukti.app t (T.coq_universe (translate_level uenv l)))
    (Dedukti.var name)
    levels

let instantiate_ind_univ_params env uenv name ind univ_instance =
  let (mib,oib) = Inductive.lookup_mind_specif env ind in
  let indtype, res =
    if Encoding.is_polymorphism_on () &&
       Environ.polymorphic_ind ind env
    then
      begin
        let univ_ctxt = Declareops.inductive_polymorphic_context mib in
        "polymorphic",
        instantiate_poly_univ_params uenv name univ_ctxt univ_instance
      end
    else
      match oib.mind_arity with
      | TemplateArity ar when Encoding.is_templ_polymorphism_on () ->
        "template",
        instantiate_template_univ_params uenv name ar.template_param_levels univ_instance
      | _ -> "", Dedukti.var name
  in
  debug "Printing %s inductive constructor: %s@@{%a} : %a" indtype name
    pp_coq_inst univ_instance
    Dedukti.pp_term res;
  res


let translate_universe uenv u =
  (* debug "Translating universe : %a" pp_coq_univ u; *)
  let translate (lvl, i) =
    let univ = translate_level uenv lvl in
    if i = 0 then univ else Translator.Succ (univ,i)
  in
  match Univ.Universe.map translate u with
  | []     -> Translator.Prop
  | [l]    -> l
  | levels -> Translator.Max levels

let translate_sort uenv = function
  | Term.Prop Sorts.Null -> Translator.Prop
  | Term.Prop Sorts.Pos  -> Translator.Set
  | Term.Type i    -> translate_universe uenv i

let convertible_sort uenv s1 s2 =
  translate_sort uenv s1 = translate_sort uenv s2

let translate_local_level l =
  assert (not (Univ.Level.is_small l));
  match Univ.Level.var_index l with
  | None -> assert false
  | Some n -> T.coq_var_univ_name n

let translate_univ_poly_params (uctxt:Univ.Instance.t) =
  if Encoding.is_polymorphism_on ()
  then
    let params_lst = Array.to_list (Univ.Instance.to_array uctxt) in
    List.map translate_local_level params_lst
  else []

let translate_univ_poly_constraints (uctxt:Univ.Constraint.t) =
  if Encoding.is_polymorphism_on ()
  then
    let aux n cstr =
      let (i, c, j) = cstr in
      let cstr_type = match c with
      | Univ.Lt -> T.cstr_lt (translate_level Info.dummy i) (translate_level Info.dummy j)
      | Univ.Le -> T.cstr_le (translate_level Info.dummy i) (translate_level Info.dummy j)
      | Univ.Eq -> T.cstr_le (translate_level Info.dummy i) (translate_level Info.dummy j)
      (*
      Error.not_supported
       (Format.asprintf "Eq constraints %a = %a" pp_coq_level i pp_coq_level j)
      *)
      in
      let cstr_name = Cname.constraint_name n in
      ( (cstr_name, cstr_type), cstr)
    in
    List.mapi aux (Univ.Constraint.elements uctxt)
  else []



(** Extracts template parameters levels and returns them with their dedukti names
    e.g.: Level(Top,42) -> "Top__42"
*)
let translate_template_params (ctxt:Univ.Level.t option list) : Univ.Level.t list * Dedukti.var list =
  if Encoding.is_templ_polymorphism_on ()
  then
    let params = Utils.filter_some ctxt in
    let aux l =
      match Univ.Level.name l with
      | Some (d,n) -> T.coq_univ_name (Univ.Level.to_string l)
      | None -> assert false
      (* No small levels (Prop/Set) or (true) polymorphism variables in template params. *)
    in
    params, List.map aux params
  else [],[]
