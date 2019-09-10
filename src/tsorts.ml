(** Translation of Coq sorts *)

open Debug
open Translator

open Declarations

let template_constructor_upoly () =
          Encoding.is_templ_polymorphism_on ()
  && not (Encoding.is_templ_polymorphism_code_on ())


let add_poly_params_type params cstr t =
  if Encoding.is_polymorphism_on ()
  then List.fold_right (function u -> Dedukti.pie (u, (T.coq_Nat (*Sort or Nat ?*) ()))) params
      ( if Encoding.is_constraints_on ()
        then List.fold_right (function (_,cstr) -> Dedukti.pie cstr) cstr t
        else t )
  else t

let add_poly_params_def params cstr t =
  if Encoding.is_polymorphism_on ()
  then List.fold_right (function u -> Dedukti.lam (u, (T.coq_Nat (*Sort or Nat ?*) ()))) params
      ( if Encoding.is_constraints_on ()
        then List.fold_right (function (_,cstr) -> Dedukti.lam cstr) cstr t
        else t )
  else t

let get_inductive_params templ_params poly_params poly_cstr =
  (
    if Encoding.is_templ_polymorphism_on ()
    then List.map (function u -> (u, T.coq_Sort())) templ_params
    else []
  ) @ (
    if Encoding.is_polymorphism_on ()
    then List.map (function u -> (u, T.coq_Nat() (*Sort or Nat ?*) )) poly_params
    else []
  ) @ (
    if Encoding.is_constraints_on ()
    then List.map snd poly_cstr
    else []
  )
let add_inductive_params templ_params poly_params poly_cstr arity =
  List.fold_right Dedukti.pie
    (get_inductive_params templ_params poly_params poly_cstr) arity

let get_constructor_params templ_params poly_params poly_cstr =
  (
    if Encoding.is_templ_polymorphism_on () &&
       template_constructor_upoly ()
    then List.map (function u -> (u, T.coq_Sort ())) templ_params
    else []
  ) @ (
    if Encoding.is_polymorphism_on ()
    then List.map (function u -> (u, T.coq_Nat (*Sort or Nat ?*) ())) poly_params
    else []
  ) @ (
    if Encoding.is_constraints_on ()
    then List.map snd poly_cstr
    else []
  )
let add_constructor_params templ_params poly_params poly_cstr arity =
  List.fold_right Dedukti.pie
    (get_constructor_params templ_params poly_params poly_cstr) arity

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


let translate_template_global_level_decl (ctxt:Univ.Level.t option list) =
  if Encoding.is_templ_polymorphism_on ()
  then
    let params = Utils.filter_some ctxt in
    let aux l =
      match Univ.Level.name l with
      | Some (d,n) ->
        let name = Univ.Level.to_string l in
        let name' = T.coq_univ_name name in
        if Encoding.is_float_univ_on ()
        then Dedukti.declaration false name' (T.coq_Nat())
        else
          let univ = Translator.mk_type (Hashtbl.find universe_table name) in
          Dedukti.definition false name' (T.coq_Nat()) (T.coq_level univ)
      | None -> assert false
      (* No small levels (Prop/Set) or (true) polymorphism variables in template params. *)
    in
    List.map aux params
  else []

(** Translates a universe level in a given local universe environment
    using universe_table global environment for named universes.  *)
let translate_univ_level uenv l =
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
      then Info.translate_template_arg uenv l
      else
        let name = Univ.Level.to_string l in
        if Encoding.is_float_univ_on () || Encoding.is_named_univ_on ()
        then Translator.NamedLevel name
        else
          try Translator.mk_type (Hashtbl.find universe_table name)
          with Not_found -> failwith (Format.sprintf "Unable to parse atom: %s" name)

let instantiate_poly_univ_params uenv univ_ctxt univ_instance term =
  let nb_params = Univ.AUContext.size univ_ctxt in
  if Univ.Instance.length univ_instance < nb_params
  then debug "Something suspicious is going on with thoses universes...";
  let levels = Univ.Instance.to_array univ_instance in
  let levels = Array.init nb_params (fun i -> levels.(i)) in
  Array.fold_left
      (fun t l -> Dedukti.app t (T.coq_level (translate_univ_level uenv l)))
      term
      levels

let instantiate_poly_ind_univ_params env uenv ind univ_instance term =
  if Encoding.is_polymorphism_on () &&
     Environ.polymorphic_ind ind env
  then
    begin
      let (mib,oib) = Inductive.lookup_mind_specif env ind in
      debug "Instantiating polymorphic inductive instance %a : {%a}"
        Dedukti.pp_term term
        pp_coq_inst univ_instance;
      let res = instantiate_poly_univ_params uenv
          (Declareops.inductive_polymorphic_context mib)
          univ_instance term in
      if Encoding.is_constraints_on ()
      then res
      (* TODO: compute the required constraints argument *)
      else res
    end
  else term

let instantiate_template_ind_univ_params env uenv ind univ_instance term =
  let (mib,oib) = Inductive.lookup_mind_specif env ind in
  match oib.mind_arity with
  | TemplateArity ar when Encoding.is_templ_polymorphism_on () ->
    debug "Instantiating template inductive instance %a : {%a}"
      Dedukti.pp_term term
      pp_coq_inst univ_instance;
    let univ_ctxt = ar.template_param_levels in
    let nb_instance = Univ.Instance.length univ_instance in
    let nb_params = List.length univ_ctxt in
    if nb_instance < nb_params
    then debug "Something suspicious is going on with thoses universes...";
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
      (fun t l -> Dedukti.app t (T.coq_universe (translate_univ_level uenv l)))
      term levels
  | _ -> term

(*
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
  debug "Printing %s inductive: %s@@{%a} : %a" indtype name
    pp_coq_inst univ_instance
    Dedukti.pp_term res;
  res
*)

let translate_universe uenv u =
  (* debug "Translating universe : %a" pp_coq_univ u; *)
  let translate (lvl, i) =
    let univ = translate_univ_level uenv lvl in
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


let translate_univ_poly_params (uctxt:Univ.Instance.t) =
  if Encoding.is_polymorphism_on ()
  then
    let translate_local_level l =
      assert (not (Univ.Level.is_small l));
      match Univ.Level.var_index l with
      | None -> assert false
      | Some n -> T.coq_var_univ_name n in
    let params_lst = Array.to_list (Univ.Instance.to_array uctxt) in
    List.map translate_local_level params_lst
  else []

let translate_univ_poly_constraints (uctxt:Univ.Constraint.t) =
  if Encoding.is_constraints_on ()
  then
    let aux n cstr =
      let (i, c, j) = cstr in
      let i' = translate_univ_level Info.dummy i in
      let j' = translate_univ_level Info.dummy j in
      let cstr_type = T.coq_cstr c i' j' in
      let cstr_name = Cname.constraint_name n in
      ( cstr, (cstr_name, cstr_type) )
    in
    List.mapi aux (Univ.Constraint.elements uctxt)
  else []



let translate_template_name l =
  match Univ.Level.name l with
  | Some (d,n) -> T.coq_univ_name (Univ.Level.to_string l)
  | _ -> assert false

(** Extracts template parameters levels and returns them with their dedukti names
    e.g.: Level(Top,42) -> "Top__42"
*)
let translate_template_params (ctxt:Univ.Level.t option list) =
  if Encoding.is_templ_polymorphism_on ()
  then
    let params = Utils.filter_some ctxt in
    params, List.map translate_template_name params
  else [],[]



(* ------------------------   Constraints handling    ------------------------ *)

let destArity a b : Univ.Constraint.t =
  let open Univ in
  (* Extracts s from A1 -> ... -> An -> Us *)
  let decla, sa = Term.destArity a in
  let declb, sb = Term.destArity b in

  let rec gather_eq_types acc lista listb =
    match lista, listb with
    | [], [] -> acc
    | ( Context.Rel.Declaration.LocalAssum (_,ta) ) :: tla ,
      ( Context.Rel.Declaration.LocalAssum (_,tb) ) :: tlb ->
      gather_eq_types ((ta,tb)::acc) tla tlb
    | _ -> assert false in
  let eq_types = gather_eq_types [] decla declb in

  let rec enforce_eq_types acc  = function
    | [] -> acc
    | (ta,tb) :: tl ->
      begin
        match Term.kind_of_type ta, Term.kind_of_type tb with
        | Term.SortType sa,  Term.SortType sb ->
          enforce_eq_types
            (enforce_eq (Sorts.univ_of_sort sa) (Sorts.univ_of_sort sb) acc)
            tl
        | Term.CastType(ta',_), _ -> enforce_eq_types acc ( (ta',tb )::tl )
        | _, Term.CastType(tb',_) -> enforce_eq_types acc ( (ta ,tb')::tl )

        | Term.ProdType(x1, a1, b1), Term.ProdType(x2, a2, b2) ->
          enforce_eq_types acc ( (a1,a2) :: (b1,b2) :: tl)

        | _ -> enforce_eq_types acc tl
      end
  in
  enforce_leq (Sorts.univ_of_sort sa) (Sorts.univ_of_sort sb)
    (enforce_eq_types Univ.Constraint.empty eq_types)


let translate_constraint :
  Info.env -> Univ.univ_constraint -> Dedukti.term = fun uenv ((i,c,j) as cstr) ->
  match Info.fetch_constraint uenv cstr with
  | Some v -> Dedukti.var v
  | None -> T.coq_I ()
  | _ ->
    (* TODO: build complicated constraint here *)
    failwith
      (Format.asprintf "Could not find constraint %a in context" pp_coq_constraint cstr)

let translate_constraint_set :
  Info.env -> Univ.Constraint.t -> Dedukti.term list = fun uenv cstr ->
  let res = ref [] in
  Univ.Constraint.iter
    (fun cstr -> res := (translate_constraint uenv cstr) :: !res)
    cstr;
  !res
