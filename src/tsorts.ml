(** Translation of Coq sorts *)

open Debug
open Translator

open Declarations


(* ------------------------   Constraints handling    ------------------------ *)

(** Maping from the string representation of global named universes to
    concrete levels. *)
let universe_table : (string, level_expr) Hashtbl.t = Hashtbl.create 10007

(** Translates a universe level in a given local universe environment
    using universe_table global environment for named universes.  *)
let level_as_level uenv l =
  if Univ.Level.is_prop l then assert false
  else if Univ.Level.is_set l then set_level
  else
    match Univ.Level.var_index l with
    | Some n ->
      if Encoding.is_polymorphism_on ()
      then Local n (* Locally bounded universe variable *)
      else failwith "Universe polymorphism no supported by this encoding "
    | None ->
      if Info.is_template_polymorphic uenv l
      then
        match Info.translate_template_arg uenv l
        with Type l -> l | _ -> assert false
      else
        let name = Univ.Level.to_string l in
        if Encoding.is_float_univ_on () || Encoding.is_named_univ_on ()
        then GlobalLevel name
        else
          try Hashtbl.find universe_table name
          with Not_found -> failwith (Format.sprintf "Unable to parse atom: %s" name)

(** Translates a universe level in a given local universe environment
    using universe_table global environment for named universes.  *)
let level_as_universe uenv l =
  if Univ.Level.is_prop l then Translator.Prop
  else if Univ.Level.is_set l then Translator.Set
  else
    match Univ.Level.var_index l with
    | Some n ->
      if Encoding.is_polymorphism_on ()
      then Type (Local n) (* Locally bounded universe variable *)
      else failwith "Universe polymorphism no supported by this encoding "
    | None ->
      if Info.is_template_polymorphic uenv l
      then Info.translate_template_arg uenv l
      else
        let name = Univ.Level.to_string l in
        if Encoding.is_float_univ_on () || Encoding.is_named_univ_on ()
        then Type (GlobalLevel name)
        else
          try Type (Hashtbl.find universe_table name)
          with Not_found -> failwith (Format.sprintf "Unable to parse atom: %s" name)



let gather_eq_types decla declb =
  let rec aux acc lista listb =
    match lista, listb with
    | [], [] -> acc
    | ( Context.Rel.Declaration.LocalAssum (_,ta) ) :: tla ,
      ( Context.Rel.Declaration.LocalAssum (_,tb) ) :: tlb ->
      aux ((ta,tb)::acc) tla tlb
    | ( Context.Rel.Declaration.LocalDef (_,ta,va) ) :: tla ,
      ( Context.Rel.Declaration.LocalDef (_,tb,vb) ) :: tlb ->
      aux ((ta,tb)::(va,vb)::acc) tla tlb
    | _ -> assert false
  in
  aux [] decla declb

let rec enforce_eq_types acc  = function
  | [] -> acc
  | (ta,tb) :: tl ->
      match Constr.kind ta, Constr.kind tb with
      | Constr.Sort sa,  Constr.Sort sb ->
        enforce_eq_types
          (Univ.enforce_eq (Sorts.univ_of_sort sa) (Sorts.univ_of_sort sb) acc)
          tl
      | Constr.Cast(ta',_,_), _ -> enforce_eq_types acc ( (ta',tb )::tl )
      | _, Constr.Cast(tb',_,_) -> enforce_eq_types acc ( (ta ,tb')::tl )

      | Constr.Prod(x1, a1, b1), Constr.Prod(x2, a2, b2) ->
        enforce_eq_types acc ( (a1,a2) :: (b1,b2) :: tl)

      | _ -> enforce_eq_types acc tl 

let trivial_cstr (i,c,j) =
  (Univ.Level.is_small i && c = Univ.Le)
  ||
  (not (Encoding.is_float_univ_on ())
   &&  (Univ.Level.var_index i = None && Univ.Level.var_index j = None))

let translate_constraint :
  Info.env -> Univ.univ_constraint -> (Dedukti.term*Dedukti.term) option
  = fun uenv ((i,c,j) as cstr) ->
  debug "Fetching %a %a %a" pp_coq_level i pp_coq_constraint_type c pp_coq_level j;
  debug "In constraints: %a" Info.pp_constraints uenv;
  if trivial_cstr cstr then None
  else
    match Info.fetch_constraint uenv cstr with
    | Some (v,c,ct) ->
      debug "Found: %s" v;
      Some (Dedukti.var v, c)
    | None ->
      match Info.find_constraint uenv cstr with
      | None ->
        failwith
          (Format.asprintf "Could not find constraint %a in context %a"
             pp_coq_constraint cstr
             Info.pp_constraints uenv)
      | Some ex_cstr ->
        debug "Found transitive: %a" (pp_list "," pp_string)
          (List.map (fun (_,_,s) -> s) ex_cstr);
        (* TODO: Build complicated constraint here *)

        let i' = level_as_universe Info.dummy i in
        let j' = level_as_universe Info.dummy j in
        let cstr_term = T.coq_cstr c i' j' in

        let to_univ add i =
          T.coq_universe (Succ (level_as_universe Info.dummy i, add)) in
        let addc = function Univ.Lt -> 1 | _ -> 0 in
        let add0 = addc c in
        let rec rev_count acc counter = function
          | [] ->
            if counter <= add0 then acc
            else rev_count ((T.coq_I(),(to_univ counter i))::acc) (counter-1) []
          | (c,l,v) :: tl ->
            rev_count ((Dedukti.var v, to_univ counter l)::acc) ((addc c)+counter) tl
        in
        Some
          ( T.coq_trans_cstr (to_univ add0 i)
              (rev_count [] 0 ex_cstr)
          , cstr_term)



let translate_constraints_as_conjunction uenv cstr =
  let aux cstr res =
    match translate_constraint uenv cstr with
    | Some c -> c :: res
    | None -> res in
  Univ.Constraint.fold aux cstr []


(**************************************************************************)

type cstr = Univ.univ_constraint * (Dedukti.var * Dedukti.term * Dedukti.term)

let cstr_decl (_,(v,_,ct)) = (v, ct)

(**************************************************************************)

let template_constructor_upoly () =
  Encoding.is_templ_polymorphism_on () && Encoding.is_templ_polymorphism_cons_poly ()

let add_poly_params_type params cstr t =
  if Encoding.is_polymorphism_on ()
  then List.fold_right (function u -> Dedukti.pie (u, (T.coq_Lvl (*Univ or Lvl ?*) ()))) params
      ( if Encoding.is_constraints_on ()
        then List.fold_right Dedukti.pie (List.map cstr_decl cstr) t
        else t )
  else t

let add_poly_params_def params cstr t =
  if Encoding.is_polymorphism_on ()
  then List.fold_right (function u -> Dedukti.lam (u, (T.coq_Lvl (*Univ or Lvl ?*) ()))) params
      ( if Encoding.is_constraints_on ()
        then List.fold_right Dedukti.lam (List.map cstr_decl cstr) t
        else t )
  else t

let add_poly_env_def uenv =
  let rec params i acc = if i = 0 then acc else params (i-1) (T.coq_var_univ_name (i-1)::acc) in
  let params = params uenv.Info.nb_polymorphic_univs [] in
  add_poly_params_def params uenv.Info.constraints

let add_poly_env_type uenv =
  let rec params i acc = if i = 0 then acc else params (i-1) (T.coq_var_univ_name (i-1)::acc) in
  let params = params uenv.Info.nb_polymorphic_univs [] in
  add_poly_params_type params uenv.Info.constraints

let get_inductive_params templ_params poly_params poly_cstr =
  (
    if Encoding.is_templ_polymorphism_on ()
    then List.map (function u -> (u, T.coq_Sort())) templ_params
    else []
  ) @ (
    if Encoding.is_polymorphism_on ()
    then List.map (function u -> (u, T.coq_Lvl() (*Univ or Lvl ?*) )) poly_params
    else []
  ) @ (
    if Encoding.is_constraints_on ()
    then List.map cstr_decl poly_cstr
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
    then List.map (function u -> (u, T.coq_Lvl (*Sort or Nat ?*) ())) poly_params
    else []
  ) @ (
    if Encoding.is_constraints_on ()
    then List.map cstr_decl poly_cstr
    else []
  )
let add_constructor_params templ_params poly_params poly_cstr arity =
  List.fold_right Dedukti.pie
    (get_constructor_params templ_params poly_params poly_cstr) arity



let sort_universes g =
  let gene = Hashtbl.create 10007 in
  let cano = Hashtbl.create 10007 in
  let open Univ in
  let g = UGraph.repr g in
  let wl = ref LSet.empty in
  let rec cano_arc u nd =
    if Hashtbl.mem cano u then Hashtbl.find cano u
    else
      match nd with
      | UGraph.Node _ ->
         Hashtbl.add gene u 0;
         wl := LSet.add u !wl;
         Hashtbl.add cano u u; u
      | UGraph.Alias v ->
         let v' = cano_arc v (LMap.find v g) in
         Hashtbl.add cano u v'; v' in
  LMap.iter (fun u v -> ignore (cano_arc u v)) g;
  let rec proc () =
    if LSet.is_empty !wl then ()
    else
      (let u = LSet.choose !wl in
       wl := LSet.remove u !wl;
       (if not (Level.is_small u)
       then
         let n = Hashtbl.find gene u in
         (match LMap.find u g with
         | UGraph.Alias _ -> assert false
         | UGraph.Node ltle ->
            LMap.iter (fun v is_lt ->
                let v = Hashtbl.find cano v in
                let m = Hashtbl.find gene v in
                let m' = if is_lt then n+1 else n in
                if m < m' && not (Level.is_small v) then (
                  (*message "Propagate: %s(%d) <= %s(%d) := %d" (Level.to_string u) m  (Level.to_string v) m m';*)
                  wl := LSet.add v !wl; Hashtbl.replace gene v m'))
              ltle));
       proc()) in
  proc();
  Hashtbl.iter (fun u v ->
      let v' = mk_level (Hashtbl.find gene v) in
      Hashtbl.add universe_table (Level.to_string u) v') cano


(** Dump universe graph [universes] in the universe table. *)
let set_universes universes =
  message "Saving universes";
  if (not (Encoding.is_float_univ_on ()))
  then sort_universes universes

let translate_template_global_level_decl (ctxt:Univ.Level.t option list) =
  if Encoding.is_templ_polymorphism_on ()
  then
    let params = Utils.filter_some ctxt in
    let aux l =
      match Univ.Level.name l with
      | Some _ ->
        let name = Univ.Level.to_string l in
        let name' = T.coq_univ_name name in
        if Encoding.is_float_univ_on ()
        then Dedukti.declaration false name' (T.coq_Lvl())
        else
          let univ = Hashtbl.find universe_table name in
          Dedukti.definition false name' (T.coq_Lvl()) (T.coq_level univ)
      | None -> assert false
      (* No small levels (Prop/Set) or (true) polymorphism variables in template params. *)
    in
    List.map aux params
  else []

let get_poly_univ_params uenv ctx univ_instance =
  let nb_params = Univ.AUContext.size ctx in
  if Univ.Instance.length univ_instance < nb_params
  then debug "Something suspicious is going on with thoses universes...";
  if not (Encoding.is_polymorphism_on ()) then []
  else
    let levels = Univ.Instance.to_array univ_instance in
    Array.to_list
      (Array.map (fun l -> T.coq_level (level_as_level uenv l)) levels)
    @
    if not (Encoding.is_constraints_on ()) then []
    else
      let subst = Univ.make_inverse_instance_subst univ_instance in
      let aux (u,d,v as c) res =
        let u' = Univ.subst_univs_level_level subst u in
        let v' = Univ.subst_univs_level_level subst v in
        let c' = if u' == u && v' == v then c else (u',d,v') in
        ( match translate_constraint uenv c' with
          | Some (v,c) -> v
          | None       -> T.coq_I() ) :: res in
      let cstr = Univ.UContext.constraints (Univ.AUContext.repr ctx) in
      debug "Translating Constraints: %a in instance %a"
        pp_coq_Constraint cstr pp_coq_inst univ_instance;
      List.rev (Univ.Constraint.fold aux cstr [])

let instantiate_poly_univ_constant env uenv (kn,u) constant =
  let cb = Environ.lookup_constant kn env in
  let ctx = Declareops.constant_polymorphic_context cb in
  Dedukti.apps constant (get_poly_univ_params uenv ctx u)

let instantiate_poly_ind_univ_params env uenv ind univ_instance term =
  if Encoding.is_polymorphism_on () &&
     Environ.polymorphic_ind ind env
  then
    begin
      let (mib,oib) = Inductive.lookup_mind_specif env ind in
      debug "Instantiating polymorphic inductive instance %a : {%a}"
        Dedukti.pp_term term
        pp_coq_inst univ_instance;
      Dedukti.apps term
        (get_poly_univ_params uenv
           (Declareops.inductive_polymorphic_context mib)
           univ_instance)
    end
  else term

let instantiate_template_ind_univ_params env uenv ind univ_instance term =
  let (mib,oib) = Inductive.lookup_mind_specif env ind in
  match mib.mind_template with
  | Some tar when Encoding.is_templ_polymorphism_on () ->
    debug "Instantiating template inductive instance %a : {%a}"
      Dedukti.pp_term term
      pp_coq_inst univ_instance;
    let univ_ctxt = tar.template_param_levels in
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
      (fun t l -> Dedukti.app t (T.coq_universe (level_as_universe uenv l)))
      term levels
  | _ -> term

let translate_universe uenv u =
  (*  message "Translating universe : %a" pp_coq_univ u;*)
  let translate (univ, i) =
    (*    message "level %a+%d" pp_coq_level univ i;*)
    let u = level_as_universe uenv univ in
    if i = 0 then u else Succ (u,i) in
  match List.map translate (Univ.Universe.repr u) with
  | []     -> Translator.Prop
  | [l]    -> l
  | levels -> Translator.Sup levels

let translate_sort uenv = function
  | Term.SProp  -> Translator.SProp
  | Term.Prop   -> Translator.Prop
  | Term.Set    -> Translator.Set
  | Term.Type i -> translate_universe uenv i

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
      let i' = level_as_universe Info.dummy i in
      let j' = level_as_universe Info.dummy j in
      let cstr_term = T.coq_cstr c i' j' in
      let cstr_type = T.coq_cstr_eps cstr_term in
      let cstr_name = Cname.constraint_name n in
      ( cstr, (cstr_name, cstr_term, cstr_type) )
    in
    List.mapi aux (Univ.Constraint.elements uctxt)
  else []


let translate_template_name l =
  match Univ.Level.name l with
  | Some _ -> T.coq_univ_name (Univ.Level.to_string l)
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
