
module LevelMap = Map.Make(
  struct
    type t = Univ.Level.t
    let compare = Univ.Level.compare
  end)

type map = Translator.universe_expr LevelMap.t

type env =
  {
    template_params : Translator.universe_expr LevelMap.t;
    poly_ctxt : UVars.AbstractContext.t;
    nb_polymorphic_univs : int;
    constraints : ( Univ.univ_constraint * (Dedukti.var * Dedukti.term * Dedukti.term) ) list
  }

let make
    (template_levels : Univ.Level.t list)
    (template_names  : Translator.universe_expr list)
    (poly_ctxt : UVars.AbstractContext.t)
    (nb_polymorphic_univs : int)
    (constraints : ( Univ.univ_constraint * (Dedukti.var * Dedukti.term * Dedukti.term)) list) =

  let aux map k v = LevelMap.add k v map in
  let template_params = List.fold_left2 aux LevelMap.empty template_levels template_names in
  (* TODO: implement here a mechanism that processes polymorphic constraints *)
  {
    template_params;
    poly_ctxt;
    nb_polymorphic_univs;
    constraints
  }

let replace_template_name uenv lvl new_name =
  { uenv with template_params = LevelMap.add lvl new_name uenv.template_params }

let is_template_polymorphic (e:env) a = LevelMap.mem a e.template_params

let translate_template_arg (e:env) a = LevelMap.find a e.template_params

let fetch_constraint uenv cstr =
  try Some (List.assoc cstr uenv.constraints)
  with Not_found -> None

let fetch_higher_sorts uenv u =
  let assoc acc ((i,c,j),(b,_,_)) =
    match c with
    | AcyclicGraph.Eq ->
       if compare i u = 0
       then (c,j,b)::acc
       else if compare j u = 0
       then (c,i,b)::acc
       else acc
    | _ ->
       if compare i u = 0
       then (c,j,b)::acc else acc
  (* FIXME: Equality constraints are not handled correctly here.
     - They should be considered both way
     - We should also prevent loops somehow *)
  in List.fold_left assoc [] uenv.constraints

let find_constraint uenv (cfirst,ccstr,clast) =
  let rec aux = function
    | [] -> None
    | (i,flag,l)::search ->
      if i = clast then if flag then Some l else aux search
      else
        let rec filter acc = function
          | [] -> acc
          | ((c,j,t) as hd)::tl ->
            let new_acc =
              match ccstr, c with
              | Univ.Eq, Univ.Eq -> acc
              | Univ.Eq, _       -> (j,flag,hd::l) :: acc
              | _      , Univ.Lt -> (j,true,hd::l) :: acc
              | _                -> (j,flag,hd::l) :: acc
            in filter new_acc tl
        in
        let newsearch = filter search (fetch_higher_sorts uenv i) in
        aux newsearch
  in aux [(cfirst,ccstr<>Univ.Lt,[])]


let pp_constraints : env Debug.printer = fun fmt s ->
  let open Debug in
  let p fmt (a,(b,c,d)) = Format.fprintf fmt "%a |-> %s" pp_coq_constraint a b in
  Format.fprintf fmt "{ %a }" (pp_list ", " p) s.constraints

let dummy = make [] [] UVars.AbstractContext.empty 0 []


let destination = ref "."
let set_destination dest = destination := dest
let get_destination_path path = Filename.concat !destination path


type info =
  {
    out         : out_channel;
    fmt         : Format.formatter;
    library     : Names.DirPath.t;
    module_path : Names.ModPath.t;
  }

let init module_path filename =
  let filename = get_destination_path (filename ^ ".dk") in
  let out = open_out filename in
  {
    out = out;
    fmt = Format.formatter_of_out_channel out;
    library =
      if module_path = Names.ModPath.dummy then Names.DirPath.dummy
      else Nametab.dirpath_of_module module_path;
    module_path = module_path;
  }

let update info label =
  { info with module_path = Names.MPdot(info.module_path, label) }

let close info =
  Format.pp_print_flush info.fmt ();
  close_out info.out
