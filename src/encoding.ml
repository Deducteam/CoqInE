let flags     = Hashtbl.create 15
let encodings = Hashtbl.create 15

let flag = Hashtbl.find flags
let symb key =
  try
    match Hashtbl.find encodings key with
    | "" -> failwith ("Symbol [" ^ key ^ "] is not available in this encoding.")
    | value -> value
  with Not_found -> failwith ("Symbol [" ^ key ^ "] is not a valid encoding identifier.")

let set_flag key value =
  if Hashtbl.mem flags key
  then Hashtbl.replace flags key value
  else failwith ("Wrong key: " ^ key)

let set_param key value =
  if Hashtbl.mem flags key
  then
    Hashtbl.replace flags key
      ( match value with
        | "true" -> true
        | "false" -> false
        | _ -> failwith ("Wrong value for key " ^ key ^ ": " ^ value)
      )
  else if Hashtbl.mem encodings key
  then Hashtbl.replace encodings key value
  else failwith ("Wrong key: " ^ key)

let set_params = List.iter (fun (x,y) -> set_param x y)

let init_flags () =
  List.iter (fun (x,y) -> Hashtbl.replace flags x y)
    [
      ("simpl_letins"    , false);
      (** Translate let-in as simpl beta redices or not ? *)

      ("upolymorphism"   , false);
      (** Is (true) polymorphism translation on ? *)

      ("tpolymorphism"   , false);
      (** Is template polymorphism translation on ? *)

      ("tpoly_code"      , false);

      ("float_univ"      , false);
      (** Is floating universe translation on ? *)

      ("constraints"     , false);
      (** Constraints translation ? Only has meaning when float is true. *)

      ("named_univ"      , false);
      (** Should we use universe names or value ? Only has meaning when float is false. *)

      ("readable"        , false);
      (** Is (pseudo-)readable translation mode on ? *)

      ("use_cast"        , false);
      (** Are we allowed to use casts (for casted lambdas) ?
          Or should we turn them into lifted lambdas instead ? *)

      ("pred_univ"       , false);
      ("pred_prod"       , false);
      ("pred_lift"       , false);
      ("pred_cast"       , false);
      ("priv_lift"       , false);
      ("priv_cast"       , false);
      ("priv_univ"       , false);
      ("priv_prod"       , false);

      ("inlined_fixpoint", false);
      (** Translate fixpoints as external body or inlined generic fixpoint operator ?
          This is a very experimental feature. *)

      ("fix_arity_sort"  , false);

      ("cast_arguments"  , false);
      (** Should arguments of an application be systematically be casted to
          their expected type ? *)

      ("code_guarded"    , false);

      ("unfold_letin"    , false)
    ]

let init_empty_symbs () =
  List.iter (fun x -> Hashtbl.replace encodings x "")
    [
      (* General encoding parameters *)
      "syntax";
      "encoding_name";
      "encoding_file";
      "universe_file";
      "lifted_type_pattern";

      (* Public construction syntax *)
      "Sort";
      "Lvl";
      "Univ";
      "Term";
      "univ";
      "prod";
      "lift";
      "cast";

      (* Infinite hierarchy of universes constructors *)
      "prop";
      "set";
      "type";
      "type0";

      "lvl0";
      "lvlS";
      "lvlMax";

      (* Functionnal universe constructors *)
      "axiom";
      "sup";
      "rule";

      (* Predicate syntax *)
      "eps";   (* Constraint proof space *)
      "Cumul"; (* s1 <= s2 predicate *)
      "Eq";    (* s1 == s2 predicate *)

      (* Predicate constructors *)
      "I";            (* trivial predicate                  *)
      "pair";         (* C1   ->   C2   ->   C1 /\ C2       *)
      "cumul_trans";  (* A < B   ->   B < C   ->   A < C    *)
      "cumul_eq";     (* A = B   ->   A < B                 *)
      "eq_sym";       (* A = B   ->   B = A                 *)
      "cumul_rule_1"; (* A < B   ->   rule(A,C) < rule(B,C)  for functionnal CTS *)
      "cumul_rule_2"; (* B < C   ->   rule(A,B) < rule(A,C)  for functionnal CTS  *)
      "cumul_axiom";  (* A < B   ->   axiom(A)  < axiom(B)   for functionnal CTS  *)

      (* Private syntax *)
      "_lift";
      "_cast";
      "_univ";
      "_prod";
      "_code";
      "_uncode";
      "_code_app";
      "_code_abs";
      "_code_univ";
      "_code_prod";
      "_code_sinf";

      (* Fixpoint syntax *)
      "0";
      "S";
      "SA";
      "MA";
      "fix";
      "fix_proj";
      "fix_oneline";
      "guard";
      "code_guard";
      "guarded";
      "level"
    ]

let _ =
  init_flags();
  init_empty_symbs ()

let is_polymorphism_on       () = flag "upolymorphism"
let is_templ_polymorphism_on () = flag "tpolymorphism"
let is_templ_polymorphism_code_on () = flag "tpoly_code"
let is_constraints_on        () = flag "constraints"
let is_named_univ_on         () = flag "named_univ"
let is_float_univ_on         () = flag "float_univ"
let is_readable_on           () = flag "readable"
let need_universe_file       () = is_float_univ_on () || is_named_univ_on ()
let is_cast_on               () = flag "use_cast"
let is_letins_simpl          () = flag "simpl_letins"
let is_fixpoint_inlined      () = flag "inlined_fixpoint"
let is_argument_casted       () = flag "cast_arguments"
