let flags     = Hashtbl.create 15
let encodings = Hashtbl.create 15


let flag = Hashtbl.find flags
let symb key =
  match Hashtbl.find encodings key with
  | "" -> failwith "Symbol \"" ^ key ^ "\" is not available in this encoding."
  | value -> value

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

let _ =
  List.iter (fun (x,y) -> Hashtbl.replace encodings x y)
    [
      (* General encoding parameters *)
      ("syntax"       , "Dedukti");
      ("encoding_name", "original");
      ("encoding_file", "Coq");
      ("universe_file", "U");
      ("lifted_type_pattern", "lift");

      (* Public construction syntax *)
      ("Sort" , "Sort");
      ("Univ" , "Univ");
      ("Term" , "Term");
      ("univ" , "univ");
      ("prod" , "prod");
      ("lift" , "lift");
      ("cast" , "cast");

      (* Infinite hierarchy of universes constructors *)
      ("prop" , "prop");
      ("set"  , "set");
      ("type" , "type");
      ("u0"   , "z");
      ("uS"   , "s");

      (* Functionnal universe constructors *)
      ("axiom", "axiom");
      ("sup"  , "sup");
      ("rule" , "rule");

      (* Predicate syntax *)
      ("I"    , "I");
      ("eps"  , "eps");
      ("Axiom", "Axiom");
      ("Rule" , "Rule");
      ("Cumul", "Cumul");
      ("Eq"   , "Eq");  (* s1 = s2 constraint *)

      (* Private syntax *)
      ("_lift"  , "lift'");
      ("_cast"  , "cast'");
      ("_univ"  , "univ'");
      ("_prod"  , "prod'");
      ("_code"  , "code");
      ("_uncode", "uncode");
      ("_code_app" , "cApp");
      ("_code_abs" , "cLam");
      ("_code_univ", "cU");
      ("_code_prod", "cPi");

      (* Fixpoint syntax *)
      ("0"          , "0");
      ("S"          , "_S");
      ("SA"         , "SA");
      ("MA"         , "make_MA");
      ("fix"        , "fix");
      ("fix_proj"   , "fix_proj");
      ("fix_oneline", "fixproj");
      ("guard"      , "guarded?");
      ("guarded"    , "guarded")
    ];
  List.iter (fun (x,y) -> Hashtbl.replace flags x y)
    [
      ("simpl_letins"      , false);
      ("polymorphism"      , false);
      ("templ_polymorphism", false);
      ("float_univ"        , false);
      ("constraints"       , false);
      ("named_univ"        , false);
      ("readable"          , false);
      ("use_cast"          , false);
      ("pred_univ"         , false);
      ("pred_prod"         , false);
      ("pred_lift"         , false);
      ("pred_cast"         , false);
      ("priv_lift"         , false);
      ("priv_cast"         , false);
      ("priv_univ"         , false);
      ("priv_prod"         , false);
      ("inlined_fixpoint"  , false)
    ]


let floating_univ () =
  set_param "float_univ" "true";
  set_param "universe_file" "U"

let template () =
  set_param "templ_polymorphism" "true" (* Template polymorphism *)

let polymorphism () =
  set_params [
    "templ_polymorphism" , "true";
    "polymorphism"       , "true";
    "constraints"        , "true";
  ]

let named () = set_param "named_univ" "true"

let set_encoding s =
  let keywords = Str.split (Str.regexp "[ \t]+") s in
  List.iter
    (function
      | "float"      -> floating_univ ()
      | "named"      -> named ()
      | "template"   -> template ()
      | "polymorph"  -> polymorphism ()
      | invalid_name ->
        failwith (Format.sprintf "Unknown encoding keyword: %s" invalid_name))
    keywords;
  set_param "encoding_name" s

let is_polymorphism_on       () = flag "polymorphism"
let is_templ_polymorphism_on () = flag "templ_polymorphism"
let is_constraints_on        () = flag "constraints"
let is_named_univ_on         () = flag "named_univ"
let is_float_univ_on         () = flag "float_univ"
let is_readable_on           () = flag "readable"
let is_cast_on               () = flag "use_cast"
let is_letins_simpl          () = flag "simpl_letins"
let is_fixpoint_inlined      () = flag "inlined_fixpoint"
