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

let init () =
  List.iter (fun (x,y) -> Hashtbl.replace encodings x y)
    [
      ("syntax", "Dedukti");
      ("encoding_name", "original");
      ("lifted_type_pattern", "lift");
      ("system_module"  , "Coq");
      ("universe_module", "U");
      ("Sort" , "Sort");
      ("Univ" , "Univ");
      ("Term" , "Term");
      ("axiom", "axiom");
      ("sup"  , "sup");
      ("rule" , "rule");
      ("univ" , "univ");
      ("prod" , "prod");
      ("lift" , "lift");
      ("cast" , "cast");
      ("I"    , "I");
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
      ("simpl_letins"      , true);
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
let _ = init ()

let lift_priv () =
  init();
  set_params [
    "pred_univ", "true";
    "pred_prod", "true";
    "pred_lift", "true";
    "priv_lift", "true";
    "priv_univ", "true";
    "priv_prod", "true"
  ]

let universo () =
  lift_priv();
  set_params [
    "use_cast"           , "true"; (* Use casts instead of lifts *)
    "pred_cast"          , "true"; (* They take a predicate argument *)
    "constraints"        , "true";
    "polymorphism"       , "true";
    "templ_polymorphism" , "true";
    "priv_cast"          , "true";
    "lifted_type_pattern", "cast";
    (* Universe lifting pattern are private cast *)
  ]


(* Casts are used for lifting functions *)
(* but lifts are still normal forms for universe lifting *)
let original_cast () =
  init();
  set_param "use_cast" "true"

let template_cast () =
  original_cast ();
  set_param "templ_polymorphism" "true" (* Template polymorphism *)

let polymorph () =
  init();
  set_params [
    "polymorphism"       , "true";
    "templ_polymorphism" , "true";
    "constraints"        , "true";
    "use_cast"           , "true";  (* Use casts instead of lifts *)
    "pred_cast"          , "true";  (* Casts take subtype predicate argument *)
    "priv_lift"          , "false";
    "priv_cast"          , "true";
    "priv_univ"          , "true";
    "priv_prod"          , "true";
    "lifted_type_pattern", "cast"; (* Casted type pattern *)
  ]


let named () = set_param "named_univ" "true"

let readable () = set_params
    [ "readable","true" ;
      "system_module", "C";
      "Sort", "S";
      "Univ", "U";
      "Term", "T";
      "univ", "u";
      "_univ", "u'"
    ]

let fixpointed () = set_param "inlined_fixpoint" "true"

let set_encoding s =
  let keywords = Str.split (Str.regexp "[ \t]+") s in
  List.iter
    (function
      | "readable" -> readable ()
      | "named" -> named ()
      | "fix" -> fixpointed ()
      | "original"      -> init ()
      | "original_cast" -> original_cast ()
      | "template_cast" -> template_cast ()
      | "lift_priv"     -> lift_priv ()
      | "universo"      -> universo ()
      | "polymorph"     -> polymorph ()
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
