
type lifted_type_pattern =
  | AsLift
  | AsCast
  | AsPrivateCast
  | AsUncodedCode

type t =
  {
    polymorphism_flag         : bool;
    templ_polymorphism_flag   : bool;
    float_univ_flag           : bool;
    constraints_flag          : bool;
    named_univ_flag           : bool;
    readable_translation_flag : bool;
    cast_flag                 : bool;
    lift_flag                 : bool;
    lifted_type_pattern       : lifted_type_pattern;
    pred_univ_flag            : bool;
    pred_prod_flag            : bool;
    pred_lift_flag            : bool;
    pred_cast_flag            : bool;
    encoding_name   : string;
    system_module   : string;
    universe_module : string;
    t_Sort : string;
    t_Univ : string;
    t_Term : string;
    t_univ : string;
    t_prod : string;
    t_lift : string;
    t_cast : string;
    t_I    : string;
    t_priv_cast   : string;
    t_priv_code   : string;
    t_priv_uncode : string;
  }

let original =
  {
    polymorphism_flag         = false;
    templ_polymorphism_flag   = false;
    constraints_flag          = false;
    float_univ_flag           = false;
    named_univ_flag           = false;
    readable_translation_flag = false;
    cast_flag                 = false;
    lift_flag                 = true;
    lifted_type_pattern       = AsLift;
    pred_univ_flag            = false;
    pred_prod_flag            = false;
    pred_lift_flag            = false;
    pred_cast_flag            = false;
    encoding_name = "original";
    system_module   = "Coq";
    universe_module = "U";
    t_Sort = "Sort";
    t_Univ = "Univ";
    t_Term = "Term";
    t_univ = "univ";
    t_prod = "prod";
    t_lift = "lift";
    t_cast = "cast";
    t_I    = "I";
    t_priv_cast   = "cast'";
    t_priv_code   = "c";
    t_priv_uncode = "u";
  }

let original_cast =
  { original with
    cast_flag = true; (* Casts are used for lifting functions *)
    lift_flag = true; (* Lifts are still normal forms for universe lifting *)
    encoding_name = "original_cast";
  }

let template_cast =
  { original_cast with
    templ_polymorphism_flag = true; (* Template polymorphism *)
    encoding_name = "template_cast";
  }

let polymorph =
  { original with
    polymorphism_flag         = true;
    templ_polymorphism_flag   = true;
    constraints_flag          = true;
    lift_flag                 = false; (* Do not use lifts *)
    cast_flag                 = true;  (* Use casts *)
    pred_cast_flag            = true;  (* Casts take subtype predicate argument *)
    lifted_type_pattern = AsPrivateCast; (* Casted type pattern *)
    encoding_name = "polymorphism";
  }


let named enc =
  { enc with
    named_univ_flag = true;
    encoding_name = "named_" ^ enc.encoding_name;
  }

let readable enc =
  {
    enc with
    readable_translation_flag = true;
    encoding_name = "readable_" ^ enc.encoding_name;
    system_module = "C";
    t_Sort = "S";
    t_Univ = "U";
    t_Term = "T";
    t_univ = "u";
  }


let current_encoding   = ref (named original_cast)

let set enc = current_encoding := enc
let get () = !current_encoding
let rec get_encoding e =
  if Utils.str_starts_with "readable " e
  then readable (get_encoding (Utils.truncate e 9))
  else if Utils.str_starts_with "named " e
  then named (get_encoding (Utils.truncate e 6))
  else match String.trim e with
    | "original"      -> original
    | "original_cast" -> original_cast
    | "template_cast" -> template_cast
    | "polymorph"     -> polymorph
    | invalid_name -> failwith (Format.sprintf "Unknown encoding: %s" invalid_name)
let set_encoding e = set (get_encoding e)

let is_polymorphism_on       () = (get()).polymorphism_flag
let is_templ_polymorphism_on () = (get()).templ_polymorphism_flag
let is_constraints_on        () = (get()).constraints_flag
let is_named_univ_on         () = (get()).named_univ_flag
let is_float_univ_on         () = (get()).float_univ_flag
let is_readable_on           () = (get()).readable_translation_flag
let is_cast_on               () = (get()).cast_flag
