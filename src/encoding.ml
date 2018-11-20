
type t =
  {
    polymorphism_flag         : bool;
    templ_polymorphism_flag   : bool;
    constraints_flag          : bool;
    float_univ_flag           : bool;
    readable_translation_flag : bool;
    encoding_name   : string;
    system_module   : string;
    universe_module : string;
    t_Sort : string;
    t_univ : string;
    t_Univ : string;
    t_Term : string;
  }

let default =
  {
    polymorphism_flag         = false;
    templ_polymorphism_flag   = false;
    constraints_flag          = false;
    float_univ_flag           = false;
    readable_translation_flag = false;
    encoding_name = "original";
    system_module   = "Coq";
    universe_module = "U";
    t_Sort = "Sort";
    t_univ = "univ";
    t_Univ = "Univ";
    t_Term = "Term";
  }
  
let readable_default =
  {
    default with
    readable_translation_flag = true;
    encoding_name = "readable original";
    system_module = "C";
    t_Sort = "S";
    t_univ = "u";
    t_Univ = "U";
    t_Term = "T";
  }
  
let polymorph =
  { default with
    polymorphism_flag         = true;
    templ_polymorphism_flag   = true;
    constraints_flag          = true;
    readable_translation_flag = true;
    encoding_name = "polymorphism";
  }
  

let current_encoding   = ref polymorph
    
let set enc = current_encoding := enc
let get () = !current_encoding
let set_encoding = function
  | "default"   -> set default
  | "readable"  -> set readable_default
  | "polymorph" -> set polymorph
  | invalid_name -> failwith (Format.sprintf "Unknown encoding: %s" invalid_name)
                      
let is_polymorphism_on       () = (get()).polymorphism_flag
let is_templ_polymorphism_on () = (get()).templ_polymorphism_flag
let is_constraints_on        () = (get()).constraints_flag
let is_float_univ_on         () = (get()).float_univ_flag
let is_readable_on           () = (get()).readable_translation_flag
