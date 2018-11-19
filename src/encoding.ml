
module Enc =
struct

  type t =
    {
      polymorphism_flag         : bool;
      templ_polymorphism_flag   : bool;
      constraints_flag          : bool;
      float_univ_flag           : bool;
      readable_translation_flag : bool;
      system_module   : string;
      universe_module : string;
      t_Sort : string;
    }

  let default =
    {
      polymorphism_flag         = false;
      templ_polymorphism_flag   = false;
      constraints_flag          = false;
      float_univ_flag           = false;
      readable_translation_flag = false;
      system_module   = "Coq";
      universe_module = "U";
      t_Sort = "Sort";
    }

  let readable_default =
    {
      polymorphism_flag         = false;
      templ_polymorphism_flag   = false;
      constraints_flag          = false;
      float_univ_flag           = false;
      readable_translation_flag = true;
      system_module   = "C";
      universe_module = "U";
      t_Sort = "Sort";
    }

  let polymorph =
    {
      polymorphism_flag         = true;
      templ_polymorphism_flag   = true;
      constraints_flag          = true;
      float_univ_flag           = false;
      readable_translation_flag = true;
      system_module   = "Coq";
      universe_module = "U";
      t_Sort = "Sort";
    }

  
  let current_encoding   = ref polymorph

  let set enc = current_encoding := enc
  let get () = !current_encoding

end



open Enc

let set_encoding = function
  | "default" -> set default
  | invalid_name -> failwith (Format.sprintf "Unknown encoding: %s" invalid_name)

let is_polymorphism_on       () = (get()).polymorphism_flag
let is_templ_polymorphism_on () = (get()).templ_polymorphism_flag
let is_constraints_on        () = (get()).constraints_flag
let is_float_univ_on         () = (get()).float_univ_flag
let is_readable_on           () = (get()).readable_translation_flag
