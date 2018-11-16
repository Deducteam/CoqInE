
type encoding =
  {
    polymorphism_flag       : bool;
    templ_polymorphism_flag : bool;
    constraints_flag        : bool;
    float_univ_flag         : bool;

    system_module : string;
    universe_module : string;
  }

let default =
  {
    polymorphism_flag       = true;
    templ_polymorphism_flag = true;
    constraints_flag        = true;
    float_univ_flag         = false;
    system_module = "Coq";
    universe_module = "U";
  }

let enc = ref default

let enc_system_module () = !enc.system_module
let enc_universe_module () = !enc.universe_module







let set_encoding enc_name =
  enc :=
    match enc_name with
    | "default" -> default
    | _ -> failwith (Format.sprintf "Unknown encoding: %s" enc_name)

let  enable_polymorphism () = enc := {!enc with polymorphism_flag = true }
let disable_polymorphism () = enc := {!enc with polymorphism_flag = false }
let is_polymorphism_on () = !enc.polymorphism_flag

let  enable_templ_polymorphism () = enc := {!enc with templ_polymorphism_flag = true }
let disable_templ_polymorphism () = enc := {!enc with templ_polymorphism_flag = false }
let is_templ_polymorphism_on () = !enc.templ_polymorphism_flag

let  enable_constraints () = enc := {!enc with constraints_flag = true }
let disable_constraints () = enc := {!enc with constraints_flag = false }
let is_constraints_on () = !enc.constraints_flag

let  enable_float_univ () = enc := {!enc with float_univ_flag = true }
let disable_float_univ () = enc := {!enc with float_univ_flag = false }
let is_float_univ_on () = !enc.float_univ_flag



