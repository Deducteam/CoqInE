open Pp
open Format

let debug_libraries = ref []
let add_debug_lib s = debug_libraries := s :: !debug_libraries
let is_debug_lib s = List.exists (fun x -> s = str x) !debug_libraries

let debug_allowed = ref false
let  enable_debug () = debug_allowed := true
let disable_debug () = debug_allowed := false

let debug_flag = ref false
let debug_start () = debug_flag := true
let debug_stop  () = debug_flag := false
let is_debug_on () = !debug_flag && !debug_allowed


let message fmt =
  kfprintf (fun _ -> pp_print_newline Format.std_formatter ()) Format.std_formatter fmt

let debug_out = ref err_formatter

let debug_to_file fn =
  message "Setting debug to: %s" fn;
  enable_debug ();
  debug_out := formatter_of_out_channel (open_out fn)

let debug fmt =
  if is_debug_on ()
  then kfprintf (fun _ -> pp_print_newline !debug_out ()) !debug_out fmt
  else ifprintf err_formatter fmt

let errdebug fmt =
  kfprintf (fun _ -> pp_print_newline err_formatter ()) err_formatter fmt

let format_of_sep str fmt () : unit =
  Format.fprintf fmt "%s" str


type 'a printer = formatter -> 'a -> unit

let pp_list sep pp fmt l = Format.pp_print_list ~pp_sep:(format_of_sep sep) pp fmt l

let pp_option none pp fmt = function
  | None -> Format.fprintf fmt "%s" none
  | Some x -> pp fmt x

let pp_array sep pp fmt a = pp_list sep pp fmt (Array.to_list a)

let pp_t = pp_with

let printer_of_std_ppcmds f fmt x = fprintf fmt "%a" pp_t (f x)

let pp_coq_term  =
  let (sigma, env) = Pfedit.get_current_context () in
  printer_of_std_ppcmds (Printer.safe_pr_constr_env env sigma)
let pp_coq_type  =
  let (sigma, env) = Pfedit.get_current_context () in
  printer_of_std_ppcmds (Printer.pr_type_env env sigma)
let pp_coq_level = printer_of_std_ppcmds Univ.Level.pr
let pp_coq_univ  = printer_of_std_ppcmds Univ.Universe.pr
let pp_coq_id    = printer_of_std_ppcmds Names.Id.print
let pp_coq_label = printer_of_std_ppcmds Names.Label.print

let pp_coq_lvl_arr = pp_array " " pp_coq_level

let pp_coq_inst fmt univ_instance =
  let levels = Univ.Instance.to_array univ_instance in
  fprintf fmt "%a" pp_coq_lvl_arr levels

let pp_coq_name fmt = function
  | Names.Name.Anonymous -> fprintf fmt "_"
  | Names.Name.Name n    -> fprintf fmt "%a" pp_coq_id n

let pp_coq_sort fmt = function
  | Term.Prop Term.Null -> fprintf fmt "Set"
  | Term.Prop Term.Pos  -> fprintf fmt "Prop"
  | Term.Type i         -> fprintf fmt "Univ(%a)" pp_coq_univ i

let pp_coq_decl fmt = function
  | Context.Rel.Declaration.LocalAssum (name, t) ->
    fprintf fmt "%a : %a" pp_coq_name name pp_coq_term t
  | Context.Rel.Declaration.LocalDef (name, v, t) ->
    fprintf fmt "%a : %a = %a" pp_coq_name name pp_coq_term t pp_coq_term t

let pp_coq_arity_ctxt fmt = pp_list " -> " pp_coq_decl fmt

let pp_coq_named_decl fmt = function
  | Context.Named.Declaration.LocalAssum (id, t) ->
    fprintf fmt "%a = %a" pp_coq_id id pp_coq_term t
  | Context.Named.Declaration.LocalDef (id, v, t) ->
    fprintf fmt "%a : %a = %a" pp_coq_id id pp_coq_term t pp_coq_term t

let pp_coq_ctxt fmt ctxt =
  fprintf fmt "[\n  %a\n]" (pp_list "\n  " pp_coq_decl) ctxt

let pp_coq_named_ctxt fmt ctxt =
  fprintf fmt "[\n  %a\n]" (pp_list "\n  " pp_coq_named_decl) ctxt

let pp_coq_env fmt e =
  fprintf fmt "%a\n%a"
    pp_coq_ctxt       (Environ.rel_context e)
    pp_coq_named_ctxt (Environ.named_context e)

let pp_fixpoint fmt (fp:(Constr.constr,Constr.types) Constr.pfixpoint) =
  let (rec_indices, i), (names, types, bodies) = fp in
  let n = Array.length names in
  let bodies = Array.init n (fun i -> (names.(i), types.(i), bodies.(i))) in
  let pp_bodies fmt (n,t,b) =
    fprintf fmt "%a : %a :=@.  %a,"
      pp_coq_name n pp_coq_term t pp_coq_term b in
  fprintf fmt "Fix %a@.  { %a }" pp_coq_name names.(i) (pp_array "@." pp_bodies) bodies


let pp_globname fmt n =
  fprintf fmt "%a" pp_coq_term (Globnames.printable_constr_of_global n)
