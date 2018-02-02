
open Pp

let debug_out = ref Format.std_formatter

let debug_to_file fn = debug_out := Format.formatter_of_out_channel (open_out fn)
let debug_msg s = pp_with !debug_out s


let debug_flag = ref false

let debug_start () = debug_flag := true
let debug_stop  () = debug_flag := false


let debug_str s = if !debug_flag then debug_msg (s ++ (str "\n"))

let debug_string s = debug_str (str s)

let debug_dk_term t =
  if !debug_flag then begin
    Dedukti.print_term !debug_out t;
    debug_msg (str "\n")
    end

let pt_coq_term  = Printer.safe_pr_constr
let pt_coq_type  = Printer.pr_type
let pt_coq_level = Univ.Level.pr
let pt_coq_univ  = Univ.Universe.pr
let pt_coq_inst  = Univ.Instance.pr (Univ.Level.pr)
let pt_coq_id    = Names.Id.print
let pt_coq_name = function
  | Names.Name.Anonymous -> (str "_")
  | Names.Name.Name n -> pt_coq_id n
let pt_coq_sort = function
  | Term.Prop Null -> (str "Set")
  | Term.Prop Pos  -> (str "Prop")
  | Term.Type i    -> (str "Univ(") ++ (pt_coq_univ i)
let pt_coq_decl = function
  | Context.Rel.Declaration.LocalAssum (name, t) ->
     (pt_coq_name name) ++ (str " = ") ++ (pt_coq_term t)
  | Context.Rel.Declaration.LocalDef (name, v, t) ->
     (pt_coq_name name) ++ (str " : ") ++ (pt_coq_term t) ++ (str " = ") ++ (pt_coq_term t)
let pt_coq_named_decl = function
  | Context.Named.Declaration.LocalAssum (id, t) ->
     (pt_coq_id id) ++ (str " = ") ++ (pt_coq_term t)
  | Context.Named.Declaration.LocalDef (id, v, t) ->
     (pt_coq_id id) ++ (str " : ") ++ (pt_coq_term t) ++ (str " = ") ++ (pt_coq_term t)
let rec pt_coq_ctxt t =
  let pt s decl = s ++ (str "\n  ") ++ (pt_coq_decl decl) in
    (List.fold_left pt (str "[") t) ++ (str "\n]")
let rec pt_coq_named_ctxt t =
  let pt s decl = s ++ (str "\n  ") ++ (pt_coq_named_decl decl) in
    (List.fold_left pt (str "[") t) ++ (str "\n]")
let pt_coq_env e =
  (pt_coq_ctxt (Environ.rel_context e)) ++
  (str "\n") ++
  (pt_coq_named_ctxt (Environ.named_context e))

let debug_coq_term  t = debug_str (pt_coq_term  t)
let debug_coq_type  t = debug_str (pt_coq_type  t)
let debug_coq_level t = debug_str (pt_coq_level t)
let debug_coq_univ  t = debug_str (pt_coq_univ  t)
let debug_coq_inst  t = debug_str (pt_coq_inst  t)
let debug_coq_id    t = debug_str (pt_coq_id    t)
let debug_coq_name  t = debug_str (pt_coq_name  t)
let debug_coq_sort  t = debug_str (pt_coq_sort  t)
let debug_coq_decl  t = debug_str (pt_coq_decl  t)
let debug_coq_ctxt  t = debug_str (pt_coq_ctxt  t)
let debug_coq_env   t = debug_str (pt_coq_env   t)
