(** Dedukti syntax and pretty-printing functions *)

type var = string

type term =
  | Type
  | Var of var
  | Pie of (var * term) * term
  | Lam of (var * term) * term
  | App of term * term
  | Dot of term (* Dot patterns *)
  | Cmt of string * term (* Comment annotations *)

type instruction =
  | Comment of string
  | Command of string * string list (* e.g. "#NAME" or "IMPORT" *)
  | Declaration of bool * var * term
  | Definition of bool * var * term * term
  | Rewrite of (var * term) list * term * term

let var x = Var(x)

let arr a b = Pie(("", a), b)

let pie (x, a) b = Pie((x, a), b)

let lam (x, a) b = Lam((x, a), b)

let app a b = App(a, b)

let dot a = Dot(a)

let cmt s a = Cmt(s, a)

let vars xs = List.map var xs

let arrs args b = List.fold_right arr args b

let pies args b = List.fold_right pie args b

let lams args b = List.fold_right lam args b

let apps a args = List.fold_left app a args

let comment c = Comment(c)

let command cmd args = Command(cmd, args)

let declaration definable x a = Declaration(definable ,x, a)

let definition opaque x a b = Definition(opaque, x, a, b)

let rewrite (context, left, right) = Rewrite(context, left, right)

let apply_context a context = apps a (List.map var (fst (List.split context)))

(** Pretty-printing using the minimal number of parentheses. *)

let print_var out x =
  (* Print anonymous variables as "__".
     The name "_" is not accepted by Dedukti. *)
  match x with
  | "" -> Format.fprintf out "__"
  | _ -> Format.fprintf out "%s" x

let rec print_term out term =
  let rec print_term out term =
    match term with
    | Pie(("", a), b) ->
      Format.fprintf out "%a ->@ %a" print_app a print_term b
    | Pie((x, a), b) ->
      Format.fprintf out "%a ->@ %a" print_binding (x, a) print_term b
    | Lam((x, a), b) ->
      Format.fprintf out "%a =>@ %a" print_binding (x, a) print_term b
    | _ ->
      Format.fprintf out "%a" print_app term
  in
  Format.fprintf out "@[<hv0>%a@]" print_term term

and print_app out term =
  let rec print_app out term =
    match term with
    | App(a, b) ->
      Format.fprintf out "%a@ %a" print_app a print_atomic b
    | _ ->
      Format.fprintf out "%a" print_atomic term
  in
  Format.fprintf out "@[<2>%a@]" print_app term

and print_atomic out term =
  match term with
  | Type ->
    Format.fprintf out "Type"
  | Var(x) ->
    Format.fprintf out "%a" print_var x
  | Dot(a) ->
    Format.fprintf out "{%a}" print_term a
  | Cmt(s, a) ->
    Format.fprintf out "(; %s ;) (%a)" s print_term a
  | _ ->
    Format.fprintf out "(%a)" print_term term

and print_binding out (x, a) =
  Format.fprintf out "@[<2>%a :@ %a@]" print_var x print_app a

let print_binding_context out (x, a) =
  Format.fprintf out "@[<2>%a@]" print_var x

let print_context out context =
  let rec print_context out context =
    match context with
    | [] -> ()
    | [xa] -> Format.fprintf out "%a" print_binding_context xa
    | xa :: context -> Format.fprintf out "%a,@ %a" print_binding_context xa print_context context
  in
  Format.fprintf out "@[<v>%a@]" print_context context

let print out instruction =
  begin match instruction with
  | Comment(c) ->
      Format.fprintf out "(; %s ;)" c
  | Command(cmd, args) ->
      let print_args out = List.iter (Format.fprintf out " %s") in
      Format.fprintf out "@[#%s%a.@]" cmd print_args args
  | Declaration(definable, x, a) ->
     Format.fprintf out "@[<v2>%s%a :@ %a.@]" (if definable then "def " else "") print_var x print_term a
  | Definition(opaque, x, a, t) ->
      Format.fprintf out "@[<v2>%s %a :@ @ %a :=@ @ %a.@]"
         (if opaque then "thm" else "def")
         print_var x print_term a print_term t
  | Rewrite(context, left, right) ->
      Format.fprintf out "@[<v2>[ %a]@ @ %a -->@ @ %a.@]" print_context context print_term left print_term right
  end;
  Format.pp_print_newline out ();
  Format.pp_print_newline out ()


type coq_universe =
  | Prop
  | Set
  | Atom of string
  | Succ of coq_universe * int
  | Max of coq_universe list

let coqify name = Printf.sprintf "Coq.%s" name

let coq_var  x = Var(coqify x)

let coq_Sort = coq_var "Sort"

let coq_z    =      coq_var "z"
let coq_s i  = app (coq_var "s") i
let rec coq_univ_index i = if i == 0 then coq_z else coq_s (coq_univ_index (i-1))

let coq_prop   =      coq_var "prop"
let coq_set    =      coq_var "set"
let coq_type i = app (coq_var "type" ) i
let coq_univ i = coq_type (coq_univ_index i)

let coq_axiom s          = app  (coq_var "axiom") s
let rec coq_axioms s i   = if i == 0 then s else coq_axiom (coq_axioms s (i-1))
let coq_rule s1 s2       = apps (coq_var "rule" ) [s1; s2]
let coq_sup  s1 s2       = apps (coq_var "sup"  ) [s1; s2]

let coq_U = function
  | Var "set"  -> coq_var "Set"
  | Var "prop" -> coq_var "Prop"
  | App( (Var "type"), (Var "z") ) -> coq_var "Type0"
  | App( (Var "type"), (App( (Var "s"), (Var "z")))) -> coq_var "Type1"
  | s -> app (coq_var "U") s
let coq_term s  a        = apps (coq_var "T"    ) [s; a]
let coq_sort s           = app  (coq_var "sort" ) s
let coq_prod s1 s2 a b   = apps (coq_var "prod" ) [s1; s2; a; b]
let coq_cast s1 s2 a b t = apps (coq_var "cast" ) [s1; s2; a; b; t]


let debug_flag = ref false

let std = Format.std_formatter

let start_debug () = debug_flag := true
let  stop_debug () = debug_flag := false


let debug_dk_term t =
  if !debug_flag then begin
    print_term std t;
    Pp.msg_with std (Pp.str "\n")
    end

let debug_coq_term t =
  if !debug_flag then begin
    Pp.msg_with std (Printer.safe_pr_constr t);
    Pp.msg_with std (Pp.str "\n")
    end

let debug_coq_type t =
  if !debug_flag then begin
    Pp.msg_with std (Printer.pr_type t);
    Pp.msg_with std (Pp.str "\n")
    end

let debug_string s =
  if !debug_flag then begin
    Pp.msg_with std (Pp.str s);
    Pp.msg_with std (Pp.str "\n")
    end
