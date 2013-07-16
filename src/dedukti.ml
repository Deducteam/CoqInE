(** Dedukti syntax and pretty-printing functions *)

type var = string

type term =
  | Type
  | Var of var
  | Pie of (var * term) * term
  | Lam of (var * term) * term
  | App of term * term
  | Dot of term

type instruction =
  | Declaration of var * term
  | Definition of bool * var * term * term
  | Rewrite of ((var * term) list * term * term) list

let var x = Var(x)

let arr a b = Pie(("", a), b)

let pie (x, a) b = Pie((x, a), b)

let lam (x, a) b = Lam((x, a), b)

let app a b = App(a, b)

let arrs args b = List.fold_right arr args b

let pies args b = List.fold_right pie args b

let lams args b = List.fold_right lam args b

let apps a args = List.fold_left app a args

let declaration x a = Declaration(x, a)

let definition opaque x a b = Definition(opaque, x, a, b)

let rewrite rules = Rewrite(rules)

(** Pretty-printing using the minimal number of parentheses. *)

let print_var out x =
  match x with
  | "" -> failwith "Empty variable name"
  | _ -> Printf.fprintf out "%s" x

let rec print_term out term =
  match term with
  | Pie(("", a), b) ->
    Printf.fprintf out "%a -> %a" print_app a print_term b
  | Pie((x, a), b) ->
    Printf.fprintf out "%a : %a -> %a" print_var x print_app a print_term b
  | Lam((x, a), b) ->
    Printf.fprintf out "%a : %a => %a" print_var x print_app a print_term b
  | _ ->
    Printf.fprintf out "%a" print_app term

and print_app out term =
  match term with
  | App(a, b) ->
    Printf.fprintf out "%a %a" print_app a print_atomic b
  | _ ->
    Printf.fprintf out "%a" print_atomic term

and print_atomic out term =
  match term with
  | Type ->
    Printf.fprintf out "Type"
  | Var(x) ->
    Printf.fprintf out "%a" print_var x
  | Dot(a) ->
    Printf.fprintf out "{%a}" print_term a
  | _ ->
    Printf.fprintf out "(%a)" print_term term

let print_binding out (x, a) =
  Printf.fprintf out "%a : %a" print_var x print_term a

let rec print_context out context =
  match context with
  | [] -> ()
  | [xa] -> Printf.fprintf out "%a" print_binding xa
  | xa :: context -> Printf.fprintf out "%a, %a" print_binding xa print_context context

let print_rule out (context, left, right) =
  Printf.fprintf out "[%a]\n  %a -->\n  %a\n" print_context context print_term left print_term right

let print_instruction out instruction =
  match instruction with
  | Declaration(x, a) ->
      Printf.fprintf out "\n%a : %a.\n" print_var x print_term a
  | Definition(opaque, x, a, t) ->
      let print_opaque out x =
        if opaque then Printf.fprintf out "{%a}" print_var x
        else Printf.fprintf out "%a" print_var x in
      Printf.fprintf out "\n%a : %a :=\n  %a.\n" print_opaque x print_term a print_term t
  | Rewrite(rules) ->
      let print_rules out = List.iter (print_rule out) in
      Printf.fprintf out "\n%a.\n" print_rules rules

let print_comment out comment =
  Printf.fprintf out "\n(; %s ;)\n" comment

(** Print the command (e.g. ["NAME"], ["IMPORT"]) followed by its arguments. *)
let print_command out command args =
  let print_args out = List.iter (Printf.fprintf out " %s") in
  Printf.fprintf out "\n#%s%a\n" command print_args args

