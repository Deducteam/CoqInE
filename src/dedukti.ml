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
  | Declaration of var * term
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

let declaration x a = Declaration(x, a)

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
  match term with
  | Pie(("", a), b) ->
    Format.fprintf out "%a -> %a" print_app a print_term b
  | Pie((x, a), b) ->
    Format.fprintf out "%a : %a -> %a" print_var x print_app a print_term b
  | Lam((x, a), b) ->
    Format.fprintf out "%a : %a => %a" print_var x print_app a print_term b
  | _ ->
    Format.fprintf out "%a" print_app term

and print_app out term =
  match term with
  | App(a, b) ->
    Format.fprintf out "%a %a" print_app a print_atomic b
  | _ ->
    Format.fprintf out "%a" print_atomic term

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

let print_binding out (x, a) =
  Format.fprintf out "%a : %a" print_var x print_term a

let rec print_context out context =
  match context with
  | [] -> ()
  | [xa] -> Format.fprintf out "%a" print_binding xa
  | xa :: context -> Format.fprintf out "%a, %a" print_binding xa print_context context

let print_rule out (context, left, right) =
  Format.fprintf out "[%a]\n  %a -->\n  %a\n" print_context context print_term left print_term right

let print out instruction =
  match instruction with
  | Comment(c) ->
      Format.fprintf out "(; %s ;)\n\n" c
  | Command(cmd, args) ->
      let print_args out = List.iter (Format.fprintf out " %s") in
      Format.fprintf out "#%s%a\n\n" cmd print_args args
  | Declaration(x, a) ->
      Format.fprintf out "%a : %a.\n\n" print_var x print_term a
  | Definition(opaque, x, a, t) ->
      let print_opaque out =
        if opaque then Format.fprintf out "{%a}" print_var
        else Format.fprintf out "%a" print_var in
      Format.fprintf out "%a : %a :=\n  %a.\n\n" print_opaque x print_term a print_term t
  | Rewrite(context, left, right) ->
      Format.fprintf out "%a.\n\n" print_rule (context, left, right)

