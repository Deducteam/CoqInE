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
  | EmptyLine
  | Comment of string
  | Command of string * string list (* e.g. "#NAME" or "IMPORT" *)
  | Declaration of bool * var * term
  | Definition of bool * var * term * term
  | UDefinition of bool * var * term
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

let udefinition opaque x a = UDefinition(opaque, x, a)

let rewrite (context, left, right) = Rewrite(context, left, right)

let apply_context a context = apps a (List.map var (List.map fst context))

(** Pretty-printing using the minimal number of parentheses. *)

(** Print anonymous variables as "__". The name "_" is not accepted by Dedukti. *)
let print_var out = function
  | "" -> Format.fprintf out "__"
  | x  -> Format.fprintf out "%s" x

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

and print_binding out (v, ty) =
  Format.fprintf out "@[<2>%a :@ %a@]" print_var v print_app ty

let pp_term = print_term

let print_context_var out (v, ty) =
  Format.fprintf out "@[<2>%a@]" print_var v

let print_context out context =
  Format.fprintf out "@[<v>%a@]" (Debug.pp_list ", " print_context_var) context

let print out instruction =
  Format.pp_print_newline out ();
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
  | UDefinition(opaque, x, t) ->
      Format.fprintf out "@[<v2>%s %a@ @ :=@ @ %a.@]"
         (if opaque then "thm" else "def")
         print_var x print_term t
  | Rewrite(context, left, right) ->
    Format.fprintf out "@[<v2>[ %a]@ @ %a -->@ @ %a.@]" print_context context print_term left print_term right
  | EmptyLine -> Format.fprintf out "@."
  end;
  Format.pp_print_newline out ()

let printc out = function
  | EmptyLine -> Format.fprintf out "@."
  | Comment(c) -> Format.fprintf out "(; %s ;)@." c
  | Declaration(definable, x, a) ->
    Format.fprintf out "@[<v2>%s%a :@ %a.@]@."
      (if definable then "def " else "") print_var x print_term a
  | Definition(opaque, x, a, t) ->
    Format.fprintf out "%s %a : %a := %a.@."
      (if opaque then "thm" else "def") print_var x print_term a print_term t
  | UDefinition(opaque, x, t) ->
    Format.fprintf out "%s %a := %a.@."
      (if opaque then "thm" else "def") print_var x print_term t
  | Rewrite(context, left, right) ->
    Format.fprintf out "[ %a] %a --> %a.@."
      print_context context print_term left print_term right
  | _ -> assert false
