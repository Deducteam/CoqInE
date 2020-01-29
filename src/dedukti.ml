(** Dedukti syntax and pretty-printing functions *)

type var = string

type term =
  | Type
  | Var of var
  | Pie of (var * term) * term
  | Lam of (var * term option) * term
  | LetIn of (var * term * term) * term
  | App of term * term
  | Cmt of string * term (* Comment annotations *)
  | Bracket of term
  | Wildcard

type context = (var * term) list

type untyped_context = var list

type instruction =
  | EmptyLine
  | Comment of string
  | Command of string * string list (* e.g. "#NAME" or "IMPORT" *)
  | Declaration of bool * var * term
  | Definition of bool * var * term * term
  | UDefinition of bool * var * term
  | Rewrite of untyped_context * term * term

let var x = Var x

let arr a b = Pie(("", a), b)

let pie (x, a) b = Pie((x, a), b)

let lam (x, a) b = Lam((x, Some a), b)

let letin (x, u, a) b = LetIn((x,u,a), b)

let ulam x b = Lam((x, None), b)

let app a b = App(a, b)

let cmt s a = Cmt(s, a)

let wildcard = Wildcard

let bracket t = Bracket t

let vars xs = List.map var xs

let arrs args b = List.fold_right arr args b

let pies args b = List.fold_right pie args b

let lams args b = List.fold_right lam args b

let ulams args b = List.fold_right ulam args b

let apps a args = List.fold_left app a args

let comment c = Comment(c)

let command cmd args = Command(cmd, args)

let declaration definable x a = Declaration(definable ,x, a)

let definition opaque x a b = Definition(opaque, x, a, b)

let udefinition opaque x a = UDefinition(opaque, x, a)

let alias a b = udefinition false a (var b)

let rewrite (context, left, right) = Rewrite(context, left, right)

let typed_rewrite (context, left, right) = rewrite (List.map fst context, left, right)

let apply_context a context = apps a (List.map var (List.map fst context))



(** Print anonymous variables as "__". The name "_" is not accepted by Dedukti. *)
let print_var out = function
  | ""    -> Format.fprintf out "__"
  | "def" -> Format.fprintf out "def__"
  | x     -> Format.fprintf out "%s" x


(** Indented pretty-printing using the minimal number of parentheses. *)
module DeduktiPrinter =
struct

  let rec print_term out term =
    let rec print_term out term =
      match term with
      | Pie(("", a), b) -> Format.fprintf out "%a ->@ %a" print_app a print_term b
      | Pie((x , a), b) -> Format.fprintf out "%a ->@ %a" print_binding (x, Some a) print_term b
      | Lam((x , a), b) -> Format.fprintf out "%a =>@ %a" print_binding (x,      a) print_term b
      | LetIn((x , u, a), b) ->
        Format.fprintf out "(%a := %a) =>@ %a" print_binding (x,Some a) print_atomic u print_term b
      | _ -> Format.fprintf out "%a" print_app term
    in
    Format.fprintf out "@[<hv0>%a@]" print_term term

  and print_app out term =
    let rec print_app out term =
      match term with
      | App(a, b) -> Format.fprintf out "%a@ %a" print_app a print_atomic b
      | _         -> Format.fprintf out "%a" print_atomic term
    in
    Format.fprintf out "@[<2>%a@]" print_app term

  and print_atomic out term =
    match term with
    | Type      -> Format.fprintf out "Type"
    | Var(x)    -> Format.fprintf out "%a" print_var x
    | Cmt(s, a) -> Format.fprintf out "(; %s ;) (%a)" s print_term a
    | Bracket(t)-> Format.fprintf out "{%a}" print_term t
    | Wildcard  -> Format.fprintf out "_"
    | _         -> Format.fprintf out "(%a)" print_term term

  and print_binding out (v, ty) =
    match ty with
    | None    -> Format.fprintf out "@[<2>%a@]" print_var v
    | Some ty -> Format.fprintf out "@[<2>%a :@ %a@]" print_var v print_app ty

  let pp_term = print_term

  let print_context out context =
    Format.fprintf out "@[<v>%a@]" (Debug.pp_list ", " print_var) context

  let print fmt = function
    | Comment(c) -> Format.fprintf fmt "(; %s ;)@." c
    | Command(cmd, args) ->
      let print_args fmt = List.iter (Format.fprintf fmt " %s") in
      Format.fprintf fmt "@[#%s%a.@]@.@." cmd print_args args
    | Declaration(definable, x, a) ->
      Format.fprintf fmt "@[<v2>%s%a :@ %a.@]@.@."
        (if definable then "def " else "") print_var x print_term a
    | Definition(opaque, x, a, t) ->
      Format.fprintf fmt "@[<v2>%s %a :@ %a@.@.:= %a.@]@.@."
        (if opaque then "thm" else "def")
        print_var x print_term a print_term t
    | UDefinition(opaque, x, t) ->
      Format.fprintf fmt "@[<v2>%s %a@ := %a.@]@.@."
        (if opaque then "thm" else "def")
        print_var x print_term t
    | Rewrite(context, left, right) ->
      Format.fprintf fmt "@[<v2>[ %a]@ %a@ -->@ %a.@]@.@."
        print_context context print_term left print_term right
    | EmptyLine -> Format.pp_print_newline fmt ()

  let printc fmt = function
    | Comment(c) -> Format.fprintf fmt "(; %s ;)@." c
    | Declaration(definable, x, a) ->
      Format.fprintf fmt "@[%s%a : %a.@]@."
        (if definable then "def " else "") print_var x print_term a
    | Definition(opaque, x, a, t) ->
      Format.fprintf fmt "@[<v2>%s %a : %a := %a.@]@."
        (if opaque then "thm" else "def")
        print_var x print_term a print_term t
    | UDefinition(opaque, x, t) ->
      Format.fprintf fmt "@[<v2>%s %a := %a.@]@."
        (if opaque then "thm" else "def")
        print_var x print_term t
    | Rewrite(context, left, right) ->
      Format.fprintf fmt "@[<v2>[ %a] %a --> %a.@]@."
      print_context context print_term left print_term right
    | instruction -> print fmt instruction
end



(** Oneliner pretty-printing using the minimal number of parentheses. *)
module DeduktiOnelinePrinter =
struct

  let rec print_term out term =
    let rec print_term out term =
      match term with
      | Pie(("", a), b) -> Format.fprintf out "%a -> %a" print_app a print_term b
      | Pie((x , a), b) -> Format.fprintf out "%a -> %a" print_binding (x, Some a) print_term b
      | Lam((x , a), b) -> Format.fprintf out "%a => %a" print_binding (x,      a) print_term b
      | LetIn((x , u, a), b) ->
        Format.fprintf out "(%a := %a) => %a" print_binding (x,Some a) print_atomic u print_term b
      | _               -> Format.fprintf out "%a" print_app term
    in
    Format.fprintf out "%a" print_term term

  and print_app out term =
    let rec print_app out term =
      match term with
      | App(a, b) -> Format.fprintf out "%a %a" print_app a print_atomic b
      | _         -> Format.fprintf out "%a" print_atomic term
    in
    Format.fprintf out "%a" print_app term

  and print_atomic out term =
    match term with
    | Type      -> Format.fprintf out "Type"
    | Var(x)    -> Format.fprintf out "%a" print_var x
    | Cmt(s, a) -> Format.fprintf out "(; %s ;) (%a)" s print_term a
    | Bracket(t)-> Format.fprintf out "{%a}" print_term t
    | Wildcard  -> Format.fprintf out "_"
    | _         -> Format.fprintf out "(%a)" print_term term

  and print_binding out (v, ty) =
    match ty with
    | None    -> Format.fprintf out "%a" print_var v
    | Some ty -> Format.fprintf out "%a : %a" print_var v print_app ty

  let pp_term = print_term

  let print_context out context =
    Format.fprintf out "%a" (Debug.pp_list ", " print_var) context

  let print fmt = function
    | Comment(c) -> Format.fprintf fmt "(; %s ;)@." c
    | Command(cmd, args) ->
      let print_args fmt = List.iter (Format.fprintf fmt " %s") in
      Format.fprintf fmt "#%s%a.@.@." cmd print_args args
    | Declaration(definable, x, a) ->
      Format.fprintf fmt "%s%a : %a.@.@."
        (if definable then "def " else "") print_var x print_term a
    | Definition(opaque, x, a, t) ->
      Format.fprintf fmt "%s %a : %a := %a.@.@."
        (if opaque then "thm" else "def")
        print_var x print_term a print_term t
    | UDefinition(opaque, x, t) ->
      Format.fprintf fmt "%s %a := %a.@.@."
        (if opaque then "thm" else "def")
        print_var x print_term t
    | Rewrite(context, left, right) ->
      Format.fprintf fmt "[ %a] %a --> %a.@.@."
        print_context context print_term left print_term right
    | EmptyLine -> Format.pp_print_newline fmt ()

  let printc fmt = function
    | Comment(c) -> Format.fprintf fmt "(; %s ;)@." c
    | Declaration(definable, x, a) ->
      Format.fprintf fmt "%s%a : %a.@."
        (if definable then "def " else "") print_var x print_term a
    | Definition(opaque, x, a, t) ->
      Format.fprintf fmt "%s %a : %a := %a.@."
        (if opaque then "thm" else "def")
        print_var x print_term a print_term t
    | UDefinition(opaque, x, t) ->
      Format.fprintf fmt "%s %a := %a.@."
        (if opaque then "thm" else "def")
        print_var x print_term t
    | Rewrite(context, left, right) ->
      Format.fprintf fmt "[ %a] %a -->  %a.@."
      print_context context print_term left print_term right
    | instruction -> print fmt instruction
end

module DeduktiCondensedPrinter =
struct

  let print fmt arg = match arg with
    | Declaration(definable, x, a) -> DeduktiPrinter.print fmt arg
    | Definition(opaque, x, a, t) ->
      Format.fprintf fmt "@[<v2>%s %a :@ %a@]@.@.:= %a.@.@."
        (if opaque then "thm" else "def")
        print_var x DeduktiPrinter.print_term a DeduktiOnelinePrinter.print_term t
    | _ -> DeduktiOnelinePrinter.print fmt arg

  let printc = DeduktiOnelinePrinter.printc
  let pp_term = DeduktiOnelinePrinter.pp_term
end


(* Supported syntaxes *)
type supportedSyntax = Dedukti | CondensedDedukti | OnelineDedukti
(* TODO: add a printer for Lambdapi *)

(* Fetching current export syntax *)
let syntax () =
  match Encoding.symb "syntax" with
  | "Dedukti"          -> Dedukti
  | "CondensedDedukti" -> CondensedDedukti
  | "OnelineDedukti"   -> OnelineDedukti
  | syntax -> failwith ("Unsupported output syntax: " ^ syntax)

(* Defining printers *)
let print x =
  match syntax() with
  | Dedukti          -> DeduktiPrinter.print x
  | OnelineDedukti   -> DeduktiOnelinePrinter.print x
  | CondensedDedukti -> DeduktiCondensedPrinter.print x

let printc x =
  match syntax() with
  | Dedukti          -> DeduktiPrinter.printc x
  | OnelineDedukti   -> DeduktiOnelinePrinter.printc x
  | CondensedDedukti -> DeduktiCondensedPrinter.printc x

let pp_term x =
  match syntax() with
  | Dedukti          -> DeduktiPrinter.pp_term x
  | OnelineDedukti   -> DeduktiOnelinePrinter.pp_term x
  | CondensedDedukti -> DeduktiCondensedPrinter.pp_term x
