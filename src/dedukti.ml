(** Dedukti syntax and pretty-printing functions *)

open Encoding

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



type cic_universe =
  | Prop
  | Set
  | LocalNamed of string
  | Local of int
  | Template of string
  | Global of string
  | Succ of cic_universe * int
  | Max of cic_universe list
  | Rule of cic_universe * cic_universe

(* Note:
  Succ(Prop,0) = Prop
  Succ(Prop,1) = Axiom(Prop) = Type_0
  ... *)
let mk_type i = Succ (Prop, i+1)


let add_prefix prefix name = Printf.sprintf "%s.%s" prefix name


open Enc

let coq_var  x = Var (add_prefix (get()).system_module   x)
let univ_var x = Var (add_prefix (get()).universe_module x)

let coq_Sort () = coq_var (get()).t_Sort

let coq_var_univ_name n = "s" ^ string_of_int n

let coq_univ_name s = String.concat "__" (String.split_on_char '.' s)

let coq_header = [ comment "This file was automatically generated by Coqine." ]
let coq_footer = [ comment "End of translation." ]

module Std =
struct

  let coq_nat n = Utils.iterate n (app (coq_var "s")) (coq_var "z")

  let coq_prop       = coq_var "prop"
  let coq_set        = coq_var "set"
  let coq_type i     = app (coq_var "type") (coq_nat i)

  let coq_axiom s    = app  (coq_var "axiom") s
  let coq_axioms s i = Utils.iterate i coq_axiom s
  let coq_rule s1 s2 = apps (coq_var "rule" ) [s1; s2]
  let rec coq_sup  = function
    | [] -> coq_prop
    | [u] -> u
    | (u :: u_list) -> apps (coq_var "sup") [u; coq_sup u_list]
  let rec cu = function
    | Succ (u   ,0) -> cu u
    | Succ (Succ(u,i), j) -> cu (Succ (u,i+j))
    | Set           -> coq_set
    | Prop          -> coq_prop
    | LocalNamed name -> var name
    | Local n       -> var ("s" ^ string_of_int n)
    | Template name -> var (coq_univ_name name)
    | Global name   -> univ_var name
    | Succ (Set ,i) -> coq_type (i-1)
    | Succ (Prop,i) -> coq_type (i-1)
    | Succ (u,i)    -> coq_axioms (cu u) i
    | Max u_list    -> coq_sup (List.map cu u_list)
    | Rule (s1,s2)  -> coq_rule (cu s1) (cu s2)

  let rec cpu = function
    | Succ (u, 0)   -> cpu u
    | Succ (Succ(u,i), j) -> cpu (Succ (u,i+j))
    | Succ (u,i)    -> coq_axioms (cu u) i
    | Max u_list    -> coq_sup (List.map cpu u_list)
    | Rule (s1,s2)  -> coq_rule (cpu s1) (cpu s2)
    | u -> cu u
  let coq_pattern_universe u = cpu u

  let coq_U    s           = app  (coq_var "U"    ) (cu s)
  let coq_term s  a        = apps (coq_var "T"    ) [cu s; a]
  let coq_sort s           = app  (coq_var "sort" ) (cu s)
  let coq_prod s1 s2 a b   = apps (coq_var "prod" ) [cu s1; cu s2; a; b]
  let coq_cast s1 s2 a b t = apps (coq_var "cast" ) [cu s1; cu s2; a; b; t]

  let cstr_leq s1 s2 = apps (coq_var "Cumul") [cu s1            ; cu s2]
  let cstr_le  s1 s2 = apps (coq_var "Cumul") [coq_axiom (cu s1); cu s2]

end


(* ------------ Redefining all functions for readable translation ------- *)
module Short =
struct

  let nat_name u = "n" ^ (string_of_int u)
  let nat_var  u = (var (nat_name u))

  let sort_name u = string_of_int u
  let sort_var  u = (var (sort_name u))

  let code_name u = "u" ^ (string_of_int u)
  let code_var  u = (var (code_name u))

  (* Redefining headers first then overriding previous definitions. *)
  let coq_header =
    let res = ref coq_header in
    let add n t = res := (udefinition false n t) :: !res in
    add (nat_name 0) (coq_var "z");
    for i = 1 to 9 do add ( nat_name i) (app (coq_var "s"   ) ( nat_var (i-1))) done;
    for i = 0 to 9 do add (sort_name i) (app (coq_var "type") ( nat_var i    )) done;
    for i = 0 to 9 do add (code_name i) (app (coq_var "sort") (sort_var i    )) done;
    add "_Set"  (Std.coq_U    Set );
    add "_Prop" (Std.coq_U    Prop);
    add "_set"  (Std.coq_sort Set );
    add "_prop" (Std.coq_sort Prop);
    List.rev (EmptyLine::!res)

  let rec short_nat i =
    if i <= 9 then nat_var i else app (coq_var "s") (short_nat (i-1))

  let short_type i =
    if i <= 9 then sort_var i else app (coq_var "type") (short_nat i)

  let short_code i =
    if i <= 9 then code_var i else app (coq_var "sort") (short_type i)

  let rec scu = function
    | Succ (u,0)    -> scu u
    | Succ (Succ(u,i), j) -> scu (Succ (u,i+j))
    | Succ (Set ,i) -> short_type i
    | Succ (Prop,i) -> short_type i
    | Succ (u,i)    -> Std.coq_axioms (scu u) i
    | Max u_list    -> Std.coq_sup (List.map scu u_list)
    | Rule (s1,s2)  -> Std.coq_rule (scu s1) (scu s2)
    | s -> Std.cu s

  let short_U    s   = app  (coq_var "U"    ) (scu s)
  let short_term s a = apps (coq_var "T"    ) [scu s; a]
  let rec short_sort = function
    | Succ (u   ,0) -> short_sort u
    | Succ (Succ(u,i), j) -> short_sort (Succ (u,i+j))
    | Max [] -> var "_prop"
    | Max [u] -> short_sort u
    | Succ (Set ,i) -> short_code (i-1)
    | Succ (Prop,i) -> short_code (i-1)
    | Set  -> var "_set"
    | Prop -> var "_prop"
    | u -> app (coq_var "sort" ) (scu u)
  let short_prod s1 s2 a b   = apps (coq_var "prod" ) [scu s1; scu s2; a; b]
  let short_cast s1 s2 a b t = apps (coq_var "cast" ) [scu s1; scu s2; a; b; t]

  let short_leq s1 s2 = apps (coq_var "Cumul") [scu s1                ; scu s2]
  let short_le  s1 s2 = apps (coq_var "Cumul") [Std.coq_axiom (scu s1); scu s2]

end

module Translator =
struct

  let coq_Sort          = coq_Sort
  let coq_var_univ_name = coq_var_univ_name
  let coq_univ_name     = coq_univ_name
  let coq_global_univ   = univ_var

  let coq_pattern_universe = Std.coq_pattern_universe

  let coq_universe s = if is_readable_on () then Std.cu       s else Short.scu s
  let coq_U        s = if is_readable_on () then Std.coq_U    s else Short.short_U s
  let coq_term     s = if is_readable_on () then Std.coq_term s else Short.short_term s
  let coq_sort     s = if is_readable_on () then Std.coq_sort s else Short.short_sort s
  let coq_prod     s = if is_readable_on () then Std.coq_prod s else Short.short_prod s
  let coq_cast     s = if is_readable_on () then Std.coq_cast s else Short.short_cast s
  let cstr_leq     s = if is_readable_on () then Std.cstr_leq s else Short.short_leq s
  let cstr_le      s = if is_readable_on () then Std.cstr_le  s else Short.short_le s
  let coq_header  () = if is_readable_on () then coq_header else Short.coq_header
  let coq_footer  () = coq_footer
end
