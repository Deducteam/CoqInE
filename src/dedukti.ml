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


module type CoqTranslator =
sig
  val coq_Sort  : term
  val coq_univ_index : int -> term
  val coq_prop  : term
  val coq_set   : term
  val coq_Sort  : term
  val coq_univ  : int -> term
  val coq_var_univ_name : int -> var
  val coq_univ_name : string -> var
  val coq_global_univ : string -> term
  val coq_universe   : cic_universe -> term
  val coq_axiom : term -> term
  val coq_axioms: term -> int -> term
  val coq_rule  : term -> term -> term
  val coq_sup   : term -> term -> term
  val coq_U     : cic_universe -> term
  val coq_term  : cic_universe -> term -> term
  val coq_sort  : cic_universe -> term
  val coq_prod  : cic_universe -> cic_universe -> term -> term -> term
  val coq_cast  : cic_universe -> cic_universe -> term -> term -> term -> term

  val cstr_leq : term -> term -> term
  val cstr_le  : term -> term -> term

  val coq_header : instruction list
  val coq_footer : instruction list
end



let add_prefix prefix name = Printf.sprintf "%s.%s" prefix name

module CoqStd : CoqTranslator =
struct
  let coq_univ_name s = String.concat "__" (String.split_on_char '.' s)

  let coq_var_univ_name n = "s" ^ string_of_int n

  let coq_global_univ u = Var ("U." ^ (coq_univ_name u))

  let coq_var  x = Var (add_prefix "Coq" x)
  let univ_var x = Var (add_prefix "U"   x)

  let coq_Sort = coq_var "Sort"
  let coq_axiom s          = app  (coq_var "axiom") s
  let coq_axioms s i = Utils.iterate i coq_axiom s
  let coq_rule s1 s2       = apps (coq_var "rule" ) [s1; s2]
  let coq_sup  s1 s2       = apps (coq_var "sup"  ) [s1; s2]

  let coq_univ_index i = Utils.iterate i (app (coq_var "s")) (coq_var "z")

  let coq_prop   = coq_var "prop"
  let coq_set    = coq_var "set"
  let coq_univ i = app (coq_var "type") (coq_univ_index i)

  let rec cu = function
    | Set     -> coq_set
    | Prop    -> coq_prop
    | LocalNamed name -> var name
    | Local n -> var ("s" ^ string_of_int n)
    | Template name -> var (coq_univ_name name)
    | Global name -> coq_global_univ name
    | Succ (u,i) -> coq_axioms (cu u) i
    | Max []  -> coq_prop
    | Max [u] -> cu u
    | Max (u :: u_list) -> coq_sup (cu u) (cu (Max u_list))
    | Rule (s1,s2) -> coq_rule (cu s1) (cu s2)

  let coq_universe = cu

  let coq_U    s        = app  (coq_var "U"    ) (cu s)
  let coq_term s  a     = apps (coq_var "T"    ) [cu s; a]
  let coq_sort s        = app  (coq_var "sort" ) (cu s)
  let coq_prod s1 s2 a b   = apps (coq_var "prod" ) [cu s1; cu s2; a; b]
  let coq_cast s1 s2 a b t = apps (coq_var "cast" ) [cu s1; cu s2; a; b; t]

  let cstr_leq s1 s2 = apps (coq_var "Cumul") [s1          ; s2]
  let cstr_le  s1 s2 = apps (coq_var "Cumul") [coq_axiom s1; s2]

  let coq_header = [ comment "This file was automatically generated by Coqine." ]
  let coq_footer = [ comment "End of translation." ]
end


module CoqShort : CoqTranslator =
struct
  include CoqStd

  let coqify = add_prefix "Coq"
  let coq_var  x = Var (coqify x)

  let rec coq_univ_index i =
    if i <= 9
    then var ("n" ^ (string_of_int i))
    else app (coq_var "s") (coq_univ_index (i-1))

  let coq_univ i =
    if i <= 9 then var (string_of_int i)
    else app (coq_var "type") (coq_univ_index i)

  let coq_U univ = app (coq_var "U") (coq_universe univ)
  let coq_sort = function
    | Succ (Prop, i) when i <= 9 -> var ("u" ^ (string_of_int i))
    | Set  -> var "_set"
    | Prop -> var "_prop"
    | s -> CoqStd.coq_sort s

  let coq_header =
    let res = ref CoqStd.coq_header in
    let add n t = res := (udefinition false n t) :: !res in
    add "n0" (coq_var "z");
    for i = 1 to 9 do
      add ("n" ^ (string_of_int i))
        (app (coq_var "s") (var ("n" ^ (string_of_int (i-1)))));
    done;
    for i = 0 to 9 do
      add (string_of_int i)
        (app (coq_var "type") (var ("n" ^ (string_of_int (i)))));
    done;
    for i = 0 to 9 do
      add ("u" ^ (string_of_int i))
        (app (coq_var "sort") (var (string_of_int (i))));
    done;
    add "_Set"  (CoqStd.coq_U Set );
    add "_Prop" (CoqStd.coq_U Prop);
    add "_set"  (CoqStd.coq_sort Set );
    add "_prop" (CoqStd.coq_sort Prop);
    List.rev (EmptyLine::!res)

end

module PatternTranslator : CoqTranslator = CoqStd

module Translator : CoqTranslator = CoqShort
