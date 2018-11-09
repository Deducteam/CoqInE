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
  | UDefinition of bool * var * term
  | Rewrite of (var * term) list * term * term

type 'a printer = Format.formatter -> 'a -> unit

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
  end;
  Format.pp_print_newline out ()

let printc out instruction =
  begin match instruction with
  | Comment(c) -> Format.fprintf out "(; %s ;)" c
  | Declaration(definable, x, a) ->
     Format.fprintf out "@[<v2>%s%a :@ %a.@]" (if definable then "def " else "") print_var x print_term a
  | Definition(opaque, x, a, t) ->
      Format.fprintf out "%s %a : %a := %a."
         (if opaque then "thm" else "def")
         print_var x print_term a print_term t
  | UDefinition(opaque, x, t) ->
      Format.fprintf out "%s %a := %a."
         (if opaque then "thm" else "def")
         print_var x print_term t
  | Rewrite(context, left, right) ->
    Format.fprintf out "[ %a] %a --> %a." print_context context print_term left print_term right
  | _ -> assert false
  end;
  Format.pp_print_newline out ()




type coq_universe =
  | Prop
  | Set
  | Atom of string
  | Succ of coq_universe * int
  | Max of coq_universe list



module type CoqTraductor =
sig
  val coq_Sort  : term
  val coq_univ_index : int -> term
  val coq_prop  : term
  val coq_set   : term
  val coq_univ  : int -> term
  val coq_axiom : term -> term
  val coq_axioms: term -> int -> term
  val coq_rule  : term -> term -> term
  val coq_sup   : term -> term -> term
  val coq_U     : term -> term
  val coq_term  : term -> term -> term
  val coq_sort  : term -> term
  val coq_prod  : term -> term -> term -> term -> term
  val coq_cast  : term -> term -> term -> term -> term -> term
    
  val coq_header : instruction list
  val coq_footer : instruction list
end

let coqify name = Printf.sprintf "Coq.%s" name
    
let coq_var  x = Var (coqify x)



module CoqStd : CoqTraductor =
struct
  let coq_Sort = coq_var "Sort"
      
  let coq_univ_index i = Utils.iterate i (app (coq_var "s")) (coq_var "z")
             
  let coq_prop   = coq_var "prop"
  let coq_set    = coq_var "set"
  let coq_univ i = app (coq_var "type") (coq_univ_index i)

  let coq_axiom s          = app  (coq_var "axiom") s
  let coq_axioms s i = Utils.iterate i coq_axiom s
  let coq_rule s1 s2       = apps (coq_var "rule" ) [s1; s2]
  let coq_sup  s1 s2       = apps (coq_var "sup"  ) [s1; s2]
      
  let coq_U    s           = app  (coq_var "U"    ) s
  let coq_term s  a        = apps (coq_var "T"    ) [s; a]
  let coq_sort s           = app  (coq_var "sort" ) s
  let coq_prod s1 s2 a b   = apps (coq_var "prod" ) [s1; s2; a; b]
  let coq_cast s1 s2 a b t = apps (coq_var "cast" ) [s1; s2; a; b; t]

  let coq_header = [ comment "This file was automatically generated by Coqine." ]
  let coq_footer = [ comment "End of translation." ]
end


module CoqShort : CoqTraductor =
struct
  let coq_Sort = coq_var "Sort"
      
  let rec coq_univ_index i =
    if i <= 9
    then var ("n" ^ (string_of_int i))
    else app (coq_var "s") (coq_univ_index (i-1))
        
  let coq_prop   =      coq_var "prop"
  let coq_set    =      coq_var "set"
  let coq_univ i =
    if i <= 9
    then var (string_of_int i)
    else app (coq_var "type") (coq_univ_index i)
        
  let coq_axiom s          = app  (coq_var "axiom") s
  let rec coq_axioms s i   = if i == 0 then s else coq_axiom (coq_axioms s (i-1))
  let coq_rule s1 s2       = apps (coq_var "rule" ) [s1; s2]
  let coq_sup  s1 s2       = apps (coq_var "sup"  ) [s1; s2]
      
  let coq_U s = app (coq_var "U") s
  let coq_term s  a = apps (coq_var "T"    ) [s; a]
  let coq_sort = function
    | Var "0" -> var "u0"
    | Var "1" -> var "u1"
    | Var "2" -> var "u2"
    | Var "3" -> var "u3"
    | Var "4" -> var "u4"
    | Var "5" -> var "u5"
    | Var "6" -> var "u6"
    | Var "7" -> var "u7"
    | Var "8" -> var "u8"
    | Var "9" -> var "u9"
    | Var "Coq.set"  -> var "_set"
    | Var "Coq.prop" -> var "_prop"
    | s -> CoqStd.coq_sort s
  let coq_prod s1 s2 a b   = apps (coq_var "prod" ) [s1; s2; a; b]
  let coq_cast s1 s2 a b t = apps (coq_var "cast" ) [s1; s2; a; b; t]
  
  let coq_header =
    let res = ref CoqStd.coq_header in
    let add n t = res := (udefinition false n t) :: !res in
    add "n0" (coq_var "z");
    for i = 1 to 9 do
      add ("n" ^ (string_of_int i))
        (var ("n" ^ (string_of_int (i-1))));
    done;
    for i = 0 to 9 do
      add (string_of_int i)
        (app (coq_var "type") (var ("n" ^ (string_of_int (i)))));
    done;
    for i = 0 to 9 do
      add ("u" ^ (string_of_int i))
        (app (coq_var "sort") (var (string_of_int (i))));
    done;
    add "_Set"  (coq_U coq_set );
    add "_Prop" (coq_U coq_prop);
    add "_set"  (CoqStd.coq_sort coq_set );
    add "_prop" (CoqStd.coq_sort coq_prop);
    List.rev !res
      
  let coq_footer = [ comment "End of translation." ]
end

module Coq : CoqTraductor = CoqStd
