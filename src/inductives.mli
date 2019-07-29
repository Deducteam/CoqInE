
(** Structure with informations on an inductive constructions *)
type ind_infos

(** Extract inductive infos from Coq representation *)
val get_infos : Declarations.mutual_inductive_body -> int -> ind_infos

(** Translate the given inductive type.
Handles both regular inductive type (no polymorphism):
    I : p1 : ||P1|| -> ... -> pr : ||Pr|| ->
        x1 : ||A1|| -> ... -> xn : ||An|| -> ||return_type||

and polymorphic inductive type:
    I : [s1] : Sort -> ... -> [sr] : Sort ->
        p1 : ||P1|| -> ... -> pr : ||Pr|| ->
        x1 : ||A1|| -> ... -> xn : ||An|| -> ||return_type||
where the s1 ... sr are either
- template polymorphic universe levels
-  "true"  polymorphic universe levels
*)
val translate_inductive :
  Info.info -> Environ.env -> Names.Label.t -> ind_infos -> unit

(** Subtyping is extended to parameters of template or true polymorphic inductive types.
    For all j such that the parameter pj has type
      A1 -> ... -> Ak -> Type_si
    where si is either
    - a head-quantified template polymorphic parameter
    - a head-quantified true polymorphic parameter with positive influence
      on the return sort expression s.
    we generate:
      I s1 ... si' ... sk
        p1
        ...
        (x1 => ... => xl => lift (u si) _ (pj x1 ... xl))
        ...
        pr
        a1 ...  ... an
      -->
      lift s[s1 ... si ..., sk]  s[s1 ... si' ... sk]
        (I s1 ... si ... sk
           p1  ... (x1 => ... => xl => pj x1 ... xl) ... pr
           a1 ... an)
    Note: we might generate identity lifts which shouldn't be an issue.
*)
val translate_inductive_subtyping :
  Info.info -> Environ.env -> Names.Label.t -> ind_infos -> unit


(** Translate the constructors of the given inductive type.
    cj : [s1 : Sort] -> ... -> [sr : Sort] ->
         p1 : ||P1||(s1) -> ... -> pr : ||Pr||(sr) ->
         y1  : ||Bj1||(s1,...,sr) ->
         ... ->
         ykj : ||Bjkj||(s1,...,sr) ->
         I  s1 ... sr  p1 ... pr  aj1(y1...ykj) ... ajn(y1...ykj)
*)
val translate_constructors :
  Info.info -> Environ.env -> Names.Label.t -> ind_infos -> unit


(** Subtyping is extended to parameters of template or true polymorphic inductive types.
    For all cl, l-th constructors of the given inductive type.
    For all j such that the parameter pj has type
      A1 -> ... -> Ak -> Type_si
    where si is either
    - a head-quantified template polymorphic parameter
    - a head-quantified true polymorphic parameter with positive influence
      on the return sort expression s.
    we generate:
      cl s1 ... si' ... sk
         p1 ... (x1 => ... => xl => lift (u si) _ (pj x1 ... xl)) ... pr
         a1 ...  ... an
      -->
      cl s1 ... si ... sk
         p1 ... (x1 => ... => xl => pj x1 ... xl) ... pr
         a1 ... an
    Note: we might generate identity lifts which shouldn't be an issue.
*)
val translate_constructors_subtyping :
  Info.info -> Environ.env -> Names.Label.t -> ind_infos -> unit


(** Translate the match function for the given inductive type.

  match_I :
    [s1:Sort] -> ... -> [sr:Sort] ->
    p1 : ||P1||(s1) ->
    ... ->
    pr : ||Pr||(sr) ->

    s : Sort ->
    P : (x1 : ||A1|| -> ... -> xn : ||An|| ->
         ||I [s1] ... [sr] p1 ... pr x1 ... xn|| ->
           Univ s) ->

    case_c1 : (y11 : ||B11|| -> ... -> y1k1 : ||B1k1|| ->
               Term s (P |u11| ... |u1n| (|c1 [s1] ... [sr] p1 ... pr y11 ... y1k1|))) -> ...
    ... ->
    case_cj : (yj1 : ||Bj1|| -> ... -> yjkj : ||Bjkj|| ->
               Term s (P |uj1| ... |ujn| (|c1 [s1] ... [sr] p1 ... pr yj1 ... yjkj|))) -> ...

    x1 : ||A1|| -> ... -> xn : ||An|| ->
    x : ||I [s1] ... [sr] p1 ... pr x1 ... xn|| ->
    Term s (P x1 ... xn x)
*)
val translate_match :
  Info.info -> Environ.env -> Names.Label.t -> ind_infos -> unit

(** Subtyping is extended to parameters of template or true polymorphic inductive types.
    For all cl, l-th constructors of the given inductive type.
    For all j such that the parameter pj has type
      A1 -> ... -> Ak -> Type_si
    where si is either
    - a head-quantified template polymorphic parameter
    - a head-quantified true polymorphic parameter with positive influence
      on the return sort expression s.
    we generate:
      match_I
        s1 ... si' ... sk
        p1
        ...
        (x1 => ... => xl => lift (u si) _ (pj x1 ... xl))
        ...
        pr
      -->
      match_I
        s1 ... si ... sk
        p1
        ...
        (x1 => ... => xl => pj x1 ... xl)
        ...
        pr
*)
val translate_match_subtyping :
  Info.info -> Environ.env -> Names.Label.t -> ind_infos -> unit

(** Fixpoint definition require to identify properly constructor-guarded expression.
    For all cl, l-th constructors of the given inductive type we generate:
      guarded? _ _ (ci s1 ... si' ... sk p1 ... pr a1 ... an)
      -->
      guarded
*)
val translate_guarded :
  Info.info -> Environ.env -> Names.Label.t -> ind_infos -> unit
