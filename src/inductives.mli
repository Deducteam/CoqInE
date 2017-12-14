

(** Translate the i-th inductive type in [mind_body].
Handles both regular inductive type (no polymorphism):
    I : p1 : ||P1|| -> ... -> pr : ||Pr|| ->
        x1 : ||A1|| -> ... -> xn : ||An|| -> ||return_type||

and template polymorphic inductive type:
    I : [s1:Sort] -> ... -> [sr:Sort] ->
        p1 : ||P1|| -> ... -> pr : ||Pr|| ->
        x1 : ||A1|| -> ... -> xn : ||An|| -> ||return_type||

(old translation:)
    [ s1:Sort -> ] -> p1 : ||P1||(s1)        ->
    [ s2:Sort -> ] -> p2 : ||P2||(s1,s2)     ->
    ... ->
    [ sr:Sort -> ] -> pr : ||Pr||(s1,...,sr) ->
    || x1 : A1 -> ... -> xn : An -> return_type||(s1,...,sr)
*)
val translate_inductive :
  Info.info -> Environ.env ->
  'a -> Declarations.mutual_inductive_body -> int -> unit


(** Translate the constructors of the i-th inductive type in [mind_body].
    cj : [s1:Sort] -> ... -> [sr:Sort] ->
         p1 : ||P1||(s1) -> ... -> pr : ||Pr||(sr) ->
         yj1  : ||Bj1||(s1,...,sr) ->
         ... ->
         yjkj : ||Bjkj||(s1,...,sr) ->
         I  s1 ... sr  p1 ... pr  yj1 ... yjkj
*)
val translate_constructors :
  Info.info -> Environ.env ->
  Names.Label.t -> Declarations.mutual_inductive_body -> int -> unit


(** Translate the match function for the i-th inductive type in [mind_body].

    match_I :
    [s1:Sort] -> ... -> [sr:Sort] ->
    p1 : ||P1||(s1) -> ... -> pr : ||Pr||(sr) ->
    
    s : Sort ->
    P : (|x1| : ||A1|| -> ... -> |xn| : ||An|| ->
            ||I [s1] p1 ... [sr] pr x1 ... xn|| ->
            type s) ->
    
    case_c1 : (|y11| : ||B11|| -> ... -> |y1k1| : ||B1k1|| ->
               term s (P |u11| ... |u1n| (|c1 [s1] p1 ... [sr] pr y11 ... y1k1|))) -> ...
    ... ->
    case_cj : (|yj1| : ||Bj1|| -> ... -> |yjkj| : ||Bjkj|| ->
               term s (P |uj1| ... |ujn| (|c1 [s1] p1 ... [sr] pr yj1 ... yjkj|))) -> ...
    
    |x1| : ||A1|| -> ... -> |xn| : ||An|| ->
    x : ||I [s1] p1 ... [sr] pr x1 ... xn|| ->
    term s (P |x1| ... |xn| x)

*)
val translate_match :
  Info.info -> Environ.env ->
  Names.Label.t -> Declarations.mutual_inductive_body -> int -> unit
