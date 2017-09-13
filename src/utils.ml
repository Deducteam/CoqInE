


(* [list_chop i l] splits [l] into two lists [(l1,l2)] such that
   [l1++l2=l] and [l1] has length [i].
   It raises [Failure] when [i] is negative or greater than the length of [l]  *)

let list_chop n l =
  let rec chop_aux i acc = function
    | tl when i=0 -> (List.rev acc, tl)
    | h::t -> chop_aux (pred i) (h::acc) t
    | [] -> failwith "list_chop"
  in
  chop_aux n [] l
