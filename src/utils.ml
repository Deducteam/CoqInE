


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


let str_starts_with p =
  let p_l = String.length p in
  let rec f i res =
    if i >= p_l
    then res
    else
      let c = String.get p i in
      fun x -> res x && String.get x i = c
  in
  f 0 (fun x -> true)

let filter_some l =
  let aux acc = function
  | None   -> acc
  | Some a -> a :: acc in
  List.rev (List.fold_left aux [] l)


let rec iterate n f x = match n with
  | 0 -> x
  | n -> iterate (n-1) f (f x)
