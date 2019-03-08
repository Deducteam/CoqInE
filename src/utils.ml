


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

(* Returns true if str2 starts with str1.
   e.g. str1="ti" and str2="titi"  *)
let str_starts_with str1 str2 =
  let l1 = String.length str1 in
  let l2 = String.length str2 in
  let rec check i =
    i >= l1 || (i < l2 && String.get str1 i = String.get str2 i && check (i+1))
  in
  check 0

let truncate str l =
  String.sub str l (String.length str - l)

let filter_some l =
  let aux acc = function
  | None   -> acc
  | Some a -> a :: acc in
  List.rev (List.fold_left aux [] l)


let rec iterate n f x = match n with
  | 0 -> x
  | n -> iterate (n-1) f (f x)

let map_opt f = function
  | None -> None
  | Some x -> Some (f x)
