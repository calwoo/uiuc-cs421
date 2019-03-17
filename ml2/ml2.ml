(* CS421 - Fall 2016
 * ML2
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You will want to change how a number of these start.
 *)

(*************************
 * Patterns of Recursion *
 *************************)

(*********************
 * Forward Recursion *
 *********************)

(* Problem 1 *)
let rec even_count_fr l =
  match l with
    | [] -> 0
    | i::is -> if i mod 2 == 0
               then 1 + even_count_fr is
               else even_count_fr is;;

(* Problem 2 *)
let rec pair_sums l = 
  match l with
    | [] -> []
    | (a,b)::xs -> (a+b)::(pair_sums xs);;

(* Problem 3 *)
let rec interleave l1 l2 =
  match l1 with
    | [] -> l2
    | (x::xs) -> x :: (interleave l2 xs);;

(* Problem 4 *)
let rec biggest l =
  match l with
    | [] -> 0
    | (x::xs) -> if x >= biggest xs then x else biggest xs;;

(******************
 * Tail Recursion *
 ******************)

(* Problem 5 *)
let rec even_count_tr l =
  let rec even_count_tr_acc l c =
    match l with
      | [] -> c
      | (x::xs) -> if x mod 2 = 0 then even_count_tr_acc xs (c+1)
                   else even_count_tr_acc xs c
  in even_count_tr_acc l 0;;

(* Problem 6 *)
let rec count_element l m =
  let rec count_element_acc l c =
    match l with
      | [] -> c
      | (x::xs) -> if x = m then count_element_acc xs (c+1)
                   else count_element_acc xs c
  in count_element_acc l 0;; 

(* Problem 7 *)
let rec sub_list l1 l2 = 
    match (l1,l2) with
      | (_, []) -> true
      | ([], _) -> false
      | ((x::xs), (y::ys)) -> if x=y then sub_list xs ys
                              else sub_list xs l2;;

(* Problem 8 *)
let rec concat s list =
  let rec concat_aux s l =
    (match l with [] -> s
      | (st::stl) -> concat_aux (s^sep^st) stl)
  in match l with [] -> ""
    | [x] -> x
    | (x::xs) -> concat_aux x xs

(**************************
 * Higher Order Functions *
 **************************)

(* Problem 9 *)
let even_count_fr_base = 0
let even_count_fr_step x rec_val = fun x -> fun acc -> if x mod 2 = 0 then 1+acc else acc

(* Problem 10 *)
let pair_sums_map_arg p = fun (x,y) -> x + y

(* Problem 11 *)
let even_count_tr_start = 0
let even_count_tr_step x rec_val = fun acc -> fun x -> if x mod 2 = 0 then 1+acc else acc

(* Problem 12 *)
let count_element_start = 0
let count_element_step m = fun acc -> fun x -> if x = m then 1+acc else acc

(* Problem 13 *)
let app_all_with fs b l = 
  List.map (fun f -> List.map (fun y -> f b y) l) fs

(* Problem 14 *)
let exists_between_start = false
let exists_between_step m n b x =
  b || (m <= x && x <= n)
