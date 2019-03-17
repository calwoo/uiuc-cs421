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
let rec pair_sums l = raise(Failure "Function not implemented yet.")

(* Problem 3 *)
let rec interleave l1 l2 = raise(Failure "Function not implemented yet.")

(* Problem 4 *)
let rec biggest l = raise(Failure "Function not implemented yet.")

(******************
 * Tail Recursion *
 ******************)

(* Problem 5 *)
let rec even_count_tr l = raise(Failure "Function not implemented yet.")

(* Problem 6 *)
let rec count_element l m = raise(Failure "Function not implemented yet.")

(* Problem 7 *)
let rec sub_list l1 l2 = raise(Failure "Function not implemented yet.")

(* Problem 8 *)
let rec concat s list = raise(Failure "Function not implemented yet.")

(**************************
 * Higher Order Functions *
 **************************)

(* Problem 9 *)
let even_count_fr_base = 1337 (* You may need to change this *)
let even_count_fr_step x rec_val = raise(Failure "Function not implemented yet.")

(* Problem 10 *)
let pair_sums_map_arg p = raise(Failure "Function not implemented yet.")

(* Problem 11 *)
let even_count_tr_start = 1337 (* You may need to change this *)
let even_count_tr_step x rec_val = raise(Failure "Function not implemented yet.")

(* Problem 12 *)
let count_element_start = 1337 (* You may need to change this *)
let count_element_step m = raise(Failure "Function not implemented yet.")

(* Problem 13 *)
let app_all_with fs b l = raise(Failure "Function not implemented yet.")

(* Problem 14 *)
let exists_between_start = true (* You may need to change this *)
let exists_between_step m n b x = raise(Failure "Function not implemented yet.")
