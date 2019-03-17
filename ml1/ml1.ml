(* CS421 - Fall 2016
 * ML1
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You will want to change how a number of these start.
 *)

open Mp1common


let random = 179  (* You want to change this *)

let title = "ML 1 -- Basic OCaml";; (* You want to change this *)

let greetings = "Hi there.";; (* You want to change this *)

let salute = "Greetings, my friend!";; (* You want to change this *)

let a = 17;; (* You want to change this *)

let pi = 3.14159;; (* You want to change this *)

let e = 2.71828;; (* You want to change this *)

let quarter = 0.25;; (* You want to change this *)

let x = 32.7;; (* You want to change this *)

let s = "Hi there";; (* You want to change this *)

let myFirstFun n = 4 * (n + 3);;

let firstFun n = 2 * n + 5;;

let square n = n * n;;

let times_13 n = 13 * n;;

let add_a n = n + a;;

let circumference r = 2. *. pi *. r;;

let divide_e_by x = e /. x;;

let plus_quarter_times_3 y = 3.0 *. (y +. quarter);;

let square_plus_x y = x +. (y *. y);;

let double_minus_one x = (2 * x) - 1;;

let diff_square_9 m = if (m >= -3) && (m <= 3) then m * m - 9 else 9 - m * m;;

let salutations name = 
    if name = "Elsa" 
    then (print_string "Halt! Who goes there!\n")
    else (print_string "Hail, "; print_string name; print_string ". We warmly welcome you!\n");;

let hail name =
    if name = "Elsa"
    then (print_string "Wayell, hah theya, Ayelsa!")
    else (print_string "Dear, "; print_string name; print_string ". I wish you the best in CS421.\n");;

let abs_diff x y =
    let diff = x -. y 
    in (if (diff <= 0.)
        then -1. *. diff
        else diff);;

let greet name =
    if name = "Elsa"
    then (print_string "Hey Elsa, cool man!")
    else (print_string "Hello, "; print_string name; print_string ". I hope you enjoy CS421.\n");;

let make_bigger x = 
    if x < 0.0
    then x *. (-1.0)
    else (if x <= 1.0
          then x +. 0.5
          else x *. x);;

let dist_double s n = print_string s; print_string ", I guess it's double or nothing!\n"; 2 * n;;

let has_smallest_square m n = 
    if m*m > n*n then n else (if m*m < n*m then m else (if m < n then m else n));;

let has_smallest_abs m n = if m*m > n*n then n else (if m*m < n*n then m else n);;

let sign n = if n > 0 then 1 else (if n < 0 then -1 else 0);;

