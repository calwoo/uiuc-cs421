open Mp2common

(*****************************)
(***** PROBLEMS FROM ML2 *****)
(*****************************)
(* Problem 1 *)
let even_count_fr_base = 0
let even_count_fr_step x rec_val = if x mod 2 = 0 then rec_val + 1 else rec_val

(* Problem 2 *)
let pair_sums_map_arg p = match p with (x,y) -> x + y

(* Problem 3 *)
let even_count_tr_start = 0
let even_count_tr_step acc_value x = if x mod 2 = 0 then acc_value + 1 else acc_value

(* Problem 4 *)
let count_element_start = 0
let count_element_step m acc_value x = if x = m then acc_value + 1 else acc_value

(* Problem 5 *)
let app_all_with fs b l = List.map (fun f -> List.map (fun y -> f b y) l) fs

(* Problem 6 *)
let exists_between_start = false
let exists_between_step m n acc_value x = acc_value || (m <= x && x <= n)

(*****************************)
(****** PROBLEMS FOR MP2 *****)
(*****************************)
(***** Problem 7: Warmup (0 Points)  ******)
let consk (x, l) k = k (x :: l);;
let concatk (s1, s2) k = k (s1^s2);;
let string_of_intk n k = k (string_of_int n);;
let truncatek n k = k (truncate n);;

(***** Problem 8: Basic CPS *****)
let diff_flipk p k = addk (1,-p) (fun z ->
                        mulk (z,p) (fun x ->
                            mulk (2,x) k));;

(***** Problem 9: Basic CPS *****)
let quadk (a, b, c) k = 
    mulk (a,a) (fun x ->
        mulk (2,x) (fun y ->
            mulk (4,b) (fun z ->
                addk (y,z) (fun w ->
                    addk (w,c) k))));;

(***** Problem 10: Basic CPS *****)
let three_freezek (s, p) k =
    concatk (s,p) (fun x ->
        concatk (x,x) (fun y ->
            concatk (y,x) k));;

(***** Problem 11: Basic CPS *****)
let shiftk (s, q) k = 
    float_addk (q,1.57) (fun x ->
    float_mulk (x,x) (fun y ->
        truncatek y (fun z ->
            string_of_intk z (fun w ->
                concatk (s,w) (fun v ->
                    concatk (v,s) k)))));;

(***** Problem 12a: Recursion & CPS ******)
let rec list_prod l = 
    match l with
        | [] -> 1
        | x::xs -> x * list_prod xs;;

(***** Problem 12b: Recursion & CPS ******)
let rec list_prodk l k = 
    match l with
        | [] -> k 1
        | x::xs -> list_prodk xs (fun z -> mulk (x,z) k);;

(***** Problem 13a: Recursion & CPS *****)
let rec all_positive l  = 
    match l with
    | [] -> true
    | x::xs -> if x > 0 then all_positive xs else false;;

(***** Problem 13b: Recursion & CPS *****)
let rec all_positivek l k =
    match l with
    | [] -> k true
    | x::xs -> if x > 0 then all_positivek xs k else k false;;

(***** Problem 14a: Recursion & CPS *****)
let rec even_count l = 
    match l with
    | [] -> 0
    | x::xs -> if x mod 2 = 0
               then 1 + even_count xs
               else even_count xs;;

(***** Problem 14b: Recursion & CPS *****)
let rec even_countk l k =
    match l with
    | [] -> k 0
    | x::xs -> 
        even_countk xs (fun a ->
            modk (x,2) (fun b ->
            eqk (b,0) (fun c ->
                if c then addk (1,a) k
                     else k a)));;


(********** EXTRA CREDIT **********)

(* Extra Credit, Problem 16a *)
let rec list_compose fs =  raise(Failure "Function not implemented yet.")

(* Extra Credit, Problem 16b *)
let rec list_composek fsk k = raise(Failure "Function not implemented yet.")
