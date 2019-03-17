(* CS421 - Fall 2016
 * MP1
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You will want to change how a number of these start.
 *)


(*Problem 1*)
let closer_to_origin p1 p2 =
    match (p1, p2) with ((x1,y1),(x2,y2)) ->
    let (d1, d2) = (x1*.x1+.y1*.y1, x2*.x2+.y2*.y2) in
    if d1 > d2 then 1 else if d1 < d2 then -1 else 0;;

(*Problem 2*)
let rec ackermann m n =
    match (m,n) 
    with (0,_) -> n + 1
      | (_,0) -> ackermann (m-1) 1
      | _ -> ackermann (m-1) (ackermann m (n-1));;

(*Problem 3*)
let rec collatz n = 
  let rec collatz_acc n count =
    if n <= 1 then count
    else if n mod 2 = 0 then collatz_acc (n/2) (count+1)
    else collatz_acc (3*n+1) (count+1)
  in collatz_acc n 0;;

(*Problem 4*)
let rec delannoy (m, n) = 
  if m <= 0 then 1
  else if n <= 0 then 1
  else delannoy (m-1,n) + delannoy (m, n-1) + delannoy (m-1,n-1);;

(*Problem 5*)
let two_funs fns ins = 
  match fns with (f,g)
    -> match ins with (x,y)
    -> (f x, g y);;

(*Problem 6*)
let rec product l =
  match l with
    | [] -> 1.0
    | (x::xs) -> x *. product xs;;

(*Problem 7*)
let rec double_all l =
  match l with
    | [] -> []
    | (x::xs) -> (2.*.x)::(double_all xs);;

(*Problem 8*)
let rec upto n =
  let rec upto_sofar m l =
    if m < 0 then l else upto_sofar (m-1) (m :: l)
  in upto_sofar n [];;

(*Problem 9*)
let rec upuntil f =
  let rec upuntil_sofar f n =
    if f n || n >= 100 then []
    else n :: (upuntil_sofar f (n+1))
  in upuntil_sofar f 0;;

(*Problem 10*)
let rec pair_with_all x l = 
  match l with
    | [] -> []
    | (y::ys) -> (x,y)::(pair_with_all x ys);;

(*Problem 11*)
let rec cross l1 l2 = 
  match l1 with
    | [] -> []
    | (x::xs) -> (pair_with_all x l2)@(cross xs l2);;

(*Problem 12*)
let rec insert_by comp x l =
  match l with
    | [] -> [x]
    | (y::ys) -> if comp x y < 0 
        then x::l else y::(insert_by comp x ys);;


(*Extra Credit - Problem 13*)
let rec collect_adjacent l = 
  match l with
    | [] -> []
    | (k,v)::xs ->
        let others = collect_adjacent xs
        in (match others with
            | [] -> [(k,[v])]
            | (j,vs)::ot -> if k=j
                            then (k,v::vs)::ot
                            else (k,[v])::others);;