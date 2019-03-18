(* File: mp3.ml *)

open Mp3common

(* Problem 1 *)
let rec import_list lst =
    match lst with
    | [] -> ConstExp NilConst
    | (a,b)::xs -> 
        let ae = ConstExp (IntConst a) in
        let be = ConstExp (IntConst b)
        in BinOpAppExp (ConsOp,
            BinOpAppExp (CommaOp, ae, be), import_list xs);;

(* Problem 2 *)
let pair_sums = LetRecInExp("pair_sums", "lst", 
    IfExp (BinOpAppExp (EqOp, VarExp "lst", ConstExp NilConst),
           ConstExp NilConst,
           LetInExp ("x", MonOpAppExp (HdOp, VarExp "lst"),
                BinOpAppExp (ConsOp,
                             BinOpAppExp (IntPlusOp, MonOpAppExp (FstOp, VarExp "x"),
                                                     MonOpAppExp (SndOp, VarExp "x")),
                             AppExp (VarExp "pair_sums", MonOpAppExp (TlOp, VarExp "lst"))))),
    AppExp (VarExp "pair_sums", import_list [(7,1);(4,2);(6,3)]));;

(* Problem 3 *)
let rec count_const_in_exp exp =
    match exp with
    | VarExp _ -> 0
    | ConstExp _ -> 1
    | MonOpAppExp (_,e) -> count_const_in_exp e
    | BinOpAppExp (_,e1,e2) -> 
        count_const_in_exp e1 + count_const_in_exp e2
    | IfExp (e1,e2,e3) ->
        count_const_in_exp e1 + count_const_in_exp e2 +
        count_const_in_exp e3
    | AppExp (e1,e2) -> 
        count_const_in_exp e1 + count_const_in_exp e2
    | FunExp (_,e) -> count_const_in_exp e
    | LetInExp (_,e1,e2) ->
        count_const_in_exp e1 + count_const_in_exp e2
    | LetRecInExp (_,_,e1,e2) ->
        count_const_in_exp e1 + count_const_in_exp e2;;

(* Problem 4 *)
let rec freeVarsInExp exp =
    match exp with
    | VarExp s -> [s]
    | ConstExp _ -> []
    | MonOpAppExp (_,e) -> freeVarsInExp e
    | BinOpAppExp (_,e1,e2) -> 
        (freeVarsInExp e1)@(freeVarsInExp e2)
    | IfExp (e1,e2,e3) ->
        (freeVarsInExp e1)@(freeVarsInExp e2)@(freeVarsInExp e3)
    | AppExp (e1,e2) ->
        (freeVarsInExp e1)@(freeVarsInExp e2)
    | FunExp (s,e) -> 
        List.filter (fun x -> x <> s) (freeVarsInExp e)
    | LetInExp (s,e1,e2) ->
        (freeVarsInExp e1)@(List.filter 
            (fun x -> x <> s) (freeVarsInExp e2))
    | LetRecInExp (f,s,e1,e2) ->
        (List.filter (fun x -> (x <> f)&&(x <> s)) (freeVarsInExp e1))@(List.filter (fun x -> x <> f) (freeVarsInExp e2));;


(* Problem 5 *)
let rec cps_exp e k =  raise (Failure "Not implemented yet.")

