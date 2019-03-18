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
let rec count_const_in_exp exp =  raise (Failure "Not implemented yet.")

(* Problem 4 *)
let rec freeVarsInExp exp = raise (Failure "Not implemented yet")

(* Problem 5 *)
let rec cps_exp e k =  raise (Failure "Not implemented yet.")

