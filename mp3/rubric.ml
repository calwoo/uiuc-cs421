(*
 * grader for mp3
 * This file will be preprocessed to generate the actual OCaml file.
 *)
let rubric_version = "1.0"
let rubric_title = "CS421 Fall 2012 MP5"

open Grader
open Test
open Mp3common
open Mp3eval

(*
 * use a timeout of 4 seconds
 *)

  let isEq i j = (i = j) (* Needs to be alpha equivalence *)

let mptest weight pair = compare isEq 4 weight pair

(**************************************************************************
 * You can add new test cases by adding new elements to the following lists
 * Format is:
 * TEST<X>ARG(<weight>, <function_name>, <arg1>, <arg2>, ..., <argX>)
 *
 * <X> is the number of argument that the function being tested takes.
 **************************************************************************)
open Mp3common

let freeVarsInExp_stu e = Mp3common.mergesort (Student.freeVarsInExp e)
let freeVarsInExp_sol e = Mp3common.mergesort (Solution.freeVarsInExp e)
let cps_exp_stu e k =
    let ec = Student.cps_exp e k
    in Mp3common.exp_cps_normalize ec (Solution.freeVarsInExp e)
let cps_exp_sol e k =
    let ec = Solution.cps_exp e k
    in Mp3common.exp_cps_normalize ec (Solution.freeVarsInExp e)

(* This list is for regular problems *)
let rubric =
[
  "import_list"^" "^"[(7,1);(4,2);(6,3)]", mptest 1 (ss_pair1 Solution.import_list Student.import_list ([(7,1);(4,2);(6,3)]));
  "pair_sums", mptest 1 (ss_pair0 Solution.pair_sums Student.pair_sums);
  "count_const_in_exp"^" "^"(BinOpAppExp (CommaOp, BinOpAppExp (CommaOp, ConstExp (FloatConst 7.3), ConstExp UnitConst), BinOpAppExp (ConsOp, BinOpAppExp (CommaOp, ConstExp (IntConst 4), ConstExp (StringConst \"a\")), BinOpAppExp (ConsOp, BinOpAppExp (CommaOp, ConstExp (IntConst 6), ConstExp (StringConst \"b\")), ConstExp NilConst))))", mptest 1 (ss_pair1 Solution.count_const_in_exp Student.count_const_in_exp ((BinOpAppExp (CommaOp, BinOpAppExp (CommaOp, ConstExp (FloatConst 7.3), ConstExp UnitConst), BinOpAppExp (ConsOp, BinOpAppExp (CommaOp, ConstExp (IntConst 4), ConstExp (StringConst "a")), BinOpAppExp (ConsOp, BinOpAppExp (CommaOp, ConstExp (IntConst 6), ConstExp (StringConst "b")), ConstExp NilConst))))))





                         ;
  "freeVarsInExp"^" "^"(VarExp \"x\")", mptest 1 (ss_pair1 Solution.freeVarsInExp Student.freeVarsInExp ((VarExp "x")));
  "freeVarsInExp_sol"^" "^"(IfExp(ConstExp (BoolConst true), VarExp \"x\", VarExp \"y\"))", mptest 1 (ss_pair1 freeVarsInExp_sol freeVarsInExp_stu ((IfExp(ConstExp (BoolConst true), VarExp "x", VarExp "y"))));
  "freeVarsInExp"^" "^"(FunExp(\"x\", VarExp \"x\"))", mptest 1 (ss_pair1 Solution.freeVarsInExp Student.freeVarsInExp ((FunExp("x", VarExp "x"))));
  "freeVarsInExp_sol"^" "^"(LetInExp(\"x\", VarExp \"y\", VarExp \"x\"))", mptest 1 (ss_pair1 freeVarsInExp_sol freeVarsInExp_stu ((LetInExp("x", VarExp "y", VarExp "x"))));
  "freeVarsInExp_sol"^" "^"(LetRecInExp(\"f\",\"x\",AppExp(VarExp \"f\",VarExp \"x\"), AppExp(VarExp \"f\",VarExp \"y\")))", mptest 1 (ss_pair1 freeVarsInExp_sol freeVarsInExp_stu ((LetRecInExp("f","x",AppExp(VarExp "f",VarExp "x"), AppExp(VarExp "f",VarExp "y")))))

                                                               ;
  "cps_exp"^" "^"(VarExp \"x\")"^" "^"(ContVarCPS Kvar)", mptest 1 (ss_pair2 Solution.cps_exp Student.cps_exp ((VarExp "x")) ((ContVarCPS Kvar)));
  "cps_exp_sol"^" "^"(IfExp (VarExp \"b\", ConstExp (IntConst 2), ConstExp (IntConst 5)))"^" "^"(ContVarCPS Kvar)", mptest 1 (ss_pair2 cps_exp_sol cps_exp_stu ((IfExp (VarExp "b", ConstExp (IntConst 2), ConstExp (IntConst 5)))) ((ContVarCPS Kvar)))

                                                   ;
  "cps_exp_sol"^" "^"(AppExp (VarExp \"f\", VarExp \"x\"))"^" "^"(ContVarCPS Kvar)", mptest 1 (ss_pair2 cps_exp_sol cps_exp_stu ((AppExp (VarExp "f", VarExp "x"))) ((ContVarCPS Kvar)))
                                           ;
  "cps_exp_sol"^" "^"(BinOpAppExp (IntPlusOp, ConstExp(IntConst 5), ConstExp(IntConst 1)))"^" "^"(ContVarCPS Kvar)", mptest 1 (ss_pair2 cps_exp_sol cps_exp_stu ((BinOpAppExp (IntPlusOp, ConstExp(IntConst 5), ConstExp(IntConst 1)))) ((ContVarCPS Kvar)))

                                            ;
  "cps_exp_sol"^" "^"(MonOpAppExp (HdOp, ConstExp NilConst))"^" "^"(ContVarCPS Kvar)", mptest 1 (ss_pair2 cps_exp_sol cps_exp_stu ((MonOpAppExp (HdOp, ConstExp NilConst))) ((ContVarCPS Kvar)))
                                                   ;
  "cps_exp_sol"^" "^"(FunExp (\"x\", VarExp \"x\"))"^" "^"(ContVarCPS Kvar)", mptest 1 (ss_pair2 cps_exp_sol cps_exp_stu ((FunExp ("x", VarExp "x"))) ((ContVarCPS Kvar)))
                                                   ;
  "cps_exp_sol"^" "^"(LetInExp (\"x\", ConstExp(IntConst 2), VarExp \"x\"))"^" "^"(ContVarCPS Kvar)", mptest 1 (ss_pair2 cps_exp_sol cps_exp_stu ((LetInExp ("x", ConstExp(IntConst 2), VarExp "x"))) ((ContVarCPS Kvar)))


]


let extra_rubric = [
  "cps_exp_sol"^" "^"(LetRecInExp (\"f\", \"x\",VarExp \"x\", ConstExp (IntConst 4)))"^" "^"(ContVarCPS Kvar)", mptest 1 (ss_pair2 cps_exp_sol cps_exp_stu ((LetRecInExp ("f", "x",VarExp "x", ConstExp (IntConst 4)))) ((ContVarCPS Kvar)))


]

let _ = Main.main rubric extra_rubric rubric_title rubric_version
