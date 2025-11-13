From Coq Require Import String ZArith List Extraction.
From Coq Require Import ExtrHaskellBasic ExtrHaskellZInteger ExtrHaskellString.
From Coq Require Import ExtrHaskellBasic ExtrHaskellZInteger ExtrHaskellString ExtrHaskellNatInteger.
Import ListNotations.
Local Open Scope string_scope.
Local Open Scope Z_scope.

From DQ Require Import Base RangeChecker PointwiseContradictions Checker ClassBalance.

Extraction Language Haskell.
Set Extraction Optimize.

(* (optional) tiny smoke sample *)
Definition tiny_ds : Dataset :=
  [ {| age := 23; balance := 1000; duration := 12;
       a1 := "A12"; a3 := "A31"; a6 := "A61";
       a9 := "A93"; a10 := "A101"; a20 := "A202"; y := "1" |} ].

(* Extract exactly what Main.hs references *)
Extraction "out/Generated.hs"
  Row age balance duration a1 a3 a6 a9 a10 a20 y Dataset

  Range lo hi Policy ageRange amountRange durationRange
  in_range rec_ok_range ds_ok_range default_policy
  rec_ok_contr ds_ok_contr

  CB.sex_balance_ok_default CB.foreign_balance_ok_default CB.all_balance_ok_default
  CB.sex_diag_default CB.foreign_diag_default
  CB.Diag CB.n_total CB.x_pos CB.passed
  CB.standard_deviations_boundary
  tiny_ds.
