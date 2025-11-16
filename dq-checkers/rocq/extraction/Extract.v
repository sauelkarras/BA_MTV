(* extraction/Extract.v *)

From Coq Require Import String ZArith List Extraction.
From Coq Require Import ExtrHaskellBasic ExtrHaskellZInteger ExtrHaskellString.
Import ListNotations.
Local Open Scope string_scope.

From DQ Require Import Base RangeChecker PointwiseContradictions ClassBalance Checker.

Extraction Language Haskell.
Set Extraction Optimize.

(* Tiny sample for a quick end-to-end check *)
Definition tiny_ds : Dataset :=
  [{| age := 23; balance := 1000; duration := 12; y := "no";  education := "unknown" |};
   {| age := 45; balance :=    0; duration := 45; y := "yes"; education := "secondary" |};
   {| age := 17; balance :=  200; duration := 20; y := "no";  education := "primary"  |}].

Definition tiny_run_ranges : bool := ds_ok_range default_policy tiny_ds.
Definition tiny_run_contr  : bool := ds_ok_contr tiny_ds.
Definition tiny_run_class  : bool := ds_ok_class tiny_ds.

(* Produce ONE file: out/Generated.hs *)
Extraction "out/Generated.hs"
  Row age balance duration y education Dataset
  Range lo hi Policy ageR balR
  in_range rec_ok_range ds_ok_range default_policy
  rec_ok_contr ds_ok_contr
  ds_ok_class edu_counts
  tiny_ds tiny_run_ranges tiny_run_contr tiny_run_class.

