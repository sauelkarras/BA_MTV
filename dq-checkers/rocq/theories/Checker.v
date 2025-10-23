From DQ Require Import Base RangeChecker PointwiseContradictions.


(* Re-export short names you’ll use from Haskell *)
Definition rec_ok_range    := check_rec_range.
Definition ds_ok_range     := check_ds_range.
Definition rec_ok_contr    := check_rec_contradiction.
Definition ds_ok_contr     := check_ds_contradiction.
Definition default_policy  := default.
