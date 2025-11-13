From Coq Require Import List.
Import ListNotations.

From DQ Require Import Base RangeChecker PointwiseContradictions.

(* ---- Exports used from Haskell ---- *)

(* Range checker re-exports *)
Definition rec_ok_range   := check_rec_range.
Definition ds_ok_range    := check_ds_range.
Definition default_policy := RangeChecker.default.

(* Contradiction checker wrappers (define locally; no external dependency) *)
Definition rec_ok_contr (r : Row) : bool :=
  rule_contr default_contr_cfg r.

Definition ds_ok_contr (ds : Dataset) : bool :=
  forallb rec_ok_contr ds.
