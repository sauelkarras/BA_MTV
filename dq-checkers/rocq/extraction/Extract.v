From Coq Require Import
     String
     ZArith
     List
     Extraction.
From Coq Require Import
     ExtrHaskellBasic
     ExtrHaskellZInteger
     ExtrHaskellString
     ExtrHaskellNatInteger.

Import ListNotations.
Local Open Scope string_scope.
Local Open Scope Z_scope.

From DQ Require Import
     RangeChecker
     PointwiseContradictions
     ClassBalance.

(* We extract only the atomic, scalar operations that Haskell will use. *)

Extraction Language Haskell.
Set Extraction Optimize.

Extraction "out/Generated.hs"
  (* Range checker: closed interval on Z *)
  Range
  in_range

  (* Pointwise contradiction atoms: numeric + categorical *)
  ContrAtom.num_lt
  ContrAtom.num_le
  ContrAtom.num_gt
  ContrAtom.num_ge
  ContrAtom.num_eq
  ContrAtom.num_neq
  ContrAtom.num_in
  ContrAtom.num_not_in
  ContrAtom.cat_eq
  ContrAtom.cat_neq
  ContrAtom.cat_in
  ContrAtom.cat_not_in

  (* Class-balance: per-class tolerance on counts *)
  ClassBalanceAtom.nat_abs
  ClassBalanceAtom.within_tolerance.
