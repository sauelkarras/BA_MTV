From Coq Require Import String ZArith List Extraction.
From Coq Require Import ExtrHaskellBasic ExtrHaskellZInteger ExtrHaskellString ExtrHaskellNatInteger.
Import ListNotations.
Local Open Scope string_scope.
Local Open Scope Z_scope.

(* Your project modules *)
From DQ Require Import
     RangeChecker
     PointwiseContradictions
     ClassBalance.

(* We extract to Haskell *)
Extraction Language Haskell.
Set Extraction Optimize.

(* Extract exactly what the Haskell Main.hs expects. *)
Extraction "out/Generated.hs"
  (* range checker primitives *)
  Range lo hi in_range

  (* pointwise-contradiction primitives: numeric + categorical atoms *)
  ContrAtom.num_lt
  ContrAtom.num_le
  ContrAtom.num_gt
  ContrAtom.num_ge
  ContrAtom.num_eq
  ContrAtom.num_neq
  ContrAtom.cat_eq.
