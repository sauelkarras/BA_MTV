From Coq Require Import
     ZArith
     Bool.

Open Scope Z_scope.

(* Closed interval on Z *)
Record Range := { lo : Z; hi : Z }.

(* Scalar checker: closed interval [lo, hi] *)
Definition in_range (rg : Range) (z : Z) : bool :=
  Z.leb (lo rg) z && Z.leb z (hi rg).

(* Logical specification of the same predicate *)
Definition in_rangeP (rg : Range) (z : Z) : Prop :=
  lo rg <= z /\ z <= hi rg.

Lemma in_range_spec (rg : Range) (z : Z) :
  in_range rg z = true <-> in_rangeP rg z.
Proof.
  (* to be proved later *)
Admitted.
