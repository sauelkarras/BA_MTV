From Coq Require Import
     Arith
     Bool
     PeanoNat.

Open Scope nat_scope.

(************************************************************)
(** Atomic predicates for class-balance checks             *)
(** Rocq only defines operations on counts and tolerance.  *)
(************************************************************)

Module ClassBalanceAtom.

  (* Absolute difference on nat *)
  Definition nat_abs (n m : nat) : nat :=
    if Nat.leb n m then m - n else n - m.

  (* Single-class tolerance check: |obs - exp| <= tol *)
  Definition within_tolerance (exp obs tol : nat) : bool :=
    Nat.leb (nat_abs exp obs) tol.

  (* Logical spec: absolute deviation is bounded by tol *)
  Definition within_toleranceP (exp obs tol : nat) : Prop :=
    nat_abs exp obs <= tol.

  Lemma within_tolerance_spec (exp obs tol : nat) :
    within_tolerance exp obs tol = true <-> within_toleranceP exp obs tol.
  Proof.
    unfold within_tolerance, within_toleranceP.
    (* Nat.leb_le: Nat.leb a b = true <-> a <= b *)
    now rewrite Nat.leb_le.
  Qed.

End ClassBalanceAtom.
