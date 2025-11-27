From Coq Require Import
     Arith
     Bool.

Open Scope nat_scope.

(************************************************************)
(** Atomic predicates for class-balance checks             *)
(** Rocq only defines operations on counts and tolerance.  *)
(************************************************************)

Module ClassBalanceAtom.

  Definition nat_abs (n m : nat) : nat :=
    if Nat.leb n m then m - n else n - m.

  (* Single-class tolerance check: |obs - exp| <= tol *)
  Definition within_tolerance (exp obs tol : nat) : bool :=
    Nat.leb (nat_abs exp obs) tol.

  (* Logical spec skeleton, for later proof *)
  Definition within_toleranceP (exp obs tol : nat) : Prop :=
    (Nat.leb (nat_abs exp obs) tol = true).

  Lemma within_tolerance_spec (exp obs tol : nat) :
    within_tolerance exp obs tol = true <-> within_toleranceP exp obs tol.
  Proof.
    (* To be refined later; currently just a placeholder. *)
  Admitted.

End ClassBalanceAtom.
