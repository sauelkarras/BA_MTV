From Coq Require Import
     ZArith
     String
     List
     Bool
     Arith.PeanoNat.

Import ListNotations.

Open Scope Z_scope.
Open Scope string_scope.

(************************************************************)
(** 1. Range kernel                                      *)
(************************************************************)

Module RangeKernel.

  (** [in_range lo hi x] returns [true] iff:
      - [lo = None] or [lo = Some l] with [l <= x], and
      - [hi = None] or [hi = Some h] with [x <= h]. *)

  Definition in_range (lo hi : option Z) (x : Z) : bool :=
    let lower_ok :=
      match lo with
      | None => true
      | Some l => Z.leb l x
      end in
    let upper_ok :=
      match hi with
      | None => true
      | Some h => Z.leb x h
      end in
    andb lower_ok upper_ok.

End RangeKernel.

(************************************************************)
(** 2. Primitive numeric / categorical comparisons        *)
(**    (for contradiction-style checks)                   *)
(************************************************************)

Module ContradictionKernel.

  (** Numeric comparisons on [Z]. *)

  Definition num_lt (x c : Z) : bool := Z.ltb x c.
  Definition num_le (x c : Z) : bool := Z.leb x c.
  Definition num_gt (x c : Z) : bool := Z.ltb c x.
  Definition num_ge (x c : Z) : bool := Z.leb c x.
  Definition num_eq (x c : Z) : bool := Z.eqb x c.
  Definition num_neq (x c : Z) : bool := negb (Z.eqb x c).

  Fixpoint num_in (x : Z) (xs : list Z) : bool :=
    match xs with
    | [] => false
    | y :: ys => if Z.eqb x y then true else num_in x ys
    end.

  Definition num_not_in (x : Z) (xs : list Z) : bool :=
    negb (num_in x xs).

  (** Categorical comparisons on [string]. *)

  Definition cat_eq (x label : string) : bool :=
    String.eqb x label.

  Definition cat_neq (x label : string) : bool :=
    negb (String.eqb x label).

  Fixpoint cat_in (x : string) (xs : list string) : bool :=
    match xs with
    | [] => false
    | y :: ys => if String.eqb x y then true else cat_in x ys
    end.

  Definition cat_not_in (x : string) (xs : list string) : bool :=
    negb (cat_in x xs).

  (** Haskell will combine these building blocks with [andb], [orb]
      to implement the full "premise & forbidden" logic row-wise. *)

End ContradictionKernel.

(************************************************************)
(** 3. Class-balance kernel                               *)
(************************************************************)

Module ClassBalanceKernel.

  (** Absolute difference on [nat]. *)

  Definition nat_abs (n m : nat) : nat :=
    if Nat.leb n m then m - n else n - m.

  (** [within_tolerance_list counts expected tol] checks that
      for each class i, the absolute deviation in counts is
      bounded by [tol].

      More precisely:
        forall i, |counts[i] - expected[i]| <= tol.

      [false] is returned if the two lists have different lengths. *)

  Fixpoint within_tolerance_list
           (counts expected : list nat) (tol : nat) : bool :=
    match counts, expected with
    | [], [] => true
    | c :: cs, e :: es =>
        let diff := nat_abs c e in
        andb (Nat.leb diff tol)
             (within_tolerance_list cs es tol)
    | _, _ => false
    end.

End ClassBalanceKernel.
