From Coq Require Import
     ZArith
     ZArith.Zbool
     String
     List
     Bool.

Import ListNotations.
Open Scope Z_scope.
Open Scope string_scope.

(************************************************************)
(** Atomic predicates for pointwise contradictions         *)
(** Rocq only defines per-value operations.                *)
(** Haskell will build row-wise rules and aggregation.     *)
(************************************************************)

Module ContrAtom.

  (**********************************************************)
  (** 1. Numeric comparisons on Z                          *)
  (**********************************************************)

  Definition num_lt  (x c : Z) : bool := Z.ltb x c.
  Definition num_le  (x c : Z) : bool := Z.leb x c.
  Definition num_gt  (x c : Z) : bool := Z.ltb c x.
  Definition num_ge  (x c : Z) : bool := Z.leb c x.
  Definition num_eq  (x c : Z) : bool := Z.eqb x c.
  Definition num_neq (x c : Z) : bool := negb (Z.eqb x c).

  (**********************************************************)
  (** 2. Numeric membership in a finite set                *)
  (**********************************************************)

  Fixpoint num_in (x : Z) (xs : list Z) : bool :=
    match xs with
    | [] => false
    | y :: ys => if Z.eqb x y then true else num_in x ys
    end.

  Definition num_not_in (x : Z) (xs : list Z) : bool :=
    negb (num_in x xs).

  (**********************************************************)
  (** 3. Categorical comparisons on strings                *)
  (**********************************************************)

  Definition cat_eq  (x label : string) : bool :=
    String.eqb x label.

  Definition cat_neq (x label : string) : bool :=
    negb (String.eqb x label).

  (**********************************************************)
  (** 4. Categorical membership in a finite set            *)
  (**********************************************************)

  Fixpoint cat_in (x : string) (xs : list string) : bool :=
    match xs with
    | [] => false
    | y :: ys => if String.eqb x y then true else cat_in x ys
    end.

  Definition cat_not_in (x : string) (xs : list string) : bool :=
    negb (cat_in x xs).

  (**********************************************************)
  (** 5. Logical specs and small lemmas                    *)
  (**********************************************************)

  Definition num_ltP (x c : Z) : Prop := x < c.
  Definition num_leP (x c : Z) : Prop := x <= c.

  Lemma num_lt_spec (x c : Z) :
    num_lt x c = true <-> num_ltP x c.
  Proof.
    unfold num_lt, num_ltP.
    apply Z.ltb_lt.
  Qed.

  Lemma num_le_spec (x c : Z) :
    num_le x c = true <-> num_leP x c.
  Proof.
    unfold num_le, num_leP.
    apply Z.leb_le.
  Qed.

End ContrAtom.
