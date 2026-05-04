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
  (** 5. Logical specs and lemmas                           *)
  (**    For every boolean predicate, define Prop + spec.   *)
  (**********************************************************)

  (* -------- 1) Numeric comparisons on Z -------- *)

  Definition num_ltP  (x c : Z) : Prop := x < c.
  Definition num_leP  (x c : Z) : Prop := x <= c.
  Definition num_gtP  (x c : Z) : Prop := c < x.
  Definition num_geP  (x c : Z) : Prop := c <= x.
  Definition num_eqP  (x c : Z) : Prop := x = c.
  Definition num_neqP (x c : Z) : Prop := x <> c.

  Lemma num_lt_spec (x c : Z) :
    num_lt x c = true <-> num_ltP x c.
  Proof.
    unfold num_lt, num_ltP. apply Z.ltb_lt.
  Qed.

  Lemma num_le_spec (x c : Z) :
    num_le x c = true <-> num_leP x c.
  Proof.
    unfold num_le, num_leP. apply Z.leb_le.
  Qed.

Lemma num_gt_spec (x c : Z) :
  num_gt x c = true <-> num_gtP x c.
Proof.
  unfold num_gt, num_gtP.
  (* num_gt x c = Z.ltb c x *)
  apply Z.ltb_lt.
Qed.

Lemma num_ge_spec (x c : Z) :
  num_ge x c = true <-> num_geP x c.
Proof.
  unfold num_ge, num_geP.
  (* num_ge x c = Z.leb c x *)
  apply Z.leb_le.
Qed.

  Lemma num_eq_spec (x c : Z) :
    num_eq x c = true <-> num_eqP x c.
  Proof.
    unfold num_eq, num_eqP. apply Z.eqb_eq.
  Qed.

  Lemma num_neq_spec (x c : Z) :
    num_neq x c = true <-> num_neqP x c.
  Proof.
    unfold num_neq, num_neqP.
    rewrite Bool.negb_true_iff.
    rewrite Z.eqb_neq.
    tauto.
  Qed.

  (* -------- 2) Numeric membership in a finite set -------- *)

  Definition num_inP (x : Z) (xs : list Z) : Prop := In x xs.
  Definition num_not_inP (x : Z) (xs : list Z) : Prop := ~ In x xs.

  Lemma num_in_spec (x : Z) (xs : list Z) :
    num_in x xs = true <-> num_inP x xs.
  Proof.
    unfold num_inP.
    induction xs as [|y ys IH]; simpl.
    - split; intro H.
      + discriminate H.
      + contradiction.
    - destruct (Z.eqb x y) eqn:Heq.
      + apply Z.eqb_eq in Heq; subst.
        split; intro H.
        * now left.
        * reflexivity.
      + split.
        * intro H. right. apply (proj1 IH) in H. exact H.
        * intro H. destruct H as [Hxy | HIn].
          { subst. rewrite Z.eqb_refl in Heq. discriminate Heq. }
          apply (proj2 IH) in HIn. exact HIn.
  Qed.

Lemma num_not_in_spec (x : Z) (xs : list Z) :
  num_not_in x xs = true <-> num_not_inP x xs.
Proof.
  unfold num_not_in, num_not_inP.
  (* num_not_in x xs = negb (num_in x xs) *)
  rewrite Bool.negb_true_iff.
  (* goal: num_in x xs = false <-> ~ In x xs *)
  rewrite <- Bool.not_true_iff_false.
  (* goal: ~(num_in x xs = true) <-> ~ In x xs *)
  rewrite num_in_spec.
  tauto.
Qed.

  (* -------- 3) Categorical comparisons on strings -------- *)

  Definition cat_eqP  (x label : string) : Prop := x = label.
  Definition cat_neqP (x label : string) : Prop := x <> label.

  Lemma cat_eq_spec (x label : string) :
    cat_eq x label = true <-> cat_eqP x label.
  Proof.
    unfold cat_eq, cat_eqP. apply String.eqb_eq.
  Qed.

  Lemma cat_neq_spec (x label : string) :
    cat_neq x label = true <-> cat_neqP x label.
  Proof.
    unfold cat_neq, cat_neqP.
    rewrite Bool.negb_true_iff.
    rewrite String.eqb_neq.
    tauto.
  Qed.

  (* -------- 4) Categorical membership in a finite set -------- *)

  Definition cat_inP (x : string) (xs : list string) : Prop := In x xs.
  Definition cat_not_inP (x : string) (xs : list string) : Prop := ~ In x xs.

  Lemma cat_in_spec (x : string) (xs : list string) :
    cat_in x xs = true <-> cat_inP x xs.
  Proof.
    unfold cat_inP.
    induction xs as [|y ys IH]; simpl.
    - split; intro H.
      + discriminate H.
      + contradiction.
    - destruct (String.eqb x y) eqn:Heq.
      + apply String.eqb_eq in Heq; subst.
        split; intro H.
        * now left.
        * reflexivity.
      + split.
        * intro H. right. apply (proj1 IH) in H. exact H.
        * intro H. destruct H as [Hxy | HIn].
          { subst. rewrite String.eqb_refl in Heq. discriminate Heq. }
          apply (proj2 IH) in HIn. exact HIn.
  Qed.

  Lemma cat_not_in_spec (x : string) (xs : list string) :
    cat_not_in x xs = true <-> cat_not_inP x xs.
  Proof.
    unfold cat_not_in, cat_not_inP.
    rewrite Bool.negb_true_iff.
    rewrite <- Bool.not_true_iff_false.
    rewrite cat_in_spec.
    tauto.
  Qed.  