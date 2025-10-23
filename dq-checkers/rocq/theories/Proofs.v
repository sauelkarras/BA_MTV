From Coq Require Import ZArith List Bool Lia.
Import ListNotations.
From DQ Require Import Base Checker.
Module DQ := DQBase.

Lemma leb_true_le : forall x y, Z.leb x y = true -> x <= y.
Proof. intros x y H. apply Z.leb_le. now rewrite H. Qed.

Lemma check_rec_range_sound :
  forall r, check_rec_range r = true -> DQ.rec_range_ok r.
Proof.
  intros r H. unfold check_rec_range in H.
  repeat rewrite andb_true_iff in H.
  destruct H as [[H1 H2] H3].
  split.
  - split; [apply leb_true_le in H1; exact H1|].
    apply leb_true_le in H2; exact H2.
  - apply leb_true_le in H3; exact H3.
Qed.

Lemma check_ds_range_sound :
  forall ds, check_ds_range ds = true -> DQ.ds_range_ok ds.
Proof.
  unfold check_ds_range, forallb.
  induction ds as [|r rs IH]; simpl; intros H.
  - constructor.
  - destruct (check_rec_range r) eqn:E; try discriminate.
    constructor; [apply check_rec_range_sound; exact E|].
    apply IH. assumption.
Qed.
