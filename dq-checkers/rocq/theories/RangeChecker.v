From Coq Require Import ZArith List Bool.
Import ListNotations.
Open Scope Z_scope.

From DQ Require Import Base.


(* A closed interval on Z *)
Record Range := { lo : Z; hi : Z }.

(* Policy collecting ranges we care about *)
Record Policy := {
  ageR : Range;
  balR : Range
}.

(* Default policy *)
Definition default : Policy :=
  {| ageR := {| lo := 18; hi := 85 |};
     balR := {| lo := -3000; hi := 1000000 |} |}.

(* Boolean def von in_range *)
Definition in_range (rg : Range) (z : Z) : bool :=
  Z.leb (lo rg) z && Z.leb z (hi rg).

(* Row-level & dataset-level boolean checkers/ anwendung von in_range auf datenreihe *)
Definition check_rec_range (p : Policy) (r : Row) : bool :=
  in_range (ageR p) (age r) && in_range (balR p) (balance r).

(* anwendung auf ganzes datenset/liste*)
Definition check_ds_range (p : Policy) (ds : Dataset) : bool :=
  forallb (check_rec_range p) ds.


(*prooves:*)
(* ------- Specs (math view) ------- *)
Definition in_rangeP (rg : Range) (z : Z) : Prop :=
  lo rg <= z /\ z <= hi rg.

Definition rec_ok (p : Policy) (r : Row) : Prop :=
  in_rangeP (ageR p) (age r) /\ in_rangeP (balR p) (balance r).

Definition ds_ok (p : Policy) (ds : Dataset) : Prop :=
  Forall (rec_ok p) ds.

(* --- Scalar: in_range --- *)
Lemma in_range_spec (rg : Range) (z : Z) :
  in_range rg z = true <-> in_rangeP rg z.
Proof.
  unfold in_range, in_rangeP.
  rewrite Bool.andb_true_iff.
  split.
  - intros [Hlo Hhi].
    split; [apply Z.leb_le in Hlo | apply Z.leb_le in Hhi]; assumption.
  - intros [Hlo Hhi].
    split; [apply Z.leb_le; exact Hlo | apply Z.leb_le; exact Hhi].
Qed.

(* --- Record: check_rec_range --- *)
Lemma check_rec_range_spec (p : Policy) (r : Row) :
  check_rec_range p r = true <-> rec_ok p r.
Proof.
  unfold check_rec_range, rec_ok.
  split.
  - intro H.
    apply Bool.andb_true_iff in H as [Ha Hb].
    apply in_range_spec in Ha.
    apply in_range_spec in Hb.
    split; assumption.
  - intros [Ha Hb].
    apply Bool.andb_true_iff.
    split; [apply in_range_spec | apply in_range_spec]; assumption.
Qed.

(* --- Dataset: check_ds_range (direct induction, no fancy tacticals) --- *)
Lemma check_ds_range_spec (p : Policy) (ds : Dataset) :
  check_ds_range p ds = true <-> ds_ok p ds.
Proof.
  unfold check_ds_range, ds_ok.
  induction ds as [|r rs IH]; simpl.
  - split; intros _; constructor.
  - split.
    + intro H. apply Bool.andb_true_iff in H as [Hr Hrs].
      apply check_rec_range_spec in Hr.
      apply IH in Hrs.
      constructor; [exact Hr | exact Hrs].
    + intro H. inversion H as [| ? ? Hr Hrs]; subst.
      apply Bool.andb_true_iff. split.
      * apply check_rec_range_spec; exact Hr.
      * apply IH; exact Hrs.
Qed.
