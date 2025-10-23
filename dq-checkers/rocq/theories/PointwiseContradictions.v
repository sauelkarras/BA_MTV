(*
  Configurable contradiction checker for:
    "if duration < min_conv_duration then y must NOT be yes_label"
*)

From Coq Require Import List String ZArith Bool.
From Coq Require Import Bool.Bool.
Import ListNotations.
Local Open Scope string_scope.

From DQ Require Import Base.

(* ---------- Tweakable properties to edit ---------- *)

Record ContrCfg := {
  min_conv_duration : Z;    (* threshold in seconds; default 30 *)
  yes_label         : string  (* value that means "converted"; default "yes" *)
}.

Definition default_contr_cfg : ContrCfg :=
  {| min_conv_duration := 25;
     yes_label         := "yes" |}.

(* ---------- Rule encoded with a config ---------- *)

(* Boolean/Checker rule: fails if NOT (duration < min_conv_duration) OR NOT (y = yes_label) // NOT(duration<min)→(y/=yes)*) 
Definition rule_shortcall_not_yes (cfg : ContrCfg) (r : Row) : bool :=
  negb (Z.ltb (duration r) (min_conv_duration cfg))
  || negb (String.eqb (y r) (yes_label cfg)).

(* Row dataset checkers parametrized by cfg /the actual checker*)
Definition check_rec_contradiction_cfg (cfg : ContrCfg) (r : Row) : bool :=
  rule_shortcall_not_yes cfg r.
(* Checks if entire list satisfies Checker*)
Definition check_ds_contradiction_cfg (cfg : ContrCfg) (ds : list Row) : bool :=
  forallb (check_rec_contradiction_cfg cfg) ds.

(* ---------- Backwards-compatible fixed versions (use default cfg) / hardwired irrgendwas aber bezieht sich auch auf default_contr_cfg...*) 

Definition check_rec_contradiction (r : Row) : bool :=
  check_rec_contradiction_cfg default_contr_cfg r.

Definition check_ds_contradiction (ds : list Row) : bool :=
  check_ds_contradiction_cfg default_contr_cfg ds.

(*proves:*)
Local Open Scope Z_scope.

(* Prop-level spec for one row *)
Definition contr_ok (cfg : ContrCfg) (r : Row) : Prop :=
  (min_conv_duration cfg <= duration r)%Z \/ (y r <> yes_label cfg).

(* Pointwise spec equivalence *)
Lemma rule_shortcall_not_yes_spec (cfg : ContrCfg) (r : Row) :
  rule_shortcall_not_yes cfg r = true <-> contr_ok cfg r.
Proof.
  unfold rule_shortcall_not_yes, contr_ok.
  rewrite orb_true_iff.
  split.
  - intro H.
    destruct H as [H|H].
    + apply negb_true_iff in H. apply Z.ltb_ge in H. left; exact H.
    + apply negb_true_iff in H. rewrite String.eqb_neq in H. right; exact H.
  - intro H.
    destruct H as [Hge|Hneq].
    + left.  apply negb_true_iff. apply Z.ltb_ge. exact Hge.
    + right. apply negb_true_iff. rewrite String.eqb_neq. exact Hneq.
Qed.

(* Dataset equivalence via forallb and Forall_forall, using positional args *)
Lemma check_ds_contradiction_cfg_spec (cfg : ContrCfg) (ds : list Row) :
  check_ds_contradiction_cfg cfg ds = true <-> Forall (contr_ok cfg) ds.
Proof.
  unfold check_ds_contradiction_cfg.
  rewrite forallb_forall.
  split.
  - (* pointwise bool success -> Forall Prop *)
    intro H.
    apply Forall_forall.
    intros x HIn.
    specialize (H x HIn).
    unfold check_rec_contradiction_cfg in H.
    apply rule_shortcall_not_yes_spec in H.
    exact H.
  - (* Forall Prop -> pointwise bool success *)
    intro HF.
    intros x HIn.
    apply rule_shortcall_not_yes_spec.
    (* Convert Forall to a pointwise lemma using positional arguments *)
    pose proof (proj1 (@Forall_forall Row (fun r => contr_ok cfg r) ds)) as conv.
    specialize (conv HF).
    specialize (conv x HIn).
    exact conv.
Qed.

