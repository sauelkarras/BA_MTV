From Coq Require Import ZArith List Bool.
Import ListNotations.
Open Scope Z_scope.

From DQ Require Import Base.

(* Closed interval on Z *)
Record Range := { lo : Z; hi : Z }.

(* Policy with three adjustable ranges *)
Record Policy := {
  ageRange       : Range;   (* Attribute 13: age in years *)
  amountRange    : Range;   (* Attribute 5 : credit amount *)
  durationRange  : Range    (* Attribute 2 : credit duration (months) *)
}.

(* Defaults: adjust as you like *)
Definition default : Policy :=
  {| ageRange      := {| lo := 18; hi := 85 |};
     amountRange   := {| lo := 0;  hi := 10000 |};
     durationRange := {| lo := 1;  hi := 72 |} |}.  (* typical loan durations *)

(* Scalar checker *)
Definition in_range (rg : Range) (z : Z) : bool :=
  Z.leb (lo rg) z && Z.leb z (hi rg).

(* Row- and dataset-level checkers *)
Definition check_rec_range (p : Policy) (r : Row) : bool :=
  in_range (ageRange p) (age r)
  && in_range (amountRange p) (balance r)
  && in_range (durationRange p) (duration r).

Definition check_ds_range (p : Policy) (ds : Dataset) : bool :=
  forallb (check_rec_range p) ds.

(* ---- Prop specs (same structure, now 3 conjuncts) ---- *)
Definition in_rangeP (rg : Range) (z : Z) : Prop :=
  lo rg <= z /\ z <= hi rg.

Definition rec_ok (p : Policy) (r : Row) : Prop :=
  in_rangeP (ageRange p) (age r) /\
  in_rangeP (amountRange p) (balance r) /\
  in_rangeP (durationRange p) (duration r).

Definition ds_ok (p : Policy) (ds : Dataset) : Prop :=
  Forall (rec_ok p) ds.

(* Proofs: we can fill these in for your Coq version after you confirm. *)
Lemma in_range_spec (rg : Range) (z : Z) :
  in_range rg z = true <-> in_rangeP rg z.
Proof. Admitted.

Lemma check_rec_range_spec (p : Policy) (r : Row) :
  check_rec_range p r = true <-> rec_ok p r.
Proof. Admitted.

Lemma check_ds_range_spec (p : Policy) (ds : Dataset) :
  check_ds_range p ds = true <-> ds_ok p ds.
Proof. Admitted.

(* Exports used by Haskell *)
Definition default_policy := default.
Definition rec_ok_range (p:Policy) (r:Row) : bool := check_rec_range p r.
