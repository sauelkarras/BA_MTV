From Coq Require Import List String ZArith Bool.
Import ListNotations.
Local Open Scope string_scope.

From DQ Require Import Base.

(* ---- Config ---- *)
Record ContrCfg := {
  a1_bad  : list string;  (* {A11,A14} *)
  a3_bad  : list string;  (* {A33,A34} *)
  a6_bad  : list string;  (* {A61,A65} *)
  a10_req : list string;  (* require A101 by default *)
  bad_lbl : string        (* "2" *)
}.

Definition default_contr_cfg : ContrCfg :=
  {| a1_bad  := ["A11"; "A14"];
     a3_bad  := ["A33"; "A34"];
     a6_bad  := ["A61"; "A65"];
     a10_req := ["A101"];
     bad_lbl := "2" |}.

Fixpoint str_in (x:string) (xs:list string) : bool :=
  match xs with
  | [] => false
  | y::ys => if String.eqb x y then true else str_in x ys
  end.

(* antecedent now includes a10 *)
Definition antecedent (cfg:ContrCfg) (r:Row) : bool :=
  str_in (a1 r) (a1_bad cfg)  &&
  str_in (a3 r) (a3_bad cfg)  &&
  str_in (a6 r) (a6_bad cfg)  &&
  str_in (a10 r) (a10_req cfg).

(* rule: ¬antecedent ∨ y = "2" *)
Definition rule_contr (cfg:ContrCfg) (r:Row) : bool :=
  negb (antecedent cfg r) || String.eqb (y r) (bad_lbl cfg).

Definition check_ds_contradiction_cfg (cfg:ContrCfg) (ds:list Row) : bool :=
  forallb (rule_contr cfg) ds.

Definition rec_ok_contr (r:Row) : bool := rule_contr default_contr_cfg r.

(* ---- Spec skeletons (admitted) ---- *)
Lemma str_in_spec x xs : str_in x xs = true <-> In x xs. Admitted.

Definition antecedentP (cfg:ContrCfg) (r:Row) : Prop :=
  In (a1 r) (a1_bad cfg) /\
  In (a3 r) (a3_bad cfg) /\
  In (a6 r) (a6_bad cfg) /\
  In (a10 r) (a10_req cfg).

Definition contr_ok (cfg:ContrCfg) (r:Row) : Prop :=
  (~ antecedentP cfg r) \/ (y r = bad_lbl cfg).

Lemma antecedent_spec (cfg:ContrCfg) (r:Row) :
  antecedent cfg r = true <-> antecedentP cfg r. Admitted.

Lemma rule_contr_spec (cfg:ContrCfg) (r:Row) :
  rule_contr cfg r = true <-> contr_ok cfg r. Admitted.

Lemma check_ds_contradiction_cfg_spec (cfg:ContrCfg) (ds:list Row) :
  check_ds_contradiction_cfg cfg ds = true <-> Forall (contr_ok cfg) ds. Admitted.
