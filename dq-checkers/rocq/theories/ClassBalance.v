From Coq Require Import String ZArith List Bool.
Import ListNotations.
Local Open Scope string_scope.
Local Open Scope Z_scope.

From DQ Require Import Base.

(* -------------------------------------------------------------------------- *)
(* Configuration: expected percentages and tolerance                          *)
(* -------------------------------------------------------------------------- *)

(* Expected percentages (0–100) among NON-unknown education datapoints *)
Definition exp_primary_pct   : Z := 20.
Definition exp_secondary_pct : Z := 50.
Definition exp_tertiary_pct  : Z := 30.

(* Allowed deviation in percentage points, e.g. 10 = ±10 %-points *)
Definition tol_pct : Z := 2.

(* You can tune exp_*_pct and tol_pct above. *)

(* -------------------------------------------------------------------------- *)
(* Education categories                                                       *)
(* -------------------------------------------------------------------------- *)

Inductive edu_cat := Primary | Secondary | Tertiary.

Definition edu_primary   : string := "primary".
Definition edu_secondary : string := "secondary".
Definition edu_tertiary  : string := "tertiary".
Definition edu_unknown   : string := "unknown".

Definition edu_of_row (r : Row) : option edu_cat :=
  let e := education r in
  if String.eqb e edu_primary then Some Primary else
  if String.eqb e edu_secondary then Some Secondary else
  if String.eqb e edu_tertiary then Some Tertiary else
  None.

(* -------------------------------------------------------------------------- *)
(* Counting in Z                                                              *)
(* -------------------------------------------------------------------------- *)

Definition counts := (Z * Z * Z * Z)%type.
(* (primary, secondary, tertiary, unknown-or-other) *)

Definition add_edu (acc : counts) (r : Row) : counts :=
  let '(cP, cS, cT, cU) := acc in
  match edu_of_row r with
  | Some Primary   => (cP + 1, cS,     cT,     cU    )
  | Some Secondary => (cP,     cS + 1, cT,     cU    )
  | Some Tertiary  => (cP,     cS,     cT + 1, cU    )
  | None           => (cP,     cS,     cT,     cU + 1)
  end.

Definition count_edu (ds : Dataset) : counts :=
  fold_left add_edu ds (0, 0, 0, 0).

(* -------------------------------------------------------------------------- *)
(* Ratio comparison: scaled to avoid division                                 *)
(* -------------------------------------------------------------------------- *)

Definition within_band (exp tol cnt tot : Z) : bool :=
  (* exp, tol in percent; tot > 0; we check:
       (exp - tol) <= 100 * cnt / tot <= (exp + tol)
     via cross-multiplication. *)
  if Z.leb tot 0 then false else
  let lower := (exp - tol) * tot in
  let upper := (exp + tol) * tot in
  let scaled := 100 * cnt in
  Z.leb lower scaled && Z.leb scaled upper.

(* -------------------------------------------------------------------------- *)
(* Class-balance checker (education)                                          *)
(* -------------------------------------------------------------------------- *)

Definition ok_class_balance (ds : Dataset) : bool :=
  let '(cP, cS, cT, cU) := count_edu ds in
  let tot := cP + cS + cT in               (* only known education rows *)
  if Z.leb tot 0 then false else
  within_band exp_primary_pct   tol_pct cP tot &&
  within_band exp_secondary_pct tol_pct cS tot &&
  within_band exp_tertiary_pct  tol_pct cT tot.

Definition ds_ok_class (ds : Dataset) : bool := ok_class_balance ds.

(* Expose raw counts for diagnostics (not for the decision). *)
Definition edu_counts (ds : Dataset) : counts :=
  count_edu ds.
