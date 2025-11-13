From Coq Require Import ZArith String List Bool.
Import ListNotations.
Local Open Scope string_scope.
Local Open Scope Z_scope.

Require Import DQ.Base.

Module CB.

(*------------------------------------------------------------------*)
(* Single adjustable knob: how many standard deviations (k).        *)
(* Change this one number and rebuild with `make -j`.               *)
(*------------------------------------------------------------------*)
Definition standard_deviations_boundary : nat := 141.  (* k/SD = 13 gender passes 141 foreign passes *)

(* Targets (assumed proportions p0 = a/b) *)
Definition sex_a     : Z        := 1.                (* 1/2 *)
Definition sex_b     : positive := 2%positive.
Definition foreign_a : Z        := 9.                (* 9/200 = 0.045 *)
Definition foreign_b : positive := 200%positive.

(* ------------------- Categorization from CSV codes ------------------- *)
Definition is_male_code (s:string) : bool :=
  (String.eqb s "A91") || (String.eqb s "A93") || (String.eqb s "A94").

Definition is_female_code (s:string) : bool :=
  (String.eqb s "A92") || (String.eqb s "A95").

Definition usable_sex (r:Row) : bool :=
  let s := a9 r in is_male_code s || is_female_code s.

Definition is_male (r:Row) : bool := is_male_code (a9 r).

Definition is_foreign_code (s:string) : bool := String.eqb s "A201".
Definition is_not_foreign_code (s:string) : bool := String.eqb s "A202".

Definition usable_foreign (r:Row) : bool :=
  let s := a20 r in is_foreign_code s || is_not_foreign_code s.

Definition is_foreign (r:Row) : bool := is_foreign_code (a20 r).

(* -------------------------- Counting ------------------------------- *)
Fixpoint count_nx {A} (usable : A -> bool) (is_pos : A -> bool)
                  (D : list A) : nat * nat :=
  match D with
  | [] => (0%nat, 0%nat)
  | r::rs =>
      let '(n,x) := count_nx usable is_pos rs in
      if usable r then (S n, if is_pos r then S x else x) else (n,x)
  end.

Definition count_male (D:Dataset) : nat * nat :=
  count_nx usable_sex is_male D.

Definition count_foreign (D:Dataset) : nat * nat :=
  count_nx usable_foreign is_foreign D.

(* ---------------- SD band: |x - n p0| <= k * sqrt(n p0 (1-p0)) --------------- *)
(* Integer-only form:
     (x*b - n*a)^2 <= (k^2) * n * a * (b - a)
   with p0 = a/b, k^2 = (Z.of_nat k)^2. *)
Definition within_k_sigma
           (a:Z) (b:positive) (k:nat) (n x:nat) : bool :=
  match n with
  | O => false (* no usable rows -> fail *)
  | S _ =>
      let bZ  : Z := Zpos b in
      let kZ  : Z := Z.of_nat k in
      let k2  : Z := kZ * kZ in
      let nZ  : Z := Z.of_nat n in
      let xZ  : Z := Z.of_nat x in
      let diff : Z := xZ * bZ - nZ * a in
      let lhs  : Z := diff * diff in
      let rhs  : Z := k2 * nZ * a * (bZ - a) in
      if (0 <=? rhs)%Z then (lhs <=? rhs)%Z else false
  end.

(* ----------------------- Public booleans (defaults) ------------------------ *)
Definition sex_balance_ok_default (D:Dataset) : bool :=
  let '(n,x) := count_male D in
  within_k_sigma sex_a sex_b standard_deviations_boundary n x.

Definition foreign_balance_ok_default (D:Dataset) : bool :=
  let '(n,x) := count_foreign D in
  within_k_sigma foreign_a foreign_b standard_deviations_boundary n x.

Definition all_balance_ok_default (D:Dataset) : bool :=
  sex_balance_ok_default D && foreign_balance_ok_default D.

(* ----------------------- Minimal diagnostics ------------------------------ *)
Record Diag := { n_total : nat; x_pos : nat; passed : bool }.

Definition sex_diag_default (D:Dataset) : Diag :=
  let '(n,x) := count_male D in
  {| n_total := n; x_pos := x;
     passed := within_k_sigma sex_a sex_b standard_deviations_boundary n x |}.

Definition foreign_diag_default (D:Dataset) : Diag :=
  let '(n,x) := count_foreign D in
  {| n_total := n; x_pos := x;
     passed := within_k_sigma foreign_a foreign_b standard_deviations_boundary n x |}.

End CB.
