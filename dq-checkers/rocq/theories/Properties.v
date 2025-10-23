From Coq Require Import ZArith Lists.List Bool.Bool.
Import ListNotations.
Local Open Scope bool_scope.

From DQ Require Import Base.

(* --- Minimal schema + tiny dataset --- *)
Record Row := { age : Z; income : Z }.

Definition tiny_ds : list Row :=
  [{| age := 5;  income := 10 |};
   {| age := 30; income := 0  |};
   (* one intentionally borderline record *)
   {| age := 120; income := 1 |}].
