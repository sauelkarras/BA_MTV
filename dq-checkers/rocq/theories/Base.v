From Coq Require Import String ZArith List.
Import ListNotations.
Local Open Scope string_scope.
Local Open Scope Z_scope.

Record Row := {
  age      : Z;
  balance  : Z;         (*höhe vom credit*)
  duration : Z;         (**)
  a1       : string;    (*kontostand*)
  a3       : string;    (*credit history*)
  a6       : string;    (*anlagen*)
  a9       : string;    (* personal status & sex) *)
  a10      : string;    (* co-applicant?*)
  a20      : string;    (* foreign worker) *)
  y        : string     (*good credit risk?*)
}.

Definition Dataset := list Row.
