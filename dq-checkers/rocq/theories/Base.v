From Coq Require Import String ZArith List.
Import ListNotations.
Local Open Scope string_scope.

(* Minimal shared schema for the bank dataset *)
Record Row := {
  age       : Z;       (* years *)
  balance   : Z;       (* euros *)
  duration  : Z;       (* seconds *)
  y         : string;  (* "yes" or "no" *)
  education : string   (* "primary","secondary","tertiary","unknown", ... *)
}.

Definition Dataset := list Row.
