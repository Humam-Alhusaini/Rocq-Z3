From Ltac2 Require Import Ltac2.

Declare ML Module "rocq_z3.rocq_z3".

(** a wrapper around "smt" *)
Ltac2 @external smt : string -> unit
  := "rocq_z3.rocq_z3" "smt".

