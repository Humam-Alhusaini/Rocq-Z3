From Ltac2 Require Import Ltac2.

Declare ML Module "rocq_z3.rocq_z3".

(** a wrapper around "print_goal" *)
Ltac2 @external print_goal : string -> unit
  := "rocq_z3.rocq_z3" "print_goal".
