From Ltac2 Require Import Ltac2.

Declare ML Module "rocq_z3.rocq_z3".

Ltac2 @external print_goal_body : unit -> unit
  := "rocq_z3.rocq_z3" "print_goal".

Ltac2 Notation "print_goal" := print_goal_body ().
