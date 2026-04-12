From Ltac2 Require Import Ltac2.

Declare ML Module "rocq_z3.rocq_z3".

Ltac2 @external print_goal_body : unit -> unit
  := "rocq_z3.rocq_z3" "print_goal".

Ltac2 @external call_z3 : unit -> unit
  := "rocq_z3.rocq_z3" "call_z3".

Ltac2 generalize_all () : unit :=
  ltac1:(repeat match goal with H : _ |- _ => generalize dependent H end).

Ltac2 Notation "print_goal" := generalize_all (); print_goal_body ().

Ltac2 Notation "discharge" := call_z3 ().
