From Ltac2 Require Import Ltac2.
From Rocq_Z3 Require Import Loader.

Goal 0 = 0.
Proof.
  print_goal. discharge.
Admitted.

Goal 1 = 0.
Proof.
  print_goal. discharge.
Admitted.

Goal 100 = 100.
Proof.
  print_goal. discharge.
Admitted.

Goal N.shiftr 1 1 = 0.
Proof.
discharge. print_goal.
Admitted.

Goal 1 + 1 = 2.
Proof.
  print_goal.
Admitted.

Close Scope N.

Theorem Forall : forall (x : nat), x = x.
Proof.
  intros. print_goal_body ().
Admitted.

Theorem exist : exists x, x = 1.
Proof.
  print_goal.
Admitted.

Theorem conj : (forall x, x = 1) /\ (forall y, y = 2).
Proof.
  split. 
  print_goal.
Admitted.

Theorem disj : (forall x, x = 1) \/ (forall y, y = 2).
Proof.
  print_goal.
Admitted.

Theorem forall_nintro : forall x, x = 1.
Proof.
  print_goal.
Admitted.

Theorem forall_intro : forall x, x = 1.
Proof.
  intro x.
  print_goal.
Admitted.

Theorem hyp_intro : forall x, x = 1 -> 1 = x.
Proof.
  intros x H.
  print_goal.
Admitted.

Theorem hyp_nintro : forall x, x = 1 -> 1 = x.
Proof.
  print_goal.
Admitted.
