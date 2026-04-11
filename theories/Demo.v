From Ltac2 Require Import Ltac2.
From Rocq_Z3 Require Import Loader.

Goal forall x y, y = 1 -> x = y -> x = 1.
Proof.
  intros. print_goal.
Admitted.

Goal 1 + 1 = 2.
Proof.
  print_goal.
Admitted.

Require Import Nat.

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
