From Ltac2 Require Import Ltac2.
From Rocq_Z3 Require Import Loader.

Goal 1 + 1 = 2.
Proof.
  print_goal "goal".
Admitted.

Require Import Nat.

Theorem exist : exists x, x = 1.
Proof.
  print_goal "exists".
Admitted.

Theorem conj : (forall x, x = 1) /\ (forall y, y = 2).
Proof.
  split.
  print_goal "conj".
Admitted.

Theorem disj : (forall x, x = 1) \/ (forall y, y = 2).
Proof.
  print_goal "disj".
Admitted.

Theorem forall_nintro : forall x, x = 1.
Proof.
print_goal "nintro".
Admitted.

Theorem forall_intro : forall x, x = 1.
Proof.
  intro x.
print_goal "intro".
Admitted.
