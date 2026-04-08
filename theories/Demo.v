From Ltac2 Require Import Ltac2.
From Rocq_Z3 Require Import Loader.

Ltac2 Eval smt "hello".

Ltac2 Eval rand_string ().

Goal 1 + 1 = 2.
Proof.
  Ltac2 Eval print_goal ().
  smt "z3".
Admitted.

Require Import Nat.

Theorem what : exists x, x = 1.
Proof.
  Ltac2 Eval print_goal ().
Admitted.
