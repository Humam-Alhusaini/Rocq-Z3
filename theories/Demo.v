From Ltac2 Require Import Ltac2.
From Rocq_Z3 Require Import Loader.

Ltac2 Eval smt "hello".

Goal 1 + 1 = 2.
Proof.
  smt "z3".
Admitted.
