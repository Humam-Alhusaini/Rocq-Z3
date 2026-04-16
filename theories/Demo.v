From Ltac2 Require Import Ltac2.
From Rocq_Z3 Require Import Loader.

Require Import Bool.
Require Import Nat.

Open Scope nat_scope.

Goal (0 =? 0)%bool = true.
Proof.
  print_lemma.
Admitted.

Theorem Forall : forall (x : nat), x = x.
Proof.
  print_lemma. 
Admitted.

Theorem exist : exists x, x = 1.
Proof.
  print_lemma.
Admitted.

Theorem conj : forall x, x = 1 \/ x = 2.
Proof.
  print_lemma.
Admitted.

Theorem disj : forall x, x = 1 \/ x = 2.
Proof.
  print_lemma.
Admitted.

Theorem hyp_intro : forall x, x = 1 -> x = 1.
Proof.
  print_lemma.
Admitted.
