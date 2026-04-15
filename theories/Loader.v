From Ltac2 Require Import Ltac2.

Declare ML Module "rocq_z3.rocq_z3".

Ltac2 @external call_z3 : unit -> unit
  := "rocq_z3.rocq_z3" "call_z3".

Ltac2 @external print_lemma : unit -> unit
  := "rocq_z3.rocq_z3" "print_lemma".

Ltac2 Notation "print_lemma" := print_lemma ().

Ltac2 Notation "discharge" := call_z3 ().
