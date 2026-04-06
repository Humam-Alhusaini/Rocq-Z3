From Ltac2 Require Import Ltac2.

Declare ML Module "rocq_z3.rocq_z3".

(** A simple function taking an integer and returning a boolean.
   Note that the internal name "the_question" does not need to match the exposed name "question". *)
Ltac2 @external question : int -> bool
  := "rocq_z3.rocq_z3" "the_question".

(** a wrapper around "exact" *)
Ltac2 @external tuto_exact : constr -> unit
  := "rocq_z3.rocq_z3" "my_exact".

(** Some custom type. *)
Ltac2 Type custom := [ A | B (constr) ].

(** A function returning true if passed [A] or [B] of some inductive type. *)
Ltac2 @external ind_or_a : custom -> bool
  := "rocq_z3.rocq_z3" "is_ind_or_a".

(** a function returning [A] if the ident is not an hypothesis,
    or [B t] where [t] is its type if it is. *)
Ltac2 @external check_in_goal : ident -> custom
  := "rocq_z3.rocq_z3" "check_in_goal".

(** Another custom type, this one abstract on the Ltac2 side. *)
Ltac2 Type custom2.

(** Build a custom2 value. *)
Ltac2 @external mk_custom2 : int -> int -> custom2
  := "rocq_z3.rocq_z3" "mk_custom2".

(** Get something from a custom2 value. *)
Ltac2 @external sum2 : custom2 -> int
  := "rocq_z3.rocq_z3" "sum2".
