open Z3

open Nat

open Printf

(* Rocq tactics are values of the [Proofview.tactic] monad.
  tclUnit in Proofview is the return operation of this monad.
   We define an alias for convenience. *)
let return = Proofview.tclUNIT;;

(*Boiler plate for making a tactic*)
let mk_tactic (tac : (Environ.env -> Evd.evar_map -> Constr.t -> unit Proofview.tactic)) : unit Proofview.tactic =
(*Applies the goal-dependent tactic t in each goal independently*)
  Proofview.Goal.enter (fun gl ->
    (*Gets hypothesis*)
    let _ = Proofview.Goal.hyps gl in
    (*Gets hypothesis and global environment*)
    let env = Proofview.Goal.env gl in
    (*Gets current evar map*)
    let evars = Proofview.Goal.sigma gl in
    (*Gets conclusion of goal (EConstr) and turns it into Constr*)
    let constr = Proofview.Goal.concl gl |> EConstr.to_constr evars in 
    (*Put all of these into a tactic tac*)
    tac env evars constr
  );;

let z3_discharge (env : Environ.env) (evars : Evd.evar_map) (constr : Constr.t) : unit Proofview.tactic =
  let ctx = mk_context [] in
  let goal : Expr.expr = parse_entire constr ctx in
  let goal_str = Expr.to_string goal |> sprintf "Goal: %s" in
  (* we want to prove goal, so we assert its negation and check unsat *)
  let solver = Solver.mk_solver ctx None in
  Solver.add solver [Boolean.mk_not ctx goal];
  let _ = match Solver.check solver [] with
  | Solver.UNSATISFIABLE ->
    goal_str |> sprintf "Z3: goal discharged successfully\n%s" |> Pp.str |> Feedback.msg_notice 
  | Solver.SATISFIABLE ->
    goal_str |> sprintf "Z3: goal is false\n%s" |> failwith
  | Solver.UNKNOWN ->
    goal_str |> sprintf "Z3: unknown\n%s" |> failwith in return ();;

let call_z3 () = z3_discharge |> mk_tactic;;

(*let constrextern_extern_constr env evars c =
  Constrextern.extern_constr env evars c;;

let print_expr (env : Environ.env) (evars : Evd.evar_map) (econstr : EConstr.t) : unit Proofview.tactic =
  let constrexpr = constrextern_extern_constr env evars econstr in
  let pp = Ppconstr.pr_constr_expr env evars constrexpr in
  Feedback.msg_notice pp;
  Proofview.tclUNIT ();;
*)

let of_coq_lemma (env : Environ.env) (sigma : Evd.evar_map) (clemma : Constr.t) : unit Proofview.tactic =
  let (rel_context, qf_lemma) : (Constr.rel_context * Constr.types) = Term.decompose_prod_decls clemma in
  let _ : Environ.env = Environ.push_rel_context rel_context env in
  Feedback.msg_notice (Constr.debug_print qf_lemma);
  Feedback.msg_notice (Constr.debug_print clemma); return ();;

let print_lemma () = of_coq_lemma |> mk_tactic;;
