open Paths

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

  
let core_f (constr : Constr.t) (args : Constr.t list) : Constr.t option = 
  if Constr.equal constr (Lazy.force ceq) then
    match args with
    | [ty; arg1; arg2] 
        when Constr.equal ty (Lazy.force cbool) && Constr.equal arg2 (Lazy.force ctrue) -> Some arg1
    | _ -> failwith ("Constr not bool or not well types")
  else failwith "Should have an eq";;

  
let of_coq_lemma (env : Environ.env) (evars : Evd.evar_map) (clemma : Constr.t) : unit Proofview.tactic =
  (*Separates the quantifiers from lemma*)
  let (rel_context, qf_lemma) : (Constr.rel_context * Constr.types) = Term.decompose_prod_decls clemma in
  (*Adds the proper quantifiers to the env instead of having empty references*)
  let env_rel : Environ.env = Environ.push_rel_context rel_context env in
  let f, args = Constr.decompose_app_list qf_lemma in
  let _ = core_f f args in
  (*let form = coq_to_form _ in
  let print = print_form in*)
  Feedback.msg_notice (Printer.pr_context_unlimited env_rel evars);
  Feedback.msg_notice (Constr.debug_print qf_lemma); return ();;

let print_lemma () = of_coq_lemma |> mk_tactic;;

(*

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

*)
(*

let warn () = Feedback.msg_notice (Pp.str "hello"); None;;

let core_f =
if Constr.equal f cis_true then
    match args with
    | [a] -> Some a
    | _ -> warn ()
  else if Constr.equal f (Lazy.force ceq) then
    match args with
    | [ty; arg1; arg2] when Constr.equal ty (Lazy.force cbool) &&
                              Constr.equal arg2 (Lazy.force ctrue) ->
      Some arg1
    | _ -> warn ()
  else warn ();;
*)

