open Ltac2_plugin  (* the Ltac2 plugin is "packaged" ie its modules are all contained in module Ltac2_plugin
   without this open we would have to refer to eg Ltac2_plugin.Tac2externals below *)
open Tac2externals  (* APIs to register new externals, including the convenience "@->" infix operator *)
open Tac2ffi  (* Translation operators between Ltac2 values and OCaml values in various types *)
open Constr

open Z3

(* Used to distinguish our primitives from some other plugin's primitives.
   By convention matches the plugin's ocamlfind name. *)
let plugin_name = "rocq_z3.rocq_z3";;

let pname s : Tac2expr.ml_tactic_name = { Tac2expr.mltac_plugin : string = plugin_name; mltac_tactic : string = s };;

(* We define for convenience a wrapper around Tac2externals.define.
   [define "foo"] has type
   [('a, 'b) Ltac2_plugin.Tac2externals.spec -> 'b -> unit].
   Type [('a, 'b) spec] represents a high-level Ltac2 tactic specification. It
   indicates how to turn a value of type ['b] into an Ltac2 tactic.
   The type parameter ['a] gives the type of value produced by interpreting the
   specification. *)
let define s = define (pname s);;

(* Rocq tactics are values of the [Proofview.tactic] monad.
  tclUnit in Proofview is the return operation of this monad.
   We define an alias for convenience. *)
let return = Proofview.tclUNIT;;

(*This file writes the goal to filename.smt2 *)
let write_to_smt2 filename txt =
  if filename ^ ".smt2" |> Sys.file_exists then
    failwith (Printf.sprintf "File '%s' already exists" filename)
  else
  let chan = filename ^ ".smt2" |> open_out in 
    let _ = Printf.fprintf chan "%s\n" txt in
      flush chan;; 

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

let format_goal (typ_str : string) (constr_str : string) (env_str : string) : Pp.t =
  Printf.sprintf "Type:\n\n %s\n\nConstr:\n\n %s\n\nEnv: %s\n" typ_str constr_str env_str |> Pp.str;;

let write_goal (env : Environ.env) (evars : Evd.evar_map) (constr : Constr.t) : unit Proofview.tactic =
  (*Constr -> Pp.t -> string*)
  let constr_str = constr |> Printer.pr_constr_env env evars |> Pp.string_of_ppcmds in
  (*Constr = Typs in constr module, which implies that this is the same or atleast similar*)
  let typ_str = constr |> Printer.pr_type_env env evars |> Pp.string_of_ppcmds in 
  (*Formats the env*)
  let env_str = Printer.pr_context_unlimited env evars |> Pp.string_of_ppcmds in
  (*Format the 3 strings nicely, and then turn it into a format that can be sent to the log window using Feedback.msg_notic*)
    let _  = Constr.debug_print constr |> Feedback.msg_notice in
    let _ = format_goal typ_str constr_str env_str |> Feedback.msg_notice in 
      return ();;

let ctx = mk_context [];;

let print_goal () = write_goal |> mk_tactic;;

let z3_discharge () : unit =
  let ctx = Z3.mk_context [] in
  let _ = Z3.Arithmetic.Integer.mk_sort ctx in
  let zero = Z3.Arithmetic.Integer.mk_numeral_i ctx 0 in
  let goal = Z3.Boolean.mk_eq ctx zero zero in
  (* we want to prove goal, so we assert its negation and check unsat *)
  let solver = Z3.Solver.mk_solver ctx None in
  Z3.Solver.add solver [Z3.Boolean.mk_not ctx goal];
  match Z3.Solver.check solver [] with
  | Z3.Solver.UNSATISFIABLE ->
    Feedback.msg_notice (Pp.str "Z3: goal discharged successfully")
  | Z3.Solver.SATISFIABLE ->
    Feedback.msg_notice (Pp.str "Z3: goal is false")
  | Z3.Solver.UNKNOWN ->
    Feedback.msg_notice (Pp.str "Z3: unknown");;

  (*
  | Construct (((sp,i),j),u) ->
      str"Constr(" ++ pr_puniverses (MutInd.print sp ++ str"," ++ int i ++ str"," ++ int j) u ++ str")"
      *)

let constr_nat (constr : Constr.t) : int = 
  (*2 if s, 1 if O*)
  match Constr.kind constr with
  | Construct ((_, 2), _) -> 1
  | Construct ((_, 1), _) -> 0
  | _ -> DestKO |> raise;;

let rec lsconstr_to_nat (lsconstr : Constr.t list) (nat : int) : int = 
  match lsconstr with
  | hd :: ls -> lsconstr_to_nat ls (constr_nat hd + nat)
  | [] -> 0;;

let constr_to_nat (constr : Constr.t) (i : int) : int = 
  match Constr.kind constr with
  | App (constr', constrs) -> lsconstr_to_nat (Array.to_list constrs) (constr_nat constr')
  | Construct ((_, 1), _) -> 0
  | _ -> DestKO |> raise;;


let () = 
  let _ = define "print_goal" (unit @-> tac unit) @@ print_goal in  
  define "discharge" (unit @-> ret unit) @@ z3_discharge;;

