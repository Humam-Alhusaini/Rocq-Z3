open Ltac2_plugin  (* the Ltac2 plugin is "packaged" ie its modules are all contained in module Ltac2_plugin
   without this open we would have to refer to eg Ltac2_plugin.Tac2externals below *)
open Tac2externals  (* APIs to register new externals, including the convenience "@->" infix operator *)
open Tac2ffi  (* Translation operators between Ltac2 values and OCaml values in various types *)
(*open Proofview *) (*Managing the proof context*)
(*open Proof*) (*Similar to Proofview*)
(*open Printer*) (*Pretty printing*)

(* Used to distinguish our primitives from some other plugin's primitives.
   By convention matches the plugin's ocamlfind name. *)
let plugin_name = "rocq_z3.rocq_z3"

let pname s : Tac2expr.ml_tactic_name = { Tac2expr.mltac_plugin : string = plugin_name; mltac_tactic : string = s }

(* We define for convenience a wrapper around Tac2externals.define.
   [define "foo"] has type
   [('a, 'b) Ltac2_plugin.Tac2externals.spec -> 'b -> unit].
   Type [('a, 'b) spec] represents a high-level Ltac2 tactic specification. It
   indicates how to turn a value of type ['b] into an Ltac2 tactic.
   The type parameter ['a] gives the type of value produced by interpreting the
   specification. *)
let define s = define (pname s)

(* Rocq tactics are values of the [Proofview.tactic] monad.
  tclUnit in Proofview is the return operation of this monad.
   We define an alias for convenience. *)
let return = Proofview.tclUNIT


let write_to_smt2 filename txt =
  if filename ^ ".smt2" |> Sys.file_exists then
    failwith (Printf.sprintf "File '%s' already exists" filename)
  else
  let chan = filename ^ ".smt2" |> open_out in 
    let _ = Printf.fprintf chan "%s\n" txt in
      flush chan;; 

let mk_tactic (tac : (Environ.env -> Evd.evar_map -> Constr.t -> unit Proofview.tactic)) : unit Proofview.tactic =
  (*Applies the goal-dependent tactic t in each goal independently*)
  Proofview.Goal.enter (fun gl ->
    (*Gets hypothesis*)
    let _ = Proofview.Goal.hyps gl in
    (*Gets hypothesis and global environment*)
    let env = Proofview.Goal.env gl in
    (*Gets current evar map, which is necessary to turn econstr into const*)
    let evars = Proofview.Goal.sigma gl in
    (*Gets conclusion of goal (EConstr) and turns it into Constr*)
    let constr = Proofview.Goal.concl gl |> EConstr.to_constr evars in 
    tac env evars constr
  );;

let write_goal (filename : string) (env : Environ.env) (evars : Evd.evar_map) (constr : Constr.t) : unit Proofview.tactic =
  (*Constr -> Pp.t -> string*)
  let _ = constr |> Printer.pr_constr_env env evars |> Pp.string_of_ppcmds |> write_to_smt2 filename in
     Proofview.tclUNIT ();;

let print_goal filename = write_goal filename |> mk_tactic

let () = 
  define "print_goal" (string @-> tac unit) @@ print_goal;;

