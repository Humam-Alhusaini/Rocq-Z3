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

let smt : (string -> unit) = fun name ->
  let chan = name ^ ".smt2" |> open_out in 
    let _ = Printf.fprintf chan "67" in
      flush chan;; 

let get_goal_string () : string Proofview.tactic =
  Proofview.Goal.enter_one (fun gl ->
    let my_econstr = Proofview.Goal.concl gl in
    let env = Proofview.Goal.env gl in
    let sigma = Tacmach.project gl in
    let s = Pp.string_of_ppcmds (Printer.pr_econstr_env env sigma my_econstr) in
    return s
  );;

let rand_string () = "hello";;

let () = 
  define "smt" (string @-> ret unit) @@ smt;
  define "print_goal" (unit @-> tac string) @@ get_goal_string;
  define "rand_string" (unit @-> ret string) @@ rand_string;;

