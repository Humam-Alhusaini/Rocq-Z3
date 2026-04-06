(* the Ltac2 plugin is "packaged" ie its modules are all contained in module Ltac2_plugin
   without this open we would have to refer to eg Ltac2_plugin.Tac2externals below *)
open Ltac2_plugin

(* APIs to register new externals, including the convenience "@->" infix operator *)
open Tac2externals

(* Translation operators between Ltac2 values and OCaml values in various types *)
open Tac2ffi

(** **** Two simple examples of tactics *)

(* Rocq tactics are values of the [Proofview.tactic] monad.
   tclUnit in Proofview is the return operation of this monad.
   We define an alias for convenience. *)
let return = Proofview.tclUNIT

(* Used to distinguish our primitives from some other plugin's primitives.
   By convention matches the plugin's ocamlfind name. *)
let plugin_name = "rocq_z3.rocq_z3"

let pname s = { Tac2expr.mltac_plugin = plugin_name; mltac_tactic = s }

(* We define for convenience a wrapper around Tac2externals.define.
   [define "foo"] has type
   [('a, 'b) Ltac2_plugin.Tac2externals.spec -> 'b -> unit].
   Type [('a, 'b) spec] represents a high-level Ltac2 tactic specification. It
   indicates how to turn a value of type ['b] into an Ltac2 tactic.
   The type parameter ['a] gives the type of value produced by interpreting the
   specification. *)
let define s = define (pname s)

let () = define "smt" (string @-> ret unit) @@ fun name ->
  let chan = name ^ ".smt2" |> open_out  in 
    let _ = Printf.fprintf chan "67" in
      flush chan;; 

