open Ltac2_plugin  (* the Ltac2 plugin is "packaged" ie its modules are all contained in module Ltac2_plugin
   without this open we would have to refer to eg Ltac2_plugin.Tac2externals below *)
open Tac2externals  (* APIs to register new externals, including the convenience "@->" infix operator *)
open Tac2ffi  (* Translation operators between Ltac2 values and OCaml values in various types *)

open Tactics


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


(*Export the tactics here*)
let () = 
  define "print_lemma" (unit @-> tac unit) @@ print_lemma;;
