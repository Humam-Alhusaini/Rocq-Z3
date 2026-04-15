open Names
open Z3

type coqTerm = Constr.t lazy_t

let gc (prefix : string) (constant : string) : Constr.t lazy_t =
  lazy (UnivGen.constr_of_monomorphic_global (Global.env ()) (Rocqlib.lib_ref (prefix ^ "." ^ constant)));;


(* is_true *)
let cis_true = gc "core.is_true" "is_true"

(* nat *)
let nat_prefix = "num.nat"
let nat_gc = gc nat_prefix
let cnat = nat_gc "type"
let cO = nat_gc "O"
let cS = nat_gc "S"

(* Positive *)
let positive_prefix = "num.pos"
let positive_gc = gc positive_prefix
let cpositive = positive_gc "type"
let cxI = positive_gc "xI"
let cxO = positive_gc "xO"
let cxH = positive_gc "xH"
let ceqbP = positive_gc "eqb"

(* N *)
let n_prefix = "num.N"
let n_gc = gc n_prefix
let cN = n_gc "type"
let cN0 = n_gc "N0"
let cNpos = n_gc "Npos"
let cof_nat = n_gc "of_nat"

(* Booleans *)
let bool_prefix = "core.bool"
let bool_gc = gc bool_prefix
let cbool = bool_gc "type"
let ctrue = bool_gc "true"
let cfalse = bool_gc "false"
let candb = bool_gc "andb"
let corb = bool_gc "orb"
let cxorb = bool_gc "xorb"
let cnegb = bool_gc "negb"
let cimplb = bool_gc "implb"
let ceqb = bool_gc "eqb"
let cifb = bool_gc "ifb"
let creflect = bool_gc "reflect"

(* Logical Operators *)
let cnot = gc "core.not" "type"
let cconj = gc "core.and" "conj"
let cand = gc "core.and" "type"
let ciff = gc "core.iff" "type"

(* Equality *)
let eq_prefix = "core.eq"
let eq_gc = gc eq_prefix
let ceq = eq_gc "type"
let crefl_equal = eq_gc "refl"

let make_modpath parts =
  MPfile (DirPath.make (List.map Id.of_string (List.rev parts)))

let datatype_path       = make_modpath ["Corelib"; "Init"; "Datatypes"]
let logic_path        = make_modpath ["Corelib"; "Init"; "Logic"]

let natid = Id.of_string "nat";;
let eqid = Id.of_string "eq";;

let ind_id ((mutind, _) : Ind.t) : Id.t = 
  MutInd.label mutind;;
 
let eq_path (ind : Ind.t) (path : ModPath.t) (id : Id.t) : bool =
  (Ind.modpath ind |> ModPath.equal path && ind_id ind |> Id.equal id)

let path_error (ind : Ind.t) (id : Id.t) = 
  let modpathstr = Ind.modpath ind |> ModPath.to_string in
  let idstr = ind_id ind |> Id.to_string in
  Printf.sprintf "Not an %s ind, modpath was %s and id was %s" (Id.to_string id)  modpathstr idstr |> failwith;;


let known_constants = [
  ((datatype_path, natid),  Arithmetic.mk_add);
  ((logic_path, eqid),  Arithmetic.mk_mul);
]

