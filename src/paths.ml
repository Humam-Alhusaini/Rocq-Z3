type coqTerm = Constr.t lazy_t

(** Functions over constr *)
let mklApp f args = Constr.mkApp (Lazy.force f, args)

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

(* Compute a nat *)
let rec mkNat = function
  | 0 -> Lazy.force cO
  | i -> mklApp cS [|mkNat (i-1)|]

(* Compute a positive *)
let rec mkPositive = function
  | 1 -> Lazy.force cxH
  | i ->
     let c = if (i mod 2) = 0 then cxO else cxI in
     mklApp c [|mkPositive (i / 2)|]

(* Compute a N *)
let mkN = function
  | 0 -> Lazy.force cN0
  | i -> mklApp cNpos [|mkPositive i|]

(* Compute a Boolean *)
let mkBool = function
  | true -> Lazy.force ctrue
  | false -> Lazy.force cfalse

(* Reification *)
let mk_bool b =
  let c, args = Constr.decompose_app_list b in
  if Constr.equal c (Lazy.force ctrue) then true
  else if Constr.equal c (Lazy.force cfalse) then false
  else assert false

let rec mk_nat n =
  let c, args = Constr.decompose_app_list n in
  if Constr.equal c (Lazy.force cO) then
    0
  else if Constr.equal c (Lazy.force cS) then
    match args with
    | [n] -> (mk_nat n) + 1
    | _ -> assert false
  else assert false

let rec mk_positive n =
  let c, args = Constr.decompose_app_list n in
  if Constr.equal c (Lazy.force cxH) then
    1
  else if Constr.equal c (Lazy.force cxO) then
    match args with
    | [n] -> 2 * (mk_positive n)
    | _ -> assert false
  else if Constr.equal c (Lazy.force cxI) then
    match args with
    | [n] -> 2 * (mk_positive n) + 1
    | _ -> assert false
  else assert false

let mk_N n =
  let c, args = Constr.decompose_app_list n in
  if Constr.equal c (Lazy.force cN0) then
    0
  else if Constr.equal c (Lazy.force cNpos) then
    match args with
    | [n] -> mk_positive n
    | _ -> assert false
  else assert false
