open Names

let corelib = "Corelib"
let stdlib = "Stdlib"

let corelib_init = [corelib; "Init"]
let corelib_numbers = [corelib; "Numbers"]
let stdlib_narith = [stdlib; "NArith"]


let make_modpath parts =
  MPfile (DirPath.make (List.map Id.of_string (List.rev parts)))

let nat_modpath       = make_modpath (corelib_init @ ["Datatypes"])
let eq_modpath        = make_modpath (corelib_init @ ["Logic"])
let binnat_modpath    = make_modpath (stdlib_narith @ ["BinNat"; "N"])
let binnatdef_modpath = make_modpath (stdlib_narith @ ["BinNatDef"; "N"])
let binnums_modpath   = make_modpath (corelib_numbers @ ["BinNums"; "N"])

let natid = Id.of_string "nat";;
let eqid = Id.of_string "eq";;

let ind_id ((mutind, _) : Ind.t) : Id.t = 
  MutInd.label mutind;;
 
let path_error (ind : Ind.t) (id : Id.t) = 
  let modpathstr = Ind.modpath ind |> ModPath.to_string in
  let idstr = ind_id ind |> Id.to_string in
  Printf.sprintf "Not an %s ind, modpath was %s and id was %s" (Id.to_string id)  modpathstr idstr |> failwith;;
