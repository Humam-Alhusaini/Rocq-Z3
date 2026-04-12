open Names


(*TODO Have a whitelist of supported modules, others get rejected*)

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
