open Names
open Z3

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

