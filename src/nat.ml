open Names
open Paths

let num_to_SO (n : int) : int = 
  match n with
  | 1 -> 0
  | 2 -> 1
  | _ -> Constr.DestKO |> raise;;

let construct_to_SO ((ind, num) : Names.constructor) =
  match Ind.modpath ind |> ModPath.equal nat_modpath with
  | false -> Constr.DestKO |> raise
  | true -> num_to_SO num;;

let constr_to_SO (constr : Constr.t) : int = 
  match Constr.kind constr with
  | Construct (constructor, _) -> construct_to_SO constructor
  | _ -> Constr.DestKO |> raise;;

let rec lconstr_to_nat (lconstr : Constr.t list) (nat : int) : int = 
  match lconstr with
  | hd :: ls -> lconstr_to_nat ls (constr_to_SO hd + nat)
  | [] -> 0;;

let constr_to_nat (constr : Constr.t) (i : int) : int = 
  match Constr.kind constr with
  | App (constr', constrs) -> lconstr_to_nat (Array.to_list constrs) (constr_to_SO constr')
  | Construct ((_, 1), _) -> 0
  | _ -> Constr.DestKO |> raise;;

