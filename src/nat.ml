open Names
open Paths

open Z3

let app_to_list (constr : Constr.t) : Constr.t list = 
  match Constr.kind constr with
  | App (constr', constrs) -> constr' :: (Array.to_list constrs)
  | _ -> failwith "app_to_list: not an App";;

let get_name_from_ind ((mutind, _) : Ind.t) : Id.t = 
  MutInd.label mutind;;
 
let eq_path (ind : Ind.t) (path : ModPath.t) (id : Id.t) : bool =
  (Ind.modpath ind |> ModPath.equal path && get_name_from_ind ind |> Id.equal id)

let rec lconstr_to_nat (lconstr : Constr.t list) (nat : int) : int = 
  match lconstr with
  | [] -> failwith "oh so empty"
  | constr :: _ -> constr_to_nat constr nat

and constr_to_nat (constr : Constr.t) (nat : int) : int = 
  match Constr.kind constr with
  | App (constr', constrs) -> 1 + lconstr_to_nat (Array.to_list constrs) nat
  | Construct _ -> 0
  | _ -> failwith "constr_to_nat: not an App or Construct";;

module Typ = struct

  let ind_to_typ (ind : inductive) =
    match eq_path ind nat_modpath natid with
    | false -> failwith (Printf.sprintf "ind_to_typ: not a nat ind, modpath was %s" (ModPath.to_string (Ind.modpath ind)))
    | true -> Arithmetic.Integer.mk_numeral_i;;

  let constr_to_typ (constr : Constr.t) (ctx : context) : int -> Z3.Expr.expr = 
    match Constr.kind constr with
    | Ind (inductive, _) -> ctx |> (ind_to_typ inductive)
    | _ -> failwith "constr_to_typ: not an Ind";;

end

module Logic = struct

  let ind_to_eq (ind : inductive) =
    match eq_path ind eq_modpath eqid with
    | false -> failwith (Printf.sprintf "ind_to_eq: not an eq ind, modpath was %s" (ModPath.to_string (Ind.modpath ind)))
    | true -> Boolean.mk_eq;;

  let constr_to_log (constr : Constr.t) (ctx : context) : Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr = 
    match Constr.kind constr with
    | Ind (inductive, _) -> ctx |> (ind_to_eq inductive)
    | _ -> failwith "constr_to_log: not an Ind";;

end

let parse_entire (constr : Constr.t) (ctx : context) : Expr.expr =
  let constrs = app_to_list constr in
  match constrs with
  | eq :: typ :: num1' :: num2' :: _ -> 
      let ftyp = Typ.constr_to_typ typ ctx in
      let num1 = lconstr_to_nat [num1'] 0 |> ftyp in
      let num2 = lconstr_to_nat [num2'] 0 |> ftyp in
      (Logic.constr_to_log eq ctx) num1 num2
  | _ -> failwith "your mom";;
