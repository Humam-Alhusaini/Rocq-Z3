open Names
open Paths

open Z3

let num_to_expr (i : int) (ctx : context) (mk_num : context -> int -> Expr.expr) : Expr.expr = 
  mk_num ctx i;;

let app_to_list (constr : Constr.t) : Constr.t list = 
  match Constr.kind constr with
  | App (constr', constrs) -> constr' :: (Array.to_list constrs)
  | _ -> failwith "app_to_list: not an App";;

module NatNums = struct

  let num_to_SO (n : int) : int = 
    match n with
    | 1 -> 0
    | 2 -> 1
    | _ -> failwith (Printf.sprintf "num_to_SO: unexpected constructor index %d" n);;

  let constructor_to_SO ((ind, num) : constructor) =
    match Ind.modpath ind |> ModPath.equal nat_modpath with
    | false -> failwith (Printf.sprintf "construct_to_SO: not a nat constructor, modpath was %s" (ModPath.to_string (Ind.modpath ind)))
    | true -> num_to_SO num;;

  let constr_to_SO (constr : Constr.t) : int = 
    match Constr.kind constr with
    | Construct (constructor, _) -> constructor_to_SO constructor
    | _ -> failwith "constr_to_SO: not a Construct";;

  let rec lconstr_to_nat (lconstr : Constr.t list) (nat : int) : int = 
    match lconstr with
    | hd :: ls -> lconstr_to_nat ls (constr_to_SO hd + nat)
    | [] -> nat;;

  let rec constr_to_nat (lconstr : Constr.t list) (nat : int) : int = 
    match lconstr with
    | [] -> failwith "oh so empty"
    | constr :: _ ->
    (match Constr.kind constr with
    | App (constr', constrs) -> 1 + constr_to_nat (Array.to_list constrs) nat
    | Construct _ -> 0
    | _ -> failwith "constr_to_nat: not an App or Construct");;

end

module Typ = struct

  let ind_to_typ (ind : inductive) =
    match Ind.modpath ind |> ModPath.equal nat_modpath with
    | false -> failwith (Printf.sprintf "ind_to_typ: not a nat ind, modpath was %s" (ModPath.to_string (Ind.modpath ind)))
    | true -> Arithmetic.Integer.mk_numeral_i;;

  let constr_to_typ (constr : Constr.t) (ctx : context) : int -> Z3.Expr.expr = 
    match Constr.kind constr with
    | Ind (inductive, _) -> ctx |> (ind_to_typ inductive)
    | _ -> failwith "constr_to_typ: not an Ind";;

end

module Logic = struct

  let ind_to_eq (ind : inductive) =
    match Ind.modpath ind |> ModPath.equal eq_modpath with
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
      let num1 = NatNums.constr_to_nat [num1'] 0 |> ftyp in
      let num2 = NatNums.constr_to_nat [num2'] 0 |> ftyp in
      (Logic.constr_to_log eq ctx) num1 num2
  | _ -> failwith "your mom";;
