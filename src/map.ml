exception Map_error of string

open Names

open Z3

type z3expr = context -> Expr.expr -> Expr.expr -> Expr.expr;;

type map = 
  | Empty
  | Elem of (ModPath.t * Id.t) * z3expr * map

let eqpath modpath1 modpath2 id1 id2 = ModPath.equal modpath1 modpath2 && Id.equal id1 id2;;

let patherror modpath id = 
  Printf.sprintf "Modpath %s:%s was not found" (ModPath.to_string modpath) (Id.to_string id) |> failwith;;

let rec find ((path, id) : (ModPath.t * Id.t)) (map : map) : z3expr =
  match map with 
  | Empty -> Map_error (patherror path id) |> raise
  | Elem ((path', id'), z3expr, map') -> 
      if eqpath path path' id id' then z3expr
      else find (path,id) map';;
