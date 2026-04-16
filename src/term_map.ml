open Paths

(** Given a coq term, build the corresponding formula *)
type coq_cst =
  | CCtrue
  | CCfalse
  | CCnot
  | CCand
  | CCor
  | CCxor
  | CCimp
  | CCiff
  | CCifb
  | CCunknown;;

type term_map = 
  | Empty   (*key      value     rest*)
  | Elem of Constr.t lazy_t * coq_cst * term_map;;

let rec find (constr : Constr.t) (map : term_map) : coq_cst =
  match map with 
  | Empty -> CCunknown
  | Elem (constr', cst, map') -> 
      if Constr.equal constr (Lazy.force constr') then cst
      else find constr map';;

let add ((key,value) : Constr.t lazy_t * coq_cst) (map : term_map) : term_map = Elem (key, value, map);; 

let rec mk_map (ls : (Constr.t lazy_t * coq_cst) list) (map : term_map) : term_map =
  match ls with
  | hd :: tl -> add hd map |> mk_map tl
  | [] -> map;;

let op_list = [(ctrue,CCtrue); (cfalse,CCfalse); (candb,CCand); (corb,CCor); (cxorb,CCxor); (cimplb,CCimp); (cnegb,CCnot); (ceqb,CCiff); (cifb,CCifb)];;

let op_tbl = mk_map op_list Empty;;

let get_cst (constr : Constr.t) = find constr op_tbl;;
