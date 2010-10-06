open Utils

(* every SSI site has a location, statement number, and predicate *)
  
type ssi_site = Cil.location * int * Cil.exp 

type site_info =
	(* Branches : standard info, stmts in the then branch, stmts in the else
	 * branch *)
	Branches of ssi_site * int list * int list
	  (* returns : standard info *)
  | Returns of ssi_site
	 (* scalar pairs: standard info, optional list of statements to associate
		with its predicate's value *)
  | Scalar_pairs of ssi_site * int list
  | Is_visited of Cil.location * int

let site_ht : (int, site_info) Hashtbl.t ref = ref (hcreate 10)
let coverage_ht : (int, Cil.stmtkind) Hashtbl.t ref = ref (hcreate 4096)
