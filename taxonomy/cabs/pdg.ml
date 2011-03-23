open Batteries
open Utils
open Cabs
open Cprint
open Pretty
open Diff2cfg

(* there exists a directed data dependency edge from program point 1 to program
   point2 if and only if the execution of p2 depends on data calculated directly
   by p1 *)

(* there exists a directed control dependency edge from p1 to p2 iff the choice
   to executed p2 depends on the test in p1 *)

module NodeSet = Set.Make (struct
  type t = cfg_node
  let compare v1 v2 = Pervasives.compare v1.cid v2.cid
end)

let get_start nodes = 
  List.find
	(fun cfg_node ->
	  match cfg_node.cnode with
		START -> true
	  | _ -> false) nodes

let compute_dominators cfg_nodes =
  let easy_access = hcreate 10 in
	liter
	  (fun cnode ->
		hadd easy_access cnode.cid cnode) cfg_nodes;
  let dominators = hcreate 10 in
  let start = get_start cfg_nodes in 
  let node_set = NodeSet.of_enum (List.enum cfg_nodes) in
  let start_set = NodeSet.singleton start in
  let full_set = NodeSet.remove start node_set in
	hadd dominators start start_set;
	NodeSet.iter
	  (fun cfg_node ->
		hadd dominators cfg_node node_set
	  ) full_set;
	let compute_dominators (cfg_node : cfg_node) : NodeSet.t =
	  let n = NodeSet.singleton cfg_node in
	  let preds = cfg_node.preds in
		NodeSet.union n 
		(lfoldl
		  (fun inter ->
			fun pred ->
			  let domp : NodeSet.t = hfind dominators (hfind easy_access pred) in
				NodeSet.inter 
				  inter 
				  domp) 
		  node_set preds)
	in
	let different (domn : NodeSet.t) (domn' : NodeSet.t) =
	  let diff = NodeSet.diff domn domn' in
		not (NodeSet.is_empty diff)
	in
    let rec fixedpoint () = 
	  let changed =
		NodeSet.fold
		  (fun n ->
			fun changed ->
			  let domn = hfind dominators n in 
			  let domn' = compute_dominators n in
				hrep dominators n domn';
				if different domn domn' then true
				else changed) full_set false in 
		if changed then fixedpoint() else ()
	in
	  fixedpoint();
	  hiter
		(fun node ->
		  fun dominators ->
			(match node.cnode with
			  BASIC_BLOCK(slist) ->
				pprintf "BASIC BLOCK %d: [ \n" node.cid;
				liter (fun stmt -> pprintf "%s\n" (Pretty.sprint ~width:80 (d_stmt () stmt))) slist;
				pprintf "]\n"
			| CONTROL_FLOW(s1,e1) -> 
			  pprintf "CONTROL FLOW %d: [ \n" node.cid;
			  pprintf "%s\n" (Pretty.sprint ~width:80 (d_exp () e1));
			  pprintf "]\n"
			| START -> pprintf "START %d\n" node.cid
			| STOP -> pprintf "STOP %d\n" node.cid
			);
			pprintf "Dominators:\n" ;
			NodeSet.iter
			  (fun dom ->
				pprintf "%d, " dom.cid) dominators;
		pprintf "\n\n") dominators
	  
let cfg2pdg cfg_nodes = ()
