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

module EdgeSet = Set.Make(struct
  type t = int * label 
  let compare (v1,l1) (v2,l2) = 
	if (Pervasives.compare v1 v2) == 0 then Pervasives.compare l1 l2 else
	  Pervasives.compare v1 v2 
end)

let compute_dominators startfun predfun cfg_nodes = 
  let dominators = hcreate 10 in
  let idoms = hcreate 10 in
  let start = startfun cfg_nodes in 
  let node_set = NodeSet.of_enum (List.enum cfg_nodes) in
  let start_set = NodeSet.singleton start in
  let full_set = NodeSet.remove start node_set in
	hadd dominators start start_set;
	NodeSet.iter
	  (fun cfg_node ->
		hadd dominators cfg_node node_set
	  ) full_set;
	let strictly_dominates dom n =
	  (n.cid != dom.cid) &&
		(let domn = hfind dominators n in 
		   NodeSet.exists
			 (fun node ->
			   node.cid == dom.cid) domn)
	in
	let compute (cfg_node : cfg_node) : NodeSet.t =
	  let n = NodeSet.singleton cfg_node in
	  let preds = predfun cfg_node in
		NodeSet.union n 
		  (lfoldl
			 (fun inter ->
			   fun pred ->
				 let domp : NodeSet.t = hfind dominators (hfind easy_access pred) in
				 NodeSet.inter inter domp)
			 node_set preds)
	in
	let different (domn : NodeSet.t) (domn' : NodeSet.t) =
	  let diff = NodeSet.diff domn domn' in
		not (NodeSet.is_empty diff)
	in
    let rec calc_doms () = 
	  let changed =
		NodeSet.fold
		  (fun n ->
			fun changed ->
			  let domn = hfind dominators n in 
			  let domn' = compute n in
				hrep dominators n domn';
				if different domn domn' then true
				else changed) full_set false in 
		if changed then calc_doms() else ()
	in
	let calc_idom n = 
	  if n.cid == start.cid then () 
	  else begin
		if hmem idoms n then () 
		else begin
		  let domn = hfind dominators n in 
		  let strict_doms = List.of_enum (NodeSet.enum (NodeSet.remove n domn)) in 
			let rec find_strict_dom  = function
			  | strict_dom :: tl -> 
				  if List.exists (fun other_dom -> strictly_dominates strict_dom other_dom && other_dom.cid != n.cid) strict_doms then 
					find_strict_dom tl
				  else 
					begin
					  if hmem idoms n then failwith "Already has an immediate dominator!" 
					  else hadd idoms n strict_dom
					end
			  | [] -> failwith "no strict dominator and not the start, FAIL\n" 
			in
			  find_strict_dom strict_doms
		end
	  end
	in
	let calc_idoms () = liter calc_idom cfg_nodes in
	  calc_doms();
	  calc_idoms();
	  liter
		(fun n ->
		  let domn = hfind dominators n in
		  let domn' = NodeSet.remove n domn in
			hrep dominators n domn') cfg_nodes;
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
			| ENTRY -> pprintf "ENTRY %d\n" node.cid
			);
			pprintf "Dominators:\n" ;
			NodeSet.iter
			  (fun dom ->
				pprintf "%d, " dom.cid) dominators;
			pprintf "\n\n";
			if node.cid == start.cid then () else begin
			  let idom = hfind idoms node in
				pprintf "Immediate dominator: %d\n" idom.cid
			end
		) dominators; dominators,idoms

(* idoms matches nodes to their immediate dominators; can we convert that into a
   tree that we can traverse easily? *)

let compute_pre_dominators = compute_dominators get_entry (fun c -> lmap (fun (p,_) -> p) c.preds)
let compute_post_dominators = compute_dominators get_end (fun c -> lmap (fun (p,_) -> p) c.succs)

let control_dependence cfg_nodes =
  let post_dominators,idoms = compute_post_dominators cfg_nodes in
	pprintf "Done computing post dominators, idoms\n"; 
	let node_set = NodeSet.of_enum (List.enum cfg_nodes) in
	let cfs = NodeSet.filter 
	  (fun node -> 
		List.exists
		  (fun (_,label) ->
			match label with
			  NONE -> false
			| _ -> true) node.succs) node_set in
	  hiter
		(fun node ->
		  fun idom ->
			pprintf "%d idoms %d\n" idom.cid node.cid) idoms;
	  let edge_ht = hcreate 10 in
	  let does_idom parent node = 
		match node.cnode with
		  STOP -> false
		| _ ->
		  let idom = hfind idoms node in
			idom.cid == parent.cid
	  in
	  let idoms_anything node = 
		let idoms = List.of_enum (Hashtbl.values idoms) in 
		  List.exists
			(fun idom -> 
			  idom.cid == node.cid) idoms
	  in
	  let processed = hcreate 10 in 

	  let rec idom_edges parent node label =
		if hmem processed (parent,node) then () else 
		  begin
			pprintf "idom_edges between parent: %d and child: %d\n" parent.cid node.cid;
			hadd processed (parent,node) ();
			if does_idom parent node then begin
			  hadd edge_ht (parent,node) label
			end
			else begin
			  liter
				(fun (child,l) ->
				  let label = if label == NONE then l else label in
				  idom_edges parent (hfind easy_access child) label) node.preds
			end
		  end
	  in
	  liter
		(fun cfg_node ->
		  if idoms_anything cfg_node then
			liter (fun (succ,l) -> idom_edges cfg_node (hfind easy_access succ) l) cfg_node.preds) cfg_nodes;
	  hiter
		(fun (parent,child) ->
		  fun label ->
			pprintf "%d --> %d, label: %s\n" parent.cid child.cid (match label with NONE -> "NONE" | TRUE -> "TRUE" | FALSE -> "FALSE")) edge_ht;
	let edges = 
	  lflat
		(lmap
		   (fun node ->
			 let succs = 
			   lfilt
				 (fun (succ,l) -> match l with NONE -> false | _ -> true) node.succs in
			   lmap (fun (succ,l) -> node,hfind easy_access succ,l) succs)
		   (NodeSet.elements cfs))
	in
	  pprintf "pairs1:\n";
	  liter
		(fun (a,b,_) ->
		  pprintf "(%d,%d), " a.cid b.cid) edges;
	  pprintf "\n";
	  let pairs =
		lfilt
		  (fun (a,b,_) ->
			let post_doms = hfind post_dominators a in
			  not (NodeSet.exists (fun node -> node.cid == b.cid) post_doms)) edges in
		pprintf "Done computing pairs, pairs:\n"; 
		liter
		  (fun (a,b,_) ->
			pprintf "(%d,%d), " a.cid b.cid) pairs;
		pprintf "\n";
		let control_dependents = hcreate 10 in
		let rec traverse_backwards a src dest =
		  if src.cid == dest.cid then () else
			begin
			  let set = ht_find control_dependents a (fun _ -> NodeSet.empty) in
				hrep control_dependents a (NodeSet.add src set);
				let next = hfind idoms src in
				  traverse_backwards a next dest 
			end
		in	
		  liter
			(fun (a,b,_) -> 
			  match a.cnode with
				STOP -> () 
			  | _ ->
				let parent = hfind idoms a in 
				  traverse_backwards a b parent) pairs;
		  pprintf "Done computing control dependents\n";
		  hiter
			(fun node ->
			  fun cont_dep ->
				pprintf "node: %d, control dependents:\n" node.cid ;
				NodeSet.iter
				  (fun n -> pprintf "%d, " n.cid) cont_dep;
				pprintf "\n\n") control_dependents;
		  let cd_preds = hcreate 10 in
			hiter
			  (fun node ->
				fun control_dependents ->
				  NodeSet.iter
					(fun cd ->
					  let set = 
						ht_find cd_preds cd
						  (fun _ -> NodeSet.empty) in
						hrep cd_preds cd (NodeSet.add node set))
					control_dependents) control_dependents
			

(*	  let filtered =
	  NodeSet.filter
	  (fun node ->
	  let cd_preds = 
	  ht_find cd_preds node (fun _ -> NodeSet.empty) in
	  if NodeSet.is_empty cd_preds then false 
	  else if (NodeSet.cardinal cd_preds) == 1 then begin
	  let ele = NodeSet.choose cd_preds in
	  
	  end else true
	  (not (NodeSet.is_empty cd_preds)) &&
	  (not ( 
	  
*)  
		
let cfg2pdg cfg_nodes = ()
