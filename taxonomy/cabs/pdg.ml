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
  type t = cfg_node * label 
  let compare (v1,l1) (v2,l2) = 
	if (Pervasives.compare v1.cid v2.cid) == 0 then Pervasives.compare l1 l2 else
	  Pervasives.compare v1.cid v2.cid 
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

type pdg_node = 
	{ cfg_node : cfg_node ;
	  mutable control_dependents : EdgeSet.t ;
	  mutable data_dependents : EdgeSet.t }

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
		let rec traverse_backwards a src dest label =
		  if src.cid == dest.cid then () else
			begin
			  let set = ht_find control_dependents a (fun _ -> EdgeSet.empty) in
				hrep control_dependents a (EdgeSet.add (src,label) set);
				let next = hfind idoms src in
				  traverse_backwards a next dest label
			end
		in	
		  liter
			(fun (a,b,label) -> 
			  match a.cnode with
				STOP -> () 
			  | _ ->
				let parent = hfind idoms a in 
				  traverse_backwards a b parent label) pairs;
		  pprintf "Done computing control dependents\n";
		  hiter
			(fun node ->
			  fun cont_dep ->
				pprintf "node: %d, control dependents:\n" node.cid ;
				EdgeSet.iter
				  (fun (cfg_node,label) -> pprintf "(%d:%s), " cfg_node.cid (labelstr label)) cont_dep;
				pprintf "\n\n") control_dependents;
		  let cd_preds = hcreate 10 in
			hiter
			  (fun node ->
				fun control_dependents ->
				  EdgeSet.iter
					(fun (cd,label) ->
					  let set = 
						ht_find cd_preds cd
						  (fun _ -> EdgeSet.empty) in
						hrep cd_preds cd (EdgeSet.add (node,label) set))
					control_dependents) control_dependents;
			pprintf "reverse table:\n";
			hiter
			  (fun node ->
				fun cd_predecessors ->
				  pprintf "Node %d's cd_preds are: " node.cid;
				  EdgeSet.iter (fun (node,label) -> pprintf "(%d:%s) " node.cid (labelstr label);
				  pprintf "\n") cd_predecessors) cd_preds;
(* Now, factoring and inserting region nodes, sigh *)
			let as_list = List.of_enum (Hashtbl.enum cd_preds) in
			let filtered = 
			  List.filter
				(fun (node,cd_preds) ->
					if EdgeSet.is_empty cd_preds then false
					else if (EdgeSet.cardinal cd_preds) == 1 then begin
					  let (_,label) = EdgeSet.choose cd_preds in
						label != NONE 
					end
					else true) as_list in
			let cd_set_ht = hcreate 10 in
			  liter
				(fun (node,cd) ->
				  let region_node = 
					ht_find cd_set_ht cd
					  (fun _ ->
						let id = new_cfg() in
						  { cid = id; cnode = REGION_NODE (EdgeSet.elements cd) ; preds = []; succs = [] })
				  in
					let cdeps = ht_find control_dependents region_node (fun _ -> EdgeSet.empty) in
					  hrep control_dependents region_node (EdgeSet.union cdeps (EdgeSet.singleton (node,NONE)));
					  EdgeSet.iter
						(fun (parent,label) ->
						  let cds = hfind control_dependents parent in 
						  let cds = EdgeSet.remove (node,label) cds in
							hrep control_dependents parent (EdgeSet.union cds (EdgeSet.singleton (region_node,label)))) cd
				) filtered ;
			  let add_regions = 
				hfold
				  (fun node ->
					fun control_dependents ->
					  fun lst ->
					  let numT,numF,numN =
						EdgeSet.fold
						  (fun (node,label) -> 
							fun (numT,numF,numN) ->
							  match label with
								NONE -> numT,numF,numN + 1
							  | TRUE -> numT + 1, numF, numN
							  | FALSE -> numT,numF + 1,numN) control_dependents (0,0,0) in
					  let lst = 
						if numT > 1 then (node,TRUE) :: lst else lst in
						if numF > 1 then (node,FALSE) :: lst else lst) control_dependents [] 
			  in
				liter
				  (fun (parent,label) ->
					match parent.cnode with
					  CONTROL_FLOW _ ->
						let region_node = 
						  ht_find cd_set_ht (EdgeSet.singleton (parent,label))
							(fun _ ->
							  let id = new_cfg() in
								{ cid = id; cnode = REGION_NODE [ (parent,label)] ; preds = []; succs = [] })
						in
						let cdeps = hfind control_dependents parent in 
						let cdeps = 
						  EdgeSet.filter
							(fun (cd,l) ->
							  if l == label then begin
								let region_cds = ht_find control_dependents region_node (fun _ -> EdgeSet.empty) in
								let region_cds = EdgeSet.union region_cds (EdgeSet.singleton (cd,l)) in
								  hrep control_dependents region_node region_cds;
								  false
							  end else true) cdeps in
						  hrep control_dependents parent (EdgeSet.union cdeps (EdgeSet.singleton (region_node,label)))
					| _ -> ()) add_regions;
				hfold
				  (fun node ->
					fun cdeps ->
					  fun lst ->
						{ cfg_node = node ; control_dependents = cdeps; data_dependents = EdgeSet.empty } :: lst) control_dependents []
					  
					  
					

let cfg2pdg cfg_nodes = 
  let control_deps = control_dependence cfg_nodes in ()
