open Batteries
open Utils
open Ref
open Set
open Cabs
open Cprint
open Cabsvisit
open Cabswalker
open Pretty
open Diff2cfg

let exp_str exp = Pretty.sprint ~width:80 (d_exp () exp)

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

module DefSet = Set.Make (struct 
  type t = int * int 
  let compare (v1,v2) (v3,v4) = 
	if (Pervasives.compare v1 v3) == 0 
	then Pervasives.compare v2 v4 
	else Pervasives.compare v1 v3
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
	  let diff1 = NodeSet.diff domn domn' in
	  let diff2 = NodeSet.diff domn' domn in
		not ((NodeSet.is_empty diff1) && (NodeSet.is_empty diff2))
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
			print_node node;
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
									NONE
								  | DATA -> numT,numF,numN + 1
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

let cabs_id_to_uses = hcreate 10
let str_to_def = hcreate 10
let def_to_str : (int, (string * int)) Hashtbl.t = hcreate 10 

let def_num = ref 0 

class usesWalker = object(self)
  inherit [string Set.t] singleCabsWalker
  method default_res () = Set.empty
  method combine set1 set2 = Set.union set1 set2

  method wExpression exp = 
	match dn exp with 
	| LABELADDR(str)
	| VARIABLE(str)
	| EXPR_PATTERN(str) -> 
	  Result(Set.singleton str)
	| _ -> Children
end

let my_uses = new usesWalker 
let gen_cache = hcreate 10
  
class labelDefs (bbnum : int) = object(self)
  inherit nopCabsVisitor

  method vexpr exp = 
	match dn exp with 
	| UNARY(uop,exp1) ->
	  begin
		match uop with 
		| PREINCR 
		| PREDECR 
		| POSINCR 
		| POSDECR -> 
		  let defs = my_uses#walkExpression exp1 in 
			hadd cabs_id_to_uses exp.id defs;
			let gens = 
			  DefSet.of_enum(
				Set.enum
				  (Set.map (fun str -> 
					let num = post_incr def_num in 
					  pprintf "exp: %d, defnum: %d, var: %s\n" exp.id num str;
					  hadd str_to_def (str,exp.id) num;
					  hadd def_to_str num (str,exp.id);
					  bbnum,num
				   ) defs)) in
			let orig = ht_find gen_cache bbnum (fun _ -> DefSet.empty) in
			  hrep gen_cache bbnum (DefSet.union orig gens)
		| _ -> ()
	  end; DoChildren
	| BINARY(bop,exp1,exp2) ->
	  begin
		match bop with 
		| ASSIGN
		| ADD_ASSIGN
		| SUB_ASSIGN
		| MUL_ASSIGN
		| DIV_ASSIGN
		| MOD_ASSIGN
		| BAND_ASSIGN
		| BOR_ASSIGN
		| XOR_ASSIGN
		| SHL_ASSIGN
		| SHR_ASSIGN ->
		  let defs = my_uses#walkExpression exp1 in 
			hadd cabs_id_to_uses exp.id defs;
			let gens = 
			  DefSet.of_enum(
				Set.enum
				  (Set.map (fun str -> 
					let num = post_incr def_num in 
					  pprintf "exp: %d, defnum: %d, var: %s\n" exp.id num str;
					  hadd str_to_def (str,exp.id) num;
					  hadd def_to_str num (str,exp.id);
					  bbnum,num
				   ) defs)) in
			let orig = ht_find gen_cache bbnum (fun _ -> DefSet.empty) in
			  hrep gen_cache bbnum (DefSet.union orig gens)
		| _ -> ()
	  end; DoChildren
	| _ -> DoChildren

  method vdef def = 
	match dn def with
	  DECDEF(ing,_) ->
		let _,ins = ing in
		let names = 
		  Set.of_enum (List.enum (
			lmap 
			  (fun (name,ie) ->
				let (str,_,_,_) = name in  str) ins)) in 
		  hadd cabs_id_to_uses def.id names;
		  let gens = 
			DefSet.of_enum(
			  Set.enum(
				Set.map (fun str -> 
				  let num = post_incr def_num in 
					pprintf "def: %d, defnum: %d, var: %s\n" def.id num str;
					hadd str_to_def (str,def.id) num;
					hadd def_to_str num (str,def.id);
					bbnum,num
				) names)) in
		  let orig = ht_find gen_cache bbnum (fun _ -> DefSet.empty) in
			hrep gen_cache bbnum (DefSet.union orig gens);
			DoChildren
	| _ -> DoChildren

end

let data_dependence cfg_nodes =
  let kill_cache = hcreate 10 in
  let out_cache = hcreate 10 in
  let rec label bb = 
	pprintf "labeling: bb: %d\n" bb.cid;
	hadd gen_cache bb.cid (DefSet.empty);
	let labelWalker = new labelDefs bb.cid in 
	  match bb.cnode with
	  | BASIC_BLOCK (slist) -> ignore(lmap (visitStatement labelWalker) slist)
	  | CONTROL_FLOW(stmt,exp) -> ignore(visitExpression labelWalker exp)
	  | REGION_NODE(cnodes) -> liter (fun (cnode,_) -> label cnode) cnodes
	  | _ -> ();
		pprintf "done labeling bb %d\n" bb.cid;
  in
	liter label cfg_nodes;
	pprintf "done labeling everything\n";
	let different domn domn' =
	  let diff1 = DefSet.diff domn domn' in
	  let diff2 = DefSet.diff domn' domn in
		not ((DefSet.is_empty diff1) && (DefSet.is_empty diff2))
	in
	let kill_b (bb : cfg_node) gen_b in_b =
	  pprintf "Calculating kill_b for %d. \n" bb.cid;
	  DefSet.fold
		(fun (bbid,generated_def) ->
		  fun killed_stuff ->
			let gen_str,_ = hfind def_to_str generated_def in
			let killed_in = 
			  DefSet.filter
				(fun (bb,in_b_def) ->
				  let def_str,_ = hfind def_to_str in_b_def in
					def_str = gen_str) in_b 
			in
			let killed_gened = 
			  DefSet.filter
				(fun (_,genned_def) ->
				  let def_str,_ = hfind def_to_str genned_def in
					def_str = gen_str && genned_def < generated_def) gen_b
			in
			  pprintf "Length of killed_gened: %d\n" (DefSet.cardinal killed_gened);
			  DefSet.union killed_stuff (DefSet.union killed_in killed_gened))
		gen_b (DefSet.empty)
	in
	let with_gen = lmap (fun bb -> bb,hfind gen_cache bb.cid) cfg_nodes in
	let rec calc_reaching() =
	  let changed = 
		lfoldl
		  (fun changed -> 
			fun (bb,gen_b) ->
			  pprintf "calculating out for bb %d\n" bb.cid;
			  let out_b = ht_find out_cache bb.cid (fun _ -> DefSet.empty) in
			  let in_b = 
				lfoldl
				  (fun inb ->
					fun (pred,_) ->
					  let outp = ht_find out_cache pred (fun _ -> DefSet.empty) in
						DefSet.union outp inb) (DefSet.empty) bb.preds in
				pprintf "in_b: \n";
				DefSet.iter (fun (defining_bb,def_num) -> pprintf "  Defined in %d, definition num %d\n" defining_bb def_num) in_b;

				let kill_b = kill_b bb gen_b in_b in
				  hrep kill_cache bb.cid kill_b;
				  let out_temp = DefSet.union gen_b in_b in 

					pprintf "gen_b: \n";
					DefSet.iter (fun (defining_bb,def_num) -> pprintf "  Defined in %d, definition num %d\n" defining_bb def_num) gen_b;
					pprintf "out_temp: ";
					DefSet.iter (fun (defining_bb,def_num) -> pprintf "  Defined in %d, definition num %d\n" defining_bb def_num) out_temp;
					pprintf "\n";
					let out_b' = (DefSet.diff out_temp kill_b) in
					  hrep out_cache bb.cid out_b'; 
					  pprintf "length out_b: %d\n" (DefSet.cardinal out_b);
					  pprintf "length out_b': %d\n" (DefSet.cardinal out_b');
					  if different out_b out_b' then (pprintf "true different!\n"; true) else (pprintf "false different!\n"; changed)) false with_gen
	  in
		if changed then begin pprintf "Changed!\n"; calc_reaching() end else begin pprintf "didn't change!\n"; () end
	in
	  calc_reaching();
	  let pdg_edges = hcreate 10 in
	  let usesWalk = new usesWalker in
	  let add_edges bb =
		let in_b = 
		  lfoldl
			(fun inb ->
			  fun (pred,_) ->
				let outp = hfind out_cache pred in
				  DefSet.union outp inb) (DefSet.empty) bb.preds 
		in 
		let gen_b = hfind gen_cache bb.cid in
		let rec calc_uses bb = 
		  match bb.cnode with
			BASIC_BLOCK (slist) ->
			  lfoldl
				(fun uses ->
				  fun stmt ->
					let genSet = usesWalk#walkStatement stmt in
					  Set.union genSet uses) (Set.empty) slist
		  | CONTROL_FLOW(stmt,exp) -> usesWalk#walkExpression exp
		  | REGION_NODE(cnodes) ->
			lfoldl (fun accum -> fun (cnode,_) -> Set.union accum (calc_uses cnode)) (Set.empty) cnodes
		  | _ -> Set.empty
		in 
		let uses : string Set.t = calc_uses bb in
		  pprintf "bbid: %d\n" bb.cid;
		  pprintf "INB:\n";
		  DefSet.iter (fun (defining_bb,def_num) -> pprintf "  Defined in %d, definition num %d\n" defining_bb def_num) in_b;
		  pprintf "OUTB:\n";
		  DefSet.iter (fun (defining_bb,def_num) -> pprintf "  Defined in %d, definition num %d\n" defining_bb def_num) (hfind out_cache bb.cid);
		  pprintf "GENS: \n";
		  DefSet.iter (fun (defining_bb,def_num) -> pprintf "  Defined in %d, definition num %d\n" defining_bb def_num) gen_b;
		  pprintf "KILLS: \n";
		  let kill_b = hfind kill_cache bb.cid in
			DefSet.iter (fun (defining_bb,def_num) -> pprintf "  Defined in %d, definition num %d\n" defining_bb def_num) kill_b;
			pprintf "USES: \n";
			Set.iter (fun varstr -> pprintf "%s, " varstr) uses;
			pprintf "\n";

			let where_defined =
			  Set.map
				(fun (varstr : string) ->
				  let res =
					DefSet.filter
					  (fun (bb_id,def_id) ->
						let def_str,_ = hfind def_to_str def_id in 
						  def_str = varstr) 
					  in_b
				  in
					pprintf "%s was defined at nodes: \n" varstr;
					DefSet.iter (fun (defining_bb,def_num) -> pprintf "  Defined in %d, definition num %d\n" defining_bb def_num) res;
					res
				) uses in
			let where_defined =
			  Set.fold
				(fun set1 ->
				  fun sets ->
					DefSet.union set1 sets) where_defined (DefSet.empty) 
			in
			  DefSet.iter
				(fun (def_node_id,_) ->
				  let init_val = ht_find pdg_edges def_node_id (fun _ -> EdgeSet.empty) in
					hrep pdg_edges def_node_id (EdgeSet.add (bb,DATA) init_val)) where_defined;
	  in
		liter add_edges cfg_nodes;
		pdg_edges
		  
let cfg2pdg cfg_nodes = 
  let control_deps = control_dependence cfg_nodes in 
  let pdg_edges = data_dependence cfg_nodes in
	hiter
	  (fun node_id ->
		fun edge_set ->
		  pprintf "Node id: %d.  PDG Edges: \n" node_id;
		  EdgeSet.iter
			(fun (cnode,label) ->
			  pprintf "  node: %d, label: %s\n" cnode.cid (labelstr label))
			edge_set) pdg_edges;
	lmap (fun node -> {node with data_dependents = hfind pdg_edges node.cfg_node.cid }) control_deps
	  
	  
type wc_graph_node = 
	{ wcn : pdg_node ;
	  mutable index : int }

type subgraph = pdg_node list 

let interesting_subgraphs (pdg_nodes : pdg_node list) =
  let easy_access : (int, pdg_node) Hashtbl.t = hcreate 10 in
  let undirected_graph : (int, IntSet.t) Hashtbl.t = hcreate 10 in
  let directed_graph : (int, IntSet.t) Hashtbl.t = hcreate 10 in
  let compressed : (int * pdg_node * IntSet.t) list =
	lmap (fun node -> 
	  hadd easy_access node.cfg_node.cid node;
	  let control_ints = 
		IntSet.of_enum
		  (Enum.map (fun (cnode,_) -> cnode.cid) (EdgeSet.enum node.control_dependents)) in
	  let data_ints = IntSet.of_enum (Enum.map (fun (cnode,_) -> cnode.cid) (EdgeSet.enum node.data_dependents)) in
		node.cfg_node.cid, node, IntSet.union control_ints data_ints) pdg_nodes
  in
	liter
	  (fun (nid,node,all_neighbors) ->
		hrep directed_graph nid all_neighbors;
		let set = ht_find undirected_graph nid (fun _ -> IntSet.empty) in
		  hrep undirected_graph nid (IntSet.union set all_neighbors);
		  IntSet.iter
			(fun neighbor ->
			  let set = ht_find undirected_graph neighbor (fun _ -> IntSet.empty) in
				hrep undirected_graph neighbor (IntSet.add nid set))
			all_neighbors) compressed;
	(* weakly-connected components: a set of statements is a weakly-connected
	   component if there exists an undirected path between all pairs of nodes in
	   the set *)
	let without_implicits : (int * pdg_node * IntSet.t) list = 
	  lfilt 
		(fun (nid,node,neighbors) -> 
		  match node.cfg_node.cnode with 
			START | STOP | ENTRY -> false
		  | _ -> true) compressed
	in
	let wc_tbl : (int, wc_graph_node) Hashtbl.t = hcreate 10 in
	let index = ref 1 in
	let wc_nodes : wc_graph_node list = 
	  lmap (fun (nid,pdg_node,_) -> 
		let wcn = {wcn = pdg_node; index = 0 } in
		  hadd wc_tbl nid wcn; wcn) without_implicits 
	in
	let reach_ht = hcreate 10 in
	let undirected (node : pdg_node) : IntSet.t = hfind undirected_graph node.cfg_node.cid in
	let directed (node : pdg_node) : IntSet.t = hfind directed_graph node.cfg_node.cid in
	let rec reachable (node : pdg_node) (neighbor_func : pdg_node -> IntSet.t) : IntSet.t = 
	  ht_find reach_ht node.cfg_node.cid 
		(fun _ ->
		  let immediate = neighbor_func node in
		  IntSet.fold
			(fun neighbor ->
			  fun all_reachable ->
				IntSet.union (reachable (hfind easy_access neighbor) undirected) all_reachable)
			immediate immediate)
	in
	let components : (int, IntSet.t) Hashtbl.t = hcreate 10 in
	let rec compute_wcs (lst : wc_graph_node list) = 
	  match lst with
		ele :: eles ->
		  let all_reachable = reachable ele.wcn undirected in
			IntSet.iter
			  (fun id ->
				let wgn = hfind wc_tbl id in
				  wgn.index <- !index) all_reachable;
			hadd components !index all_reachable;
			incr index;
			let remaining = 
			  lfilt 
				(fun ele ->
				  let wgn = hfind wc_tbl ele.wcn.cfg_node.cid in
					wgn.index == 0) lst in
			  compute_wcs remaining
	  | [] -> ()
	in
	  compute_wcs wc_nodes;
	  (* semantic threads *)
	  let rec add_slice (ist : IntSet.t Set.t) (slice : IntSet.t) = 
		let conflicts = hcreate 10 in
		  Set.iter 
			(fun t ->
			  if (IntSet.cardinal (IntSet.inter slice t)) > 2 (* 2 is arbitrarily selected *) then 
				hadd conflicts t ();
			) ist;
		  if hlen conflicts == 0 then 
			Set.add slice ist
		  else begin
			let all_cs = Set.of_enum (Hashtbl.keys conflicts) in
			let slice = 
			  Set.fold 
				(fun new_slice -> 
				  fun thread -> 
					IntSet.union new_slice thread)
				all_cs slice in
			  add_slice (Set.diff ist all_cs) slice
		  end
	  in
	  let bst pdg_nodes = 
		hclear reach_ht;
		let visited = hcreate 10 in 
		  lfoldl
			(fun ist ->
			  fun pnode ->
				if not (hmem visited pnode.cfg_node.cid) then begin
				  hadd visited pnode.cfg_node.cid ();
				  let slice = reachable pnode directed in
					IntSet.iter (fun node -> hadd visited node ()) slice;
					add_slice ist slice
				end else ist 
			) (Set.empty) pdg_nodes
	in 
	  let components_to_subgraphs (components : (int, IntSet.t) Hashtbl.t) : subgraph list = 
		let comps = List.of_enum (Hashtbl.values components) in
		let one_component (component : IntSet.t) = 
		  let as_nodes : wc_graph_node list = 
			lmap (fun nodeid -> hfind wc_tbl nodeid) (List.of_enum (IntSet.enum component)) in
			lmap (fun node -> node.wcn) as_nodes
		in
		  lmap one_component comps
	  in
	  let ist_to_subgraphs ist = 
		let ists = List.of_enum (Set.enum ist) in
		let one_thread thread = 
		  lmap (fun id -> hfind easy_access id) (List.of_enum (IntSet.enum thread))
		in
		  lmap one_thread ists
	  in
	  let ist = bst pdg_nodes in
		(components_to_subgraphs components) @ (ist_to_subgraphs ist)

