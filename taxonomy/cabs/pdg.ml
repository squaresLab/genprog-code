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
	| EXPR_PATTERN(str) -> Result(Set.singleton str)
	| _ -> Children
end

let my_uses = new usesWalker 
	  
class labelDefs = object(self)
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
			Set.iter (fun str -> 
			  let num = post_incr def_num in 
				pprintf "exp: %d, defnum: %d, var: %s\n" exp.id num str;
				hadd str_to_def (str,exp.id) num;
				hadd def_to_str num (str,exp.id)
			) defs
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
			Set.iter (fun str -> 
			  let num = post_incr def_num in 
				pprintf "exp: %d, defnum: %d, var: %s\n" exp.id num str;
				hadd str_to_def (str,exp.id) num;
				hadd def_to_str num (str,exp.id)
			) defs
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
		  Set.iter (fun str -> 
			  let num = post_incr def_num in 
				pprintf "def: %d, defnum: %d, var: %s\n" def.id num str;
				hadd str_to_def (str,def.id) num;
				hadd def_to_str num (str,def.id)
		  ) names;
		  DoChildren
	| _ -> DoChildren

end

class genKillSets = object(self)
  inherit [int Set.t] singleCabsWalker
	
  method default_res () = Set.empty
  method combine set1 set2 = Set.union set1 set2

  method wExpression exp = 
	match dn exp with
	| UNARY(uop,exp1) ->
	  begin
		match uop with 
		| PREINCR 
		| PREDECR 
		| POSINCR 
		| POSDECR -> 
		  let def = hfind cabs_id_to_uses exp.id in
		  let asnums = Set.map (fun str -> hfind str_to_def (str,exp.id)) def in
		  CombineChildren(asnums)
		| _ -> Children
	  end
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
		  let def = hfind cabs_id_to_uses exp.id in
		  let asnums = Set.map (fun str -> hfind str_to_def (str,exp.id)) def in
		  CombineChildren(asnums)
		| _ -> Children
	  end
	| _ -> Children

  method wDefinition def = 
	match dn def with
	  DECDEF(ing,_) ->
		let _,ins = ing in
		let strs = 
		  Set.of_enum
			(List.enum
			   (lmap
				  (fun (name,ie) ->
					let (str,_,_,_) = name in str) ins)) in
			let asnums = Set.map (fun str -> hfind str_to_def (str,def.id)) strs in
			  CombineChildren(asnums)
	| _ -> Children
end

let data_dependence cfg_nodes =
  pprintf "one\n"; flush stdout;
  let gen_cache = hcreate 10 in
  let kill_cache = hcreate 10 in
  let out_cache = hcreate 10 in
  let labelWalker = new labelDefs in 
  let genKillWalk = new genKillSets in
  pprintf "two\n"; flush stdout;
  let rec label bb = 
	pprintf "labeling: bb: %d\n" bb.cid;
	match bb.cnode with
	| BASIC_BLOCK (slist) -> ignore(lmap (visitStatement labelWalker) slist)
	| CONTROL_FLOW(stmt,exp) -> ignore(visitExpression labelWalker exp)
	| REGION_NODE(cnodes) -> liter (fun (cnode,_) -> label cnode) cnodes
	| _ -> ();
	  pprintf "done labeling bb %d\n" bb.cid;
  in
	liter label cfg_nodes;
  pprintf "three\n"; flush stdout;
  let different domn domn' =
	let diff = Set.diff domn domn' in
	  not (Set.is_empty diff)
  in
  let rec gen_b (bb : cfg_node) : (int * int) Set.t =
	ht_find gen_cache bb.cid
	  (fun _ -> 
		match bb.cnode with 
		  BASIC_BLOCK (slist) ->
			lfoldl
			  (fun defs ->
				fun stmt ->
				  let genSet = genKillWalk#walkStatement stmt in
				  let paired = Set.map (fun gen -> bb.cid,gen) genSet in
					Set.union paired defs) (Set.empty) slist
		| CONTROL_FLOW(stmt,exp) -> 
		  Set.map (fun gen -> bb.cid, gen) (genKillWalk#walkExpression exp)
		| REGION_NODE(cnodes) ->
		  lfoldl (fun accum -> fun (cnode,_) -> Set.union accum (gen_b cnode)) (Set.empty) cnodes
		| _ -> Set.empty)
  in
  let kill_b (bb : cfg_node) (existing_defs : (int * int) Set.t) = 
	let kills def gen =
	  let def_str,_ = hfind def_to_str def in
		Set.exists
		  (fun (bb_gen,gen) ->
			let gen_str,_ = hfind def_to_str gen in
			  gen_str = def_str
		  ) gen
	in
	let gen = hfind gen_cache bb.cid in
	let kill_b,remaining = 
	  Set.partition (fun (_,def) -> kills def gen) existing_defs 
	in
	  hadd kill_cache bb.cid kill_b; 
	  Set.union gen remaining
  in
  let on_entry_cache = hcreate 10 in
  let rec get_kills bb existing_defs = 
	if not (hmem kill_cache bb.cid) then begin
	  let existing_defs' = kill_b bb existing_defs in
		liter 
		  (fun (succ,_) -> 
			hadd on_entry_cache succ existing_defs';
			get_kills (hfind easy_access succ) existing_defs') 
		  bb.succs
	end
  in
	pprintf "four\n"; flush stdout;
	liter (fun bb -> ignore(gen_b bb)) cfg_nodes;
	pprintf "five\n"; flush stdout;
	get_kills (get_start cfg_nodes) (Set.empty);
	pprintf "six\n"; flush stdout;
	let with_info = 
	  lmap (fun bb -> bb,hfind gen_cache bb.cid,ht_find kill_cache bb.cid (fun _ -> Set.empty)) cfg_nodes in
	pprintf "seven\n"; flush stdout;
  let rec calc_reaching() =
	let changed = 
	  lfoldl
		(fun changed -> 
		  fun (bb,gen_b,kill_b) ->
			let out_b = ht_find out_cache bb.cid (fun _ -> Set.empty) in
			let in_b = 
			  lfoldl
				(fun out_preds ->
				  fun (pred,_) ->
					Set.union out_preds 
					  (ht_find out_cache pred (fun _ -> Set.empty)))
				Set.empty bb.preds
			in
			let out_b' = Set.union gen_b (Set.diff in_b kill_b) in
			  if different out_b out_b' then true else changed) false with_info
	in
	  if changed then calc_reaching() else ()
  in
	calc_reaching();
	pprintf "eight\n"; flush stdout;
	let pdg_edges = hcreate 10 in
	let uses_cache = hcreate 10 in
	let usesWalk = new usesWalker in
	let add_edges bb (existing_defs : (int * int) Set.t) =
	  let this_bb_gens : (int * int) Set.t = hfind gen_cache bb.cid in
	  let rec calc_uses bb = 
		ht_find uses_cache bb.cid
		  (fun _ ->
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
			| _ -> Set.empty)
	  in 
	  let uses : string Set.t = calc_uses bb in
		pprintf "bbid: %d\n" bb.cid;
		pprintf "GENS: \n";
		Set.iter (fun (defining_bb,def_num) -> pprintf "  Defined in %d, definition num %d\n" defining_bb def_num) this_bb_gens;
		pprintf "KILLS: \n";
		let killset = hfind kill_cache bb.cid in
		  Set.iter (fun (defining_bb,def_num) -> pprintf "  Defined in %d, definition num %d\n" defining_bb def_num) killset;
		  pprintf "USES: \n";
		  Set.iter (fun varstr -> pprintf "%s, " varstr) uses;
		  pprintf "\n";
	  let all_gen : (int * int) Set.t = Set.union existing_defs this_bb_gens in 
	  let where_defined =
		Set.map
		  (fun (varstr : string) ->
			let res : (int * int) Set.t = 
			  Set.fold
				(fun (bb_id,def_id) ->
				  fun all_defs ->
					let def_str,_ = hfind def_to_str def_id in 
					  if def_str = varstr then Set.add (bb_id,def_id) all_defs 
					  else all_defs) all_gen (Set.empty)
			in
			  res
		  ) uses in
	  let where_defined =
		Set.fold
		  (fun set1 ->
			fun sets ->
			  Set.union set1 sets) where_defined (Set.empty) 
	  in
	  let without_me = 
		Set.filter
		  (fun (bb_id,_) -> bb_id <> bb.cid) where_defined in
		Set.iter
		  (fun (def_node_id,_) ->
			let init_val = ht_find pdg_edges def_node_id (fun _ -> EdgeSet.empty) in
			  hrep pdg_edges def_node_id (EdgeSet.add (bb,DATA) init_val)) without_me;
	in
	pprintf "nine\n"; flush stdout;
	  ignore(lfoldl
			   (fun existing ->
				 fun node -> 
				   add_edges node existing; 
				   let kills = hfind kill_cache node.cid in
				   let gens = hfind gen_cache node.cid in
					 Set.union gens (Set.diff existing kills)) (Set.empty) cfg_nodes);
	pprintf "ten\n"; flush stdout;
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
			  pprintf "  node: %d, label: %s\n" cnode.cid (labelstr label)) edge_set) pdg_edges
	
