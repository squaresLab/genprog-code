open Batteries
open List
open Utils
open Cabs
open Cprint
open Globals
open Ttypes
open Cabswalker
open Tprint
open Doublewalk
open Difftypes
open Convert
open Treediff
open Diffs
open Canon
open Distance

let lexists = List.exists (* FIXME: add to utils *)

let tfold ts c fn =
  lfoldl
	(fun ts ->
	  fun ele -> fn ts c ele) ts

let tfold2 ts c fn1 fn2 val1 val2 =
  let ts' = fn1 ts c val1 in 
	fn2 ts' c val2

let tfold3 ts c fn1 fn2 fn3 val1 val2 val3 = 
  let ts' = fn1 ts c val1 in 
  let ts'' = fn2 ts' c val2 in
	fn3 ts'' c val3

let tfold_d ts c fn1 fn2 =
  lfoldl
	(fun ts ->
	  fun ele -> fn1 ts c (fn2 ele)) ts 

let make_dum_def dlist = lmap (fun d -> DEF(d)) dlist
let make_dum_stmt slist = lmap (fun s -> STMT(s)) slist
let make_dum_exp elist = lmap (fun e -> EXP(e)) elist

class contextConvertWalker initial_context context_ht = object (self)
  inherit [init_template list] singleCabsWalker

  val mutable context = initial_context 
  val context_ht = context_ht

  method default_res () = []

  method combine res1 res2 = res1 @ res2

  method wTreenode tn =
	let temp = context in
	  match tn with
	  | Globals(dlist) ->
		let defs = make_dum_def dlist in 
		  context <- {context with surrounding = DumSet.union context.surrounding (DumSet.of_enum (List.enum  defs))};
		  let res = 
			lfoldl
			  (fun result ->
				fun def ->
				  self#combine result (self#walkDefinition def) ) [] dlist in
			context <- temp; Result(res)
	  | Stmts(slist) ->
		let stmts = make_dum_stmt slist in 
		  context <- {context with surrounding = DumSet.union context.surrounding (DumSet.of_enum (List.enum stmts))};
		  let res = 
			lfoldl
			  (fun result ->
				fun stmt ->
				  self#combine result (self#walkStatement stmt) ) [] slist in
			context <- temp; Result(res)
	  | Exps(elist) -> 
		let exps = make_dum_exp elist in 
		  context <- {context with surrounding = DumSet.union context.surrounding (DumSet.of_enum (List.enum exps))};
		  let res = 
			lfoldl
			  (fun result ->
				fun exp ->
				  self#combine result (doWalk self#combine self#wExpression self#childrenExpression exp)) [] elist in
			context <- temp; Result(res)
	  | _ -> failwith "I really should get rid of the syntax tree node type since I don't use it."

  method wBlock block = (* FIXME: this doesn't handle attributes at all *)
	let temp = context in
	  context <- {context with surrounding = DumSet.of_enum (List.enum (make_dum_stmt block.bstmts))};
	  let res = 
		lfoldl
		  (fun result ->
			fun stmt ->
			  self#combine result (self#walkStatement stmt) ) [] block.bstmts in
		context <- temp; Result(res)

  method wStatement stmt = 
	let diff_tree_node = hfind cabs_id_to_diff_tree_node stmt.id in
	  let stmt_p = 
		match diff_tree_node.original_node with
		  STMT(s) -> 
			let node' = 
			  match (dn s) with
				NOP(_) -> NOP(dummyLoc)
			  | COMPUTATION(exp,_) -> COMPUTATION(exp,dummyLoc)
			  | BLOCK(b,_) -> BLOCK(dummyBlock,dummyLoc)
			  | SEQUENCE(s1,s2,loc) -> SEQUENCE(dummyStmt,dummyStmt,dummyLoc)
			  | IF(exp,s1,s2,_) -> IF(exp,dummyStmt,dummyStmt,dummyLoc)
			  | WHILE(exp,s1,_) -> WHILE(exp,dummyStmt,dummyLoc)
			  | DOWHILE(exp,s1,_) -> DOWHILE(exp,dummyStmt,dummyLoc)
			  | FOR(fc,exp1,exp2,s1,_) -> 
				FOR(fc,exp1,exp2,dummyStmt,dummyLoc)
			  | BREAK(_) -> BREAK(dummyLoc)
			  | CONTINUE(_) -> CONTINUE(dummyLoc)
			  | RETURN(exp,_) -> RETURN(exp,dummyLoc)
			  | SWITCH(exp,s1,_) -> SWITCH(exp,dummyStmt,dummyLoc)
			  | CASE(exp,s1,_) -> CASE(exp,dummyStmt,dummyLoc)
			  | CASERANGE(e1,e2,s1,_) -> CASERANGE(e1,e2,dummyStmt,dummyLoc)
			  | DEFAULT(s1,_) -> DEFAULT(dummyStmt,dummyLoc)
			  | LABEL(str,s1,_) -> LABEL(str,dummyStmt,dummyLoc)
			  | GOTO(str,_) -> GOTO(str,dummyLoc)
			  | COMPGOTO(exp,_) -> COMPGOTO(exp,dummyLoc)
			  | DEFINITION(d) -> DEFINITION(def_dum d)
			  | ASM(attrs,strs,dets,loc) -> 
				let dummed_attrs = lmap attr_dum attrs in 
				let dummed_dets = dets_dum dets in 
				  ASM(dummed_attrs,[],dummed_dets,dummyLoc) 
			  | TRY_EXCEPT(b1,exp,b2,_) -> 
				TRY_EXCEPT(dummyBlock,exp,dummyBlock,dummyLoc)
			  | TRY_FINALLY(b1,b2,_) -> TRY_FINALLY(dummyBlock,dummyBlock,dummyLoc)
			in 
			let s_copy = nd (node') in
			  Some(s_copy) (* Some(s)*)
		| _ -> failwith "Expected statement dummyNode, got something else"
	  in
		context <- {context with parent_statement=stmt_p};
	  let ts = 
		if hmem context_ht diff_tree_node.nid then begin
		  [(context,hfind context_ht diff_tree_node.nid)]
		end
		else []
	  in

		match stmt.node with
		| IF(e1,s1,s2,_) -> 
		  let temp = context in
			context <-  {context with guarding = DumSet.union (DumSet.singleton (STMT(s1))) context.guarding};
			let res1 = self#walkExpression e1 in
			  context <-  {temp with guarded_by = ((EXPG,e1)::temp.guarded_by)};
			  let res2 = self#walkStatement s1 in 
				context <- {temp with guarded_by = ((EXPG, nd(UNARY(NOT, e1)))::temp.guarded_by)};
				let res3 = self#walkStatement s2 in
				  context <- temp; Result(self#combine res1 (self#combine res2 (self#combine res3 ts)))
		| WHILE(e1,s1,_) 
		| DOWHILE(e1,s1,_) -> 
		  let temp = context in
			context <- {context with guarding = (DumSet.union (DumSet.singleton (STMT(s1))) context.guarding)};
			let res1 = self#walkExpression e1 in
			  context <-  {temp with guarded_by = ((EXPG,e1)::temp.guarded_by)};
			  let res2 = self#walkStatement s1 in
				context <- temp; Result(self#combine res1 (self#combine res2 ts))
		| FOR(fc,e1,e2,s1,_) ->
		  let res1 = 
			match fc with 
			  FC_EXP(e) -> self#walkExpression e1 
			| FC_DECL(def) -> self#walkDefinition def 
		  in 
		  let temp = context in 
			context <-  {context with guarding=(DumSet.union (DumSet.singleton (STMT(s1))) context.guarding)};
			let res2 = self#walkExpression e2 in
			  context <- {temp with guarded_by=((EXPG,e1)::context.guarded_by)};
			  let res3 = self#walkStatement s1 in
				context <- temp; Result(self#combine res1 (self#combine res2 (self#combine res3 ts)))
		(* FIXME: check the "guarded" and "guarded_by" on the case statements *)
		| TRY_EXCEPT(b1,e1,b2,_)->
		  let res1 = self#walkBlock b1 in
		  let temp = context in 
			context <-  {context with guarding=DumSet.of_enum (List.enum (make_dum_stmt b2.bstmts))};
			let res2 = self#walkExpression e1 in 
			  context <-  {temp with guarded_by = ((CATCH,e1)::context.guarded_by)};
			  let res3 = self#walkBlock b2 in
				context <- temp; Result(self#combine res1 (self#combine res2 (self#combine res3 ts)))
		| _ ->
		  CombineChildren(ts)

  method wExpression expression = 
	let diff_tree_node = hfind cabs_id_to_diff_tree_node expression.id in
	let exp_p = 
		match diff_tree_node.original_node with
		  EXP(e) -> 
			let node' = 
			  match e.node with 
				NOTHING -> NOTHING
			  | UNARY(uop,e1) -> UNARY(uop,dummyExp)
			  | LABELADDR(str) -> LABELADDR(str)
			  | BINARY(bop,e1,e2) -> BINARY(bop,dummyExp,dummyExp)
			  | QUESTION(e1,e2,e3) -> QUESTION(dummyExp,dummyExp,dummyExp)
			  | CAST((spec,dtype),ie) -> 
				let dummed_specs = lmap spec_dum spec in 
				let dummed_dt = dt_dum dtype in 
				let dummed_IE = ie_dum ie in
				  CAST((dummed_specs,dummed_dt),dummed_IE)
			  | CALL(exp,elist) -> CALL(dummyExp,[])
			  | COMMA(elist) -> COMMA([])
			  | CONSTANT(c) -> CONSTANT(c)
			  | PAREN(e1) -> PAREN(dummyExp)
			  | VARIABLE(str) -> VARIABLE(str)
			  | EXPR_SIZEOF(e1) -> EXPR_SIZEOF(dummyExp)
			  | TYPE_SIZEOF(spec,dtype) -> 
				let dummed_specs = lmap spec_dum spec in 
				let dummed_dt = dt_dum dtype in
				  TYPE_SIZEOF(dummed_specs,dummed_dt)
			  | EXPR_ALIGNOF(e1) -> EXPR_ALIGNOF(dummyExp)
			  | TYPE_ALIGNOF(spec,dtype) -> 
				let dummed_specs = lmap spec_dum spec in 
				let dummed_dt = dt_dum dtype in
				  TYPE_ALIGNOF(dummed_specs, dummed_dt)
			  | INDEX(e1,e2) -> INDEX(dummyExp,dummyExp)
			  | MEMBEROF(e1,str) -> MEMBEROF(dummyExp,str)
			  | MEMBEROFPTR(e1,str) -> MEMBEROFPTR(dummyExp,str)
			  | GNU_BODY(b) -> GNU_BODY(dummyBlock)
			  | EXPR_PATTERN(str) -> EXPR_PATTERN(str) in
			let e_copy = nd(node') in
			  Some(e_copy)
		| _ -> failwith "Expected expression dummyNode, got something else"
	  in
		context <- {context with parent_expression=exp_p};
	  let ts = 
		if hmem context_ht diff_tree_node.nid then begin
		  [(context,hfind context_ht diff_tree_node.nid)]
		end
		else []
	  in
		CombineChildren(ts)

  method wDefinition definition =
	let diff_tree_node = hfind cabs_id_to_diff_tree_node definition.id in
	let def_p = match diff_tree_node.original_node with
		DEF(d) -> 
		  let node' =
			match d.node with
			  FUNDEF(sn,b1,_,_) -> 
				FUNDEF(single_name_dum sn,dummyBlock,dummyLoc,dummyLoc)
			| DIRECTIVE(_) -> (dn d)
			| DECDEF(ing,_) -> DECDEF(ing_dum ing,dummyLoc)
			| TYPEDEF(ng,_) -> TYPEDEF(ng_dum ng,dummyLoc)
			| ONLYTYPEDEF(spec,_) -> ONLYTYPEDEF(lmap spec_dum spec,dummyLoc)
			| GLOBASM(str,_) -> GLOBASM("",dummyLoc)
			| PRAGMA(exp,_) -> PRAGMA(dummyExp,dummyLoc)
			| LINKAGE(str,_,_) -> LINKAGE("",dummyLoc,[])
		  in 
		  let d_copy = nd(node') in
			Some(d_copy)
	  | _ -> failwith "Expected def dummyNode, found something else"
	in
	  context <- {context with parent_definition=def_p};
	let ts = 
	  if hmem context_ht diff_tree_node.nid then begin
		[(context,hfind context_ht diff_tree_node.nid)]
	  end
	  else []
	in
	  CombineChildren(ts)
end

(*let alpha = new alphaRenameWalker*)

let treediff_to_templates (tree1 : tree) (difftree1 : diff_tree_node) (tdiff : changes) =
  let context_ht = hcreate 10 in
  let potential_parents = hcreate 10 in
  let add_to_context parent change = 
	let lst = ht_find context_ht parent (fun x -> []) in
	  hrep context_ht parent (change::lst)
  in
  let rec organized_changes changes = 
	(* FIXME: this is definitely imperfect, especially handling of delete/replace *)
	match changes with 
	  | SInsert((inserted,_),Some(parent,_),location) :: rest
	  | SInsertTree((inserted,_),Some(parent,_),location) :: rest
	  | SMove((inserted,_),Some(parent,_),location) :: rest -> 
		if node_in_tree difftree1 parent then begin
		  add_to_context parent (List.hd changes);
		  hadd potential_parents inserted parent;
		  organized_changes rest
		end else
		  (parent, (List.hd changes)) :: organized_changes rest 
	  | SDelete(deleted,_) :: rest ->
		(match (parent_of_nid difftree1 deleted) with
		  Some(parent) -> 
			add_to_context parent.nid (List.hd changes); organized_changes rest 
		| None -> (deleted,(List.hd changes)) :: organized_changes rest)
	  | SReplace((replacer,_),(replacee,_)) :: rest ->
		(match (parent_of_nid difftree1 replacee) with
		  Some(parent) -> 
			hadd potential_parents replacer replacee;
			add_to_context parent.nid (List.hd changes); organized_changes rest 
		| None -> (replacee,(List.hd changes)) :: organized_changes rest)
	  | [] -> []
	  | _ -> failwith "Why are we inserting nowhere, anyway?"
  in
  let unmatched = organized_changes tdiff in
  let rec handle_rest = function
	| (lookfor,change) :: rest ->
	  if hmem potential_parents lookfor then begin
		let parent = hfind potential_parents lookfor in 
		  hadd potential_parents lookfor parent;
		  add_to_context parent change;
		  handle_rest rest
	  end else 
		(lookfor,change) :: handle_rest rest
	| [] -> []
  in
  let unmatched = handle_rest unmatched in
	ignore(handle_rest unmatched);
(*	let alpha_map = alpha#walkTree tree1 in (* FIXME: reset table between tests! *)
	let alpha_map2 = alpha#walkChanges tdiff in
	let alpha_map3 = Map.union alpha_map alpha_map2 in*)
	let initial_context = make_icontext None None None 
	  (DumSet.empty) [] (DumSet.empty) in (*alpha_map3 in *)
	let con_convert = new contextConvertWalker initial_context context_ht in
	let res : init_template list = con_convert#walkTree tree1 in
	  res
  
let dummy_ht = hcreate 10
let change_ht = hcreate 10
let changes_ht = hcreate 10

class changesDoubleWalker = object(self)
  inherit templateDoubleWalker as super

  method private distance_change = 
	distance standard_eas_to_str print_change_gen self#walkChange

  method wDummyNode (dum1,dum2) =
	let hash1,hash2 = dummy_node_to_str dum1, dummy_node_to_str dum2 in 
(*	  pprintf "In wDummyNode. Hash1: %s, Hash2: %s\n" hash1 hash2; flush stdout;*)
	  ht_find dummy_ht (hash1,hash2) 
		(fun _ -> 
		  if hash1 = hash2 then Result(DUMBASE(dum1)) else
			match dum1,dum2 with
			| TREE(t1),TREE(t2) -> Result(TREEGEN(super#walkTree (t1,t2)))
			| STMT(s1),STMT(s2) -> Result(STMTGEN(super#walkStatement (s1,s2)))
			| EXP(e1),EXP(e2) -> Result(EXPGEN(super#walkExpression (e1,e2)))
			| DEF(def1),DEF(def2) -> Result(DEFGEN(super#walkDefinition (def1,def2)))
			| DELETED,_ 
			| _,DELETED 
			| CHANGE(_),_ 
			| _,CHANGE(_)
			| CHANGE_LIST(_),_
			| _,CHANGE_LIST(_) -> failwith "Unexpected dummynode in walk_dummy_node"
			| _,_ -> (*pprintf "comparison not yet implemented, returning star"; *)Result(DUMLIFTED(STAR))
		)
    
  method wChange (change1,change2) =
	let hash1,hash2 = standard_eas_to_str change1, standard_eas_to_str change2 in
	  ht_find change_ht (hash1,hash2) 
		(fun _ ->
		  if hash1 = hash2 then Result(ChangeBase(change1)) else
			match change1,change2 with
			  SInsert((_,insert1),_,_), SInsert((_,insert2),_,_)
			| SInsertTree((_,insert1),_,_), SInsertTree((_,insert2),_,_) -> Result(InsertGen(self#walkDummyNode (insert1,insert2)))
			| SMove((_,move1),_,_), SMove((_,move2),_,_) -> Result(MoveGen(self#walkDummyNode (move1,move2)))
			| SDelete(id1,delete1),SDelete(id2,delete2) -> Result(DeleteGen(self#walkDummyNode(delete1,delete2)))
			| SReplace((_,replace1),(_,replace2)), SReplace((_,replace3),(_,replace4)) -> 
			  Result(ReplaceGen(self#walkDummyNode (replace1,replace3), self#walkDummyNode (replace2,replace4)))
			| _,_ -> (*pprintf "wChangeWalker comparison not yet implemented, returning star";*) Result(ChangeLifted(STAR)))

  method walkDummyNode (d1,d2) = 
	doWalk compare self#wDummyNode 
	  (fun (d1,d2) -> failwith "We shouldn't be calling children on dummy nodes!") (d1,d2)

  method walkChange (change1,change2) = 
	doWalk compare self#wChange 
	  (fun (d1,d2) -> failwith "We shouldn't be calling children on changes!")  (change1,change2)

  method walkChanges (changes1,changes2) = 
	ht_find changes_ht (changes1,changes2)
	  (fun _ -> 
		let res = 
		lmap
		  (fun (c1,c2) ->
(*			pprintf "best mapping 1: %s ----> %s\n" (standard_eas_to_str c1) (standard_eas_to_str c2); flush stdout;*)
			self#walkChange (c1,c2)) (best_mapping ~print:(fun c -> pprintf "%s, " (standard_eas_to_str c)) self#distance_change changes1 changes2)
		in
		  if (llen changes1) <> (llen changes2) then CHANGEATLEAST(res) else BASECHANGES(res))

end

let guard_ht = hcreate 10
class guardsDoubleWalker = object(self)
  inherit templateDoubleWalker as super

  method private distance_guard = 
	distance print_guard print_guard_gen self#walkGuard

  method wGuard (guard1,guard2) =
	ht_find guard_ht (guard1,guard2) (fun _ ->
	match guard1,guard2 with
	  (EXPG,exp1),(EXPG,exp2) -> Result(EXPG,self#walkExpression (exp1,exp2))
	| (CATCH,exp1),(CATCH,exp2) -> Result(CATCH,self#walkExpression (exp1,exp2))
	| (EXPG,exp1),(CATCH,exp2) 
	| (CATCH,exp2),(EXPG,exp1) -> Result(GUARDLIFTED(STAR), self#walkExpression (exp1,exp2))
	| _,_ -> failwith "Unmatched guard double walker")

  method walkGuard g = 
	doWalk compare self#wGuard (fun _ -> failwith "Shouldn't call children on guards!") g

  method walkGuards (guards1,guards2) =
	lmap (fun (g1,g2) -> self#walkGuard (g1,g2)) (best_mapping self#distance_guard guards1 guards2)

end

let mytemplate = new templateDoubleWalker
let mycontext = new changesDoubleWalker
let myguard = new guardsDoubleWalker

let init_to_template (con,changes) =
  let get_opt opt construct = 
	match opt with 
	  Some(foo) -> Some(construct foo)
	| None -> None
  in
  let context' = 
	make_context 
	  (get_opt con.parent_definition (fun x -> DBASE(x)))
	  (get_opt con.parent_statement (fun x -> STMTBASE(x))) 
	  (get_opt con.parent_expression (fun x -> EXPBASE(x)))
	  (DumSet.fold
		 (fun d ->
		   fun set -> 
			 Set.add (DUMBASE(d)) set) con.surrounding (Set.empty))
	  (lmap (fun (g,e) -> (g,EXPBASE(e))) con.guarded_by) 
	  (DumSet.fold
		 (fun d ->
		   fun set -> 
			 Set.add (DUMBASE(d)) set) con.guarding (Set.empty))
  in
  let changes' = BASECHANGES(lmap (fun x -> ChangeBase(x)) changes) in
	context',changes'

let unify_itemplate (t1 : init_template) (t2 : init_template) : template = 
  let context1,changes1 = t1 in
  let context2,changes2 = t2 in
  let parent_definition' =
	match context1.parent_definition,context2.parent_definition with
	  Some(def1),Some(def2) -> Some(mytemplate#walkDefinition (def1,def2))
	| None,None -> None
	| _ -> Some(DLIFTED(LNOTHING))
  in
  let parent_statement' =
	match context1.parent_statement,context2.parent_statement with
	  Some(s1),Some(s2) -> 
		Some(mytemplate#walkStatement (s1,s2))
	| None,None -> None
	| _ -> Some(SLIFTED(LNOTHING))
  in
  let parent_expression' =
	match context1.parent_expression,context2.parent_expression with
	  Some(e1),Some(e2) -> 
		let e3 = mytemplate#walkExpression (e1,e2) in 
		  Some(e3)
	| None,None ->  None
	| _ -> Some(ELIFTED(LNOTHING))
  in
  let guards' =
	myguard#walkGuards (context1.guarded_by,context2.guarded_by) in 
  let lst1 = List.of_enum (DumSet.enum (context1.surrounding)) in
  let lst2 = List.of_enum (DumSet.enum (context2.surrounding)) in
  let distance_dummy_node =
	distance dummy_node_to_str print_dummy_gen mycontext#walkDummyNode in
  let permut = 
	best_mapping distance_dummy_node lst1 lst2
  in
  let surrounding' = 
	Set.of_enum (List.enum (lmap (fun (s1,s2) -> mycontext#walkDummyNode (s1,s2)) permut))
  in
  let guarding' = 
	Set.of_enum (List.enum (lmap (fun (s1,s2) -> mycontext#walkDummyNode (s1,s2)) 
							  (best_mapping distance_dummy_node 
								 (List.of_enum (DumSet.enum context1.guarding))
								 (List.of_enum (DumSet.enum context2.guarding)))))
  in
  let changes' = mycontext#walkChanges (changes1,changes2) in
	{ pdef = parent_definition';
	 pstmt = parent_statement';
	 pexp = parent_expression';
	 sding = surrounding';
	 gby = guards';
	 ging = guarding';
			(*			 renamed = Map.empty;*)
	}, changes'

let init_template_tbl = hcreate 10 

	
let diffs_to_templates (big_diff_ht) (outfile : string) (load : bool) =
  if load then 
	let fin = open_in_bin outfile in
	let res1 = Marshal.input fin in 
	  hiter (fun k -> fun v -> hadd init_template_tbl k v) res1; 
	  close_in fin; res1
  else begin
	let count = ref 0 in
	  hiter 
		(fun diffid ->
		  fun diff ->
			pprintf "Processing diffid %d\n" diffid;
			liter
			  (fun change -> 
				pprintf "change %d: %s\n" change.changeid change.syntactic; 
				let temps = treediff_to_templates change.tree change.head_node change.treediff in
				  pprintf "templates: %s \n" (lst_str itemplate_to_str temps); flush stdout;
				  liter (fun temp -> 
					let info = measure_info temp in
					  pprintf "info info: %d\n" info; flush stdout;
					hadd init_template_tbl !count (temp,info,change.syntactic); 
					Pervasives.incr count) temps)
			  diff.changes) big_diff_ht;
	  pprintf "printing out\n"; flush stdout;
	  let fout = open_out_bin outfile in
		Marshal.output fout init_template_tbl; close_out fout; init_template_tbl
  end

let test_template files = 
  let diff1 = List.hd files in
  let diff2 = List.hd (List.tl files) in
  let old_file_tree, new_file_tree =
	fst (Diffparse.parse_file diff1), fst (Diffparse.parse_file diff2) in
	Printf.printf "\n\n"; flush stdout;
	pprintf "Generating a diff:\n";
	let old_file_tree = process_tree old_file_tree in
	let new_file_tree = process_tree new_file_tree in
	let tree,patch,_ = tree_diff_cabs old_file_tree new_file_tree "test_generate" in 
	  pprintf "Tree print:\n"; flush stdout;
	  Difftypes.print_tree tree;
	  pprintf "Printing standardized patch:\n";
	  verbose := true;
	  print_standard_diff patch; 
	  pprintf "Templatizing:\n";
	  let ts = treediff_to_templates (diff1,old_file_tree)  tree patch in
		liter (fun temp -> print_itemplate temp) ts;
		pprintf "\n\n Done in test_template\n\n"; flush stdout

let testWalker files = 
  let parsed = lmap 
	(fun file -> pprintf "Parsing: %s\n" file; flush stdout; 
	  let parsed = fst (Diffparse.parse_file file) in
	  let parsed = process_tree parsed in
		pprintf "dumping parsed cabs: ";
		dumpTree defaultCabsPrinter Pervasives.stdout (file,parsed);
		pprintf "end dumped to stdout\n"; flush stdout;
		(file, parsed))
	files 
  in
  let rec cabs_diff_pairs = function
  (f1,hd1)::(f2,hd2)::tl -> pprintf "Diffing cabs for %s with %s\n" f1 f2; flush stdout;
	let tree,patch,_ = tree_diff_cabs hd1 hd2 "test_diff_change" in
	  ((f1,hd1),tree,patch) :: (cabs_diff_pairs tl)
	| [(f2,hd2)] -> pprintf "Warning: odd-length snippet list in test_diff_change: %s\n" f2; flush stdout; []
	| [] -> [] in
	pprintf "Step 2: diff pairs of files\n"; flush stdout; 
	let diffs = cabs_diff_pairs parsed in 
	  pprintf "Step 2a: printing diffs from pairs of files\n"; flush stdout;
	  verbose := true;
	  liter (fun (x,y,z) -> pprintf "A DIFF:\n\n"; print_standard_diff z; pprintf "END A DIFF\n\n"; flush stdout) diffs; flush stdout;
	  verbose := false;
	  pprintf "Templatizing. Difflist %d length:\n" (llen diffs);
	  let count = ref 0 in
	  let temp_ht = hcreate 10 in
		liter
		  (fun (tree,diff,patch) ->
			let temps = treediff_to_templates tree diff patch in
		  liter (fun temp -> hadd temp_ht (Ref.post_incr count) (temp,measure_info temp)) temps) diffs;
	  let cache_ht = hcreate 10 in
	  let testDistance it1 it2 = 
		let it1, it2 = if it1 < it2 then it1,it2 else it2,it2 in 
		  ignore(ht_find cache_ht (it1,it2) 
			(fun _ ->
			  pprintf "%d: distance between %d, %d\n" !count it1 it2; flush stdout; Pervasives.incr count;
			  if it1 == it2 then 0.0 else 
				let template1,info1 = hfind temp_ht it1 in
				let template2,info2 = hfind temp_ht it2 in
				let synth = unify_itemplate template1 template2 in
				let synth_info = measure_info synth in
				  pprintf "template1: %s\n template2: %s\n synth: %s\n" (itemplate_to_str template1) (itemplate_to_str template2) (template_to_str synth); 
				  let maxinfo = 2.0 /. ((1.0 /. float_of_int(info1)) +. (1.0 /. (float_of_int(info2)))) in
				  let retval = (maxinfo -. float_of_int(synth_info)) /. maxinfo in
				  let retval = if retval < 0.0 then 0.0 else retval in
					pprintf "Info1: %d, info2: %d, maxinfo: %g synth_info: %d distance: %g\n" info1 info2 maxinfo synth_info retval; retval))
	  in
		hiter 
		  (fun num1 ->
			fun temp1 -> 
			  hiter 
				(fun num2 ->
				  fun temp2 ->
					testDistance num1 num2) temp_ht) temp_ht;
		  pprintf "\n\n Done in testWalk\n\n"; flush stdout

