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

let context_ht = hcreate 10

class contextConvertWalker initial_context = object (self)
  inherit [init_template list] singleCabsWalker

  val mutable context = initial_context 

  method default_res () = []

  method combine res1 res2 = res1 @ res2

  method wTreeNode tn =
	pprintf "in wTreenode\n"; flush stdout;
	let diff_tree_node = hfind cabs_id_to_diff_tree_node tn.id in
	let dummy_node = diff_tree_node.original_node in
	let tn_p = 
	  match dummy_node with 
		TREENODE(tn) -> 
		  let tn_copy = copy tn in 
			(match (dn tn) with
			| Globals(defs) -> tn_copy.node <- Globals([])
			| Stmts(ss) -> tn_copy.node <- Stmts([])
			| Exps(exps) -> tn_copy.node <- Exps([])
			| Syntax(str) -> tn_copy.node <- Syntax(""));
(*			Some(tn_copy)*) Some(tn)
	  | _ -> failwith "Expected a treenode dummy type, got something else"
	in
	  context <- {context with parent_treenode = tn_p};
	  let ts = 
		if hmem context_ht diff_tree_node.nid then begin
		  pprintf "Found a parent at %d, a treenode\n" diff_tree_node.nid; flush stdout;
		  [(initial_context,hfind context_ht diff_tree_node.nid)]
		end
		else []
	  in
	  let temp = context in
		match tn.node with
		| Globals(dlist) ->
		  pprintf "In globals\n"; flush stdout;
		  let defs = make_dum_def dlist in 
			context <- {context with surrounding = Set.union context.surrounding (Set.of_enum (List.enum  defs))};
			let res = 
			  lfoldl
				(fun result ->
				  fun def ->
					self#combine result (self#walkDefinition def) ) ts dlist in
			  context <- temp; Result(res)
		| Stmts(slist) ->
		  pprintf "In stmts\n"; flush stdout;
		  let stmts = make_dum_stmt slist in 
			context <- {context with surrounding = Set.union context.surrounding (Set.of_enum (List.enum stmts))};
			let res = 
			  lfoldl
				(fun result ->
				  fun stmt ->
					self#combine result (self#walkStatement stmt) ) ts slist in
			  context <- temp; Result(res)
		| Exps(elist) -> 
		  pprintf "in Exps\n"; flush stdout;
		  let exps = make_dum_exp elist in 
			context <- {context with surrounding = Set.union context.surrounding (Set.of_enum (List.enum exps))};
			let res = 
			  lfoldl
				(fun result ->
				  fun exp ->
					self#combine result (doWalk self#combine self#wExpression self#childrenExpression exp)) ts elist in
			  context <- temp; Result(res)
		| _ -> failwith "I really should get rid of the syntax tree node type since I don't use it."

  method wBlock block = (* FIXME: this doesn't handle attributes at all *)
	let temp = context in
	  context <- {context with surrounding = Set.of_enum (List.enum (make_dum_stmt block.bstmts))};
	  let res = 
		lfoldl
		  (fun result ->
			fun stmt ->
			  self#combine result (self#walkStatement stmt) ) [] block.bstmts in
		context <- temp; Result(res)

  method wStatement stmt = 
	let diff_tree_node = hfind cabs_id_to_diff_tree_node stmt.id in
	  pprintf "in wStatement, nid: %d\n" diff_tree_node.nid; flush stdout;
	  let stmt_p = 
		match diff_tree_node.original_node with
		  STMT(s) -> 
			let s_copy = copy s in 
			let node' = 
			  match (dn s) with
				NOP(_) -> NOP(dummyLoc)
			  | COMPUTATION(exp,_) -> COMPUTATION(dummyExp,dummyLoc)
			  | BLOCK(b,_) -> BLOCK(dummyBlock,dummyLoc)
			  | SEQUENCE(s1,s2,loc) -> SEQUENCE(dummyStmt,dummyStmt,dummyLoc)
			  | IF(exp,s1,s2,_) -> IF(dummyExp,dummyStmt,dummyStmt,dummyLoc)
			  | WHILE(exp,s1,_) -> WHILE(dummyExp,dummyStmt,dummyLoc)
			  | DOWHILE(exp,s1,_) -> DOWHILE(dummyExp,dummyStmt,dummyLoc)
			  | FOR(fc,exp1,exp2,s1,_) -> 
				FOR(dummyFC,dummyExp,dummyExp,dummyStmt,dummyLoc)
			  | BREAK(_) -> BREAK(dummyLoc)
			  | CONTINUE(_) -> CONTINUE(dummyLoc)
			  | RETURN(exp,_) -> RETURN(dummyExp,dummyLoc)
			  | SWITCH(exp,s1,_) -> SWITCH(dummyExp,dummyStmt,dummyLoc)
			  | CASE(exp,s1,_) -> CASE(dummyExp,dummyStmt,dummyLoc)
			  | CASERANGE(e1,e2,s1,_) -> CASERANGE(dummyExp,dummyExp,dummyStmt,dummyLoc)
			  | DEFAULT(s1,_) -> DEFAULT(dummyStmt,dummyLoc)
			  | LABEL(str,s1,_) -> LABEL("",dummyStmt,dummyLoc)
			  | GOTO(str,_) -> GOTO("",dummyLoc)
			  | COMPGOTO(exp,_) -> COMPGOTO(dummyExp,dummyLoc)
			  | DEFINITION(d) -> DEFINITION(def_dum d)
			  | ASM(attrs,strs,dets,loc) -> 
				let dummed_attrs = lmap attr_dum attrs in 
				let dummed_dets = dets_dum dets in 
				  ASM(dummed_attrs,[],dummed_dets,dummyLoc) 
			  | TRY_EXCEPT(b1,exp,b2,_) -> 
				TRY_EXCEPT(dummyBlock,dummyExp,dummyBlock,dummyLoc)
			  | TRY_FINALLY(b1,b2,_) -> TRY_FINALLY(dummyBlock,dummyBlock,dummyLoc)
			in 
			  s_copy.node <- node'; (*Some(s_copy)*) Some(s)
		| _ -> failwith "Expected statement dummyNode, got something else"
	  in
		context <- {context with parent_statement=stmt_p};
	  let ts = 
		if hmem context_ht diff_tree_node.nid then begin
		  pprintf "Found a parent at %d, a statement\n" diff_tree_node.nid; flush stdout;
		  [(context,hfind context_ht diff_tree_node.nid)]
		end
		else []
	  in

		match stmt.node with
		| IF(e1,s1,s2,_) -> 
		  let temp = context in
			context <-  {context with guarding = Set.union (Set.singleton (STMT(s1))) context.guarding};
			let res1 = self#walkExpression e1 in
			  context <-  {temp with guarded_by = ((EXPG,e1)::temp.guarded_by)};
			  let res2 = self#walkStatement s1 in 
				context <- {temp with guarded_by = ((EXPG, nd(UNARY(NOT, e1)))::temp.guarded_by)};
				let res3 = self#walkStatement s2 in
				  context <- temp; Result(self#combine res1 (self#combine res2 (self#combine res3 ts)))
		| WHILE(e1,s1,_) 
		| DOWHILE(e1,s1,_) -> 
		  let temp = context in
			context <- {context with guarding = (Set.union (Set.singleton (STMT(s1))) context.guarding)};
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
			context <-  {context with guarding=(Set.union (Set.singleton (STMT(s1))) context.guarding)};
			let res2 = self#walkExpression e2 in
			  context <- {temp with guarded_by=((EXPG,e1)::context.guarded_by)};
			  let res3 = self#walkStatement s1 in
				context <- temp; Result(self#combine res1 (self#combine res2 (self#combine res3 ts)))
		(* FIXME: check the "guarded" and "guarded_by" on the case statements *)
		| TRY_EXCEPT(b1,e1,b2,_)->
		  let res1 = self#walkBlock b1 in
		  let temp = context in 
			context <-  {context with guarding=Set.of_enum (List.enum (make_dum_stmt b2.bstmts))};
			let res2 = self#walkExpression e1 in 
			  context <-  {temp with guarded_by = ((CATCH,e1)::context.guarded_by)};
			  let res3 = self#walkBlock b2 in
				context <- temp; Result(self#combine res1 (self#combine res2 (self#combine res3 ts)))
		| _ ->
		  CombineChildren(ts)

  method wExpression expression = 
	let diff_tree_node = hfind cabs_id_to_diff_tree_node expression.id in
	  pprintf "in wExpression, nid: %d\n" diff_tree_node.nid; flush stdout;
	  let exp_p = 
		match diff_tree_node.original_node with
		  EXP(e) -> 
			let e_copy = copy e in
			let node' = 
			  match e.node with 
				NOTHING -> NOTHING
			  | UNARY(uop,e1) -> UNARY(uop,dummyExp)
			  | LABELADDR(str) -> LABELADDR("")
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
			  | MEMBEROF(e1,str) -> MEMBEROF(dummyExp,"")
			  | MEMBEROFPTR(e1,str) -> MEMBEROFPTR(dummyExp,"")
			  | GNU_BODY(b) -> GNU_BODY(dummyBlock)
			  | EXPR_PATTERN(str) -> EXPR_PATTERN("")
			in e_copy.node <- node'; (*Some(e_copy)*) Some(e)
		| _ -> failwith "Expected expression dummyNode, got something else"
	  in
		context <- {context with parent_expression=exp_p};
	  let ts = 
		if hmem context_ht diff_tree_node.nid then begin
		  pprintf "Found a parent at %d, an expression\n" diff_tree_node.nid; flush stdout;
		  [(context,hfind context_ht diff_tree_node.nid)]
		end
		else []
	  in
		CombineChildren(ts)

  method wDefinition definition =
	pprintf "in wDefinition\n"; flush stdout;
	let diff_tree_node = hfind cabs_id_to_diff_tree_node definition.id in
	let def_p = match diff_tree_node.original_node with
		DEF(d) -> 
		  let d_copy = copy d in
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
			d_copy.node <- node'; (*Some(d_copy) *) Some(d)  (* TODO: I think this is a good idea *)
	  | _ -> failwith "Expected def dummyNode, found something else"
	in
	  context <- {context with parent_definition=def_p};
	let ts = 
	  if hmem context_ht diff_tree_node.nid then begin
		pprintf "Found a parent at %d, a definition\n" diff_tree_node.nid; flush stdout;
		[(context,hfind context_ht diff_tree_node.nid)]
	  end
	  else []
	in
	  CombineChildren(ts)
end

(*let alpha = new alphaRenameWalker*)

let treediff_to_templates (tree1 : tree) (difftree1 : diff_tree_node) (tdiff : changes) =
  let add_to_context parent change = 
	let lst = ht_find context_ht parent (fun x -> []) in
	  hrep context_ht parent (change::lst)
  in
  let organized_changes change = 
 	match change with 
	| SInsert((inserted,_),Some(parent,_),location)
	| SInsertTree((inserted,_),Some(parent,_),location)
	| SMove((inserted,_),Some(parent,_),location) -> 
	  add_to_context parent change
	| SDelete(deleted,_) ->
	  (match (parent_of_nid difftree1 deleted) with
		Some(parent) -> pprintf "adding %d to table2\n" parent.nid; add_to_context parent.nid change
	  | None -> ())
	| SReplace((replacer,_),(replacee,_)) ->
	  (match (parent_of_nid difftree1 replacee) with
		Some(parent) -> pprintf "adding %d to table3\n" parent.nid; add_to_context parent.nid change
	  | None -> ())
	| _ -> failwith "Why are we inserting nowhere, anyway?"
  in
	liter organized_changes tdiff;
(*	let alpha_map = alpha#walkTree tree1 in (* FIXME: reset table between tests! *)
	let alpha_map2 = alpha#walkChanges tdiff in
	let alpha_map3 = Map.union alpha_map alpha_map2 in*)
	let initial_context = make_icontext None None None None (Set.empty) [] (Set.empty) in (*alpha_map3 in *)
	let con_convert = new contextConvertWalker initial_context in
	  con_convert#walkTree tree1 
  
let dummy_ht = hcreate 10
let change_ht = hcreate 10
let changes_ht = hcreate 10

class changesDoubleWalker = object(self)
  inherit templateDoubleWalker as super

  method change_compare c1 c2 = 
	let best = self#walkChange (c1,c2) in
	let i = Objsize.objsize best in 
	  i.Objsize.data

  method wDummyNode (dum1,dum2) =
	let hash1,hash2 = dummy_node_to_str dum1, dummy_node_to_str dum2 in 
	  ht_find dummy_ht (hash1,hash2) 
		(fun _ -> 
		  if hash1 = hash2 then Result(DUMBASE(dum1)) else
			match dum1,dum2 with
			| TREE(t1),TREE(t2) -> Result(TREEGEN(super#walkTree (t1,t2)))
			| STMT(s1),STMT(s2) -> Result(STMTGEN(super#walkStatement (s1,s2)))
			| EXP(e1),EXP(e2) -> Result(EXPGEN(super#walkExpression (e1,e2)))
			| TREENODE(tn1),TREENODE(tn2) -> Result(TNGEN(super#walkTreenode (tn1,tn2)))
			| DEF(def1),DEF(def2) -> Result(DEFGEN(super#walkDefinition (def1,def2)))
			| STRING(str1),STRING(str2) -> Result(DUMBASE(STRING(unify_string str1 str2)))
			| DELETED,_ 
			| _,DELETED 
			| CHANGE(_),_ 
			| _,CHANGE(_)
			| CHANGE_LIST(_),_
			| _,CHANGE_LIST(_) -> failwith "Unexpected dummynode in walk_dummy_node"
			| _,_ -> pprintf "comparison not yet implemented, returning star"; CombineChildren(DUMLIFTED(STAR))
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
			| _,_ -> pprintf "wChangeWalker comparison not yet implemented, returning star"; Result(ChangeLifted(STAR)))

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
			self#walkChange (c1,c2)) (best_permutation (self#change_compare) changes1 changes2)
		in
		  if (llen changes1) <> (llen changes2) then CHANGEATLEAST(res) else BASECHANGES(res))

end

class guardsDoubleWalker = object(self)
  inherit templateDoubleWalker as super

  method private guard_compare g1 g2 = 
	let best = self#walkGuard (g1,g2) in
	let i = Objsize.objsize best in 
	  i.Objsize.data

  method wGuard (guard1,guard2) =
	match guard1,guard2 with
	  (EXPG,exp1),(EXPG,exp2) -> Result(EXPG,self#walkExpression (exp1,exp2))
	| (CATCH,exp1),(CATCH,exp2) -> Result(CATCH,self#walkExpression (exp1,exp2))
	| (EXPG,exp1),(CATCH,exp2) 
	| (CATCH,exp2),(EXPG,exp1) -> Result(GUARDLIFTED(STAR), self#walkExpression (exp1,exp2))
	| _,_ -> failwith "Unmatched guard double walker"

  method walkGuard g = 
	doWalk compare self#wGuard (fun _ -> failwith "Shouldn't call children on guards!") g

  method walkGuards (guards1,guards2) =
	lmap (fun (g1,g2) -> self#walkGuard (g1,g2)) (best_permutation (self#guard_compare) guards1 guards2)

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
	  (get_opt con.parent_treenode (fun x -> TNBASE(x)))
	  (get_opt con.parent_definition (fun x -> DBASE(x)))
	  (get_opt con.parent_statement (fun x -> STMTBASE(x)))
	  (get_opt con.parent_expression (fun x -> EXPBASE(x)))
	  (Set.map (fun d -> DUMBASE(d)) con.surrounding)
	  (lmap (fun (g,e) -> (g,EXPBASE(e))) con.guarded_by)
	  (Set.map (fun d -> DUMBASE(d)) con.guarding)
  in
  let changes' = BASECHANGES(lmap (fun x -> ChangeBase(x)) changes) in
	context',changes'

let template_ht = hcreate 10
let hash_itemp it = itemplate_to_str it


let unify_itemplate (t1 : init_template) (t2 : init_template) : template = 
  let hash1,hash2 = hash_itemp t1,hash_itemp t2 in
	ht_find template_ht (hash1,hash2) 
	  (fun _ ->
		if hash1 = hash2 then (init_to_template t1) else begin
		  let context1,changes1 = t1 in
		  let context2,changes2 = t2 in
		  let parent_treenode' = 
			match context1.parent_treenode,context2.parent_treenode with
			  Some(tn1),Some(tn2) -> Some(mytemplate#walkTreenode (tn1,tn2))
			| None,None -> None
			| _ -> Some(TNLIFTED(LNOTHING))
		  in
		  let parent_definition' =
			match context1.parent_definition,context2.parent_definition with
			  Some(def1),Some(def2) -> Some(mytemplate#walkDefinition (def1,def2))
			| None,None -> None
			| _ -> Some(DLIFTED(LNOTHING))
		  in
		  let parent_statement' =
			match context1.parent_statement,context2.parent_statement with
			  Some(s1),Some(s2) -> Some(mytemplate#walkStatement (s1,s2))
			| None,None -> None
			| _ -> Some(SLIFTED(LNOTHING))
		  in
		  let parent_expression' =
			match context1.parent_expression,context2.parent_expression with
			  Some(e1),Some(e2) -> 
				let e3 = mytemplate#walkExpression (e1,e2) in 
				  Some(e3)
			| None,None -> pprintf "Neither has a parent expression?\n"; flush stdout; None
			| _ -> pprintf "Only one has a parent expression\n"; flush stdout; Some(ELIFTED(LNOTHING))
		  in
		  let guards' = 
			myguard#walkGuards (context1.guarded_by,context2.guarded_by) in
		  let surrounding' = 
			Set.of_enum (List.enum (lmap (fun (s1,s2) -> mycontext#walkDummyNode (s1,s2)) 
						   (best_permutation (distance mycontext#walkDummyNode) 
							  (List.of_enum (Set.enum (context1.surrounding)))
							  (List.of_enum (Set.enum (context2.surrounding))))))
		  in
		  let guarding' = 
			Set.of_enum (List.enum (lmap (fun (s1,s2) -> mycontext#walkDummyNode (s1,s2)) 
						   (best_permutation (distance mycontext#walkDummyNode) 
							  (List.of_enum (Set.enum (context1.guarding)))
							  (List.of_enum (Set.enum (context2.guarding))))))
		  in
		  let changes' = mycontext#walkChanges (changes1,changes2) in
			{ptn = parent_treenode';
			 pdef = parent_definition';
			 pstmt = parent_statement';
			 pexp = parent_expression';
			 sding = surrounding';
			 gby = guards';
			 ging = guarding';
			 renamed = Map.empty;
			}, changes'
		end)
	
let diffs_to_templates big_diff_ht outfile load =
  if load then 
	let fin = open_in_bin outfile in
	let res = Marshal.input fin in 
	  close_in fin; res
  else begin
  let template_tbl = hcreate 10 in
  hiter 
	(fun diffid ->
	  fun diff ->
		let templates = 
		  lmap 
		  (fun change -> treediff_to_templates change.tree change.head_node change.treediff)
		  diff.changes 
		in
		  pprintf "diff %d:\n" diffid;
		liter (fun temp -> liter print_itemplate temp) templates;
		pprintf "done with diff %d's template!\n" diffid;
		  hadd template_tbl diffid templates) big_diff_ht;
	let fout = open_out_bin outfile in
	  Marshal.output fout template_tbl; close_out fout; template_tbl
  end
let test_template files = 
  let diff1 = List.hd files in
  let diff2 = List.hd (List.tl files) in
  let old_file_tree, new_file_tree =
	fst (Diffparse.parse_file diff1), fst (Diffparse.parse_file diff2) in
	Printf.printf "\n\n"; flush stdout;
	pprintf "Generating a diff:\n";
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

let template_distance t1 t2 = 
  let combination = unify_itemplate t1 t2 in
  let bestsize = Objsize.objsize combination in
	bestsize.Objsize.data

let testWalker files = 
  let parsed = lmap 
	(fun file -> pprintf "Parsing: %s\n" file; flush stdout; 
	  let parsed = fst (Diffparse.parse_file file) in
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
	  let ts =
		lmap
		  (fun (tree,diff,patch) ->
			(treediff_to_templates tree diff patch)) diffs in (* OK, we don't actually want to flatten because the templates each correspond to a set of changes to a particular file. FIXME *)
		pprintf "Templates: \n"; 
		liter (fun x -> liter print_itemplate x) ts;
		pprintf "\n"; flush stdout;
		let rec synth_diff_pairs = function
		  | templates1::templates2::tl ->
			let best_map = 
			  lmap (fun (t1,t2) -> unify_itemplate t1 t2)
				(best_permutation template_distance templates1 templates2) in
			  liter (fun t -> pprintf "One match: \n"; print_template t) best_map
		  | [template1] -> pprintf "Warning: odd-length list in synth_diff_pairs" ; flush stdout;
		  | [] -> ()
		in
		  synth_diff_pairs ts;
		  pprintf "\n\n Done in testWalk\n\n"; flush stdout

module TemplateDP =
struct
  type t = init_template 
  let template_ht = hcreate 10 

  let to_string it = itemplate_to_str it (* this is just one change, not sets of changes! Remember that!*)

  let distance it1 it2 = 
	let hash1,hash2 = itemplate_to_str it1, itemplate_to_str it2 in
	ht_find template_ht (hash1,hash2) 
	  (fun _ ->
		let synth = unify_itemplate it1 it2 in
		let i = Objsize.objsize synth in 
		  float_of_int(i.Objsize.data))
end

module ChangesDP =
struct
  type t = init_template list
  let templates_ht = hcreate 10
  let to_string it = lst_str itemplate_to_str it 
	
  let distance its1 its2 = 
	let hash1,hash2 = to_string its1, to_string its2 in
	  ht_find templates_ht (hash1,hash2) 
		(fun _ ->
			let best_map = 
			  lmap (fun (t1,t2) -> unify_itemplate t1 t2)
				(best_permutation template_distance its1 its2) in
			  float_of_int (lfoldl (fun total -> fun t -> total + (Objsize.objsize t).Objsize.data) 0 best_map))

end
				
			  
