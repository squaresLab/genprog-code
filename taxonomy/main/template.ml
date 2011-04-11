open Batteries
open List
open Utils
open Cabs
open Cabsvisit
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
open Datapoint
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


let num_tbl_exp = hcreate 10 (* FIXME: I don't know if we need this ultimately but whatever *)
let num_tbl_stmt = hcreate 10 (* FIXME: I don't know if we need this ultimately but whatever *)
let num_tbl_def = hcreate 10 (* FIXME: I don't know if we need this ultimately but whatever *)
let num_tbl_tn = hcreate 10 (* FIXME: I don't know if we need this ultimately but whatever *)

class numVisitor = object(self)
  inherit nopCabsVisitor

  method vexpr e = hadd num_tbl_exp e.id e; DoChildren   
  method vstmt s = hadd num_tbl_stmt s.id s; DoChildren
  method vdef d = hadd num_tbl_def d.id d; DoChildren
  method vtreenode t = hadd num_tbl_tn t.id t; DoChildren
end

class contextConvertWalker initial_context context_ht = object (self)
  inherit [init_template list] singleCabsWalker

  val mutable context = initial_context 
  val context_ht = context_ht

  method default_res () = []

  method combine res1 res2 = res1 @ res2

  method wTreenode tn =
	let temp = context in
	let ts = 
	  if hmem context_ht tn.id then 
		[(context,hfind context_ht tn.id)] 
	  else [] 
	in
	  match tn.node with
		MODSITE(num) -> Result(ts)
		  (* FIXME: question, is it the case that all modsites are marked as such? If so, we can lose the ts thing above *)
	  | NODE(node) -> begin
		match node with
		| Globals(dlist) ->
		  let defs = lmap (fun def -> def.id) dlist in 
			context <- {context with surrounding = Set.union context.surrounding (Set.of_enum (List.enum defs))};
			let res = (lflat (lmap self#walkDefinition dlist)) @ ts in
			  context <- temp; Result(res)
		| Stmts(slist) ->
		  let stmts = lmap (fun stmt -> stmt.id) slist in 
			context <- {context with surrounding = Set.union context.surrounding (Set.of_enum (List.enum stmts))};
			let res = (lflat (lmap self#walkStatement slist)) @ ts in
			  context <- temp; Result(res)
		| Exps(elist) -> 
		  let exps = lmap (fun exp -> exp.id) elist in
			context <- {context with surrounding = Set.union context.surrounding (Set.of_enum (List.enum exps))};
			let res = (lflat (lmap self#walkExpression elist)) @ ts in
			  context <- temp; Result(res)
	  end

  method wBlock block = (* FIXME: this doesn't handle attributes at all *)
	let temp = context in
	  context <- {context with surrounding = Set.of_enum (List.enum (lmap (fun stmt -> stmt.id) block.bstmts)) } ;
	  let res = lflat (lmap self#walkStatement block.bstmts) in
		context <- temp; Result(res)

  method wStatement stmt = 
	let temp = context in 
	  context <- {context with parent_statement = Some(stmt) } ;
	  let ts = 
		if hmem context_ht stmt.id then 
		  [(context,hfind context_ht stmt.id)]
		else []
	  in
		match dn stmt with
		| IF(e1,s1,s2,_) -> 
		  let temp = context in
			context <-  {context with guarding = Set.union (Set.singleton s1.id) context.guarding};
			let res1 = self#walkExpression e1 in
			  context <-  {temp with guarded_by = ((EXPG,e1)::temp.guarded_by)};
			  let res2 = self#walkStatement s1 in 
				context <- {temp with guarded_by = ((EXPG, nd(UNARY(NOT, e1)))::temp.guarded_by)};
				let res3 = self#walkStatement s2 in
				  context <- temp; Result(res1 @ res2 @ res3 @ ts)
		| WHILE(e1,s1,_) 
		| DOWHILE(e1,s1,_) -> 
		  let temp = context in
			context <- {context with guarding = (Set.union (Set.singleton s1.id) context.guarding)};
			let res1 = self#walkExpression e1 in
			  context <-  {temp with guarded_by = ((EXPG,e1)::temp.guarded_by)};
			  let res2 = self#walkStatement s1 in
				context <- temp; Result(res1 @ res2 @ ts)
		| FOR(fc,e1,e2,s1,_) ->
		  let res1 = 
			match fc with 
			  FC_EXP(e) -> self#walkExpression e1 
			| FC_DECL(def) -> self#walkDefinition def 
		  in 
		  let temp = context in 
			context <-  {context with guarding=Set.union (Set.singleton s1.id) context.guarding};
			let res2 = self#walkExpression e2 in
			  context <- {temp with guarded_by=((EXPG,e1)::context.guarded_by)};
			  let res3 = self#walkStatement s1 in
				context <- temp; Result(res1 @ res2 @ res3 @ ts)
		(* FIXME: check the "guarded" and "guarded_by" on the case statements *)
		| TRY_EXCEPT(b1,e1,b2,_)->
		  let res1 = self#walkBlock b1 in
		  let temp = context in 
			context <-  {context with guarding=Set.of_enum (List.enum (lmap (fun s -> s.id) b2.bstmts))};
			let res2 = self#walkExpression e1 in 
			  context <-  {temp with guarded_by = ((CATCH,e1)::context.guarded_by)};
			  let res3 = self#walkBlock b2 in
				context <- temp; Result(res1 @ res2 @ res3 @ ts)
		| _ -> CombineChildrenPost(ts, (fun res -> context <- temp; res))
		  

  method wExpression exp = 
	let temp = context in 
	  context <- {context with parent_expression=Some(exp)};
	  let ts = 
		if hmem context_ht exp.id then
		  [(context,hfind context_ht exp.id)]
		else []
	  in
		CombineChildrenPost(ts, (fun res -> context <- temp; res))

  method wDefinition def =
	let temp = context in 
	  context <- {context with parent_definition=Some(def)};
	let ts = 
	  if hmem context_ht def.id then 
		[(context,hfind context_ht def.id)]
	  else []
	in
	  CombineChildrenPost(ts,(fun res -> context <- temp; res) )
end

(*let alpha = new alphaRenameWalker*)

let treediff_to_templates (tree1 : tree) (tdiff : changes) =
  let context_ht = hcreate 10 in
  let add_to_context parent change = 
	let lst = ht_find context_ht parent (fun x -> []) in
	  hrep context_ht parent (change::lst)
  in
  let organized_changes change =
	match snd change with
	| InsertTreeNode _
	| ReorderTreeNode _
	| ReplaceTreeNode _ -> failwith "deal with this"
	| InsertDefinition(_,par,_,_) | ReplaceDefinition(_,_,par,_,_)
	| MoveDefinition(_,par,_,_,_) | ReorderDefinition(_,par,_,_,_)
	| InsertStatement(_,par,_,_) | ReplaceStatement(_,_,par,_,_)
	| MoveStatement(_,par,_,_,_) | ReorderStatement(_,par,_,_,_)
	| InsertExpression(_,par,_,_) | ReplaceExpression(_,_,par,_,_)
	| MoveExpression(_,par,_,_,_) | ReorderExpression(_,par,_,_,_)
	| DeleteTN (_,par) | DeleteDef (_,par) 
	| DeleteStmt (_,par) | DeleteExp (_,par) -> add_to_context par change
  in
	liter organized_changes tdiff;
	let initial_context = 
	  make_icontext None None None (Set.empty) [] (Set.empty) in (*alpha_map3 in *)
	let con_convert = new contextConvertWalker initial_context context_ht in
	  con_convert#walkTree tree1
  
let changes_ht = hcreate 10

class changesDoubleWalker = object(self)
  inherit templateDoubleWalker as super

  method private distance_change = 
	distance (fun change -> edit_str (snd change)) print_change_gen self#walkChange

  method wChange (change1,change2) = failwith "Not implemented"
(*	let hash1,hash2 =  change1, standard_eas_to_str change2 in
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
			| _,_ -> (*pprintf "wChangeWalker comparison not yet implemented, returning star";*) Result(ChangeLifted(STAR)))*)

  method walkChange ((change1,change2) : (int * edit) * (int * edit) ) = 
	doWalk compare self#wChange 
	  (fun (d1,d2) -> failwith "We shouldn't be calling children on changes!")  (change1,change2)

  method walkChanges (changes1,changes2) = 
	ht_find changes_ht (changes1,changes2)
	  (fun _ -> 
		let res = 
		lmap
		  (fun (c1,c2) ->
(*			pprintf "best mapping 1: %s ----> %s\n" (standard_eas_to_str c1) (standard_eas_to_str c2); flush stdout;*)
			self#walkChange (c1,c2)) (best_mapping ~print:(fun c -> pprintf "%s, " (edit_str (snd c))) self#distance_change changes1 changes2)
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
	  con.surrounding
	  (lmap (fun (g,e) -> (g,EXPBASE(e))) con.guarded_by) 
	  con.guarding
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
(*  let lst1 = List.of_enum (Set.enum (context1.surrounding)) in
  let lst2 = List.of_enum (Set.enum (context2.surrounding)) in
  let distance_dummy_node =
	distance dummy_node_to_str print_dummy_gen mycontext#walkDummyNode in
  let permut = best_mapping distance_dummy_node lst1 lst2 in*)
  let surrounding' = Set.empty (* FIXME *) in
(*	Set.of_enum (List.enum (lmap (fun (s1,s2) -> mycontext#walkDummyNode (s1,s2)) permut))*)
  let guarding' = Set.empty (* FIXME*) in
(*	Set.of_enum (List.enum (lmap (fun (s1,s2) -> mycontext#walkDummyNode (s1,s2)) 
							  (best_mapping distance_dummy_node 
								 (List.of_enum (DumSet.enum context1.guarding))
								 (List.of_enum (DumSet.enum context2.guarding)))))
  in*)
  let changes' = mycontext#walkChanges (changes1,changes2) in
	{ pdef = parent_definition';
	 pstmt = parent_statement';
	 pexp = parent_expression';
	 sding = surrounding';
	 gby = guards';
	 ging = guarding';
			(*			 renamed = Map.empty;*)
	}, changes'

let vector_tbl = hcreate 10 
let init_template_tbl : (int, (init_template * int * string)) Hashtbl.t = hcreate 10

let diffs_to_templates (big_diff_ht) (outfile : string) (load : bool) =
  if load then 
	let fin = open_in_bin outfile in
	let res1 = Marshal.input fin in 
	  hiter (fun k -> fun v -> hadd vector_tbl k v) res1; 
	  close_in fin; res1
  else begin
	let count = ref 0 in
	let all_vecs = 
	  lflat (hfold
		(fun diffid ->
		  fun diff ->
			fun lst ->
			  pprintf "Processing diffid %d\n" diffid;
			  let vecs = 
				lflat (lmap
						 (fun change -> 
						   pprintf "count %d, change %d: %s, " (Ref.post_incr count) change.changeid change.syntactic; 
						   if (llen change.treediff) > 0 then begin 
						   let modsites = 
							 lmap (fun (_,edit) ->   
							   match edit with
							   | InsertTreeNode _
							   | ReorderTreeNode _
							   | ReplaceTreeNode _ -> -1
							   | InsertDefinition(_,par,_,_) | ReplaceDefinition(_,_,par,_,_)
							   | MoveDefinition(_,par,_,_,_) | ReorderDefinition(_,par,_,_,_)
							   | InsertStatement(_,par,_,_) | ReplaceStatement(_,_,par,_,_)
							   | MoveStatement(_,par,_,_,_) | ReorderStatement(_,par,_,_,_)
							   | InsertExpression(_,par,_,_) | ReplaceExpression(_,_,par,_,_)
							   | MoveExpression(_,par,_,_,_) | ReorderExpression(_,par,_,_,_)
							   | DeleteTN (_,par) | DeleteDef (_,par) 
							   | DeleteStmt (_,par) | DeleteExp (_,par) -> par
							 ) change.treediff in
						   let vectors = Vectors.template_to_vectors change.old_tree change.new_tree modsites change.treediff in
							 pprintf "id %d\n" vectors.VectPoint.vid; 
							 hadd vector_tbl change.changeid vectors; (*vectors.VectPoint.change @ *)lmap (fun v -> vectors.VectPoint.vid,v) vectors.VectPoint.context
						   end else []) diff.changes) 
			  in
				vecs :: lst) big_diff_ht []) in
	  pprintf "printing out\n"; flush stdout;
	  let fout = open_out_bin outfile in
		Marshal.output fout vector_tbl;  close_out fout; vector_tbl,all_vecs 
  end

let test_template files =
  pprintf "Test template!\n"; flush stdout;
  let diffs = Treediff.test_mapping files in
  let retval = 
	lmap
	  (fun (tree1,tree2,patch) ->
		pprintf "Generating a diff:\n";
		liter print_edit patch; 
		pprintf "Templatizing:\n";
		let modsites = 
		  lmap (fun (_,edit) ->   
			match edit with
			| InsertTreeNode _
			| ReorderTreeNode _
			| ReplaceTreeNode _ -> -1
			| InsertDefinition(_,par,_,_) | ReplaceDefinition(_,_,par,_,_)
			| MoveDefinition(_,par,_,_,_) | ReorderDefinition(_,par,_,_,_)
			| InsertStatement(_,par,_,_) | ReplaceStatement(_,_,par,_,_)
			| MoveStatement(_,par,_,_,_) | ReorderStatement(_,par,_,_,_)
			| InsertExpression(_,par,_,_) | ReplaceExpression(_,_,par,_,_)
			| MoveExpression(_,par,_,_,_) | ReorderExpression(_,par,_,_,_)
			| DeleteTN (_,par) | DeleteDef (_,par) 
			| DeleteStmt (_,par) | DeleteExp (_,par) -> par
		  ) patch in
		  tree1,tree2,modsites,patch
		(*		  let ts = treediff_to_templates ("",tree) patch in
				  lmap (fun temp -> print_itemplate temp; tree,temp) ts*)
	  ) diffs
  in
	pprintf "\n\n Done in test_template\n\n"; flush stdout;
	retval

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
	let patch,_ = tree_diff_cabs hd1 hd2 "test_diff_change" in
	  ((f1,hd1),patch) :: (cabs_diff_pairs tl)
	| [(f2,hd2)] -> pprintf "Warning: odd-length snippet list in test_diff_change: %s\n" f2; flush stdout; []
	| [] -> [] in
	pprintf "Step 2: diff pairs of files\n"; flush stdout; 
	let diffs = cabs_diff_pairs parsed in 
	  pprintf "Step 2a: printing diffs from pairs of files\n"; flush stdout;
	  liter (fun (x,z) -> pprintf "A DIFF:\n\n"; liter print_edit z; pprintf "\nEND A DIFF\n\n"; flush stdout) diffs; flush stdout;
	  pprintf "Templatizing. Difflist %d length:\n" (llen diffs);
	  let count = ref 0 in
	  let temp_ht = hcreate 10 in
		liter
		  (fun (tree,patch) ->
			let temps = treediff_to_templates tree patch in
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

