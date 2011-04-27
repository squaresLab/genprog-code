open Batteries
open Set
open List
open Utils
open Cabs
open Cabsvisit
open Cprint
open Globals
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

class getNames ht = object(self)
  inherit [StringSet.t] singleCabsWalker

  val name_ht = ht

  method default_res () = StringSet.empty
  method combine set1 set2 = StringSet.union set1 set2 

  method wExpression exp =
	let set =
	  match dn exp with (* FIXME: constants? *)
	  | LABELADDR(str)
	  | VARIABLE(str)
	  | EXPR_PATTERN(str) -> StringSet.singleton str
	  | _ -> StringSet.empty
	in
	  CombineChildrenPost(set,
						  (fun children -> hrep name_ht exp.id (StringSet.union set children); children))

  method wStatement stmt = 
	let set = 
	  match dn stmt with
	  | LABEL(str,_,_)
	  | GOTO(str,_) -> StringSet.singleton str
	  | _ -> StringSet.empty
	in
	CombineChildrenPost(set, (fun children -> hrep name_ht stmt.id (StringSet.union set children); children))

  method wDefinition def = 
	let set = 
	  match dn def with
	  | GLOBASM(str,_) -> StringSet.singleton str 
	  | _ -> StringSet.empty
	in
	CombineChildrenPost(set, (fun children -> hrep name_ht def.id (StringSet.union set children); children))

  method wName name = 
	let str,_,_,_ = name in 
	  CombineChildren(StringSet.singleton str)
end

class getGuards ht = object(self)
  inherit [unit] singleCabsWalker

  val guard_ht = ht
  val guard_set = ref Set.empty

  val switch_exp = ref None
  val cases = ref None

  method default_res () = ()
  method combine one two = ()

  method wStatement stmt = 
    hadd guard_ht stmt.id !guard_set;
    match dn stmt with
    | IF(e1,s1,s2,_) ->
	let old_set = !guard_set in
	  guard_set := Set.add (EXPG,e1) !guard_set;
	  self#walkStatement s1;
	  guard_set := Set.add (EXPG, nd(UNARY(NOT, e1))) old_set;
	  self#walkStatement s2;
	  guard_set := old_set; Result()
    | WHILE(e1,s1,_)
    | DOWHILE(e1,s1,_) ->
	let old_set = !guard_set in
	  guard_set := Set.add (EXPG,e1) !guard_set;
	  self#walkStatement s1;
	  guard_set := Set.add (EXPG, nd(UNARY(NOT, e1))) old_set;
	  Result()
  | FOR(fc,e1,e2,s1,_) ->
      (match fc with | FC_EXP(e) -> self#walkExpression e1
	  | FC_DECL(def) -> self#walkDefinition def);
      let old_set = !guard_set in 
	guard_set := Set.add (EXPG,e1) !guard_set;
	self#walkStatement s1;
	guard_set := Set.add (EXPG, nd(UNARY(NOT, e1))) old_set;
	Result()
  | SWITCH(e1,s1,_) ->
      let old_set = !guard_set in 
      let old_switch = !switch_exp in
      let old_cases = !cases in
	switch_exp := Some(e1);
	self#walkStatement s1;
	guard_set := old_set;
	switch_exp := old_switch;
	cases := old_cases;
	Result()
  | CASE(e1,s1,_) ->
	let old_guards = !guard_set in 
	let switch_exp =
	  match !switch_exp with
		None -> nd(VARIABLE("unknown"))
	  | Some(exp) -> exp in
	let this_case_guard = nd(BINARY(EQ,switch_exp,e1)) in
	let case_guard = 
	  match !cases with
		None -> this_case_guard
	  | Some(not_case) -> nd(BINARY(AND,not_case,this_case_guard))
	in
	  guard_set := Set.add (CASEG,case_guard) !guard_set;
	  self#walkStatement s1;
	  let not_this_case_guard = nd(UNARY(NOT,this_case_guard)) in
		(match !cases with
		  None -> cases := (Some(not_this_case_guard))
		| Some(exp) -> cases := (Some(nd(BINARY(AND,exp,not_this_case_guard)))));
		guard_set := old_guards;
		Result ()
  | CASERANGE(e1,e2,s1,_) ->
	let old_guards = !guard_set in 
	let switch_exp =
	  match !switch_exp with
		None -> nd(VARIABLE("unknown"))
	  | Some(exp) -> exp in
	let this_case_guard = 
	  nd(BINARY(AND,nd(BINARY(GE,switch_exp,e1)), 
				nd(BINARY(LE,switch_exp,e2)))) in
	let case_guard = 
	  match !cases with
		None -> this_case_guard
	  | Some(not_case) -> nd(BINARY(AND,not_case,this_case_guard))
	in
	  guard_set := Set.add (CASEG,case_guard) !guard_set;
	  self#walkStatement s1;
	  let not_this_case_guard = nd(UNARY(NOT,this_case_guard)) in
		(match !cases with
		  None -> cases := (Some(not_this_case_guard))
		| Some(exp) -> cases := (Some(nd(BINARY(AND,exp,not_this_case_guard)))));
		guard_set := old_guards;
		Result ()
  | DEFAULT(s1,_) ->
	(match !cases with
	  Some(exp) ->
		let old_guard_set = !guard_set in
		  guard_set := Set.add (CASEG,exp) !guard_set;
		  self#walkStatement s1;
		  guard_set := old_guard_set;
	| None -> self#walkStatement s1); Result()
  | TRY_EXCEPT(b1,e1,b2,_) ->
	self#walkBlock b1;
	let old_guards = !guard_set in
	  guard_set := Set.add (CATCH,e1) !guard_set;
	  self#walkBlock b2;
	  guard_set := old_guards;
	  Result ()
  | _ -> Children

  method wExpression exp = hadd guard_ht exp.id !guard_set; Children
  method wDefinition def = hadd guard_ht def.id !guard_set; Children
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

let unify_itemplate (t1 : init_template) (t2 : init_template) =
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

let init_template_tbl : (int, template) Hashtbl.t = hcreate 10

class numPrinter = object
  inherit nopCabsVisitor

  method vexpr exp = pprintf "EXP: ((%d:%s))\n" exp.id (exp_str exp); DoChildren
  method vstmt stmt = pprintf "STMT: ((%d:%s))\n" stmt.id (stmt_str stmt); DoChildren
  method vdef def = pprintf "DEF: ((%d:%s))\n" def.id (def_str def); DoChildren
  method vtreenode tn = pprintf "TN: ((%d:%s))\n" tn.id (tn_str tn); DoChildren
end

let diff_to_templates diff change (def : definition node) (tree : tree) =
  let patch = 
    lfilt
      (fun (_,edit) ->
	 match edit with
	 | InsertStatement _ | ReplaceStatement _
	 | MoveStatement _ | ReorderStatement _
	 | InsertExpression _ | ReplaceExpression _
	 | MoveExpression _ | ReorderExpression _
	 | DeleteStmt _ | DeleteExp _ -> true
	 | InsertDefinition(_,_,_,ptype) 
	 | ReplaceDefinition(_,_,_,_,ptype)
	 | MoveDefinition(_,_,_,_,_,ptype) when ptype <> PDEF && ptype <> PARENTTN -> true
	 | _ -> pprintf "WARNING/FIXME: unhandled edit operation."; false) change.treediff 
  in
  let linestart,lineend = 
    match dn def with
      FUNDEF(_,_,l1,l2) -> l1.lineno,l2.lineno
    | _ -> (-1),(-1)
  in
  let cfg_info,def1 = Cfg.ast2cfg def in 
  let pdg = Pdg.cfg2pdg cfg_info in
  let stmt_ht = hcreate 10 in
  let stmtvisit = new findStmtVisitor stmt_ht in
    ignore(visitCabsDefinition stmtvisit def);
    (* just group edits by statement, for now, and filter changes at the toplevel *)
    let stmts_and_edits = 
	  lmap
		(fun (stmt,edits) ->
		  match stmt with
			Some(stmt) -> stmt,edits
		  | None -> failwith "impossible match")
		(lfilt (fun (stmt,edits) -> match stmt with Some(_) -> true | None -> false)
		   (find_stmt_parents patch def)) 
	in 
(*      pprintf "pdg: %d\n" (llen pdg); flush stdout;*)
    let subgraphs = Pdg.interesting_subgraphs pdg in
(*      pprintf "subgraphs: %d\n" (llen subgraphs); flush stdout;*)
    let printer = new numPrinter in
	let guard_ht = hcreate 10 in
	let name_ht = hcreate 10 in
	let guard_walker = new getGuards guard_ht in
	let name_walker = new getNames name_ht in 
	let _ =
	  guard_walker#walkDefinition def;
	  ignore(name_walker#walkDefinition def);
 in
    let res = 
	  lmap
		(fun (stmt,edits) ->
		  guard_walker#walkStatement stmt;
		  ignore(name_walker#walkStatement stmt);
		  let guards = hfind guard_ht stmt.id in
		  let names = hfind name_ht stmt.id in
(*	         pprintf "Def: ";
		   ignore(visitCabsDefinition printer def);
	         pprintf "Stmt: ";
	         ignore(visitCabsStatement printer stmt);
		   pprintf "Edits: ";
		   liter print_edit edits;*)
	     let subgraph = 
	       if stmt.id > 1 then begin
		 let res = Pdg.relevant_to_context stmt.id pdg subgraphs  in res
	       end else begin
		 let positions = 
		   lmap 
		     (fun (num,edit) ->
			match edit with 
			| InsertDefinition(_,_,pos,_)
			| MoveDefinition(_,_,_,pos,_,_)
			| ReorderDefinition(_,_,_,pos,_)
			| InsertStatement(_,_,pos,_)
			| MoveStatement(_,_,_,pos,_,_)
			| ReorderStatement(_,_,_,pos,_)
			| InsertExpression(_,_,pos,_)
			| MoveExpression(_,_,_,pos,_,_)
			| ReorderExpression(_,_,_,pos,_)
			| DeleteDef(_,_,pos,_)
			| DeleteStmt(_,_,pos,_)
			| DeleteExp(_,_,pos,_) -> pos
			| _ -> failwith "Unexpected edit type in pick_subset") edits in
		 let min_pos = List.min positions in
		   match dn def with FUNDEF(_,b,_,_) -> 
		   let stmt = try List.nth b.bstmts min_pos with Invalid_argument _ -> List.hd (lrev b.bstmts) in
		   Pdg.relevant_to_context stmt.id pdg subgraphs
		   | _ -> [] (* FIXME: pick something random? *)
	       end
	     in
	       (* relevant to context returns a subset of pdg nodes per subgraph *)
	     let temp = {template_id = new_template () ; 
			 diff = diff;
			 change = change;
			 linestart = linestart;
			 lineend = lineend;
			 def = def;
			 stmt = Some(stmt); 
			 edits = edits;
			 names = names; 
			 guards = guards; 
			 subgraph = subgraph;} 
	     in  hadd init_template_tbl temp.template_id temp; temp
	) stmts_and_edits in 
	  res
      

let diffs_to_templates (big_diff_ht) (outfile : string) (load : bool) =
  if load then 
	let fin = open_in_bin outfile in
	let res1 = Marshal.input fin in 
	  hiter (fun k -> fun v -> hadd init_template_tbl k v) res1; 
	  close_in fin; res1
  else begin
	let count = ref 0 in
	let vector_fout = File.open_out "vectors.vec" in
	let all_vecs = 
	  hfold
		(fun diffid ->
		  fun diff ->
			fun lst ->
			  pprintf "Count: %d, processing diffid %d, rev_num: %d \n" (Ref.post_incr count) diffid diff.rev_num; flush stdout;
			  lfoldl
				(fun lst ->
				  fun change ->
				    pprintf "Change fname: %s, rev_num: %d\n" change.fname diff.rev_num; flush stdout;
					let res = diff_to_templates diff change change.tree ("",[nd(Globals([change.tree]))]) in
					let vectors = lmap Vectors.template_to_vectors res in
					  liter (Vectors.print_vectors vector_fout) vectors;
					lst @ res) 
				lst diff.changes)
		big_diff_ht [] in
	  close_out vector_fout;
	  pprintf "done all_vecs\n"; flush stdout;
	let fout = open_out_bin outfile in
	  Marshal.output fout ~closures:true init_template_tbl;  close_out fout; all_vecs 
  end

let test_template (files : string list) =
  let diffs = 
	lfilt
	  (fun (_,(defo,_),_) ->
		match defo with Some(d) -> true | None -> false)
	(Treediff.test_mapping files) in
  let diffs = lmap (fun (a,(defo,b),c)->
		match defo with Some(d) -> (a,(d,b),c) | None -> failwith "Impossible match") diffs
  in
    lfoldl
      (fun lst ->
	 fun (fname,(tree1,patch),info) ->
	   let change = Difftypes.new_change fname tree1 patch info [] in
	   let diff = Difftypes.new_diff (-1) "test template revision" [change] "test_template" in
	     lst @ (diff_to_templates diff change change.tree ("",[nd(Globals[tree1])]))
      ) [] diffs

let testWalker files = failwith "Not implemented" (*
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

												  *)
