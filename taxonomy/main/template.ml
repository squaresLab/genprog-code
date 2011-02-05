open Batteries
open List
open Utils
open Cabs
open Cprint
open Globals
open Difftypes
open Treediff

type guard = EXPG of expression node 
			 | OPP of expression node 
			 | CATCH of expression node 
			 | ATLEAST of expression list 
			 | NOTHING 
			 | STAR
 
type 'a comp = { ele : 'a; mutable comp : string }

let mk_comp ele fn = {ele=ele;comp=fn ele}

type context = 
	{
	  parent_treenode : tree_node node option;
	  parent_definition : definition node option;
	  parent_statement : statement node option;
	  parent_expression : expression node option;
	  surrounding : dummyNode Set.t;
	  guarded_by: guard list;
	  guarding: dummyNode Set.t;
	}

let make_context tn def s e sur gby ging = 
  {
	  parent_treenode=tn;
	  parent_definition=def;
	  parent_statement=s;
	  parent_expression=e;
	  surrounding=sur;
	  guarded_by=gby;
	  guarding=ging;
  }

type template = context * changes

let get_opt pfunc = function
    Some(o) -> pfunc o
  | None -> pprintf "None"

let print_template (con,changes) =
  pprintf "*****Context*****\n";
  pprintf "parent_treenode: ";
  get_opt (dumpTreeNode defaultCabsPrinter (Pervasives.stdout)) con.parent_treenode;
  pprintf "\nparent_definition: ";
  get_opt (dumpDefinition defaultCabsPrinter (Pervasives.stdout)) con.parent_definition;
  pprintf "\nparent_statement: ";
  get_opt (dumpStmt defaultCabsPrinter (Pervasives.stdout) 0) con.parent_statement;
  pprintf "\nparent_expression: ";
  get_opt (dumpExpression defaultCabsPrinter (Pervasives.stdout) 0) con.parent_expression;
  pprintf "\nsurrounding: ";
  Set.iter (fun dumNode -> print_dummy_node dumNode; pprintf "\n") con.surrounding;
  pprintf "\nguarded_by: ";
  liter 
	(fun guard -> 
	  (match guard with 
		EXPG(e) -> pprintf "EXPG: "; dumpExpression defaultCabsPrinter (Pervasives.stdout) 0 e
	  | OPP(e) -> pprintf "OPP: "; dumpExpression defaultCabsPrinter (Pervasives.stdout) 0 e
	  | CATCH(e) -> pprintf "CATCH: "; dumpExpression defaultCabsPrinter (Pervasives.stdout) 0 e
	  | NOTHING -> pprintf "NOTHING"
	  );
	  pprintf "\n")
	con.guarded_by;
  pprintf "\nguarding: ";
  Set.iter (fun dumNode -> Difftypes.print_dummy_node dumNode; pprintf "\n") con.guarding;
  pprintf "*****END CONTEXT*****\n";
  pprintf "*****CHANGES*****\n";
  liter (fun change -> pprintf "%s\n" (standard_eas_to_str2 change)) changes;
  pprintf "*****END CHANGES*****\n"; flush stdout 

(* a template is one change to one location in code, meaning a treediff converts
   into a list of templates *)

let tfold ts c fn =
  lfoldl
	(fun ts ->
	  fun ele -> fn ts c ele) ts

let tfold2 (ts : template list) (c : context) (fn1 : template list -> context -> 'a -> template list) 
	(fn2 : template list -> context -> 'b -> template list) (val1 : 'a) (val2 : 'b) =
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

let treediff_to_templates (tree1 : tree) (difftree1 : diff_tree_node) (tdiff : changes) : template list = 
  let context_ht = hcreate 10 in
  let add_to_context parent change = 
	let lst = ht_find context_ht parent (fun x -> []) in
	  hrep context_ht parent (change::lst)
  in
  let organized_changes change = 
 	match change with 
	| SInsert(inserted,Some(parent),location)
	| SInsertTree(inserted,Some(parent),location)
	| SMove(inserted,Some(parent),location) -> add_to_context parent.nid change
	| SDelete(deleted) ->
	  (match (parent_of difftree1 deleted) with
		Some(parent) -> add_to_context parent.nid change
	  | None -> ())
	| SReplace(replacer,replacee) ->
	  (match (parent_of difftree1 replacee) with
		Some(parent) -> add_to_context parent.nid change
	  | None -> ())
	| _ -> failwith "Why are we inserting nowhere, anyway?"
  in
	liter organized_changes tdiff;
	let rec walk_treenode tlist initial_context tn =
	  let diff_tree_node = hfind cabs_id_to_diff_tree_node tn.id in
	  let dummy_node = diff_tree_node.original_node in
	  let tn_p = 
		match dummy_node with 
		  TREENODE(tn) -> Some(tn)
		| _ -> failwith "Expected a treenode dummy type, got something else"
	  in
	  let initial_context = {initial_context with parent_treenode = tn_p} in
	  let tlist = 
		if hmem context_ht diff_tree_node.nid then
		  (initial_context,hfind context_ht diff_tree_node.nid)::tlist
		else tlist
	  in
		match tn.node with
		| Globals(dlist) -> walk_defs tlist initial_context dlist
		| Stmts(slist) -> walk_stmts tlist initial_context slist 
		| Exps(elist) -> walk_exps tlist initial_context elist
		| _ -> failwith "I really should get rid of the syntax tree node type since I don't use it."
	and wattr ts c (str,elist) = walk_exps ts c elist
	and wattrs ts c attrs = tfold ts c wattr attrs
	and wname ts c (str,dt,attrs,_) = tfold2 ts c wdt wattrs dt attrs
	and wfg ts c (spec, lst) = 
	  let ts' = wspecs ts c spec in
		lfoldl
		  (fun ts ->
			fun (name,eno) ->
			  let ts' = wname ts c name in 
				match eno with
				  None -> ts'
				| Some(e) -> walk_exp ts' c e)
		  ts' lst
	and wfgs ts c fgs = tfold ts c wfg fgs
	and wei ts c lst = tfold_d ts c walk_exp (fun (str,en,_) -> en) lst
	and wiw ts c = function
	NEXT_INIT -> ts
	  | INFIELD_INIT(str,iw) -> wiw ts c iw
	  | ATINDEX_INIT(en,iw) -> tfold2 ts c walk_exp wiw en iw
	  | ATINDEXRANGE_INIT(e1,e2) -> tfold2 ts c walk_exp walk_exp e1 e2
	and wsn ts c (spec,name) = tfold2 ts c wspecs wname spec name
	and wsns ts c sns = tfold ts c wsn sns
	and wie ts c = function
	NO_INIT -> ts
	  | SINGLE_INIT(en) -> walk_exp ts c en 
	  | COMPOUND_INIT (iwies) ->
		lfoldl
		  (fun templates ->
			fun (iw,ie) ->
			  let ts' = wiw ts c iw in
				wie ts' c ie) ts iwies
	and wspecs ts c specs = 
	  lfoldl 
		(fun ts ->
		  fun se -> 
			match se with
			  SpecAttr(attr) -> wattr ts c attr
			| SpecType(tys) ->
			  (match tys with (* FIXME: strings *)
			  | Tunion(str, Some(fgo), attrs) 
			  | Tstruct(str,Some(fgo),attrs) -> tfold2 ts c wfgs wattrs fgo attrs
			  | Tenum(str,None,attrs)
			  | Tunion(str,None, attrs) 
			  | Tstruct(str,None,attrs) -> wattrs ts c attrs
			  | Tenum(str,Some(eno),attrs) ->
				tfold2 ts c wei wattrs eno attrs
			  | TtypeofE(en) -> walk_exp ts c en
			  | TtypeofT(spec,dt) -> tfold2 ts c wspecs wdt spec dt
			  | _ -> ts)
			| _ -> ts) ts specs
	and wdt ts c dt = 
	  match dt with
	  | PARENTYPE(attrs1,dt,attrs2) -> 
		tfold3 ts c wattrs wdt wattrs attrs1 dt attrs2
	  | ARRAY(dt,attrs,en) ->
		tfold3 ts c wdt wattrs walk_exp dt attrs en
	  | PTR(attrs,dt) -> tfold2 ts c wattrs wdt attrs dt
	  | PROTO(dt,sns,_) -> tfold2 ts c wdt wsns dt sns
	  | _ -> ts
	and win ts c (nme,ie) = tfold2 ts c wname wie nme ie
	and wins ts c ins = tfold ts c win ins
	and wing ts c (spec,ins) = tfold2 ts c wspecs wins spec ins
	and wnames ts c nms = tfold ts c wname nms
	and wng ts c (spec,names) = tfold2 ts c wspecs wnames spec names
	and newc c lst = {c with surrounding = Set.union c.surrounding (Set.of_enum (List.enum lst))}
	and walk_stmts t c lst = 
	  tfold t (newc c (make_dum_stmt lst)) walk_stmt lst
	and walk_exps t c lst = 
	  tfold t (newc c (make_dum_exp lst)) walk_exp lst
	and walk_defs t c lst = 
	  tfold t (newc c (make_dum_def lst)) walk_def lst
	and walk_stmt ts c stmt =
	  let diff_tree_node = hfind cabs_id_to_diff_tree_node stmt.id in
	  let stmt_p = 
		match diff_tree_node.original_node with
		  STMT(s) -> Some(s )
		| _ -> failwith "Expected statement dummyNode, got something else"
	  in
	  let c = {c with parent_statement=stmt_p} in
	  let ts = 
		if hmem context_ht diff_tree_node.nid then (* FIXME: guarding needs to be changed! I think *)
		  (c,hfind context_ht diff_tree_node.nid) ::ts
		else ts
	  in
	  let wexp = walk_exp ts c in
	  let wstmt = walk_stmt ts c in
	  let wdef = walk_def ts c in 
	  let wstmts = walk_stmts ts c in
		match stmt.node with
		| COMPUTATION(expn,_) -> wexp expn
		| BLOCK(blk,_) -> wstmts blk.bstmts
		| SEQUENCE(s1,s2,_) -> wstmts [s1;s2]
		| IF(e1,s1,s2,_) -> 
		  let context1 = {c with guarding = Set.union (Set.singleton (STMT(s1))) c.guarding} in
		  let context_t = {context1 with guarded_by = (EXPG(e1)::context1.guarded_by)} in
		  let context_f = {context1 with guarded_by = (OPP(e1)::context1.guarded_by)} in
		  let ts' = walk_exp ts context1 e1 in
		  let ts'' = walk_stmt ts' context_t s1 in
			walk_stmt ts'' context_f s2
		| WHILE(e1,s1,_) 
		| DOWHILE(e1,s1,_) -> 
		  let context_e = {c with guarding = (Set.union (Set.singleton (STMT(s1))) c.guarding)} in
		  let context_s = {c with guarded_by = (EXPG(e1)::c.guarded_by)} in
		  let ts' = walk_exp ts context_e e1 in
			walk_stmt ts' context_s s1
		| FOR(fc,e1,e2,s1,_) ->
		  let context_e2 = {c with guarding=(Set.union (Set.singleton (STMT(s1))) c.guarding)} in
		  let context_s1 = {c with guarded_by=(EXPG(e1)::c.guarded_by)} in
		  let ts' = match fc with
			  FC_EXP(e3) -> wexp e3
			| FC_DECL(d) -> wdef d
		  in
		  let ts'' = walk_exp ts' c e1 in
		  let ts''' = walk_exp ts'' context_e2 e2 in
			walk_stmt ts''' context_s1 s1
		| RETURN(e1,_) -> wexp e1 
		| SWITCH(e1,s1,_) -> tfold2 ts c walk_exp walk_stmt e1 s1
		| CASE(e1,s1,_) -> tfold2 ts c walk_exp walk_stmt e1 s1
		  (* FIXME: check the "guarded" and "guarded_by" on the case statements *)
		| CASERANGE(e1,e2,s1,_) -> tfold3 ts c walk_exp walk_exp walk_stmt e1 e2 s1
		| DEFAULT(s1,_) -> wstmt s1 
		(* FIXME: walking the strings? *)
		| LABEL(str,s1,_) -> wstmt s1 
		| GOTO(str,_) -> ts (* FIXME *)
		| COMPGOTO(e1,_) ->	wexp e1
		| DEFINITION(def) -> wdef def
		| ASM(alist,strs,asdets,_) -> 
		  let ts' = wattrs ts c alist in
		  let asmdets ts = 
			lfoldl
			  (fun ts ->
				fun (so,s,en) ->
				  walk_exp ts c en)
			  ts in
			(match asdets with Some(asdets) -> 
			let ts'' = asmdets ts' asdets.aoutputs in
			  asmdets ts'' asdets.ainputs
			| None -> ts')
		| TRY_EXCEPT(b1,e1,b2,_)->
		  let ts' =  wstmts b1.bstmts in
		  let context_e1 = {c with guarding=Set.of_enum (List.enum (make_dum_stmt b2.bstmts))} in
		  let ts'' = walk_exp ts' context_e1 e1 in
		  let context_s1 = {c with guarded_by = (CATCH(e1)::c.guarded_by)} in
			walk_stmts ts'' context_s1 b2.bstmts
		| TRY_FINALLY(b1,b2,_) -> tfold2 ts c walk_stmts walk_stmts b1.bstmts b2.bstmts
		| _ -> ts
	and walk_def ts c def =
	  let diff_tree_node = hfind cabs_id_to_diff_tree_node def.id in
	  let def_p = match diff_tree_node.original_node with
		  DEF(d) -> Some( d)
		| _ -> failwith "Expected def dummyNode, found something else"
	  in
	  let c = {c with parent_definition=def_p} in
	  let ts = 
		if hmem context_ht diff_tree_node.nid then
		  (c,hfind context_ht diff_tree_node.nid) :: ts
		else ts
	  in
	  let wexp = walk_exp ts c in
		match def.node with
		  FUNDEF(sn,b,_,_) -> walk_stmts ts c b.bstmts
		| DIRECTIVE(dn) -> ts (* FIXME *)
		| DECDEF(ing,_) -> wing ts c ing
		| TYPEDEF(ng,_) -> wng ts c ng
		| ONLYTYPEDEF(spec,_) -> wspecs ts c spec
		| GLOBASM(_) -> ts
		| PRAGMA(en,_) -> wexp en
		| LINKAGE(_,_,dlist) -> walk_defs ts c dlist 
	and walk_exp ts c exp =
	  let diff_tree_node = hfind cabs_id_to_diff_tree_node exp.id in
	  let exp_p = 
		match diff_tree_node.original_node with
		  EXP(e) -> Some( e)
		| _ -> failwith "Expected expression dummyNode, got something else"
	  in
	  let c = {c with parent_expression=exp_p} in
	  let ts = 
		if hmem context_ht diff_tree_node.nid then
		  (c,hfind context_ht diff_tree_node.nid) ::ts
		else ts
	  in
	  let wstmts = walk_stmts ts c in
	  let wexp = walk_exp ts c in
		match exp.node with
		| MEMBEROF(e1,str)  (* FIXME: strs *)
		| MEMBEROFPTR(e1,str) -> wexp e1
		| UNARY(_,e1)
		| EXPR_ALIGNOF(e1)
		| PAREN(e1) 
		| EXPR_SIZEOF(e1) -> wexp e1
		| LABELADDR(str) -> ts (* FIXME *) 
		| INDEX(e1,e2)
		| BINARY(_,e1,e2) -> tfold2 ts c walk_exp walk_exp e1 e2
		| QUESTION(e1,e2,e3) -> tfold2 ts c walk_exp walk_exp e1 e2
		| CAST((spec,dt),ie) -> tfold3 ts c wspecs wdt wie spec dt ie
		| CALL(e1,elist) -> tfold2 ts c walk_exp walk_exps e1 elist
		| COMMA(elist) -> walk_exps ts c elist
		| VARIABLE(str) -> ts (* FIXME *)
		| TYPE_SIZEOF(spec,dt) 
		| TYPE_ALIGNOF(spec,dt) -> tfold2 ts c wspecs wdt spec dt
		| GNU_BODY(b) -> wstmts b.bstmts
		| _ -> ts
	in
	let (fname,tns) = tree1 in
	let initial_context = make_context None None None None (Set.empty) [] (Set.empty) in
	  lfoldl
		(fun ts -> 
		  fun tn -> (* FIXME: this is wrong in the handling of befores and afters; deal with teh fact that we can start in stmts or something *)
			walk_treenode ts initial_context tn) []
		tns
  
(* location can be a function, right, that takes changes and applies them to the
   right part of the tree *)

(* but then how do you compare them? *)

(*let unify_change (c1 : changes) (c2 : changes) : change = failwith "Not implemented"
let unify_context (c1 : context) (c2 : context) : context = failwith "Not implemented"
  
let unify_dummyNode (node1: dummyNode) (node2: dummyNode) : dummyNode =
  let unify_treenode (tn1 : tree_node node) (tn2 : tree_node node) : tree_node node = failwith "Not implemented" in
  let unify_treenodes (tns1 : tree_node node list) (tns2 : tree_node node list) : tree_node node list = failwith "Not implemented" in
  let unify_statement (s1 : statement node) (s2 : statement node) : statement node = failwith "Not implemented" in
  let unify_expression (e1 : expression node) (e2 : expression node) : expression node = failwith "Not implemented" in
  let unify_definition (d1 : definition node) (d2 : definition node) : definition node = failwith "Not implemented" in
  let unify_string (s1: string) (s2: string) : string = failwith "Not implemented" in
	match node1,node2 with
	| TREE(t1), TREE(t2) ->
	  let fname1,tlist1 = t1 in 
	  let fname2,tlist2 = t2 in
		TREE(fname1^"."^fname2, unify_treenodes t1 t2)
	| TREE(t1), _ -> failwith "Not implemented"
	| STMT(s1), STMT(s2) -> STMT(unify_statement s1 s2)
	| STMT(s1), _ -> failwith "Not implemented"
	| EXP(e1), EXP(e2) -> EXP(unify_expression e1 e2)
	| EXP(e1), _ -> failwith "Not implemented"
	| TREENODE(tn1),TREENODE(tn2) -> TREENODE(unify_treenode tn1 tn2)
	| TREENODE(tn1), _ -> failwith "Not implemented"
	| DEF(def1),DEF(def2) -> DEF(unify_definition def1 def2)
	| DEF(def1),_ -> failwith "Not implemented"
(*	| STRING(str1),STRING(str2) 
	| STRING(str1),_ *)
	| STAR, STAR -> STAR
	| _ -> failwith "Impossible nodes node in unify_dummyNode"
*)
let unify_guards gset1 gset2 inter = 
  (* given list1 of N elements and list2 of M elements, there are M choose N
	 permutations of list2 that map to list1.  Each permutation has a cost.  We
	 want the lowest one.  Now the thing is, is it the case that if we have a
	 permutation and we make one swap and the cost is lower than the original,
	 then that is definitely a win? *)
	
	
(*  let unify_template (t1 : template) (t2 : template) : template = 
	let changes1,context1 = t1 in
	let changes2,context2 = t2 in*)
	  failwith "Not implemented"

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
		liter (fun temp -> print_template temp) ts;
		pprintf "\n\n Done in test_template\n\n"; flush stdout
