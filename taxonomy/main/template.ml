open Batteries
open List
open Utils
open Cabs
open Cprint
open Globals
open Difftypes
open Treediff

type guard = EXPG of expression node | OPP of expression node | CATCH of expression node(*SOMETHING of expression list | NOTHING*)
 
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
	| CATCH(e) -> pprintf "CATCH: "; dumpExpression defaultCabsPrinter (Pervasives.stdout) 0 e);
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

let tfold (ts : template list) (cons : context) (fn : template list -> context -> 'a -> template list) =
  lfoldl
	(fun templates ->
	  fun ele -> fn templates cons ele) ts
		
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
	let rec walk_treenode templates initial_context tn =
	  let diff_tree_node = hfind cabs_id_to_diff_tree_node tn.id in
	  let dummy_node = diff_tree_node.original_node in
	  let tn_p = 
		match dummy_node with 
		  TREENODE(tn) -> Some(tn)
		| _ -> failwith "Expected a treenode dummy type, got something else"
	  in
	  let initial_context = {initial_context with parent_treenode = tn_p} in
	  let templates = 
		if hmem context_ht diff_tree_node.nid then
		  (initial_context,hfind context_ht diff_tree_node.nid)::templates
		else templates
	  in
		match tn.node with
		| Globals(dlist) -> walk_defs templates initial_context dlist
		| Stmts(slist) -> walk_stmts templates initial_context slist 
		| Exps(elist) -> walk_exps templates initial_context elist
		| _ -> failwith "I really should get rid of the syntax tree node type since I don't use it."
	and wattr templates context (str,elist) =
	  walk_exps templates context elist
	and wattrs templates context attrs = 
	  tfold templates context wattr attrs
	and wname templates context (str,dt,attrs,_) = 
	  let templates' = wdt templates context dt in
		wattrs templates' context attrs
	and wfg templates context (spec, lst) = 
	  let templates' = wspecs templates context spec in
		lfoldl
		  (fun templates ->
			fun (name,eno) ->
			  let templates' = wname templates context name in 
				match eno with
				  None -> templates'
				| Some(e) -> walk_exp templates' context e)
		  templates' lst
	and wfgs templates context fgs = tfold templates context wfg fgs
	and wei templates context lst = 
	  lfoldl
		(fun templates ->
		  fun (str,en,_) ->
			walk_exp templates context en) templates lst 
	and wiw templates context = function
	NEXT_INIT -> templates
	  | INFIELD_INIT(str,iw) -> wiw templates context iw
	  | ATINDEX_INIT(en,iw) ->
		let templates' = walk_exp templates context en in
		  wiw templates' context iw
	  | ATINDEXRANGE_INIT(e1,e2) ->
		let templates' = walk_exp templates context e1 in
		  walk_exp templates' context e2
	and wsn templates context (spec,name) =
	  let templates' = wspecs templates context spec in 
		wname templates' context name
	and wsns templates context sns = 
	  tfold templates context wsn sns
	and wie templates context = function
	NO_INIT -> templates
	  | SINGLE_INIT(en) -> walk_exp templates context en 
	  | COMPOUND_INIT (iwies) ->
		lfoldl
		  (fun templates ->
			fun (iw,ie) ->
			  let templates' = wiw templates context iw in
				wie templates' context ie) templates iwies
	and wspecs templates context specs = 
	  lfoldl 
		(fun templates ->
		  fun se -> 
			match se with
			  SpecAttr(attr) -> wattr templates context attr
			| SpecType(ts) ->
			  (match ts with (* FIXME: strings *)
			  | Tunion(str, Some(fgo), attrs) 
			  | Tstruct(str,Some(fgo),attrs) ->
				let templates' = wfgs templates context fgo in 
				  wattrs templates' context attrs
			  | Tenum(str,None,attrs)
			  | Tunion(str,None, attrs) 
			  | Tstruct(str,None,attrs) -> wattrs templates context attrs
			  | Tenum(str,Some(eno),attrs) ->
				let templates' = wei templates context eno in 
				  wattrs templates' context attrs
			  | TtypeofE(en) -> walk_exp templates context en
			  | TtypeofT(spec,dt) ->
				let templates' = wspecs templates context spec in 
				  wdt templates' context dt
			  | _ -> templates)
			| _ -> templates) templates specs
	and wdt templates context dt = 
	  match dt with
	  | PARENTYPE(attrs1,dt,attrs2) ->
		let templates' = wattrs templates context attrs1 in 
		let templates'' = wdt templates' context dt in
		  wattrs templates'' context attrs2
	  | ARRAY(dt,attrs,en) ->
		let templates' = wdt templates context dt in
		let templates'' = wattrs templates' context attrs in 
		  walk_exp templates'' context en
	  | PTR(attrs,dt) -> 
		let templates' = wattrs templates context attrs in 
		  wdt templates' context dt
	  | PROTO(dt,sns,_) ->
		let templates' = wdt templates context dt in
		  wsns templates' context sns
	  | _ -> templates
	and win templates context (nme,ie) = 
	  let templates' = wname templates context nme in 
		wie templates' context ie
	and wins templates context ins = tfold templates context win ins
	and wing templates context (spec,ins) =
	  let templates' = wspecs templates context spec in
		wins templates' context ins
	and wng templates context (spec,names) = 
	  let templates' = wspecs templates context spec in
		tfold templates' context wname names
	and newc c lst = {c with surrounding = Set.union c.surrounding (Set.of_enum (List.enum lst))}
	and walk_stmts t c lst = 
	  tfold t (newc c (make_dum_stmt lst)) walk_stmt lst
	and walk_exps t c lst = 
	  tfold t (newc c (make_dum_exp lst)) walk_exp lst
	and walk_defs t c lst = 
	  tfold t (newc c (make_dum_def lst)) walk_def lst
	and walk_stmt templates context stmt =
	  let diff_tree_node = hfind cabs_id_to_diff_tree_node stmt.id in
	  let stmt_p = 
		match diff_tree_node.original_node with
		  STMT(s) -> Some(s) 
		| _ -> failwith "Expected statement dummyNode, got something else"
	  in
	  let context = {context with parent_statement=stmt_p} in
	  let templates = 
		if hmem context_ht diff_tree_node.nid then (* FIXME: guarding needs to be changed! I think *)
		  (context,hfind context_ht diff_tree_node.nid) ::templates
		else templates
	  in
	  let wexp = walk_exp templates context in
	  let wstmt = walk_stmt templates context in
	  let wdef = walk_def templates context in 
	  let wstmts = walk_stmts templates context in
		match stmt.node with
		| COMPUTATION(expn,_) -> wexp expn
		| BLOCK(blk,_) -> wstmts blk.bstmts
		| SEQUENCE(s1,s2,_) -> wstmts [s1;s2]
		| IF(e1,s1,s2,_) -> 
		  let context1 = {context with guarding = Set.union (Set.singleton (STMT(s1))) context.guarding} in
		  let context_t = {context1 with guarded_by = (EXPG(e1)::context1.guarded_by)} in
		  let context_f = {context1 with guarded_by = (OPP(e1)::context1.guarded_by)} in
		  let templates' = walk_exp templates context_t e1 in
		  let templates'' = walk_stmt templates' context_t s1 in
			walk_stmt templates'' context_f s2
		| WHILE(e1,s1,_) 
		| DOWHILE(e1,s1,_) -> 
		  let context_e = {context with guarding = (Set.union (Set.singleton (STMT(s1))) context.guarding)} in
		  let context_s = {context with guarded_by = (EXPG(e1)::context.guarded_by)} in
		  let templates' = walk_exp templates context_e e1 in
			walk_stmt templates' context_s s1
		| FOR(fc,e1,e2,s1,_) ->
		  let context_e2 = {context with guarding=(Set.union (Set.singleton (STMT(s1))) context.guarding)} in
		  let context_s1 = {context with guarded_by=(EXPG(e1)::context.guarded_by)} in
		  let templates' = match fc with
			  FC_EXP(e3) -> wexp e3
			| FC_DECL(d) -> wdef d
		  in
		  let templates'' = walk_exp templates' context e1 in
		  let templates''' = walk_exp templates'' context_e2 e2 in
			walk_stmt templates''' context_s1 s1
		| RETURN(e1,_) -> wexp e1 
		| SWITCH(e1,s1,_) ->
		  let templates' = wexp e1 in
			walk_stmt templates' context s1 
		| CASE(e1,s1,_) -> 
		  (* FIXME: check the "guarded" and "guarded_by" on the case statements *)
		  let templates' = wexp e1 in
			walk_stmt templates' context s1
		| CASERANGE(e1,e2,s1,_) ->
		  let templates' = wexp e1 in
		  let templates'' = walk_exp templates' context e2 in
			walk_stmt templates'' context s1
		| DEFAULT(s1,_) -> wstmt s1 
		(* FIXME: walking the strings? *)
		| LABEL(str,s1,_) -> wstmt s1 
		| GOTO(str,_) -> templates (* FIXME *)
		| COMPGOTO(e1,_) ->	wexp e1
		| DEFINITION(def) -> wdef def
		| ASM(alist,strs,asdets,_) -> 
		  let templates' = wattrs templates context alist in
		  let asmdets templates = 
			lfoldl
			  (fun templates ->
				fun (so,s,en) ->
				  walk_exp templates context en)
			  templates in
			(match asdets with Some(asdets) -> 
			let templates'' = asmdets templates' asdets.aoutputs in
			  asmdets templates'' asdets.ainputs
			| None -> templates')
		| TRY_EXCEPT(b1,e1,b2,_)->
		  let templates' =  wstmts b1.bstmts in
		  let context_e1 = {context with guarding=Set.of_enum (List.enum (make_dum_stmt b2.bstmts))} in
		  let templates'' = walk_exp templates' context_e1 e1 in
		  let context_s1 = {context with guarded_by = (CATCH(e1)::context.guarded_by)} in
			walk_stmts templates'' context_s1 b2.bstmts
		| TRY_FINALLY(b1,b2,_) ->
		  let templates' = wstmts b1.bstmts in 
			walk_stmts templates' context b2.bstmts 
		| _ -> templates
	and walk_def templates context def =
	  let diff_tree_node = hfind cabs_id_to_diff_tree_node def.id in
	  let def_p = match diff_tree_node.original_node with
		  DEF(d) -> Some(d) 
		| _ -> failwith "Expected def dummyNode, found something else"
	  in
	  let context = {context with parent_definition=def_p} in
	  let templates = 
		if hmem context_ht diff_tree_node.nid then
		  (context,hfind context_ht diff_tree_node.nid) :: templates
		else templates
	  in
	  let wexp = walk_exp templates context in
		match def.node with
		  FUNDEF(sn,b,_,_) -> walk_stmts templates context b.bstmts
		| DIRECTIVE(dn) -> templates (* FIXME *)
		| DECDEF(ing,_) -> wing templates context ing
		| TYPEDEF(ng,_) -> wng templates context ng
		| ONLYTYPEDEF(spec,_) -> wspecs templates context spec
		| GLOBASM(_) -> templates
		| PRAGMA(en,_) -> wexp en
		| LINKAGE(_,_,dlist) -> tfold templates context walk_def dlist  (* FIXME: add to "surrounding" *)
	and walk_exp templates context exp =
	  let diff_tree_node = hfind cabs_id_to_diff_tree_node exp.id in
	  let exp_p = 
		match diff_tree_node.original_node with
		  EXP(e) -> Some(e) 
		| _ -> failwith "Expected expression dummyNode, got something else"
	  in
	  let context = {context with parent_expression=exp_p} in
	  let templates = 
		if hmem context_ht diff_tree_node.nid then
		  (context,hfind context_ht diff_tree_node.nid) ::templates
		else templates
	  in
	  let wstmts = walk_stmts templates context in
	  let wexp = walk_exp templates context in
		match exp.node with
		| MEMBEROF(e1,str)  (* FIXME: strs *)
		| MEMBEROFPTR(e1,str) -> wexp e1
		| UNARY(_,e1)
		| EXPR_ALIGNOF(e1)
		| PAREN(e1) 
		| EXPR_SIZEOF(e1) -> wexp e1
		| LABELADDR(str) -> templates (* FIXME *) 
		| INDEX(e1,e2)
		| BINARY(_,e1,e2) ->
		  let templates' = wexp e1 in
			walk_exp templates' context e2
		| QUESTION(e1,e2,e3) ->
		  let templates' = wexp e1 in
		  let templates'' = walk_exp templates' context e2 in
			walk_exp templates'' context e2
		| CAST((spec,dt),ie) -> 
		  let templates' = wspecs templates context spec in
		  let templates'' = wdt templates' context dt in 
			wie templates'' context ie
		| CALL(e1,elist) ->
		  let templates' = wexp e1 in 
			walk_exps templates' context elist
		| COMMA(elist) -> walk_exps templates context elist
		| VARIABLE(str) -> templates (* FIXME *)
		| TYPE_SIZEOF(spec,dt) 
		| TYPE_ALIGNOF(spec,dt) ->
		  let templates' = wspecs templates context spec in
			wdt templates' context dt
		| GNU_BODY(b) -> wstmts b.bstmts
		| _ -> templates
	in
	let (fname,tns) = tree1 in
	let initial_context = make_context None None None None (Set.empty) [] (Set.empty) in
	  lfoldl
		(fun templates -> 
		  fun tn -> (* FIXME: this is wrong in the handling of befores and afters; deal with teh fact that we can start in stmts or something *)
			walk_treenode templates initial_context tn) []
		tns
  
(* location can be a function, right, that takes changes and applies them to the
   right part of the tree *)

(* but then how do you compare them? *)

let unify_change (c1 : changes) (c2 : changes) : change = failwith "Not implemented"
let unify_context (c1 : context) (c2 : context) : context = failwith "Not implemented"
  
(*let unify_dummyNode (node1: dummyNode) (node2: dummyNode) : dummyNode =
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

let unify_template (t1 : template) (t2 : template) : template = 
  let changes1,context1 = t1 in
  let changes2,context2 = t2 in
	failwith "Not implemented"*)

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
	  let templates = treediff_to_templates (diff1,old_file_tree)  tree patch in
		liter (fun temp -> print_template temp) templates;
	  pprintf "\n\n Done in test_template\n\n"; flush stdout
