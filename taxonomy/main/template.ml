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

(* FIXME: add to utils *)
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
	pprintf "after organized changes\n"; flush stdout;
	let rec walk_treenode tn arounds guarded_by templates =
	  let diff_tree_node = hfind cabs_id_to_diff_tree_node tn.id in
		pprintf "walk_treenode tl: %s\n" diff_tree_node.tl_str; flush stdout;
	  let dummy_node = diff_tree_node.original_node in
	  let tn_p = 
		match dummy_node with 
		  TREENODE(tn) -> Some(tn)
		| _ -> failwith "Expected a treenode dummy type, got something else"
	  in
	  let templates = 
		if hmem context_ht diff_tree_node.nid then
		  ((make_context tn_p None None None arounds guarded_by (Set.empty)),hfind context_ht diff_tree_node.nid)::templates
		else templates
	  in
		match tn.node with
		| Globals(dlist) ->
		  lfoldl
			(fun templates -> 
			  fun def ->
				walk_def def tn_p None None (Set.union (Set.of_enum (List.enum (make_dum_def dlist))) arounds) guarded_by (Set.empty) templates)
			templates dlist
		| Stmts(slist) -> walk_stmt_list slist tn_p None None arounds guarded_by (Set.empty)  templates
		| Exps(elist) ->
		  lfoldl 
			(fun templates -> 
			  fun exp ->
				walk_exp exp tn_p None None (Set.union (Set.of_enum (List.enum (make_dum_exp elist))) arounds) [] (Set.empty) templates)
			templates elist
		| _ -> failwith "I really should get rid of the syntax tree node type since I don't use it."

	and walk_stmt_list slist tn_p def_p exp_p arounds guarded_by guarding templates =
	  pprintf "in walk_stmt_list\n"; flush stdout;
	  let arounds = Set.union arounds (Set.of_enum (List.enum (make_dum_stmt slist))) in
	  lfoldl 
		(fun templates -> 
		  fun stmt ->
			walk_stmt stmt tn_p def_p exp_p arounds guarded_by guarding templates)
		templates slist

	and walk_stmt sn tn_p def_p exp_p (arounds :dummyNode Set.t) (guarded_by : guard list) (guarding : dummyNode Set.t) templates = 
	  pprintf "In walk_stmt\n"; flush stdout;
	  let catchify exp = failwith "Catchify not implemented" in
	  let diff_tree_node = hfind cabs_id_to_diff_tree_node sn.id in
	  let stmt_p = 
		match diff_tree_node.original_node with
		  STMT(s) -> Some(s) 
		| _ -> failwith "Expected statement dummyNode, got something else"
	  in
	  let templates = 
		if hmem context_ht diff_tree_node.nid then (* FIXME: guarding needs to be changed! I think *)
		  (make_context tn_p def_p stmt_p exp_p arounds guarded_by  (Set.empty),hfind context_ht diff_tree_node.nid) ::templates
		else templates
	  in
	  let wdef def = walk_def def tn_p stmt_p exp_p arounds guarded_by guarding templates in
	  let wstmt ?guarded_by:(guarded_by=guarded_by) ?templates:(templates=templates) stmt = walk_stmt stmt tn_p def_p exp_p arounds guarded_by guarding templates in
	  let wstmts ?guarded_by:(guarded_by=guarded_by) ?templates:(templates=templates) slist = walk_stmt_list slist tn_p def_p exp_p arounds guarded_by guarding templates in
	  let wexp ?guarding:(guarding=guarding) ?templates:(templates=templates) exp = walk_exp exp tn_p def_p stmt_p arounds guarded_by guarding templates in

		match sn.node with
		| COMPUTATION(expn,_) -> wexp expn
		| BLOCK(blk,_) -> wstmts blk.bstmts
		| SEQUENCE(s1,s2,_) -> wstmts [s1;s2]
		| IF(e1,s1,s2,_) -> 
		  let templates' =
			wexp ~guarding:(Set.union (Set.singleton (STMT(s1))) guarding) e1 in
		  let templates'' =
			wstmt ~guarded_by:((EXPG(e1))::guarded_by) ~templates:templates' s1 
		  in
			wstmt ~guarded_by:((OPP(e1))::guarded_by) ~templates:templates'' s2
		| WHILE(e1,s1,_) 
		| DOWHILE(e1,s1,_) -> 
		  let templates' = wexp ~guarding:(Set.union (Set.singleton (STMT(s1))) guarding) e1 in
			wstmt ~guarded_by:(EXPG(e1)::guarded_by) ~templates:templates' s1
		| FOR(fc,e1,e2,s1,_) ->
		  let templates' = match fc with
			  FC_EXP(e3) -> failwith "Not implemented"
			| FC_DECL(d) -> failwith "Not implemented"
		  in
		  let templates'' = wexp e1 ~templates:templates' in
			
		  let templates''' = wexp ~templates:templates'' ~guarding:(Set.union (Set.singleton (STMT(s1))) guarding) e2 in
			wstmt ~guarded_by:(EXPG(e1)::guarded_by) ~templates:templates''' s1 
		| RETURN(e1,_) -> wexp e1 
		| SWITCH(e1,s1,_) ->
		  let templates' = wexp e1 in
			wstmt ~templates:templates' s1 
		| CASE(e1,s1,_) -> 
		  (* FIXME: check the "guarded" and "guarded_by" on the case statements *)
		  let templates' = wexp e1 in
			wstmt ~templates:templates' s1 
		| CASERANGE(e1,e2,s1,_) ->
		  let templates' = wexp e1 in
		  let templates'' = wexp ~templates:templates' e2 in
			wstmt ~templates:templates'' s1
		| DEFAULT(s1,_) -> wstmt s1 
		(* FIXME: walking the strings? *)
		| LABEL(str,s1,_) -> wstmt s1 
		| GOTO(str,_) -> templates (* FIXME *)
		| COMPGOTO(e1,_) ->	wexp e1
		| DEFINITION(def) -> wdef def
		| ASM(slist,strs,asdets,_) -> failwith "Screw ASM"
		| TRY_EXCEPT(b1,e1,b2,_)->
		  let templates' =  wstmts b1.bstmts in
		  let templates'' = wexp ~guarding:(Set.of_enum (List.enum (make_dum_stmt b2.bstmts))) ~templates:templates' e1 in
			wstmts ~guarded_by:((catchify e1)::guarded_by) ~templates:templates'' b2.bstmts
		| TRY_FINALLY(b1,b2,_) ->
		  let templates' = wstmts b1.bstmts in 
			wstmts ~templates:templates' b2.bstmts 
		| _ -> templates
	and walk_def def tn_p stmt_p exp_p (arounds : dummyNode Set.t) (guarded_by : guard list) (guarding : dummyNode Set.t) templates = 
	  pprintf "in walk def\n"; flush stdout;
	  let diff_tree_node = hfind cabs_id_to_diff_tree_node def.id in
	  let def_p = match diff_tree_node.original_node with
		  DEF(d) -> Some(d) 
		| _ -> failwith "Expected def dummyNode, found something else"
	  in
	  let templates = 
		if hmem context_ht diff_tree_node.nid then
		  (make_context tn_p def_p stmt_p exp_p arounds guarded_by guarding,hfind context_ht diff_tree_node.nid) :: templates
		else templates
	  in
		failwith "Not implemented"
	(*		  match def.node with
			  FUNDEF of single_name * block * cabsloc * cabsloc
			  | DIRECTIVE of directive node
			  | DECDEF of init_name_group * cabsloc        (* global variable(s), or function prototype *)
			  | TYPEDEF of name_group * cabsloc
			  | ONLYTYPEDEF of specifier * cabsloc
			  | GLOBASM of string * cabsloc
			  | PRAGMA of expression node * cabsloc
			  | LINKAGE of string * cabsloc * definition node list (* extern "C" { ... } *)*)
	and walk_exp exp tn_p def_p stmt_p arounds guarded_by guarding templates = 
	  pprintf "In walk_exp: %d\n" exp.id; flush stdout;
	  let diff_tree_node = hfind cabs_id_to_diff_tree_node exp.id in
		pprintf "Found it?\n"; flush stdout;
	  let exp_p = 
		match diff_tree_node.original_node with
		  EXP(e) -> Some(e) 
		| _ -> failwith "Expected expression dummyNode, got something else"
	  in
	  let templates = 
		if hmem context_ht diff_tree_node.nid then
		  (make_context tn_p def_p stmt_p exp_p arounds guarded_by guarding,hfind context_ht diff_tree_node.nid) ::templates
		else templates
	  in
	  let wdef def = walk_def def tn_p stmt_p exp_p arounds guarded_by guarding templates in
	  let wstmt ?guarded_by:(g=guarded_by) ?templates:(t=templates) stmt = walk_stmt stmt tn_p def_p exp_p arounds guarded_by guarding templates in
	  let wstmts ?guarded_by:(g=guarded_by) ?templates:(t=templates) slist = walk_stmt_list slist tn_p def_p exp_p arounds guarded_by guarding templates in
	  let wexp ?guarding:(g=guarding) ?templates:(t=templates) exp = walk_exp exp tn_p def_p stmt_p arounds guarded_by guarding templates in

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
			wexp ~templates:templates' e2
		| QUESTION(e1,e2,e3) ->
		  let templates' = wexp e1 in
		  let templates'' = wexp ~templates:templates' e2 in
			wexp ~templates:templates'' e2
		(*		| CAST of (specifier * decl_type) * init_expression
				| CALL of expression node * expression node list
				| COMMA of expression node list
				| VARIABLE(str) -> templates (* FIXME *)
				| TYPE_SIZEOF of specifier * decl_type
				| TYPE_ALIGNOF of specifier * decl_type
				| GNU_BODY of block*)
		| _ -> templates
	in
	let (fname,tns) = tree1 in
		lfoldl
		  (fun templates -> 
			fun tn -> (* FIXME: this is wrong in the handling of befores and afters; deal with teh fact that we can start in stmts or something *)
			  walk_treenode tn (Set.empty) [] templates) []
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
