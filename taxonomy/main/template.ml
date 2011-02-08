open Batteries
open List
open Utils
open Cabs
open Cprint
open Globals
open Cabswalker
open Difftypes
open Treediff
open Distance


let lexists = List.exists (* FIXME: add to utils *)

(* types for generalized AST nodes *)
 
type 'a lifted = STAR | SET of 'a lifted | ATLEAST of 'a list | LNOTHING | OPERATION_ON of 'a lifted | EXACT of 'a

(*type exp_gen = EXPBASE of expression node lifted
			   | GENUNARY of unary_operator lifted * exp_gen
			   | GENLABELADDR of string lifted
			   | GENBINARY of binary_operator * exp_gen node * exp_gen node
			   | GENQUEST of exp_gen * exp_gen * exp_gen
			   | GENCAST of (spec_gen * decl_type_gen) * init_expression_gen 
			   | GENCALL of exp_gen * exp_gen list
			   | GENCOMMA of exp_gen list 
			   | GENCONSTANT of gen_constant
			   | GENPAREN of exp_gen
			   | GENVAR of string
			   | GENEXPR_SIZEOF of exp_gen
			   | GENTYPE_SIZEOF of spec_gen * decl_type_gen 
			   | GENEXPR_ALIGNOF of exp_gen
			   | GENTYPE_ALIGNOF of spec_gen * decl_type_gen
			   | GENINDEX of exp_gen * exp_gen
			   | GENMEMBEROF of exp_gen * string 
			   | GENMEMBEROFPTR of exp_gen * string
			   | GENGNU_BODY of block_gen
			   | GENEXPR_PATTERN of string

let exp_lit exp = EXPBASE(EXACT(exp))*)

type stmt_gen = STMTBASE of statement node

type def_gen = DEFBASE of definition node

type tn_gen = TNBASE of tree_node node


type guard = EXPG of expression node 
			 | OPP of expression node 
			 | CATCH of expression node 
			 | ATLEAST of expression list 
			 | NOGUARD
			 | STAR

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
	  | NOGUARD -> pprintf "NOTHING"
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

let context_ht = hcreate 10

class convertWalker initial_context = object (self)
  inherit [template list] nopCabsWalker as super

  val mutable context = initial_context 

  method combine res1 res2 = res1 @ res2

  method wTreeNode tn =
	let diff_tree_node = hfind cabs_id_to_diff_tree_node tn.id in
	let dummy_node = diff_tree_node.original_node in
	  let tn_p = 
		match dummy_node with 
		  TREENODE(tn) -> Some(tn)
		| _ -> failwith "Expected a treenode dummy type, got something else"
	  in
		context <- {context with parent_treenode = tn_p};
	  let ts = 
		if hmem context_ht diff_tree_node.nid then
		  [(initial_context,hfind context_ht diff_tree_node.nid)]
		else []
	  in
	  let temp = context in
		match tn.node with
		| Globals(dlist) ->
		  let defs = make_dum_def dlist in 
			context <- {context with surrounding = Set.union context.surrounding (Set.of_enum (List.enum defs))};
			let res = 
			  lfoldl
				(fun result ->
				  fun def ->
					self#combine result (self#walkDefinition def) ) ts dlist in
			  context <- temp; Result(res)
		| Stmts(slist) ->
		  let stmts = make_dum_stmt slist in 
			context <- {context with surrounding = Set.union context.surrounding (Set.of_enum (List.enum stmts))};
			let res = 
			  lfoldl
				(fun result ->
				  fun stmt ->
					self#combine result (self#walkStatement stmt) ) ts slist in
			  context <- temp; Result(res)
		| Exps(elist) -> 
		  let exps = make_dum_exp elist in 
			context <- {context with surrounding = Set.union context.surrounding (Set.of_enum (List.enum exps))};
			let res = 
			  lfoldl
				(fun result ->
				  fun exp ->
					self#combine result (self#walkExpression exp) ) ts elist in
			  context <- temp; Result(res)
		| _ -> failwith "I really should get rid of the syntax tree node type since I don't use it."

  method wStatement stmt = 
	let diff_tree_node = hfind cabs_id_to_diff_tree_node stmt.id in
	let stmt_p = 
	  match diff_tree_node.original_node with
		STMT(s) -> Some(s )
	  | _ -> failwith "Expected statement dummyNode, got something else"
	in
	let ts = 
	  if hmem context_ht diff_tree_node.nid then
		[(context,hfind context_ht diff_tree_node.nid)]
	  else []
	in
	  context <- {context with parent_statement=stmt_p};

	match stmt.node with
	| IF(e1,s1,s2,_) -> 
	  let temp = context in
		context <-  {context with guarding = Set.union (Set.singleton (STMT(s1))) context.guarding};
		let res1 = self#walkExpression e1 in
		  context <-  {temp with guarded_by = (EXPG(e1)::temp.guarded_by)};
		  let res2 = self#walkStatement s1 in 
			context <- {temp with guarded_by = (OPP(e1)::temp.guarded_by)};
			let res3 = self#walkStatement s2 in
			  context <- temp; Result(self#combine res1 (self#combine res2 (self#combine res3 ts)))
	| WHILE(e1,s1,_) 
	| DOWHILE(e1,s1,_) -> 
	  let temp = context in
	  context <- {context with guarding = (Set.union (Set.singleton (STMT(s1))) context.guarding)};
		let res1 = self#walkExpression e1 in
		  context <-  {temp with guarded_by = (EXPG(e1)::temp.guarded_by)};
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
		  context <- {temp with guarded_by=(EXPG(e1)::context.guarded_by)};
		  let res3 = self#walkStatement s1 in
			context <- temp; Result(self#combine res1 (self#combine res2 (self#combine res3 ts)))
		(* FIXME: check the "guarded" and "guarded_by" on the case statements *)
	| TRY_EXCEPT(b1,e1,b2,_)->
	  let res1 = self#walkBlock b1 in
	  let temp = context in 
		context <-  {context with guarding=Set.of_enum (List.enum (make_dum_stmt b2.bstmts))};
		let res2 = self#walkExpression e1 in 
		context <-  {temp with guarded_by = (CATCH(e1)::context.guarded_by)};
	  let res3 = self#walkBlock b2 in
		context <- temp; Result(self#combine res1 (self#combine res2 (self#combine res3 ts)))
	| _ ->
		CombineChildren(ts)

  method wExpression expression = 
	let diff_tree_node = hfind cabs_id_to_diff_tree_node expression.id in
	let exp_p = 
	  match diff_tree_node.original_node with
		EXP(e) -> Some( e)
	  | _ -> failwith "Expected expression dummyNode, got something else"
	in
	let ts = 
	  if hmem context_ht diff_tree_node.nid then
		[(context,hfind context_ht diff_tree_node.nid)]
	  else []
	in
	  context <- {context with parent_expression=exp_p};
	  CombineChildren(ts)

  method wDefinition definition =
	let diff_tree_node = hfind cabs_id_to_diff_tree_node definition.id in
	let def_p = match diff_tree_node.original_node with
		  DEF(d) -> Some(d)
		| _ -> failwith "Expected def dummyNode, found something else"
	  in
	  let ts = 
		if hmem context_ht diff_tree_node.nid then
		  [(context,hfind context_ht diff_tree_node.nid)]
		else []
	  in
	  context <- {context with parent_definition=def_p};
		CombineChildren(ts)
end

let initial_context = make_context None None None None (Set.empty) [] (Set.empty) 

let my_convert = new convertWalker initial_context 

let treediff_to_templates (tree1 : tree) (difftree1 : diff_tree_node) (tdiff : changes) : template list = 
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
	my_convert#treeWalk tree1
  
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

class nothingWalker = object
  inherit [bool] nopCabsWalker

  method combine one two =  one || two
  method wStatement stmt = 
	match stmt.node with
	  NOP(_) -> Result(true)
	| _ -> CombineChildren(false)

  method wExpression exp = 
	match exp.node with
	  NOTHING -> Result(true)
	| _ -> CombineChildren (false)

  method wDefinition def = CombineChildren(false)

  method default_res () = false
end

let my_not = new nothingWalker
let rec unify_expression exp1 exp2 = 
  let expntg = my_not#walkExpression in
  let specntg = my_not#walkSpecifier in
  let dtntg = my_not#walkDeclType in 
  let expsntg = my_not#walkExpressionList in 
  let blkntg = my_not#walkBlock in 

  let at_least e = ATLEAST [e] in
  let nting = nd(NOTHING) in
	match exp1.node,exp2.node with
      NOTHING,NOTHING -> exp_lit exp1
  (* TODO/fixme: compare the strings in these expressions to nothing, or
	 similar? *)
	| NOTHING, UNARY(_,exp3) 
	| UNARY(_,exp3), NOTHING
	| NOTHING,MEMBEROF(exp3,_)
	| MEMBEROF(exp3,_), NOTHING 
	| NOTHING,MEMBEROFPTR(exp3,_)
	| MEMBEROFPTR(exp3,_), NOTHING 
	| NOTHING, EXPR_SIZEOF(exp3)
	| EXPR_SIZEOF(exp3), NOTHING 
	| NOTHING, EXPR_ALIGNOF(exp3)
	| EXPR_ALIGNOF(exp3), NOTHING 
	| NOTHING, PAREN(exp3)
	| PAREN(exp3), NOTHING -> if expntg exp3 then at_least nting else LNOTHING
	| NOTHING, INDEX(exp3,exp4)
	| INDEX(exp3,exp4), NOTHING 
	| NOTHING, BINARY(_,exp3,exp4)
	| BINARY(_,exp3,exp4), NOTHING -> if expntg exp3 || expntg exp4 then at_least nting else LNOTHING
	| NOTHING, QUESTION(exp3,exp4,exp5) 
	| QUESTION(exp3,exp4,exp5), NOTHING -> if expntg exp3 || expntg exp4 || expntg exp5 then at_least nting else LNOTHING
	| NOTHING, CAST((spec,dt), ie) 
	| CAST((spec,dt), ie), NOTHING -> if specntg spec || dtntg dt || ientg ie then at_least nting else LNOTHING
	| NOTHING, CALL(exp3,elist) 
	| CALL(exp3,elist), NOTHING -> if expntg exp3 || expsntg elist then at_least nting else LNOTHING
	| NOTHING, COMMA(elist)
	| COMMA(elist), NOTHING -> if expsntg elist then at_least nting else LNOTHING
	| NOTHING, LABELADDR(_)
	| LABELADDR(_), NOTHING 
	| NOTHING, EXPR_PATTERN(_)
	| EXPR_PATTERN(_), NOTHING 
	| NOTHING, VARIABLE(_)
	| VARIABLE(_), NOTHING
	| NOTHING, CONSTANT(_)
	| CONSTANT(_), NOTHING -> LNOTHING
	| NOTHING,TYPE_SIZEOF(spec,dt)
	| TYPE_SIZEOF(spec,dt), NOTHING 
	| NOTHING, TYPE_ALIGNOF(spec,dt)
	| TYPE_ALIGNOF(spec,dt), NOTHING -> if specntg spec || dtntg dt then at_least nting else LNOTHING
	| NOTHING, GNU_BODY(b)
	| GNU_BODY(b), NOTHING -> if blkntg b then at_least nting else LNOTHING

	| UNARY(uop1,exp3),UNARY(uop2,exp4) -> EXPBASE(UNARY(unify_uop uop1 uop2, unify_exp exp3 exp4))
(*	| UNARY(uop1,exp3), LABELADDR(str)
	| LABELADDR(str), UNARY(uop1,exp3) ->
	| UNARY(uop1,exp3), BINARY(bop,exp3,exp4)
	| BINARY(bop,exp3,exp4), UNARY(uop1,exp3) ->
	| UNARY(uop1,exp3), QUESTION(exp3,exp4,exp5) 
	| QUESTION(exp3,exp4,exp5), UNARY(uop1,exp3) ->
	| UNARY(uop1,exp3), CAST((spec,dt), ie) 
	| CAST((spec,dt), ie), UNARY(uop1,exp3) ->
	| UNARY(uop1,exp3), CALL(exp3,elist) 
	| CALL(exp3,elist), UNARY(uop1,exp3) ->
	| UNARY(uop1,exp3), COMMA(elist)
	| COMMA(elist), UNARY(uop1,exp3) ->
	| UNARY(uop1,exp3), CONSTANT(cn)
	| CONSTANT(cn), UNARY(uop1,exp3) ->
	| UNARY(uop1,exp3), PAREN(exp3) 
	| PAREN(exp3), UNARY(uop1,exp3) ->
	| UNARY(uop1,exp3), VARIABLE(str) 
	| VARIABLE(str), UNARY(uop1,exp3) ->
	| UNARY(uop1,exp3), EXPR_SIZEOF(exp) 
	| EXPR_SIZEOF(exp), UNARY(uop1,exp3) ->
	| UNARY(uop1,exp3),TYPE_SIZEOF(spec,dt) 
	| TYPE_SIZEOF(spec,dt), UNARY(uop1,exp3) ->
	| UNARY(uop1,exp3), EXPR_ALIGNOF(exp3) 
	| EXPR_ALIGNOF(exp3), UNARY(uop1,exp3) ->
	| UNARY(uop1,exp3), TYPE_ALIGNOF(spec,dt) 
	| TYPE_ALIGNOF(spec,dt), UNARY(uop1,exp3) ->
	| UNARY(uop1,exp3), INDEX(exp3,exp4) 
	| INDEX(exp3,exp4), UNARY(uop1,exp3) ->
	| UNARY(uop1,exp3),MEMBEROF(exp3,str) 
	| MEMBEROF(exp3,str), UNARY(uop1,exp3) ->
	| UNARY(uop1,exp3),MEMBEROFPTR(exp3,str) 
	| MEMBEROFPTR(exp3,str), UNARY(uop1,exp3) ->
	| UNARY(uop1,exp3), GNU_BODY(b) 
	| GNU_BODY(b), UNARY(uop1,exp3) ->
	| UNARY(uop1,exp3), EXPR_PATTERN(str) 
	| EXPR_PATTERN(str), UNARY(uop1,exp3) ->
	| LABELADDR(str1),LABELADDR(str2) -> LABELADDR(unify_string str1 str2)
	| LABELADDR(str1), BINARY(bop,exp3,exp4)
	| BINARY(bop,exp3,exp4), LABELADDR(str1) ->
	| LABELADDR(str1), QUESTION(exp3,exp4,exp5) 
	| QUESTION(exp3,exp4,exp5), LABELADDR(str1) ->
	| LABELADDR(str1), CAST((spec,dt), ie) 
	| CAST((spec,dt), ie), LABELADDR(str1) ->
	| LABELADDR(str1), CALL(exp3,elist) 
	| CALL(exp3,elist), LABELADDR(str1) ->
	| LABELADDR(str1), COMMA(elist)
	| COMMA(elist), LABELADDR(str1) ->
	| LABELADDR(str1), CONSTANT(cn)
	| CONSTANT(cn), LABELADDR(str1) ->
	| LABELADDR(str1), PAREN(exp3) 
	| PAREN(exp3), LABELADDR(str1) ->
	| LABELADDR(str1), VARIABLE(str) 
	| VARIABLE(str), LABELADDR(str1) ->
	| LABELADDR(str1), EXPR_SIZEOF(exp) 
	| EXPR_SIZEOF(exp), LABELADDR(str1) ->
	| LABELADDR(str1),TYPE_SIZEOF(spec,dt) 
	| TYPE_SIZEOF(spec,dt), LABELADDR(str1) ->
	| LABELADDR(str1), EXPR_ALIGNOF(exp3) 
	| EXPR_ALIGNOF(exp3), LABELADDR(str1) ->
	| LABELADDR(str1), TYPE_ALIGNOF(spec,dt) 
	| TYPE_ALIGNOF(spec,dt), LABELADDR(str1) ->
	| LABELADDR(str1), INDEX(exp3,exp4) 
	| INDEX(exp3,exp4), LABELADDR(str1) ->
	| LABELADDR(str1),MEMBEROF(exp3,str) 
	| MEMBEROF(exp3,str), LABELADDR(str1) ->
	| LABELADDR(str1),MEMBEROFPTR(exp3,str) 
	| MEMBEROFPTR(exp3,str), LABELADDR(str1) ->
	| LABELADDR(str1), GNU_BODY(b) 
	| GNU_BODY(b), LABELADDR(str1) ->
	| LABELADDR(str1), EXPR_PATTERN(str) 
	| EXPR_PATTERN(str), LABELADDR(str1) ->
	| BINARY(bop,exp3,exp4),BINARY(bop,exp3,exp4) -> 	
	  best_of(BINARY(bop,unify_exp exp3 exp5,unify_exp exp4 exp6) 
				BINARY(bop,unify_exp exp3 exp6, unify_exp exp4 exp5))
	| BINARY(bop,exp3,exp4), QUESTION(exp3,exp4,exp5) 
	| QUESTION(exp3,exp4,exp5), BINARY(bop,exp3,exp4) ->
	| BINARY(bop,exp3,exp4), CAST((spec,dt), ie) 
	| CAST((spec,dt), ie), BINARY(bop,exp3,exp4) ->
	| BINARY(bop,exp3,exp4), CALL(exp3,elist) 
	| CALL(exp3,elist), BINARY(bop,exp3,exp4) ->
	| BINARY(bop,exp3,exp4), COMMA(elist)
	| COMMA(elist), BINARY(bop,exp3,exp4) ->
	| BINARY(bop,exp3,exp4), CONSTANT(cn)
	| CONSTANT(cn), BINARY(bop,exp3,exp4) ->
	| BINARY(bop,exp3,exp4), PAREN(exp3) 
	| PAREN(exp3), BINARY(bop,exp3,exp4) ->
	| BINARY(bop,exp3,exp4), VARIABLE(str)
	| VARIABLE(str), BINARY(bop,exp3,exp4) ->
	| BINARY(bop,exp3,exp4), EXPR_SIZEOF(exp)
	| EXPR_SIZEOF(exp), BINARY(bop,exp3,exp4) ->
	| BINARY(bop,exp3,exp4),TYPE_SIZEOF(spec,dt)
	| TYPE_SIZEOF(spec,dt), BINARY(bop,exp3,exp4) ->
	| BINARY(bop,exp3,exp4), EXPR_ALIGNOF(exp3)
	| EXPR_ALIGNOF(exp3), BINARY(bop,exp3,exp4) ->
	| BINARY(bop,exp3,exp4), TYPE_ALIGNOF(spec,dt)
	| TYPE_ALIGNOF(spec,dt), BINARY(bop,exp3,exp4) ->
	| BINARY(bop,exp3,exp4), INDEX(exp3,exp4)
	| INDEX(exp3,exp4), BINARY(bop,exp3,exp4) ->
	| BINARY(bop,exp3,exp4),MEMBEROF(exp3,str)
	| MEMBEROF(exp3,str), BINARY(bop,exp3,exp4) ->
	| BINARY(bop,exp3,exp4),MEMBEROFPTR(exp3,str)
	| MEMBEROFPTR(exp3,str), BINARY(bop,exp3,exp4) ->
	| BINARY(bop,exp3,exp4), GNU_BODY(b)
	| GNU_BODY(b), BINARY(bop,exp3,exp4) ->
	| BINARY(bop,exp3,exp4), EXPR_PATTERN(str)
	| EXPR_PATTERN(str), BINARY(bop,exp3,exp4) ->
	| QUESTION(exp1,exp2,exp3), QUESTION(exp3,exp4,exp5) ->
	  QUESTION(unify_exp exp1 exp4,
			   unify_exp exp2 exp5,
			   unify_exp exp3 exp6)
	| QUESTION(exp1,exp2,exp3), CAST((spec,dt), ie) 
	| CAST((spec,dt), ie), QUESTION(exp1,exp2,exp3) ->
	| QUESTION(exp1,exp2,exp3), CALL(exp3,elist) 
	| CALL(exp3,elist), QUESTION(exp1,exp2,exp3) ->
	| QUESTION(exp1,exp2,exp3), COMMA(elist)
	| COMMA(elist), QUESTION(exp1,exp2,exp3) ->
	| QUESTION(exp1,exp2,exp3), CONSTANT(cn)
	| CONSTANT(cn), QUESTION(exp1,exp2,exp3) ->
	| QUESTION(exp1,exp2,exp3), PAREN(exp3)
	| PAREN(exp3), QUESTION(exp1,exp2,exp3) ->
	| QUESTION(exp1,exp2,exp3), VARIABLE(str)
	| VARIABLE(str), QUESTION(exp1,exp2,exp3) ->
	| QUESTION(exp1,exp2,exp3), EXPR_SIZEOF(exp)
	| EXPR_SIZEOF(exp), QUESTION(exp1,exp2,exp3) ->
	| QUESTION(exp1,exp2,exp3),TYPE_SIZEOF(spec,dt)
	| TYPE_SIZEOF(spec,dt), QUESTION(exp1,exp2,exp3) ->
	| QUESTION(exp1,exp2,exp3), EXPR_ALIGNOF(exp3)
	| EXPR_ALIGNOF(exp3), QUESTION(exp1,exp2,exp3) ->
	| QUESTION(exp1,exp2,exp3), TYPE_ALIGNOF(spec,dt)
	| TYPE_ALIGNOF(spec,dt), QUESTION(exp1,exp2,exp3) ->
	| QUESTION(exp1,exp2,exp3), INDEX(exp3,exp4)
	| INDEX(exp3,exp4), QUESTION(exp1,exp2,exp3) ->
	| QUESTION(exp1,exp2,exp3),MEMBEROF(exp3,str)
	| MEMBEROF(exp3,str), QUESTION(exp1,exp2,exp3) ->
	| QUESTION(exp1,exp2,exp3),MEMBEROFPTR(exp3,str)
	| MEMBEROFPTR(exp3,str), QUESTION(exp1,exp2,exp3) ->
	| QUESTION(exp1,exp2,exp3), GNU_BODY(b)
	| GNU_BODY(b), QUESTION(exp1,exp2,exp3) ->
	| QUESTION(exp1,exp2,exp3), EXPR_PATTERN(str)
	| EXPR_PATTERN(str), QUESTION(exp1,exp2,exp3) ->
	| CAST((spec1,dt1),ie1),CAST((spec,dt), ie) ->
	  CAST((unify_spec spec1 spec2, unify_dt dt1 dt2),
		   unify_ie ie1 ie2)
	| CAST((spec1,dt1),ie1), CALL(exp3,elist) 
	| CALL(exp3,elist), CAST((spec1,dt1),ie1) ->
	| CAST((spec1,dt1),ie1), COMMA(elist)
	| COMMA(elist), CAST((spec1,dt1),ie1) ->
	| CAST((spec1,dt1),ie1), CONSTANT(cn)
	| CONSTANT(cn), CAST((spec1,dt1),ie1) ->
	| CAST((spec1,dt1),ie1), PAREN(exp3) 
	| PAREN(exp3), CAST((spec1,dt1),ie1) ->
	| CAST((spec1,dt1),ie1), VARIABLE(str)
	| VARIABLE(str), CAST((spec1,dt1),ie1) ->
	| CAST((spec1,dt1),ie1), EXPR_SIZEOF(exp)
	| EXPR_SIZEOF(exp), CAST((spec1,dt1),ie1) ->
	| CAST((spec1,dt1),ie1),TYPE_SIZEOF(spec,dt)
	| TYPE_SIZEOF(spec,dt), CAST((spec1,dt1),ie1) ->
	| CAST((spec1,dt1),ie1), EXPR_ALIGNOF(exp3)
	| EXPR_ALIGNOF(exp3), CAST((spec1,dt1),ie1) ->
	| CAST((spec1,dt1),ie1), TYPE_ALIGNOF(spec,dt)
	| TYPE_ALIGNOF(spec,dt), CAST((spec1,dt1),ie1) ->
	| CAST((spec1,dt1),ie1), INDEX(exp3,exp4)
	| INDEX(exp3,exp4), CAST((spec1,dt1),ie1) ->
	| CAST((spec1,dt1),ie1),MEMBEROF(exp3,str)
	| MEMBEROF(exp3,str), CAST((spec1,dt1),ie1) ->
	| CAST((spec1,dt1),ie1),MEMBEROFPTR(exp3,str)
	| MEMBEROFPTR(exp3,str), CAST((spec1,dt1),ie1) ->
	| CAST((spec1,dt1),ie1), GNU_BODY(b)
	| GNU_BODY(b), CAST((spec1,dt1),ie1) ->
	| CAST((spec1,dt1),ie1), EXPR_PATTERN(str)
	| EXPR_PATTERN(str), CAST((spec1,dt1),ie1) ->
	| CALL(e1,elist1), CALL(exp3,elist) ->
	  CALL(unify_exp e1 e2,
		   unify_call_list elist1 elist2)
	| CALL(e1,elist1), COMMA(elist)
	| COMMA(elist), CALL(e1,elist1) ->
	| CALL(e1,elist1), CONSTANT(cn)
	| CONSTANT(cn), CALL(e1,elist1) ->
	| CALL(e1,elist1), PAREN(exp3)
	| PAREN(exp3), CALL(e1,elist1) ->
	| CALL(e1,elist1), VARIABLE(str)
	| VARIABLE(str), CALL(e1,elist1) ->
	| CALL(e1,elist1), EXPR_SIZEOF(exp)
	| EXPR_SIZEOF(exp), CALL(e1,elist1) ->
	| CALL(e1,elist1),TYPE_SIZEOF(spec,dt)
	| TYPE_SIZEOF(spec,dt), CALL(e1,elist1) ->
	| CALL(e1,elist1), EXPR_ALIGNOF(exp3)
	| EXPR_ALIGNOF(exp3), CALL(e1,elist1) ->
	| CALL(e1,elist1), TYPE_ALIGNOF(spec,dt)
	| TYPE_ALIGNOF(spec,dt), CALL(e1,elist1) ->
	| CALL(e1,elist1), INDEX(exp3,exp4)
	| INDEX(exp3,exp4), CALL(e1,elist1) ->
	| CALL(e1,elist1),MEMBEROF(exp3,str)
	| MEMBEROF(exp3,str), CALL(e1,elist1) ->
	| CALL(e1,elist1),MEMBEROFPTR(exp3,str)
	| MEMBEROFPTR(exp3,str), CALL(e1,elist1) ->
	| CALL(e1,elist1), GNU_BODY(b)
	| GNU_BODY(b), CALL(e1,elist1) ->
	| CALL(e1,elist1), EXPR_PATTERN(str)
	| EXPR_PATTERN(str), CALL(e1,elist1) ->
	| COMMA(elist1), COMMA(elist2) -> COMMA(unify_exp_list elist1 elist2)
	| COMMA(elist1), CONSTANT(cn)
	| CONSTANT(cn), COMMA(elist1) ->
	| COMMA(elist1), PAREN(exp3)
	| PAREN(exp3), COMMA(elist1) ->
	| COMMA(elist1), VARIABLE(str)
	| VARIABLE(str), COMMA(elist1) ->
	| COMMA(elist1), EXPR_SIZEOF(exp)
	| EXPR_SIZEOF(exp), COMMA(elist1) ->
	| COMMA(elist1),TYPE_SIZEOF(spec,dt)
	| TYPE_SIZEOF(spec,dt), COMMA(elist1) ->
	| COMMA(elist1), EXPR_ALIGNOF(exp3)
	| EXPR_ALIGNOF(exp3), COMMA(elist1) ->
	| COMMA(elist1), TYPE_ALIGNOF(spec,dt)
	| TYPE_ALIGNOF(spec,dt), COMMA(elist1) ->
	| COMMA(elist1), INDEX(exp3,exp4)
	| INDEX(exp3,exp4), COMMA(elist1) ->
	| COMMA(elist1),MEMBEROF(exp3,str)
	| MEMBEROF(exp3,str), COMMA(elist1) ->
	| COMMA(elist1),MEMBEROFPTR(exp3,str)
	| MEMBEROFPTR(exp3,str), COMMA(elist1) ->
	| COMMA(elist1), GNU_BODY(b)
	| GNU_BODY(b), COMMA(elist1) ->
	| COMMA(elist1), EXPR_PATTERN(str)
	| EXPR_PATTERN(str), COMMA(elist1) ->
	| CONSTANT(c1), CONSTANT(c2) -> CONSTANT(unify_constant c1 c2)
	| CONSTANT(c1), PAREN(exp)
	| PAREN(exp3), CONSTANT(c1) ->
	| CONSTANT(c1), VARIABLE(str)
	| VARIABLE(str), CONSTANT(c1) ->
	| CONSTANT(c1), EXPR_SIZEOF(exp)
	| EXPR_SIZEOF(exp), CONSTANT(c1) ->
	| CONSTANT(c1),TYPE_SIZEOF(spec,dt)
	| TYPE_SIZEOF(spec,dt), CONSTANT(c1) ->
	| CONSTANT(c1), EXPR_ALIGNOF(exp3)
	| EXPR_ALIGNOF(exp3), CONSTANT(c1) ->
	| CONSTANT(c1), TYPE_ALIGNOF(spec,dt)
	| TYPE_ALIGNOF(spec,dt), CONSTANT(c1) ->
	| CONSTANT(c1), INDEX(exp3,exp4)
	| INDEX(exp3,exp4), CONSTANT(c1) ->
	| CONSTANT(c1),MEMBEROF(exp3,str)
	| MEMBEROF(exp3,str), CONSTANT(c1) ->
	| CONSTANT(c1),MEMBEROFPTR(exp3,str)
	| MEMBEROFPTR(exp3,str), CONSTANT(c1) ->
	| CONSTANT(c1), GNU_BODY(b)
	| GNU_BODY(b), CONSTANT(c1) ->
	| CONSTANT(c1), EXPR_PATTERN(str)
	| EXPR_PATTERN(str), CONSTANT(c1) ->
	| PAREN(e1),PAREN(e2) ->  PAREN(unify_exp e1 e2)
	| PAREN(e1), VARIABLE(str)
	| VARIABLE(str), PAREN(e1) ->
	| PAREN(e1), EXPR_SIZEOF(exp)
	| EXPR_SIZEOF(exp), PAREN(e1) ->
	| PAREN(e1),TYPE_SIZEOF(spec,dt)
	| TYPE_SIZEOF(spec,dt), PAREN(e1) ->
	| PAREN(e1), EXPR_ALIGNOF(exp3)
	| EXPR_ALIGNOF(exp3), PAREN(e1) ->
	| PAREN(e1), TYPE_ALIGNOF(spec,dt)
	| TYPE_ALIGNOF(spec,dt), PAREN(e1) ->
	| PAREN(e1), INDEX(exp3,exp4)
	| INDEX(exp3,exp4), PAREN(e1) ->
	| PAREN(e1),MEMBEROF(exp3,str)
	| MEMBEROF(exp3,str), PAREN(e1) ->
	| PAREN(e1),MEMBEROFPTR(exp3,str)
	| MEMBEROFPTR(exp3,str), PAREN(e1) ->
	| PAREN(e1), GNU_BODY(b)
	| GNU_BODY(b), PAREN(e1) ->
	| PAREN(e1), EXPR_PATTERN(str)
	| EXPR_PATTERN(str), PAREN(e1) ->
	| VARIABLE(str1),VARIABLE(str2) -> VARIABLE(unify_string str1 str2) 
	| VARIABLE(str1), EXPR_SIZEOF(exp)
	| EXPR_SIZEOF(exp), VARIABLE(str1) ->
	| VARIABLE(str1),TYPE_SIZEOF(spec,dt)
	| TYPE_SIZEOF(spec,dt), VARIABLE(str1) ->
	| VARIABLE(str1), EXPR_ALIGNOF(exp3)
	| EXPR_ALIGNOF(exp3), VARIABLE(str1) ->
	| VARIABLE(str1), TYPE_ALIGNOF(spec,dt)
	| TYPE_ALIGNOF(spec,dt), VARIABLE(str1) ->
	| VARIABLE(str1), INDEX(exp3,exp4)
	| INDEX(exp3,exp4), VARIABLE(str1) ->
	| VARIABLE(str1),MEMBEROF(exp3,str)
	| MEMBEROF(exp3,str), VARIABLE(str1) ->
	| VARIABLE(str1),MEMBEROFPTR(exp3,str)
	| MEMBEROFPTR(exp3,str), VARIABLE(str1) ->
	| VARIABLE(str1), GNU_BODY(b)
	| GNU_BODY(b), VARIABLE(str1) ->
	| VARIABLE(str1), EXPR_PATTERN(str)
	| EXPR_PATTERN(str), VARIABLE(str1) ->
	| EXPR_SIZEOF(e1),EXPR_SIZEOF(e2) -> EXPR_SIZEOF(unify_exp e1 e2) 
	| EXPR_SIZEOF(e1),TYPE_SIZEOF(spec,dt)
	| TYPE_SIZEOF(spec,dt), EXPR_SIZEOF(e1) ->
	| EXPR_SIZEOF(e1), EXPR_ALIGNOF(exp3)
	| EXPR_ALIGNOF(exp3), EXPR_SIZEOF(e1) ->
	| EXPR_SIZEOF(e1), TYPE_ALIGNOF(spec,dt)
	| TYPE_ALIGNOF(spec,dt), EXPR_SIZEOF(e1) ->
	| EXPR_SIZEOF(e1), INDEX(exp3,exp4)
	| INDEX(exp3,exp4), EXPR_SIZEOF(e1) ->
	| EXPR_SIZEOF(e1),MEMBEROF(exp3,str)
	| MEMBEROF(exp3,str), EXPR_SIZEOF(e1) ->
	| EXPR_SIZEOF(e1),MEMBEROFPTR(exp3,str)
	| MEMBEROFPTR(exp3,str), EXPR_SIZEOF(e1) ->
	| EXPR_SIZEOF(e1), GNU_BODY(b)
	| GNU_BODY(b), EXPR_SIZEOF(e1) ->
	| EXPR_SIZEOF(e1), EXPR_PATTERN(str)
	| EXPR_PATTERN(str), EXPR_SIZEOF(e1) ->
	| TYPE_SIZEOF(spec1,dt1), TYPE_SIZEOF(spec2,d2t) -> TYPE_SIZEOF(unify_spec spec1 spec2, unify_dt dt1 dt2)
	| TYPE_SIZEOF(spec1,dt1), EXPR_ALIGNOF(exp3)
	| EXPR_ALIGNOF(exp3), TYPE_SIZEOF(spec1,dt1) ->
	| TYPE_SIZEOF(spec1,dt1), TYPE_ALIGNOF(spec,dt)
	| TYPE_ALIGNOF(spec,dt), TYPE_SIZEOF(spec1,dt1) ->
	| TYPE_SIZEOF(spec1,dt1), INDEX(exp3,exp4)
	| INDEX(exp3,exp4), TYPE_SIZEOF(spec1,dt1) ->
	| TYPE_SIZEOF(spec1,dt1),MEMBEROF(exp3,str)
	| MEMBEROF(exp3,str), TYPE_SIZEOF(spec1,dt1) ->
	| TYPE_SIZEOF(spec1,dt1),MEMBEROFPTR(exp3,str)
	| MEMBEROFPTR(exp3,str), TYPE_SIZEOF(spec1,dt1) ->
	| TYPE_SIZEOF(spec1,dt1), GNU_BODY(b)
	| GNU_BODY(b), TYPE_SIZEOF(spec1,dt1) ->
	| TYPE_SIZEOF(spec1,dt1), EXPR_PATTERN(str)
	| EXPR_PATTERN(str), TYPE_SIZEOF(spec1,dt1) ->
	| EXPR_ALIGNOF(e1), EXPR_ALIGNOF(e2) -> EXPR_ALIGNOF(unify_exp e1 e2)
	| EXPR_ALIGNOF(e1), TYPE_ALIGNOF(spec,dt)
	| TYPE_ALIGNOF(spec,dt), EXPR_ALIGNOF(e1) ->
	| EXPR_ALIGNOF(e1), INDEX(exp3,exp4)
	| INDEX(exp3,exp4), EXPR_ALIGNOF(e1) ->
	| EXPR_ALIGNOF(e1),MEMBEROF(exp3,str)
	| MEMBEROF(exp3,str), EXPR_ALIGNOF(e1) ->
	| EXPR_ALIGNOF(e1),MEMBEROFPTR(exp3,str)
	| MEMBEROFPTR(exp3,str), EXPR_ALIGNOF(e1) ->
	| EXPR_ALIGNOF(e1), GNU_BODY(b)
	| GNU_BODY(b), EXPR_ALIGNOF(e1) ->
	| EXPR_ALIGNOF(e1), EXPR_PATTERN(str)
	| EXPR_PATTERN(str), EXPR_ALIGNOF(e1) ->
	| TYPE_ALIGNOF(spec1,dt1),TYPE_ALIGNOF(spec2,dt2) -> 
	  TYPE_ALIGNOF(unify_spec spec1 spec2,
				   unify_dt dt1 dt2)
	| TYPE_ALIGNOF(spec1,dt1), INDEX(exp3,exp4)
	| INDEX(exp3,exp4), TYPE_ALIGNOF(spec1,dt1) ->
	| TYPE_ALIGNOF(spec1,dt1),MEMBEROF(exp3,str)
	| MEMBEROF(exp3,str), TYPE_ALIGNOF(spec1,dt1) ->
	| TYPE_ALIGNOF(spec1,dt1),MEMBEROFPTR(exp3,str)
	| MEMBEROFPTR(exp3,str), TYPE_ALIGNOF(spec1,dt1) ->
	| TYPE_ALIGNOF(spec1,dt1), GNU_BODY(b)
	| GNU_BODY(b), TYPE_ALIGNOF(spec1,dt1) ->
	| TYPE_ALIGNOF(spec1,dt1), EXPR_PATTERN(str)
	| EXPR_PATTERN(str), TYPE_ALIGNOF(spec1,dt1) ->
	| INDEX(e1,e2),INDEX(e3,e4) -> INDEX(unify_exp e1 e3, unify_exp e2 e4) ->
	| INDEX(e1,e2),MEMBEROF(exp3,str)
	| MEMBEROF(exp3,str), INDEX(e1,e2) ->
	| INDEX(e1,e2),MEMBEROFPTR(exp3,str)
	| MEMBEROFPTR(exp3,str), INDEX(e1,e2) ->
	| INDEX(e1,e2), GNU_BODY(b)
	| GNU_BODY(b), INDEX(e1,e2) ->
	| INDEX(e1,e2), EXPR_PATTERN(str)
	| EXPR_PATTERN(str), INDEX(e1,e2) ->
	| MEMBEROF(e1,str1),MEMBEROF(e2,str2) -> MEMBEROF(unify_exp e1 e2,unify_string str1 str2)
	| MEMBEROF(e1,str1),MEMBEROFPTR(exp3,str)
	| MEMBEROFPTR(exp3,str), MEMBEROF(e1,str1) ->
	| MEMBEROF(e1,str1), GNU_BODY(b)
	| GNU_BODY(b), MEMBEROF(e1,str1) ->
	| MEMBEROF(e1,str1), EXPR_PATTERN(str)
	| EXPR_PATTERN(str), MEMBEROF(e1,str1) ->
	| MEMBEROFPTR(e1,str1),MEMBEROFPTR(e2,str2) -> MEMBEROFPTR(unify_exp e1 e2, unify_string str1 str2)
	| MEMBEROFPTR(e1,str1), GNU_BODY(b)
	| GNU_BODY(b), MEMBEROFPTR(e1,str1) ->
	| MEMBEROFPTR(e1,str1), EXPR_PATTERN(str)
	| EXPR_PATTERN(str), MEMBEROFPTR(e1,str1) ->
	| GNU_BODY(b1),GNU_BODY(b2) -> GNU_BODY(unify_block b1 b2)
	| GNU_BODY(b1)(e1,str1), EXPR_PATTERN(str)
	| EXPR_PATTERN(str), GNU_BODY(b1)(e1,str1) ->
	| EXPR_PATTERN(str1),EXPR_PATTRN(str2) -> EXPR_PATTERN(unify_string str1 str2)*)
	| _ -> failwith "Not implemented"

let unify_guards gset1 gset2 inter = failwith "Not implemented"

  (* given list1 of N elements and list2 of M elements, there are M choose N
	 permutations of list2 that map to list1.  Each permutation has a cost.  We
	 want the lowest one.  Now the thing is, is it the case that if we have a
	 permutation and we make one swap and the cost is lower than the original,
	 then that is definitely a win? *)
	
	
  let unify_template (t1 : template) (t2 : template) : template = 
	let changes1,context1 = t1 in
	let changes2,context2 = t2 in
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
