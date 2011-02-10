open Batteries
open List
open Utils
open Cabs
open Cprint
open Globals
open Ttypes
open Cabswalker
open Difftypes
open Treediff
open Distance


let lexists = List.exists (* FIXME: add to utils *)

(* types for generalized AST nodes *)
 
type guard = EXPG of expression node 
			 | OPP of expression node 
			 | CATCH of expression node 
			 | LEAST of expression list 
			 | NOGUARD

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
	  | LEAST(elist) -> pprintf "ELIST" 
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
  inherit [template list] singleCabsWalker as super

  val mutable context = initial_context 

  method default_res () = []

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
					self#combine result (doWalk self#combine self#wExpression self#childrenExpression exp)) ts elist in
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
*)

(*let rec unify_exp exp1 exp2 = 

  let member_of_ptr exp1 exp2 = 
	match exp1.node,exp2.node with
	  MEMBEROF(exp3,str1),MEMBEROFPTR(exp4,str2) ->
		SOMEMEMBER(unify_exp exp3 exp4, unify_string str1 str2)
	| _ -> failwith "Unexpected arguments to member_of_ptr"
  in
  let hash1 = hash exp1 in 
  let hash2 = hash exp2 in
	else begin
	  let unified = 
		if hash1 = hash2 (* = or ==? *) then EXPBASE(exp1) 
		else 		
		  match exp1.node,exp2.node with 
			(* TODO/fixme: compare the strings in these expressions to nothing, or
			   similar? *)


			| UNARY(_), CALL(_) -> unary_call exp1 exp2
			| CALL(_), UNARY(_) -> unary_call exp2 exp1
			| UNARY(_), COMMA(_) -> unary_comma exp1 exp2
			| COMMA(_), UNARY(_) -> unary_comma exp2 exp1
			| UNARY(uop1,exp3), CONSTANT(cn) -> unary_constant exp1 exp2
			| CONSTANT(cn), UNARY(uop1,exp3) -> unary_constant exp2 exp1
			| UNARY(uop1,exp3), PAREN(exp3) -> unary_paren exp1 exp2
			| PAREN(exp3), UNARY(uop1,exp3) -> unary_paren exp2 exp1
			| UNARY(uop1,exp3), VARIABLE(str) -> unary_variable exp1 exp2
			| VARIABLE(str), UNARY(uop1,exp3) -> unary_variable exp2 exp1
			| UNARY(uop1,exp3),TYPE_SIZEOF(spec,dt)
			| UNARY(uop1,exp3), EXPR_SIZEOF(exp) -> unary_sizeof exp1 exp2
			| TYPE_SIZEOF(spec,dt), UNARY(uop1,exp3)
			| EXPR_SIZEOF(exp), UNARY(uop1,exp3) -> unary_sizeof exp2 exp1
			| UNARY(uop1,exp3), EXPR_ALIGNOF(exp3) 
			| UNARY(uop1,exp3), TYPE_ALIGNOF(spec,dt) -> unary_alignof exp1 exp2
			| TYPE_ALIGNOF(spec,dt), UNARY(uop1,exp3) 
			| EXPR_ALIGNOF(exp3), UNARY(uop1,exp3) -> unary_alignof exp2 exp1
			| _ -> failwith "Not implemented"
	  in
		hadd unify_ht (hash1,hash2) unified; unified
	end
*)

let sign = function MINUS | PLUS -> true | _ -> false 
let notm = function NOT | BNOT -> true | _ -> false 
let mem = function ADDROF | MEMOF -> true | _ -> false 
let pre = function PREDECR | PREINCR -> true | _ -> false 
let post = function POSDECR | POSINCR -> true | _ -> false 
let incr = function POSINCR | PREINCR -> true | _ -> false
let decr = function POSDECR | PREDECR -> true | _ -> false
let uop_mod uop = incr uop || decr uop 
let uop_num uop = uop_mod uop || sign uop 

let bop_arithmod = function
	ADD_ASSIGN | SUB_ASSIGN | MUL_ASSIGN | DIV_ASSIGN | MOD_ASSIGN -> true
  | _ -> false
let bop_bitmod = function
	BAND_ASSIGN | BOR_ASSIGN | XOR_ASSIGN | SHL_ASSIGN | SHR_ASSIGN -> true
  | _ -> false 
let bop_modify bop = bop_arithmod bop || bop_bitmod bop 
let bop_notbittruth = function AND | OR | EQ | NE | LT | GT | LE | GE -> true | _ -> false
let bop_bittruth = function BAND | BOR | XOR -> true | _ -> false
let bop_truth bop = bop_notbittruth bop || bop_bittruth bop 
let bop_onnumbers bop =
  match bop with
	ADD | SUB | MUL | DIV | MOD | BAND |BOR | XOR |SHL | SHR -> true
  | _ -> bop_modify bop
let bop_bit_assign = function BAND_ASSIGN | BOR_ASSIGN | XOR_ASSIGN -> true | _ -> false
let bop_logic = function AND | OR | EQ | NE | LT | GT | LE | GE -> true | _ -> false
let bop_commut = function ADD | SUB | MUL | DIV | OR | BOR | EQ | NE -> true | _ -> false
let bop_bits bop = bop_bitmod bop || bop_bittruth bop || bop_bit_assign bop 
let pair_match one two three four =
  match one,two with
	three,four 
  | four,three -> true
  | _ -> false 

let unify_exp_ht = hcreate 10
let hash exp = 5

let unify_string str1 str2 = failwith "Not implemented"
let unify_constant c1 c2 = failwith "Not implemented"
class expTemplateDoubleWalker = object(self)
  inherit [exp_gen] doubleCabsWalker as super

  method default_res () = LIFTED(LNOTHING)
  method combine res1 res2 = res1

  method walkSpecifier (spec1,spec2) = failwith "Not implemented"
  method walkInitExpression (ie1,ie2) = failwith "Not implemented"
  method walkExpressions (elist1,elist2) = failwith "Not implemented"
  method walkDeclType (dt1,dt2) = failwith "Not implemented"

  method wExpression (exp1,exp2) =
	let hash1,hash2 = hash exp1,hash exp2 in
	  if hmem unify_exp_ht (hash1,hash2) then
		hfind unify_exp_ht (hash1,hash2)
	  else  
		let res = if hash1 = hash2 then Result(EXPBASE(exp1))
		  else begin
			let unify_uop uop1 uop2 = 
			  let both fn = fn uop1 && fn uop2 in
				if uop1 = uop2 then Uop(uop1)
				else if both sign then Sign_modifier
				else if both notm then Not_operator
				else if both mem then Memory_operator
				else if both pre then Pre_operator
				else if both post then Post_operator
				else if both incr then Increment
				else if both decr then Decrement
				else if both uop_mod then Ugen(Modify_value)
				else if both uop_num then Ugen(OnNumbers)
				else Uop_gen(STAR) in
			let unary_unary uop1 uop2 exp3 exp4 =
			  Result(UNARYOP(unify_uop uop1 uop2,self#walkExpression (exp3,exp4)))
			in 
			let unary_labeladdr str uop exp = 
			  match uop with
				ADDROF -> Result(ADDROFEXP(self#walkExpression (nd(VARIABLE(str)), exp)))
			  | MEMOF -> Result(UNARYOP(Memory_operator, self#walkExpression (nd(VARIABLE(str)),exp)))
			  | _ -> CombineChildren(OPERATION(Uop_op(unify_uop ADDROF uop), self#walkExpression (nd(VARIABLE(str)),exp)))
			in
			let unary_binary uop unexp bop binexp1 binexp2 = 
			  let constant1 = nd(CONSTANT(CONST_INT("1"))) in
			  let const_binop op = 
				Result(BINOP(op, self#walkExpression (unexp,binexp1), self#walkExpression (binexp2,constant1)))
			  in
			  let const_op op = 
				Result(OPERATION(op, self#combine (self#walkExpression (unexp,binexp1)) (self#walkExpression (unexp,binexp2))))
			  in
				if (incr uop) && bop = ADD_ASSIGN then const_binop (Bop(ADD_ASSIGN))
				else if (uop_mod uop) && (bop_modify bop) then const_binop (Bgen(Modify_value))
				(* FIXME: the bitwise operators? *)
				else if (decr uop) && bop = SUB_ASSIGN then const_binop (Bop(SUB_ASSIGN))
				else if uop = NOT && bop_notbittruth bop then const_op (Bop_op(NotBitTruth))
				else if uop = BNOT && bop_bittruth bop then const_op (Bop_op(BitTruth))
				else if (uop = NOT && bop_bittruth bop) || (uop = BNOT && bop_notbittruth bop) then const_op Logic
				else if bop_onnumbers bop && uop_num uop then const_op OnNumbers
				else if uop = BNOT && bop_bit_assign bop then const_op OnBits
				else CombineChildrenPost(
				  LIFTED(STAR), 
				  (fun res -> OPERATION(Lifted_ops(STAR),LIFTED(ATLEAST([res])))))
			in
			let unary_question uexp qexp1 qexp2 qexp3 = function
			  | NOT
			  | BNOT -> ChildrenPost(fun res -> OPERATION(Lifted_ops(ATLEAST[Logic]), res)) (*(OPERATION(ATLEAST([Truth]), ATLEAST([self#combine (self#walkExpression uexp qexp1)
																						 (self#combine (self#walkExpression uexp qexp2)
																						 (self#walkExpression uexp qexp3))]))) That may not work? *)
			  | _ -> Children
			in
			let unary_cast uexp spec dt ie = function
			  | _ -> failwith "Not implemented"
			in
			let unary_call uexp fn args = function
			  | PREINCR | PREDECR | POSINCR | POSDECR -> 
				CombineChildren(OPERATION(Lifted_ops(MAYBE([Modify_value])),
										  lfoldl (fun result -> fun exp -> self#combine (self#walkExpression (uexp,exp)) result) (self#default_res()) args))
			  | _ -> Children
			in
			let unary_index = failwith "Not implemented" in
			let unary_member = failwith "Not implemented" in
			let unary_constant = failwith "Not implemented" in
			let unary_memberptr = failwith "Not implemented" in
			let unary_variable uexp varexp = Result(UNARYOP(Uop_gen(LNOTHING),self#walkExpression (uexp,varexp))) in
			  match exp1.node,exp2.node with
			  | _,GNU_BODY(b)
			  | GNU_BODY(b),_ -> failwith "Not implemented"
			  | UNARY(uop1,exp3),UNARY(uop2,exp4) -> unary_unary uop1 uop2 exp3 exp4
			  | LABELADDR(str1),LABELADDR(str2) -> Result(EXPBASE(nd(LABELADDR(unify_string str1 str2))))
			  | BINARY(bop1,exp3,exp4),BINARY(bop2,exp5,exp6) ->
 				let pair_match = pair_match bop1 bop2 in 
				let binole bop = Bop(bop) in
				let binsla bops = Bop_gen(ATLEAST([Bop(bops)])) in
				let bopsla bops = Bop_gen(ATLEAST[bops]) in
				let commut bop =
				  Result(self#combine (BINOP(bop, self#walkExpression (exp3,exp5), self#walkExpression (exp4,exp6)))
						   (BINOP(bop, self#walkExpression (exp3,exp6), self#walkExpression (exp4,exp5))))
				in
				let not_commut bop =
				  Result(self#combine (BINOP(bop, self#walkExpression (exp3,exp5), self#walkExpression (exp4,exp6)))
						   (BINOP(bop, self#walkExpression (exp3,exp6), self#walkExpression (exp4,exp5))))
				in
				let not_commut_bin_a lst = not_commut (binsla lst) in
				let not_commut_bop_a lst = not_commut (bopsla lst) in
				  if bop1 = bop2 then
					if bop_commut bop1 then commut (binole bop1)
					else not_commut (binole bop1)
				  else begin
					if pair_match ADD ADD_ASSIGN then not_commut_bin_a ADD
					else if pair_match SUB SUB_ASSIGN then not_commut_bin_a SUB
					else if pair_match MUL MUL_ASSIGN then not_commut_bin_a MUL
					else if pair_match DIV DIV_ASSIGN then not_commut_bin_a DIV
					else if pair_match MOD MOD_ASSIGN then not_commut_bin_a MOD
					else if pair_match BAND BAND_ASSIGN then not_commut_bin_a BAND
					else if pair_match BOR BOR_ASSIGN then not_commut_bin_a BOR
					else if pair_match SHL SHL_ASSIGN then not_commut_bin_a SHL
					else if pair_match SHR SHR_ASSIGN then not_commut_bin_a SHR
					else if pair_match SHR SHL_ASSIGN || pair_match SHL SHR_ASSIGN || pair_match SHR SHL then
					  not_commut_bop_a Shift
					else if ((bop1 = ASSIGN) && (bop_modify bop2)) || (bop_modify bop1 && bop2 = ASSIGN) then not_commut_bin_a ASSIGN
					else if (bop_arithmod bop1) && (bop_arithmod bop2) then not_commut (Bop_gen(ATLEAST[Modify_assign;Bgen(Arithmetic)]))
					else if (bop_bitmod bop1) && (bop_bitmod bop2) then not_commut (Bop_gen(ATLEAST[Modify_assign;Bgen(Bitwise)]))
					else if (bop_modify bop1) && (bop_modify bop2) then
					  Result(self#combine (BINOP(Bgen(Arithmetic), self#walkExpression (exp3, exp5), self#walkExpression (exp4, exp6)))
							   (BINOP(Bgen(Arithmetic), self#walkExpression (exp3, exp6), self#walkExpression (exp4, exp5))))
					else if pair_match AND BAND || pair_match OR BOR || pair_match OR XOR then 
					  failwith "FIXME"
					else if pair_match AND BAND_ASSIGN || pair_match OR BOR_ASSIGN || pair_match XOR XOR_ASSIGN then
					  failwith "FIXME"
					(* fixme: shl/shr relationship to add/multiply/etc *)
					else if pair_match LT LE then not_commut_bin_a LT
					else if pair_match GT GE then not_commut_bin_a GT
					else if pair_match LE EQ || pair_match GE EQ then not_commut_bin_a EQ
					else if pair_match GE EQ then not_commut_bin_a EQ
					else if (bop_logic bop1) && (bop_logic bop2) then
					  if bop_commut bop1 || bop_commut bop2 then commut (Bgen(Logic))
					  else not_commut (Bgen(Logic))
					else if bop_bits bop1 && bop_bits bop2 then commut  (Bgen(Bitwise))
					else if bop_onnumbers bop1 && bop_onnumbers bop2 then commut (Bgen(Arithmetic))
					else commut (Bop_gen(STAR))
				  end
			  | QUESTION(exp1,exp2,exp3), QUESTION(exp4,exp5,exp6) -> 
				Result(QUESTOP(self#walkExpression (exp1,exp4),
							   self#walkExpression (exp2,exp5),
							   self#walkExpression (exp3, exp6)))
			  | CAST((spec1,dt1),ie1),CAST((spec2,dt2), ie2) ->
				Result(CASTOP((self#walkSpecifier (spec1,spec2), self#walkDeclType (dt1,dt2)),
					   self#walkInitExpression (ie1,ie2)))
			  | CALL(e1,elist1), CALL(e2,elist2) ->
				Result(CALLOP(self#walkExpression (e1,e2),
							  self#walkExpressions (elist1,elist2)))
			  | COMMA(elist1), COMMA(elist2) -> Result(COMMAOP(self#walkExpressions (elist1,elist2)))
			  | CONSTANT(c1),CONSTANT(c2) -> Result(EXPBASE(nd(CONSTANT(unify_constant c1 c2))))
			  | PAREN(exp1),PAREN(exp2) -> Result(PARENOP(self#walkExpression (exp1, exp2)))
			  | VARIABLE(str1),VARIABLE(str2) -> Result(EXPBASE(nd(VARIABLE(unify_string str1 str2))))
			  | EXPR_SIZEOF(exp1),EXPR_SIZEOF(exp2) -> Result(EXPSIZOFOP(self#walkExpression (exp1,exp2)))
			  | TYPE_SIZEOF(spec1,dt1),TYPE_SIZEOF(spec2,dt2) -> Result(TYPESIZOFOP(self#walkSpecifier (spec1,spec2),
																				   self#walkDeclType (dt1,dt2)))
			  | EXPR_ALIGNOF(exp1),EXPR_ALIGNOF(exp2) -> Result(EXPALIGNOFOP(self#walkExpression (exp1,exp2)))
			  | TYPE_ALIGNOF(spec1,dt1),TYPE_ALIGNOF(spec2,dt2) -> Result(TYPEALIGNOFOP(self#walkSpecifier (spec1,spec2),
																						self#walkDeclType (dt1,dt2)))
			  | INDEX(exp1,exp2),INDEX(exp3,exp4) -> Result(INDEXOP(self#walkExpression (exp1,exp3),
																	self#walkExpression (exp2,exp4)))
			  | MEMBEROF(exp1,str1),MEMBEROF(exp2,str2) -> Result(MEMBEROFOP(self#walkExpression (exp1,exp2),
																			 unify_string str1 str2))
			  | MEMBEROFPTR(exp1,str1),MEMBEROFPTR(exp2,str2) ->
				Result(MEMBEROFPTROP(self#walkExpression (exp1,exp2),
									 unify_string str1 str2))
			  | EXPR_PATTERN(str1),EXPR_PATTERN(str2) -> Result(EXPBASE(nd(EXPR_PATTERN(unify_string str1 str2))))
				
			  (* those were the direct (easy) matches; now the harder stuff *)
			  (* Unary expressions first *)
			  | UNARY(uop1,exp3),LABELADDR(str) 
			  | LABELADDR(str),UNARY(uop1,exp3) -> unary_labeladdr str uop1 exp3
			  | UNARY(uop,exp3),BINARY(bop,exp4,exp5)
			  | BINARY(bop,exp4,exp5),UNARY(uop,exp3) -> unary_binary uop exp3 bop exp4 exp5
			  | UNARY(uop,uexp),QUESTION(qexp1,qexp2,qexp3)
			  | QUESTION(qexp1,qexp2,qexp3),UNARY(uop,uexp) -> unary_question uexp qexp1 qexp2 qexp3 uop
			  | UNARY(uop,uexp),CAST((spec,dt),ie)
			  | CAST((spec,dt),ie), UNARY(uop,uexp) -> unary_cast uexp spec dt ie uop (* sort of accesses the value *)
			  | UNARY(_,uexp),VARIABLE(str) -> unary_variable uexp exp2
			  | VARIABLE(str), UNARY(_,uexp) -> unary_variable uexp exp1
			  | UNARY(uop,uexp),CALL(fn,args)
			  | CALL(fn,args),UNARY(uop,uexp) -> unary_call uexp fn args uop
			  (* we need a special case for calls, to deal with function names *)
			  | UNARY(_), EXPR_SIZEOF(_)
			  | EXPR_SIZEOF(_),UNARY(_) 
			  | UNARY(_),TYPE_SIZEOF(_)
			  | TYPE_SIZEOF(_),UNARY(_) 
			  | UNARY(_),EXPR_ALIGNOF(_)
			  | EXPR_ALIGNOF(_),UNARY(_) ->
				ChildrenPost((fun res -> UNARYOP(Uop_gen(STAR),res))) (* this may not work *)
			  | UNARY(uop,uexp), INDEX(iexp1,iexp2) 
			  | INDEX(iexp1,iexp2),UNARY(uop,uexp) -> unary_index uexp iexp1 iexp2 uop (* also sort of accesses the value *)
			  | UNARY(uop,uexp),MEMBEROF(mexp,str)
			  | MEMBEROF(mexp,str),UNARY(uop,uexp) -> unary_member uexp mexp str uop
			  | UNARY(uop,uexp),MEMBEROFPTR(mexp,str)
			  | MEMBEROFPTR(mexp,str),UNARY(uop,uexp) -> unary_memberptr uexp mexp str uop  (* also sort of accesses the value *)
				
			  (* Binary expressions *)
			  | BINARY(_),QUESTION(_)
			  | QUESTION(_),BINARY(_) -> CombineChildren(OPERATION(Lifted_ops(ATLEAST[Logic]),LIFTED(STAR)))
			  | _,_ -> Children
		  end in
		  hadd unify_exp_ht (hash1,hash2) res; res
end

(*
let unify_guards gset1 gset2 inter = failwith "Not implemented"
	
let unify_template (t1 : template) (t2 : template) : template = 
  let changes1,context1 = t1 in
  let changes2,context2 = t2 in
	failwith "Not implemented"
*)

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


(*	| UNARY(uop1,exp3), INDEX(exp3,exp4) 
	| INDEX(exp3,exp4), UNARY(uop1,exp3) ->
	| UNARY(uop1,exp3),MEMBEROF(exp3,str) 
	| MEMBEROF(exp3,str), UNARY(uop1,exp3) ->
	| UNARY(uop1,exp3),MEMBEROFPTR(exp3,str) 
	| MEMBEROFPTR(exp3,str), UNARY(uop1,exp3) ->
	| UNARY(uop1,exp3), GNU_BODY(b) 
	| GNU_BODY(b), UNARY(uop1,exp3) ->
	| UNARY(uop1,exp3), EXPR_PATTERN(str) 
	| EXPR_PATTERN(str), UNARY(uop1,exp3) ->
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
	| INDEX(e1,e2),MEMBEROF(exp3,str)
	| MEMBEROF(exp3,str), INDEX(e1,e2) ->
	| INDEX(e1,e2),MEMBEROFPTR(exp3,str)
	| MEMBEROFPTR(exp3,str), INDEX(e1,e2) ->
	| INDEX(e1,e2), GNU_BODY(b)
	| GNU_BODY(b), INDEX(e1,e2) ->
	| INDEX(e1,e2), EXPR_PATTERN(str)
	| EXPR_PATTERN(str), INDEX(e1,e2) ->
	| MEMBEROF(e1,str1),MEMBEROFPTR(exp3,str) -> member_of_ptr exp1 exp2
	| MEMBEROFPTR(exp3,str), MEMBEROF(e1,str1) -> member_of_ptr exp2 exp1
	| MEMBEROF(e1,str1), GNU_BODY(b)
	| GNU_BODY(b), MEMBEROF(e1,str1) ->
	| MEMBEROF(e1,str1), EXPR_PATTERN(str)
	| EXPR_PATTERN(str), MEMBEROF(e1,str1) ->
	| MEMBEROFPTR(e1,str1), GNU_BODY(b) ->
	| GNU_BODY(b), MEMBEROFPTR(e1,str1) ->
	| MEMBEROFPTR(e1,str1), EXPR_PATTERN(str) ->
	| EXPR_PATTERN(str), MEMBEROFPTR(e1,str1) ->
	| GNU_BODY(b1)(e1,str1), EXPR_PATTERN(str) ->
	| EXPR_PATTERN(str), GNU_BODY(b1)(e1,str1) ->*)
