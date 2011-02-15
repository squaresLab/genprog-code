open Batteries
open List
open Utils
open Cabs
open Cprint
open Globals
open Ttypes
open Cabswalker
open Difftypes
open Convert
open Treediff
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
  (one = three && two = four) || (one = four && two = three)

let str_hash = hcreate 10
let unify_string str1 str2 = ht_find str_hash (str1,str2) (fun _ -> gcs str1 str2)

let unify_constant c1 c2 = failwith "Not implemented unify_constant"
(*  if c1 = c2 then EXPBASE(c1) else 
  match c1.node,c2.node with 
  | CONST_INT(i1),CONST_INT(i2) -> 
	let int1 = int_of_string i1 in
	let int2 = int_of_string i2 in 
	  UNUNIFIED([c1;c2])
  | CONST_INT(i1),CONST_FLOAT(f2) 
  | CONST_FLOAT(f2),CONST_INT(i1) ->
	let int1 = int_of_string i1 in 
	let float1 = float_of_int int1 in 
	  UNUNIFIED([CONST_FLOAT(string_of_float float1);CONST_FLOAT(f2)])
  | _ -> failwith "Not implemented unify_constant"
  | CONST_INT(i1),CONST_CHAR(cs1)
  | CONST_CHAR(cs1),CONST_INT(i1) 
  | CONST_INT(i1),CONST_WCHAR(cs2)
  | CONST_WCHAR(cs2),CONST_INT(i1)
  | CONST_INT(i1),CONST_STRING(s2)
  | CONST_STRING(s2),CONST_INT(i1)
  | CONST_INT(i1),CONST_WSTRING(strs2)
  | CONST_WSTRING(strs2),CONST_INT(i1)
  | CONST_FLOAT(f1),CONST_FLOAT(f2) 
  | CONST_FLOAT(f1),CONST_CHAR(cs1)
  | CONST_CHAR(cs1),CONST_FLOAT(f1)
  | CONST_FLOAT(f1),CONST_WCHAR(cs1)
  | CONST_WCHAR(cs1),CONST_FLOAT(f1)
  | CONST_FLOAT(f1),CONST_STRING(str2)
  | CONST_STRING(str2),CONST_FLOAT(f1)
  | CONST_FLOAT(f1),CONST_WSTRING(strs2)
  | CONST_WSTRING(strs2),CONST_FLOAT(f1)
  | CONST_CHAR(cs1),CONST_CHAR(cs2) 
  | CONST_CHAR(cs1),CONST_WCHAR(cs1)
  | CONST_WCHAR(cs1),CONST_CHAR(cs1)
  | CONST_CHAR(cs1),CONST_STRING(str2)
  | CONST_STRING(str2),CONST_CHAR(cs1)
  | CONST_CHAR(cs1),CONST_WSTRING(strs2)
  | CONST_WSTRING(strs2),CONST_CHAR(cs1)
  | CONST_WCHAR(cs1),CONST_WCHAR(cs2) 
  | CONST_WCHAR(cs1),CONST_STRING(str2)
  | CONST_STRING(str2),CONST_WCHAR(cs1)
  | CONST_WCHAR(cs1),CONST_WSTRING(cs2)
  | CONST_WSTRING(cs2),CONST_WCHAR(cs1)
  | CONST_STRING(str1),CONST_STRING(str2) 
  | CONST_STRING(str1),CONST_WSTRING(strs2)
  | CONST_WSTRING(strs2),CONST_STRING(str1)
  | CONST_WSTRING(strs1),CONST_WSTRING(strs2) -> () *)

let unify_exp_ht = hcreate 10
let unify_stmt_ht = hcreate 10
let hash_exp exp = Pretty.sprint ~width:80 (d_exp () exp)
let hash_stmt stmt = Pretty.sprint ~width:80 (d_stmt () stmt)
let spec_ht = hcreate 10
let ie_ht = hcreate 10
let exps_ht = hcreate 10
let stmts_ht = hcreate 10
let tn_ht = hcreate 10
let iw_ht = hcreate 10
let dts_ht = hcreate 10

let atleast res = ATLEAST([res])
(* is this actually what I want? I *think* so, but I worry the "ATLEAST" will throw things off in comparisons.  Hmmm...FIXME *)

let check_hash ht key1 key2 hash ifeq ifnot = 
  let hash1,hash2 = hash key1,hash key2 in
	ht_find ht (hash1,hash2) 
	  (fun _ -> if hash1 = hash2 then ifeq key1 else ifnot ())

let compare (val1 : 'a) (val2 : 'a) : 'a =
  	let comp1 = Objsize.objsize val1 in 
	let comp2 = Objsize.objsize val2 in 
	  if comp1 > comp2 then val1 else val2

let distance  (fn : 'a * 'a -> 'b) (val1 : 'a) (val2 : 'a) : int =
  let combination : 'b = fn (val1,val2) in
  let bestsize = Objsize.objsize combination in
	bestsize.Objsize.data

class templateDoubleWalker = object(self)
  inherit [tree_gen,typeSpec_gen,se_gen,spec_gen,dt_gen,ng_gen,ing_gen,name_gen,in_gen,sn_gen,def_gen,block_gen,stmt_gen,exp_gen,ie_gen,attr_gen,tn_gen] doubleCabsWalker as super

  method default_res () = TREELIFTED(STAR)
  method combine res1 res2 = res1

  method default_exp () = ELIFTED(STAR)
  method default_stmt () = SLIFTED(STAR)
  method default_def () = DLIFTED(STAR)
  method default_tn () = TNLIFTED(STAR)
  method combine_exp e1 e2 = compare e1 e2
  method combine_stmt s1 s2 = compare s1 s2
  method combine_def d1 d2 = compare d1 d2
  method combine_tn tn1 tn2 = compare tn1 tn2

  method wTreeNode (tn1,tn2) =
	check_hash tn_ht tn1 tn2 
	  (fun tn -> Pretty.sprint ~width:80 (d_tree_node () tn))
	  (fun k -> Result(TNBASE(k)))  
	  (fun _ ->
		match tn1.node,tn2.node with
		| Globals(dlist1),Globals(dlist2) ->
		  Result(GENDEFS(lmap
						   (fun (d1,d2) -> 
							 self#walkDefinition (d1,d2)) (best_permutation (distance self#walkDefinition) dlist1 dlist2)))
		| Stmts(slist1),Stmts(slist2) ->
		  Result(GENSTMTS(lmap
							(fun (s1,s2) -> 
							  self#walkStatement (s1,s2)) (best_permutation (distance self#walkStatement) slist1 slist2)))
		| Exps(elist1),Exps(elist2) ->
		  Result(GENEXPS(self#walkExpressions (elist1,elist2)))
		| _ -> Result(self#default_tn()) (* FIXME *))


  method wSpecElem ((se1,se2) : (spec_elem * spec_elem)) =
	check_hash se_ht se1 se2 
	  (fun spec -> Pretty.sprint ~width:80 (d_spec_elem () se) spec)
	  (fun k -> Result(Spec_elem(k)))
	  (fun _ ->
		match se1,se2 with
		| SpecCV(cv1),SpecCV(cv2) -> Result(Se_CV(STAR)))
		| SpecAttr(attr1),SpecAttr(attr2) -> Result(Se_attr (self#walkAttribute (attr1,attr2)))
		| SpecStorage(st1),SpecStorage(st2) -> Result(Se_storage(STAR))
		| SpecType(ts1),SpecType(ts2) -> Result(Se_type(self#walkTypeSpecifier (ts1,ts2)))
		| SpecPattern(str1),SpecPattern(str2) -> Result(Spec_elem(SpecPattern(unify_string str1 str2)))
		| _,_ -> CombineChildren(Se_lifted(STAR)))

  method wSpecifier (spec1,spec2) = 
	check_hash spec_ht spec1 spec2 
	  (fun spec -> lst_str (fun se -> Pretty.sprint ~width:80 (d_spec_elem () se)) spec)
	  (fun k -> Result(Spec_base(k)))
	  (fun _ ->
		Result(Spec_list(
		  lmap
			(fun(spec1,spec2) -> self#walkSpecElem (spec1,spec2)) (best_permutation (distance self#walkSpecElem) spec1 spec2))))
	  
  method walkIwIes (iwies1,iwies2) = failwith "Not implemented"
	
  method wInitWhat (iw1,iw2) =
	check_hash iw_ht iw1 iw2 
	  (fun iw -> d_init_what () iw) 
	  (fun k -> Result(IWBASE(k)))
	  (fun _ ->
		match iw1,iw2 with
		  NEXT_INIT,_ 
		| _,NEXT_INIT -> CombineChildren(IWLIFTED(STAR)) (* FIXME: what does next_init mean? *)
		| INFIELD_INIT(str1,iw1),INFIELD_INIT(str2,iw2) -> CombineChildren(IWINFIELD(unify_string str1 str2, self#walkInitWhat (iw1,iw2)))
		| INFIELD_INIT(str1,iw1), ATINDEX_INIT(exp1,iw2)
		| ATINDEX_INIT(exp1,iw2), INFIELD_INIT(str1,iw1) -> CombineChildren(IWSOME(self#walkExpression (nd(VARIABLE(str1)), exp1), self#walkInitWhat (iw1,iw2) ))
		| INFIELD_INIT(str1,iw1), ATINDEXRANGE_INIT(exp1,exp2) 
		| ATINDEXRANGE_INIT(exp1,exp2), INFIELD_INIT(str1,iw1) -> CombineChildren(IWLIFTED(STAR))
		| ATINDEX_INIT(exp1,iw1), ATINDEX_INIT(exp2,iw2) -> Result(IWATINDEX(self#walkExpression (exp1,exp2), self#walkInitWhat (iw1,iw2)))
		| ATINDEX_INIT(exp3,iw1), ATINDEXRANGE_INIT(exp1,exp2)   (* We're missing many cases here *)
		| ATINDEXRANGE_INIT(exp1,exp2),ATINDEX_INIT(exp3,iw1) -> 
		  CombineChildren(IWATINDEX(self#combine_exp (self#walkExpression (exp1,exp3)) (self#walkExpression (exp2,exp3)),IWLIFTED(LNOTHING)))
	  )
  method private childrenInitWhat (iw1,iw2) = failwith "children initwhat not implemented"

  method private walkInitWhat (iw1,iw2) = 
	doWalk compare self#wInitWhat self#childrenInitWhat (iw1,iw2)

  method wInitExpression (ie1,ie2) = 
	check_hash ie_ht ie1 ie2 (fun ie -> d_init_expression () ie)
	  (fun k -> Result(IEBASE(k)))
	  (fun _ ->
		match ie1,ie2 with
		| SINGLE_INIT(exp1),SINGLE_INIT(exp2) -> Result(GENSINGLE(self#walkExpression (exp1,exp2)))
		| COMPOUND_INIT(iwies1),COMPOUND_INIT(iwies2) -> 
		  CombineChildren(GENCOMPOUND(self#walkIwIes (iwies1,iwies2)))
		| NO_INIT,_
		| _,NO_INIT -> CombineChildren(IELIFTED(LNOTHING))
		| SINGLE_INIT(exp1),COMPOUND_INIT(iwies1)
		| COMPOUND_INIT(iwies1),SINGLE_INIT(exp1) -> CombineChildren(IELIFTED(STAR)))

  method walkExpressions (elist1,elist2) =
	check_hash exps_ht elist1 elist2 
	  (fun elist -> lst_str (fun e -> Pretty.sprint ~width:80 (d_exp () e)) elist)
	  (fun elist1 ->  lmap (fun e -> EXPBASE(e)) elist1)
	  (fun _ ->
		lmap
		  (fun (e1,e2) -> 
			self#walkExpression (e1,e2)) (best_permutation (distance self#walkExpression) elist1 elist2))

  method wDeclType (dt1,dt2) = 
	check_hash dts_ht dt1 dt2
	  (fun dt -> Pretty.sprint ~width:80 (d_decl_type () dt))
	  (fun dt1 -> Result(DTBASE(dt1)))
	  (fun _ -> Children)
(*		match dt1,dt2 with FIXME
		| JUSTBASE,_ 
		| _,JUSTBASE -> 
  | PARENTYPE of attribute list * decl_type * attribute list
      (* Prints "(attrs1 decl attrs2)".
       * attrs2 are attributes of the
       * declared identifier and it is as
       * if they appeared at the very end
       * of the declarator. attrs1 can
       * contain attributes for the
       * identifier or attributes for the
       * enclosing type.  *)
  | ARRAY of decl_type * attribute list * expression node
      (* Prints "decl [ attrs exp ]".
       * decl is never a PTR. *)
  | PTR of attribute list * decl_type      (* Prints "* attrs decl" *)
  | PROTO of decl_type * single_name list * bool 
      (* Prints "decl (args[, ...])".
		* decl is never a PTR.*)*)

  method wExpression (exp1,exp2) = 
	check_hash unify_exp_ht exp1 exp2 hash_exp
	  (fun exp1 -> Result(EXPBASE(exp1)))
	  (fun _ ->
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
		  | _ -> CombineChildrenPost(OPERATION(Uop_op(unify_uop ADDROF uop), self#walkExpression (nd(VARIABLE(str)),exp)),(fun e -> ELIFTED(atleast e)))
		in
		let unary_binary uop unexp bop binexp1 binexp2 = 
		  let constant1 = nd(CONSTANT(CONST_INT("1"))) in
		  let const_binop op = 
			Result(BINOP(op, self#walkExpression (unexp,binexp1), self#walkExpression (binexp2,constant1)))
		  in
		  let const_op op = 
			Result(OPERATION(op, self#combine_exp (self#walkExpression (unexp,binexp1)) (self#walkExpression (unexp,binexp2))))
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
			  ELIFTED(STAR), 
			  (fun res -> OPERATION(Lifted_ops(STAR),ELIFTED(ATLEAST([res])))))
		in
		let unary_question uexp qexp1 qexp2 qexp3 = function
		  | NOT
		  | BNOT -> ChildrenPost(fun res -> OPERATION(Lifted_ops(ATLEAST[Logic]), res)) (*(OPERATION(ATLEAST([Truth]), ATLEAST([self#combine (self#walkExpression uexp qexp1)
																						  (self#combine (self#walkExpression uexp qexp2)
																						  (self#walkExpression uexp qexp3))]))) That may not work? *)
		  | _ -> Children
		in
		let unary_cast uexp spec dt ie = failwith "Not implemented11" (*function
																		| MINUS | PLUS | NOT | BNOT | MEMOF | ADDROF -> CombineChildren(VALUE(self#walkExpInitExp (uexp,iexp1)))
																		| _ -> Children*)
		in
		let unary_call uexp fn args = function
		  | PREINCR | PREDECR | POSINCR | POSDECR -> 
			CombineChildrenPost(OPERATION(Lifted_ops(MAYBE([Modify_value])),
										  lfoldl (fun result -> fun exp -> self#combine_exp (self#walkExpression (uexp,exp)) result) (self#default_exp()) args),(fun e -> ELIFTED(atleast e)))
		  | _ -> Children
		in
		let unary_value uexp exp1 = function 
		  | MINUS | PLUS | NOT | BNOT | MEMOF | ADDROF -> CombineChildrenPost(VALUE(self#walkExpression (uexp,exp1)),(fun e -> ELIFTED(atleast e)))
		  | _ -> Children (* FIXME: maybe? *)
		in
		let unary_variable uexp varexp = Result(UNARYOP(Uop_gen(LNOTHING),self#walkExpression (uexp,varexp))) in
		  match exp1.node,exp2.node with
		  | _,GNU_BODY(b)
		  | GNU_BODY(b),_ -> failwith "Not implemented12"
		  | UNARY(uop1,exp3),UNARY(uop2,exp4) -> unary_unary uop1 uop2 exp3 exp4
		  | LABELADDR(str1),LABELADDR(str2) -> Result(EXPBASE(nd(LABELADDR(unify_string str1 str2))))
		  | BINARY(bop1,exp3,exp4),BINARY(bop2,exp5,exp6) ->
			pprintf "comparing two binary statements\n"; flush stdout;
 			let pair_match = pair_match bop1 bop2 in 
			let binole bop = Bop(bop) in
			let binsla bops = Bop_gen(ATLEAST([Bop(bops)])) in
			let bopsla bops = Bop_gen(ATLEAST[bops]) in
			let commut bop =
			  CombineChildrenPost(self#combine_exp (BINOP(bop, self#walkExpression (exp3,exp5), self#walkExpression (exp4,exp6)))
									(BINOP(bop, self#walkExpression (exp3,exp6), self#walkExpression (exp4,exp5))),(fun e -> ELIFTED(atleast e)))
			in
			let not_commut bop =
			  CombineChildrenPost(self#combine_exp (BINOP(bop, self#walkExpression (exp3,exp5), self#walkExpression (exp4,exp6)))
									(BINOP(bop, self#walkExpression (exp3,exp6), self#walkExpression (exp4,exp5))),(fun e -> ELIFTED(atleast e)))
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
				  Result(self#combine_exp (BINOP(Bgen(Arithmetic), self#walkExpression (exp3, exp5), self#walkExpression (exp4, exp6)))
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
		  | INDEX(iexp1,iexp2),UNARY(uop,uexp) -> unary_value uexp iexp1 uop (* also sort of accesses the value *)
		  | UNARY(uop,uexp),MEMBEROF(mexp,str)
		  | MEMBEROF(mexp,str),UNARY(uop,uexp)
		  | UNARY(uop,uexp),MEMBEROFPTR(mexp,str) (* should we distinguish between pointer dereferences here? *)
		  | MEMBEROFPTR(mexp,str),UNARY(uop,uexp) -> failwith "Not implemented13" (* unary_member uexp mexp uop*)  (* also sort of accesses the value *)
			
		  (* Binary expressions *)
		  | BINARY(_),QUESTION(_)
		  | QUESTION(_),BINARY(_) -> CombineChildrenPost(OPERATION(Lifted_ops(ATLEAST[Logic]),ELIFTED(STAR)),(fun e -> ELIFTED(atleast e)))
		  (* LEFT OFF HERE *)
		  | _,_ -> Children)

  method wBlock (b1,b2) =
	check_hash stmts_ht b1 b2 
	  (fun b -> Pretty.sprint ~width:80 (d_block () b))
	  (fun b -> Result(BLOCKBASE(b)))
	  (fun _ ->
		let res = lmap
		  (fun (s1,s2) -> 
			self#walkStatement (s1,s2)) (best_permutation (distance self#walkStatement) b1.bstmts b2.bstmts) in
		if (llen b1.bstmts) <> (llen b2.bstmts) then Result(BLKLIFTED(ATLEAST([Reg(res)]))) else Result(Reg(res)))

  method walkForClause (fc1,fc2) = failwith "Not implemented15"

  method wStatement (stmt1,stmt2) =  (* FIXME: note to self: clear the locations before comparing the statements!*)
	check_hash unify_stmt_ht stmt1 stmt2
	  hash_stmt
	  (fun s1 -> Result(STMTBASE(s1)))
	  (fun _ ->
		match stmt1.node,stmt2.node with
		| COMPUTATION(exp1,_),COMPUTATION(exp2,_) -> Result(STMTCOMP(self#walkExpression(exp1,exp2)))
		| BLOCK(b1,_),BLOCK(b2,_) -> Result(STMTBLOCK(self#walkBlock (b1,b2))) (* result or combine children? *)
		| SEQUENCE(s1,s2,_),SEQUENCE(s3,s4,_) ->
		  CombineChildrenPost(self#combine_stmt (STMTSEQ(self#walkStatement (s1,s3), self#walkStatement(s2,s4))) 
								(STMTSEQ(self#walkStatement(s1,s4), self#walkStatement(s2,s3))),(fun s -> SLIFTED(atleast s)))
		| IF(e1,s1,s2,_), IF(e2,s3,s4,_) ->
		  Result(STMTIF(self#walkExpression (e1,e2), self#walkStatement (s1,s3), self#walkStatement (s2,s4)))
		| WHILE(e1,s1,_), WHILE(e2,s2,_) ->
		  Result(STMTLOOP(While, self#walkExpression (e1,e2), self#walkStatement(s1,s2)))
		| DOWHILE(e1,s1,_), DOWHILE(e2,s2,_) ->
		  Result(STMTLOOP(DoWhile, self#walkExpression (e1,e2), self#walkStatement(s1,s2)))
		| FOR(fc1,e1,e2,s1,_), FOR(fc2,e3,e4,s2,_) ->
		  Result(STMTFOR(self#walkForClause (fc1,fc2), self#walkExpression (e1,e3), self#walkExpression (e2,e4),
						 self#walkStatement(s1,s2)))
		| RETURN(e1,_), RETURN(e2,_) -> Result(STMTRET(self#walkExpression(e1,e2)))
		| SWITCH(e1,s1,_),SWITCH(e2,s2,_) ->
		  Result(STMTSWITCH(self#walkExpression (e1,e2), self#walkStatement (s1,s2)))
		| CASE(e1,s1,_),CASE(e2,s2,_) ->
		  Result(STMTCASE(self#walkExpression (e1,e2), self#walkStatement (s1,s2)))
		| CASERANGE(e1,e2,s1,_), CASERANGE(e3,e4,s2,_) ->
		  Result(STMTCASERANGE(self#walkExpression (e1,e3), self#walkExpression (e2,e4),
							   self#walkStatement (s1,s2)))
		| DEFAULT(s1,_), DEFAULT(s2,_) -> Result(STMTDEFAULT(self#walkStatement (s1,s2)))
		| LABEL(str1,s1,_),LABEL(str2,s2,_) -> Result(STMTLABEL(unify_string str1 str2, self#walkStatement (s1,s2)))
		| GOTO(str1,c),GOTO(str2,_) -> Result(STMTBASE(nd(GOTO(unify_string str1 str2,c))))
		| COMPGOTO(e1,_),COMPGOTO(e2,_) -> Result(STMTCOMPGOTO(self#walkExpression(e1,e2)))
		| DEFINITION(def1), DEFINITION(def2) -> Result(STMTDEF(self#walkDefinition (def1,def2)))
			(* FIXME: Ommitting ASM *)
		| TRY_EXCEPT(b1,e1,b2,_), TRY_EXCEPT(b3,e2,b4,_) ->
		  Result(STMTTRYE(self#walkBlock(b1,b3), self#walkExpression(e2,e2), self#walkBlock(b2,b4)))
		| TRY_FINALLY(b1,b2,_), TRY_FINALLY(b3,b4,_) ->
		  Result(STMTTRYF(self#walkBlock(b1,b3),self#walkBlock(b2,b4)))
		| _ -> failwith "Not implemented16")

  method childrenTree ((_,tns1),(_,tns2)) = 
	TNS(lmap (fun (tn1,tn2) -> self#walkTreenode (tn1,tn2)) (best_permutation (distance self#walkTreenode) tns1 tns2))
	  
  method walkTypeSpecifier ts = doWalk compare self#wTypeSpecifier self#childrenTypeSpecifier ts
  method walkSpecifier s = doWalk compare self#wSpecifier self#childrenSpec s
  method walkSpecElem s = doWalk compare self#wSpecElem self#childrenSpecElem s
  method walkDeclType dt = doWalk compare self#wDeclType self#childrenDeclType dt
  method walkExpression exp = doWalk compare self#wExpression self#childrenExpression exp
  method walkInitExpression ie = doWalk compare self#wInitExpression self#childrenInitExpression ie
  method walkStatement stmt = doWalk compare self#wStatement self#childrenStatement stmt
  method walkBlock block = doWalk compare self#wBlock self#childrenBlock block
  method walkDefinition def = doWalk compare self#wDefinition self#childrenDefinition def
  method walkName name = doWalk compare self#wName self#childrenName name
  method walkAttribute a = doWalk compare self#wAttribute self#childrenAttribute a
  method walkTreenode tn = doWalk compare self#wTreeNode self#childrenTreenode tn
  method walkTree t = doWalk compare self#wTree self#childrenTree t

end

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

  method walkGuard g = 
	doWalk compare self#wGuard (fun _ -> failwith "Shouldn't call children on guards!") g

  method walkGuards (guards1,guards2) =
	lmap (fun (g1,g2) -> self#walkGuard (g1,g2)) (best_permutation (self#guard_compare) guards1 guards2)

end

let mytemplate = new templateDoubleWalker
let mycontext = new changesDoubleWalker
let myguard = new guardsDoubleWalker


let unify_guards gset1 gset2 inter = failwith "Not implemented17"
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
	  (Set.map dum_to_context con.surrounding)
	  (lmap (fun (g,e) -> (g,EXPBASE(e))) con.guarded_by)
	  (Set.map dum_to_context con.guarding)
  in
  let changes' = BASECHANGES(lmap (fun x -> ChangeBase(x)) changes) in
	context',changes'

let template_ht = hcreate 10
let hash_itemp it = itemplate_to_str it (* FIXME: SO BACKWARDS *)

let rec template_compare t1 t2 = 
  let best = unify_itemplate t1 t2 in
	let i = Objsize.objsize best in 
	  i.Objsize.data

and unify_itemplate (t1 : init_template) (t2 : init_template) : template = 
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
		  let changes' = mycontext#walkChanges (changes1,changes2) in
			{ptn = parent_treenode';
			 pdef = parent_definition';
			 pstmt = parent_statement';
			 pexp = parent_expression';
			 sding = Set.empty;
			 gby = [];
			 ging = Set.empty;
			 renamed = Map.empty;
			}, changes'
		end)
	
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
				(best_permutation 
				  template_compare templates1 templates2) in
			  liter (fun t -> pprintf "One match: \n"; print_template t) best_map
		  | [template1] -> pprintf "Warning: odd-length list in synth_diff_pairs" ; flush stdout;
		  | [] -> ()
		in
		  synth_diff_pairs ts;
		  pprintf "\n\n Done in testWalk\n\n"; flush stdout


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
