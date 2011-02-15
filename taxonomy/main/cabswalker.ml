open Batteries
open Utils
open Cabs
open Globals
open Ttypes

type 'a walkAction =
	Result of 'a
  | Children
  | ChildrenPost of ('a -> 'a)
  | CombineChildren of 'a
  | CombineChildrenPost of 'a * ('a -> 'a)


let doWalk combine
	(startvisit : 'node -> 'a walkAction)  
	(children : 'node -> 'a)
	(node : 'node) =
  let action = startvisit node in
	match action with
	  Result(result) -> result
	| Children -> children node
	| CombineChildren(result) -> 
	  let result' = children node in
		combine result result'
	| CombineChildrenPost(result,resfun) ->
	  let result' = children node in 
		combine result (resfun result')
	| ChildrenPost(fn) -> fn (children node)

class virtual ['result_type,'ts_type,'se_type,'spec_type,'dt_type,'ng_type,'ing_type,'name_type,'init_name_type,'single_name_type,'def_type,'block_type,'stmt_type,'exp_type,'ie_type,'attr_type,'tn_type,'tree_type,
			   'ts_rt,'se_rt,'spec_rt,'dt_rt,'ng_rt,'ing_rt,'name_rt,'init_name_rt,'single_name_rt,'def_rt,'block_rt,'stmt_rt,'exp_rt,'ie_rt,'attr_rt,'tn_rt] cabsWalker =
object(self)
			   
  method virtual default_res : unit -> 'result_type
  method virtual combine : 'result_type -> 'result_type -> 'result_type

  method wTypeSpecifier (t : 'ts_type) : 'ts_rt walkAction = Children
  method wSpecifier (s : 'spec_type) : 'spec_rt walkAction = Children
  method wDeclType (d: 'dt_type) : 'dt_rt walkAction = Children
  method wExpression (e : 'exp_type) : 'exp_rt walkAction = Children
  method wInitExpression (ie : 'ie_type) : 'ie_rt walkAction = Children
  method wStatement (s : 'stmt_type) : 'stmt_rt walkAction = Children
  method wBlock (b : 'block_type) : 'block_rt walkAction = Children
  method wDefinition (d : 'def_type) : 'def_rt walkAction = Children
  method wName (n: 'name_type) : 'name_rt walkAction = Children
  method wAttribute (a : 'attr_type) : 'attr_rt walkAction = Children
  method wTreeNode (tn : 'tn_type) : 'tn_rt walkAction = Children
  method wTree (tree : 'tree_type) : 'result_type walkAction = Children
  method wSpecElem (se : 'se_type) : 'se_rt walkAction = Children

  method virtual childrenTypeSpecifier : 'ts_type -> 'ts_rt
  method virtual childrenSpecElem : 'se_type -> 'se_rt
  method virtual childrenSpec : 'spec_type -> 'spec_rt
  method virtual childrenDeclType : 'dt_type -> 'dt_rt
  method virtual childrenNameGroup : 'ng_type -> 'ng_rt
  method virtual childrenInitNameGroup : 'ing_type -> 'ing_rt
  method virtual childrenName : 'name_type -> 'name_rt
  method virtual childrenInitName : 'init_name_type -> 'init_name_rt
  method virtual childrenSingleName : 'single_name_type -> 'single_name_rt
  method virtual childrenDefinition : 'def_type -> 'def_rt
  method virtual childrenBlock : 'block_type -> 'block_rt
  method virtual childrenStatement : 'stmt_type -> 'stmt_rt
  method virtual childrenExpression : 'exp_type -> 'exp_rt
  method virtual childrenInitExpression : 'ie_type -> 'ie_rt
  method virtual childrenAttribute: 'attr_type -> 'attr_rt
  method virtual childrenTreenode : 'tn_type -> 'tn_rt
  method virtual childrenTree : 'tree_type -> 'result_type

  method enterScope () = ()
  method exitScope () = ()

  method virtual walkTypeSpecifier : 'ts_type -> 'ts_rt
  method virtual walkSpecifier : 'spec_type -> 'spec_rt
  method virtual walkSpecElem : 'se_type-> 'se_rt
  method virtual walkDeclType : 'dt_type -> 'dt_rt
  method virtual walkExpression : 'exp_type -> 'exp_rt
  method virtual walkInitExpression : 'ie_type -> 'ie_rt
  method virtual walkStatement : 'stmt_type -> 'stmt_rt
  method virtual walkBlock : 'block_type -> 'block_rt
  method virtual walkDefinition : 'def_type -> 'def_rt
  method virtual walkName : 'name_type -> 'name_rt
  method virtual walkAttribute : 'attr_type -> 'attr_rt
  method virtual walkTreenode : 'tn_type -> 'tn_rt
  method virtual walkTree : 'tree_type -> 'result_type

end

let childrenExpression (exp : expression node) : expression node list =
(* fixme: only return list of expressions, so be careful with childrenspecifier, for example *)
  match exp.node with
  | MEMBEROF(e1,_)
  | MEMBEROFPTR(e1,_)
  | EXPR_ALIGNOF(e1)
  | PAREN(e1)
  | EXPR_SIZEOF(e1)
  | UNARY(_,e1) -> [e1]
  | INDEX(e1,e2)
  | BINARY(_,e1,e2) -> [e1;e2]
  | QUESTION(e1,e2,e3) -> [e1;e2;e3]
  | CAST((spec,dt),ie) -> failwith "Not implemented1" (* (childrenSpecifier spec)@(childrenDt dt)@(childrenInitExpression ie)*)
  | CALL(e1,elist) -> e1::elist
  | COMMA(elist) -> elist
  | TYPE_SIZEOF(spec,dt)
  | TYPE_ALIGNOF(spec,dt) -> failwith "Not implemented2" (* (childrenSpecifier spec)@(childrenDt dt)*)
  | _ -> []

let childrenStatement (stmt : statement node) : statement node list =
  match stmt.node with
  | BLOCK(b,_) -> b.bstmts
  | SEQUENCE(s1,s2,_)
  | IF(_,s1,s2,_) -> [s1;s2]
  | WHILE(_,s1,_)
  | DOWHILE(_,s1,_) 
  | SWITCH(_,s1,_)
  | CASE(_,s1,_)
  | CASERANGE(_,_,s1,_)
  | DEFAULT(s1,_)
  | LABEL(_,s1,_)
  | FOR(_,_,_,s1,_) -> [s1]
  | TRY_EXCEPT(b1,_,b2,_)
  | TRY_FINALLY(b1,b2,_) -> b1.bstmts @ b2.bstmts
  | _ -> []
	
let best_ofs lst combine default =
  lfoldl
	(fun result ->
	  fun eval ->
		combine result eval) default lst

class virtual ['a] singleCabsWalker = object(self)
  inherit ['a,typeSpecifier,spec_elem,specifier,decl_type,name_group,init_name_group,name,init_name,single_name,definition node,block,statement node,expression node,init_expression,attribute,tree_node node,tree,
		   'a,'a,'a,'a,'a,'a,'a,'a,'a,'a,'a,'a,'a,'a,'a,'a] cabsWalker as super

  method private walkAttributes (attrs : attribute list) : 'a = 
	lfoldl (fun res -> fun attr -> self#combine res (self#walkAttribute attr)) (self#default_res()) attrs

  method childrenTypeSpecifier (ts : typeSpecifier) : 'a =
	match ts with
  | Tstruct(_,Some(fgs),attrs)
  | Tunion(_,Some(fgs),attrs) ->
	self#combine
	  (self#walkAttributes attrs)
	  (lfoldl
		(fun (result : 'a) ->
		  fun (spec,lst) ->
			self#combine 
			  (self#walkSpecifier spec)
			  (lfoldl
				 (fun result ->
				   fun (name,eo) ->
					 let name1 = self#walkName name in
					   match eo with
						 None -> name1
					   | Some(e) ->
						 self#combine (self#walkExpression e) name1) 
				 result lst))
		(self#default_res()) fgs)
  | Tenum(_,Some(eis),attrs) ->
	self#combine
	  (lfoldl
		 (fun result ->
		   fun (_,e1,_) ->
			 self#combine result (self#walkExpression e1)) (self#default_res()) eis)
	  (self#walkAttributes attrs)
  | Tenum(_,None,attrs)
  | Tstruct(_,None,attrs)
  | Tunion(_,None,attrs) -> self#walkAttributes attrs
  | TtypeofE(e1) -> self#walkExpression e1
  | TtypeofT(spec,dt) ->
	self#combine (self#walkSpecifier spec)
	  (self#walkDeclType dt)
  | _ -> self#default_res ()

  method childrenSpecElem se =
	match se with
  | SpecAttr(attr) -> self#walkAttribute attr
  | SpecType(ts) -> self#walkTypeSpecifier ts
  | _ -> self#default_res ()

  method childrenTree (_,tns) =
	lfoldl
	  (fun res ->
		fun tn ->
		  self#combine res (self#walkTreenode tn))
	  (self#default_res()) tns

  method childrenSpec specifier = 
	lfoldl (* Maybe make this the same as the others? *)
	  (fun result ->
		fun specelem ->
		  self#combine result (self#walkSpecElem specelem)) (self#default_res()) specifier

  method childrenDeclType dt =
	match dt with
	| JUSTBASE -> self#default_res ()
	| PARENTYPE(alist1,dt,alist2) -> 
	  self#combine 
		(self#walkAttributes alist1)
		(self#combine (self#walkDeclType dt)
		   (self#walkAttributes alist2))
	| ARRAY(dt,alist,exp) ->
	  self#combine 
		(self#walkDeclType dt)
		(self#combine
		   (self#walkAttributes alist)
		   (self#walkExpression exp))
	| PTR(alist,dt) -> 
	  self#combine 
		(self#walkAttributes alist)
		(self#walkDeclType dt)
	| PROTO(dt,sns,_) ->
	  self#combine (self#walkDeclType dt) 
		(lfoldl (fun res -> fun sn -> self#combine (self#childrenSingleName sn) res) (self#default_res()) sns)

  method childrenNameGroup (spec,names) = 
	lfoldl (fun res -> fun name -> self#combine res (self#walkName name))
	  (self#walkSpecifier spec) names

  method childrenInitNameGroup (spec,ins) =
	lfoldl (fun res -> fun iname -> self#combine res (self#childrenInitName iname)) (self#walkSpecifier spec) ins

  method childrenInitName (name,ie) =
	self#combine (self#walkName name) (self#walkInitExpression ie)

  method childrenName name = 
	let sn,dt,al,loc = name in
	let val1 = self#walkDeclType dt in
	let val2 = self#walkAttributes al in 
	  self#combine val1 val2

  method childrenSingleName (spec,name) = 
	self#combine (self#walkSpecifier spec) (self#walkName name)

  method childrenDefinition def = failwith "Not implemented4a"

  method childrenBlock (block : block) = 
	lfoldl
	  (fun res ->
		fun stmt -> self#combine (self#walkStatement stmt) res)
	  (lfoldl
		 (fun res ->
		   fun attr ->
			 self#combine (self#walkAttribute attr) res)
		 (self#default_res()) block.battrs) block.bstmts

  method childrenStatement (stmt : statement node) =
	match stmt.node with
	| COMPUTATION(exp,_) -> self#walkExpression exp
	| BLOCK(b,_) -> self#walkBlock b
	| SEQUENCE(s1,s2,_) -> 
	  self#combine (self#walkStatement s1) (self#walkStatement s2)
	| IF(e1,s1,s2,_) -> 
	  self#combine 
		(self#walkExpression e1)
		(self#combine
		   (self#walkStatement s1)
		   (self#walkStatement s2))
	| WHILE(e1,s1,_) 
	| DOWHILE(e1,s1,_) -> 
	  self#combine (self#walkExpression e1)
		(self#walkStatement s1)
	| FOR(fc,e1,e2,s1,_) ->
	  self#combine
		(match fc with
		  FC_EXP(fce) -> self#walkExpression fce 
		| FC_DECL(def) -> self#walkDefinition def)
		(self#combine (self#walkExpression e1)
		   (self#combine (self#walkExpression e2)
			  (self#walkStatement s1)))
	| COMPGOTO(e1,_)
	| RETURN(e1,_) -> self#walkExpression e1
	| SWITCH(e1,s1,_)
	| CASE(e1,s1,_) ->
	  self#combine (self#walkExpression e1)
		(self#walkStatement s1)
	| CASERANGE(e1,e2,s1,_) -> 
	  self#combine (self#walkExpression e1)
		(self#combine (self#walkExpression e2)
		   (self#walkStatement s1))
	| DEFAULT(s1,_)
	| LABEL(_,s1,_) -> self#walkStatement s1
	| DEFINITION(def) -> self#walkDefinition def
	| ASM(alist,_,ao,_) -> 
	  let alist1 = self#walkAttributes alist in
		begin
		  match ao with
			None -> alist1
		  | Some(ao) ->
			let doao = 
			  lfoldl (* Maybe make this the same as the others? *)
				(fun result ->
				  fun (_,_,exp) ->
					self#combine result (self#walkExpression exp)) (self#default_res())
			in
			  self#combine (doao ao.aoutputs) 
				(self#combine (doao ao.ainputs)
				   alist1)
		end
	| TRY_EXCEPT(b1,e1,b2,_) ->
	  self#combine (self#walkBlock b1)
		(self#combine (self#walkExpression e1)
		   (self#walkBlock b2))
	| TRY_FINALLY(b1,b2,_) ->
	  self#combine (self#walkBlock b1) (self#walkBlock b2)
	| _ -> self#default_res ()

  method childrenExpression exp =
	match exp.node with 
	  GNU_BODY(b) -> failwith "Not implemented3"
	| _ ->
	  let echildren = childrenExpression exp in
		lfoldl
		  (fun result ->
			fun exp -> 
			  self#combine result (self#walkExpression exp))
		  (self#default_res()) echildren

  method childrenInitExpression ie = 
	let rec childrenInitWhat iw = 
	  match iw with
		NEXT_INIT -> self#default_res()
	    | INFIELD_INIT(str,iw) -> childrenInitWhat iw
		| ATINDEX_INIT(exp,iw) -> self#combine (self#walkExpression exp) (childrenInitWhat iw)
		| ATINDEXRANGE_INIT(exp1,exp2) -> self#combine (self#walkExpression exp1) (self#walkExpression exp2)
	in
	  match ie with
	| NO_INIT ->self#default_res()
	| SINGLE_INIT(e1) -> self#walkExpression e1
	| COMPOUND_INIT(iwies) -> 
	  lfoldl
		(fun result ->
		  fun (iw,ie) ->
			self#combine result  (self#combine (self#walkInitExpression ie)
									(childrenInitWhat iw)))
		(self#default_res()) iwies

  method private childrenAttribute attr =
	let _,elist = attr in 
	  self#walkExpressionList elist

  method childrenTreenode tn = 
	match tn.node with
	| Globals(dlist) ->
	  lfoldl (* Maybe make this the same as the others? *)
		(fun result ->
		  fun def ->
			let result1 = self#walkDefinition def in
			  self#combine result1 result) (self#default_res()) dlist
	| Stmts(slist) ->
	  lfoldl (* Maybe make this the same as the others? *)
		(fun result ->
		  fun stmt ->
			let result1 = self#walkStatement stmt in
			  self#combine result1 result) (self#default_res()) slist
	| Exps(elist) -> self#walkExpressionList elist
	| Syntax(_) -> self#default_res()

  method private walkExpressionList elist = 
	lfoldl (* Maybe make this the same as the others? *)
	  (fun result ->
		fun specelem ->
		  self#combine result (self#walkExpression specelem)) (self#default_res()) elist

  method walkTree (tree : tree) = 
	let str,tns = tree in
	  lfoldl
		(fun result ->
		  fun tn ->
			self#combine result (self#walkTreenode tn)) (self#default_res()) tns

  method walkTypeSpecifier ts = doWalk self#combine self#wTypeSpecifier self#childrenTypeSpecifier ts
  method walkSpecifier s = doWalk self#combine self#wSpecifier self#childrenSpec s
  method walkSpecElem s = doWalk self#combine self#wSpecElem self#childrenSpecElem s
  method walkDeclType dt = doWalk self#combine self#wDeclType self#childrenDeclType dt
  method walkExpression exp = doWalk self#combine self#wExpression self#childrenExpression exp
  method walkInitExpression ie = doWalk self#combine self#wInitExpression self#childrenInitExpression ie
  method walkStatement stmt = doWalk self#combine self#wStatement self#childrenStatement stmt
  method walkBlock block = doWalk self#combine self#wBlock self#childrenBlock block
  method walkDefinition def = doWalk self#combine self#wDefinition self#childrenDefinition def
  method walkName name = doWalk self#combine self#wName self#childrenName name
  method walkAttribute a = doWalk self#combine self#wAttribute self#childrenAttribute a
  method walkTreenode tn = doWalk self#combine self#wTreeNode self#childrenTreenode tn
  method walkTree t = doWalk self#combine self#wTree self#childrenTree t
end

class virtual ['result_type,'ts_rt,'se_rt,'spec_rt,'dt_rt,'ng_rt,'ing_rt,'name_rt,'init_name_rt,'single_name_rt,'def_rt,'block_rt,'stmt_rt,'exp_rt,'ie_rt,'attr_rt,'tn_rt] doubleCabsWalker = object(self)
  inherit
	['result_type, (typeSpecifier * typeSpecifier),(spec_elem * spec_elem),(specifier * specifier),decl_type * decl_type,
	 name_group * name_group, init_name_group * init_name_group, name * name, init_name * init_name, single_name * single_name,
	 definition node * definition node, block * block, statement node * statement node, expression node * expression node, init_expression * init_expression,
	 attribute * attribute, tree_node node * tree_node node, tree * tree, 
	 'ts_rt,'se_rt,'spec_rt,'dt_rt,'ng_rt,'ing_rt,'name_rt,'init_name_rt,'single_name_rt,'def_rt,'block_rt,'stmt_rt,'exp_rt,'ie_rt,'attr_rt,'tn_rt] cabsWalker as super

  method virtual default_exp : unit -> 'exp_rt
  method virtual default_stmt : unit -> 'stmt_rt
  method virtual default_def : unit -> 'def_rt
  method virtual default_tn : unit -> 'tn_rt
  method virtual combine_exp : 'exp_rt -> 'exp_rt -> 'exp_rt
  method virtual combine_stmt : 'stmt_rt -> 'stmt_rt -> 'stmt_rt
  method virtual combine_def : 'def_rt -> 'def_rt -> 'def_rt
  method virtual combine_tn : 'tn_rt -> 'tn_rt -> 'tn_rt

  method childrenExpression (exp1,exp2) =
	let nothing_exp exp1 exp2 =
	  let echildren = childrenExpression exp1 in 
		lfoldl
		  (fun result ->
			fun child ->
			  self#combine_exp
				result (self#walkExpression (exp2,child))) (* FIXME: "onChildren??" *)
		  (self#default_exp()) echildren
	in
	  match exp1.node,exp2.node with
	  | GNU_BODY(_),_
	  | _,GNU_BODY(_) -> failwith "Not implemented6"
	  | LABELADDR(_),_
	  | VARIABLE(_),_
	  | EXPR_PATTERN(_),_
	  | NOTHING,_
	  | CONSTANT(_),_ -> nothing_exp exp2 exp1
	  | _,LABELADDR(_)
	  | _,VARIABLE(_)
	  | _,EXPR_PATTERN(_) 
	  | _,CONSTANT(_) 
	  | _,NOTHING -> nothing_exp exp1 exp2
	  | _,_ -> 
		let children1 = childrenExpression exp1 in
		let children2 = childrenExpression exp2 in
		let compare_to_all_children exp1 lst =
		  lfoldl
			(fun res ->
			  fun child2 ->
				self#combine_exp res (self#walkExpression (exp1,child2)))
			(self#default_exp()) lst
		in
		  best_ofs 
			[(compare_to_all_children exp1 children2);
			 (compare_to_all_children exp2 children1);
			 (lfoldl
				(fun res ->
				  fun child -> 
					self#combine_exp res
					  (compare_to_all_children child children2))
				(self#default_exp()) children1)] (self#combine_exp) (self#default_exp())

  method childrenStatement (stmt1,stmt2) = 
	let nothing_stmt stmt1 stmt2 =
	  let schildren = childrenStatement stmt1 in
		lfoldl
		  (fun result ->
			fun child ->
			  self#combine_stmt
				result (self#walkStatement (stmt2,child))) (* FIXME: "onChildren??" *)
		  (self#default_stmt()) schildren
	in
	  match stmt1.node,stmt2.node with
	  | NOP(_),_
	  | BREAK(_),_
	  | CONTINUE(_),_
	  | GOTO(_),_ -> nothing_stmt stmt2 stmt1
	  | _,NOP(_)
	  | _,BREAK(_)
	  | _,CONTINUE(_) 
	  | _,GOTO(_) -> nothing_stmt stmt1 stmt2 
	  | DEFINITION(def),_
	  | _,DEFINITION(def) -> failwith "Not implemented7"
	  | _,_ ->
		let children1 = childrenStatement stmt1 in
		let children2 = childrenStatement stmt2 in
		let compare_to_all_children stmt1 lst =
		  lfoldl
			(fun res ->
			  fun child2 ->
				self#combine_stmt res (self#walkStatement (stmt1,child2)))
			(self#default_stmt()) lst
		in

		  best_ofs
			[(compare_to_all_children stmt1 children2);
			 (compare_to_all_children stmt2 children1);
			 (lfoldl 
				(fun res ->
				  fun child ->
					self#combine_stmt res
					  (compare_to_all_children child children2))
				(self#default_stmt()) children1)] (self#combine_stmt) (self#default_stmt())

  method childrenDefinition (def1,def2) = failwith "Children definition ont implemented"
  method childrenTypeSpecifier (ts1,ts2) = failwith "Children type specifier not implemented"
  method childrenSpecElem (se1,se2) =  failwith "Children type specifier not implemented"
  method childrenSpec (se1,se2) =  failwith "Children type specifier not implemented"
  method childrenSingleName (se1,se2) =  failwith "Children type specifier not implemented"
  method childrenNameGroup (se1,se2) =  failwith "Children type specifier not implemented"
  method childrenName (se1,se2) =  failwith "Children type specifier not implemented"
  method childrenAttribute (se1,se2) =  failwith "Children type specifier not implemented"
  method childrenInitNameGroup (se1,se2) =  failwith "Children type specifier not implemented"
  method childrenInitName (se1,se2) =  failwith "Children type specifier not implemented"
  method childrenInitExpression (ie1,ie2) =    failwith "Children type specifier not implemented"
  method childrenDeclType (se1,se2) =  failwith "Children type specifier not implemented"
  method childrenBlock (se1,se2) =  failwith "Children type specifier not implemented"
  method childrenTreenode (tn1,tn2) = self#default_tn ()
end

