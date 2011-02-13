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

class virtual ['exp_rt,'stmt_rt,'def_rt,'tn_rt,'result_type,'exp_type,'stmt_type,'block_type,'def_type,'tn_type,'tree_type] cabsWalker = object(self)
  method virtual default_res : unit -> 'result_type
  method virtual combine : 'result_type -> 'result_type -> 'result_type
  method virtual combine_exp : 'exp_rt -> 'exp_rt -> 'exp_rt
  method virtual combine_stmt : 'stmt_rt -> 'stmt_rt -> 'stmt_rt
  method virtual combine_def : 'def_rt -> 'def_rt -> 'def_rt
  method virtual combine_tn : 'tn_rt -> 'tn_rt -> 'tn_rt

  method wBlock (b : 'block_type) : 'stmt_rt walkAction = Children
  method wExpression (e : 'exp_type) : 'exp_rt walkAction = Children
  method wStatement (s : 'stmt_type) : 'stmt_rt walkAction = Children
  method wDefinition (d : 'def_type) : 'def_rt walkAction = Children
  method wTreeNode (tn : 'tn_type) : 'tn_rt walkAction = Children
  method wTree (tree : 'tree_type) : 'resultType walkAction = Children

  method virtual childrenExpression : 'exp_type -> 'exp_rt
  method virtual childrenStatement : 'stmt_type -> 'stmt_rt
  method virtual childrenDefinition : 'def_type -> 'def_rt
  method virtual childrenTreenode : 'tn_type -> 'tn_rt
  method virtual childrenTree : 'tree_type -> 'result_type

  method walkExpression (exp : 'exp_type) : 'exp_rt = 
	doWalk self#combine_exp self#wExpression self#childrenExpression exp

  method walkStatement (stmt : 'stmt_type) : 'stmt_rt = 
	doWalk self#combine_stmt self#wStatement self#childrenStatement stmt

  method walkDefinition (def : 'def_type) : 'def_rt =
	doWalk self#combine_def self#wDefinition self#childrenDefinition def

  method walkTreenode (tn : 'tn_type) : 'tn_rt =
	doWalk self#combine_tn self#wTreeNode self#childrenTreenode tn

  method walkTree (t : 'tree_type) : 'tree_rt = 
	doWalk self#combine self#wTree self#childrenTree t
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
  inherit ['a,'a,'a,'a,'a,expression node,statement node,block,definition node,tree_node node,tree] cabsWalker

  method combine_exp e1 e2 = self#combine e1 e2
  method combine_stmt s1 s2 = self#combine s1 s2
  method combine_def d1 d2 = self#combine d1 d2
  method combine_tn tn1 tn2 = self#combine tn1 tn2

  method private wDeclType (d : decl_type) : 'a walkAction = Children
  method private wInitExpression (i : init_expression) : 'a walkAction = Children
  method private wName (n : name) : 'a walkAction = Children

  method childrenTree (_,tns) =
	lfoldl
	  (fun res ->
		fun tn ->
		  self#combine res (self#walkTreenode tn))
	  (self#default_res()) tns

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

  method childrenDefinition def = failwith "Not implemented4a"


  method walkBlock (block : block) = 
	doWalk self#combine_stmt self#wBlock self#childrenBlock block

  method childrenBlock (block : block) = self#default_res()
	
  method private walkDefinition = failwith "Not implemented5"

  method private walkSpecElem spec_elem = 
	match spec_elem with
	| SpecAttr(attr) -> self#walkAttribute attr
	| SpecType(ts) -> 
	  begin
		match ts with
		| Tstruct(_,Some(fgs),attrs)
		| Tunion(_,Some(fgs),attrs) ->
		  self#combine
			(lfoldl
			   (fun result ->
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
			(self#walkAttributes attrs)
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
	  end
	| _ -> self#default_res ()

  method private walkSpecifier specifier = 
	lfoldl (* Maybe make this the same as the others? *)
	  (fun result ->
		fun specelem ->
		  self#combine result (self#walkSpecElem specelem)) (self#default_res()) specifier

  method private walkDeclType dt =
	let childrenDeclType dt =
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
		self#combine (self#walkDeclType dt) (self#walkSingleNames sns)
	in
	  doWalk self#combine self#wDeclType childrenDeclType dt

  method private walkInitExpression ie =
	let childrenInitExpression ie =
	  match ie with
	  | NO_INIT ->self#default_res()
	  | SINGLE_INIT(e1) -> self#walkExpression e1
	  | COMPOUND_INIT(iwies) -> 
		lfoldl
		  (fun result ->
			fun (iw,ie) ->
			  self#combine result
				(self#combine (self#walkInitWhat iw) (self#walkInitExpression ie)))
		  (self#default_res()) iwies
	in
	  doWalk self#combine self#wInitExpression childrenInitExpression ie

  method private walkExpressionList elist = 
	lfoldl (* Maybe make this the same as the others? *)
	  (fun result ->
		fun specelem ->
		  self#combine result (self#walkExpression specelem)) (self#default_res()) elist

  method private walkAttribute attr =
	let _,elist = attr in 
	  self#walkExpressionList elist

  method private walkName name = 
	let childrenName name = 
	  let _,dt,alist,_ = name in
		self#combine (self#walkDeclType dt)
		  (self#walkAttributes alist)
	in
	  doWalk self#combine self#wName childrenName name

  method private walkAttributes alist =
	lfoldl (* Maybe make this the same as the others? *)
	  (fun result ->
		fun (_,elist) ->
		  self#combine result (self#walkExpressionList elist))
	  (self#default_res()) alist
	  
  method private walkSingleNames sns =
	lfoldl (* Maybe make this the same as the others? *)
	  (fun result ->
		fun (spec,name) ->
		  self#combine (self#walkSpecifier spec)
			(self#walkName name))
	  (self#default_res()) sns

  method private walkInitWhat iw =
	match iw with
      NEXT_INIT -> self#default_res()
	| INFIELD_INIT(_,iw) -> self#walkInitWhat iw
	| ATINDEX_INIT(e1,iw1) ->
	  self#combine (self#walkExpression e1) (self#walkInitWhat iw)
		
	| ATINDEXRANGE_INIT(e1,e2) ->
	  self#combine (self#walkExpression e1)
		(self#walkExpression e2)

  method walkTree (tree : tree) = 
	let str,tns = tree in
	  lfoldl
		(fun result ->
		  fun tn ->
			self#combine result (self#walkTreenode tn)) (self#default_res()) tns

end

class virtual ['exp_rt,'stmt_rt,'def_rt,'tn_rt,'tree_rt] doubleCabsWalker = object(self)
  inherit ['exp_rt,'stmt_rt,'def_rt,'tn_rt,'tree_rt,(expression node *  expression node),
		   (statement node * statement node),(block * block),(definition node * definition node),
		   (tree_node node * tree_node node),(tree * tree)] cabsWalker

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

  method childrenDefinition (def1,def2) = failwith "Children definition not implemented"

  method childrenTreenode (tn1,tn2) = self#default_tn ()
end

