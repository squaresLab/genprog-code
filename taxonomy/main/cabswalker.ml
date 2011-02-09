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


class virtual ['result_type,'def_type,'exp_type,'stmt_type,'tn_type] cabsWalker = object
  method virtual default_res : unit -> 'result_type
  method virtual combine : 'result_type -> 'result_type -> 'result_type
  
  method virtual wDefinition : 'def_type -> 'result_type walkAction
  method virtual wExpression : 'exp_type -> 'result_type walkAction
  method virtual wStatement : 'stmt_type -> 'result_type walkAction
  method virtual wTreeNode : 'tn_type -> 'result_type walkAction

  method virtual walkTreenode : 'tn_type -> 'result_type
  method virtual walkStatement : 'stmt_type -> 'result_type
  method virtual walkExpression : 'exp_type -> 'result_type

  method virtual treeWalk : tree -> 'result_type
end

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
  | CAST((spec,dt),ie) -> failwith "Not implemented" (* (childrenSpecifier spec)@(childrenDt dt)@(childrenInitExpression ie)*)
  | CALL(e1,elist) -> e1::elist
  | COMMA(elist) -> elist
  | TYPE_SIZEOF(spec,dt)
  | TYPE_ALIGNOF(spec,dt) -> failwith "Not implemented" (* (childrenSpecifier spec)@(childrenDt dt)*)
  | _ -> []

class ['a] singleCabsWalker = object(self)(* ['a,expression node,statement node,definition node,tree_node node,block,decl_type,init_expression,name] singleCabsWalker = object(walker)*)

  method default_res () = failwith "No default on a single cabs walker!"
  method combine _ = failwith "No combine on a single cabs walker!"

  method wStatement (s : statement node) : 'a walkAction = Children
  method wDefinition (d : definition node) : 'a walkAction = Children
  method wExpression (e : expression node) : 'a walkAction = Children 
  method wTreeNode (t : tree_node node) : 'a walkAction = Children
  method private wDeclType (d : decl_type) : 'a walkAction = Children
  method private wInitExpression (i : init_expression) : 'a walkAction = Children
  method private wName (n : name) : 'a walkAction = Children

  method walkTreenode (tn : tree_node node) : 'a =
  let childrenTreeNode tn = 
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
	in
	doWalk self#combine self#wTreeNode childrenTreeNode tn

  method walkStatement (stmt : statement node) : 'a = 
	let childrenStatement (stmt : statement node) =
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
	in
	  doWalk self#combine self#wStatement childrenStatement stmt

  method walkExpression (e : expression node) = 
	let childExpression exp =
		match exp.node with 
		  GNU_BODY(b) -> failwith "Not implemented"
		| _ ->
	  let echildren = childrenExpression exp in

		  lfoldl
			(fun result ->
			  fun exp -> 
				self#combine result (self#walkExpression exp))
			(self#default_res()) echildren
	in
	  doWalk self#combine self#wExpression childExpression e

  method private walkBlock (block : block) : 'a = failwith "Not implemented"
	
  method private walkDefinition = failwith "Not implemented"



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

  method treeWalk (tree : tree) = 
	let str,tns = tree in
	  lfoldl
		(fun result ->
		  fun tn ->
			self#combine result (self#walkTreenode tn)) (self#default_res()) tns

end


class ['a] doubleCabsWalker = object(self)

  method default_exp () : 'a = failwith "No default on a nop walker!"
  method best_of (res1 : 'a) (res2 : 'a) = res1
  method walkTreeNode (tn : tree_node node) =  self#default_exp ()
  method walkStatement (stmt : statement node) = self#default_exp() 
  method walkExpression ((exp1,exp2) : (expression node * expression node)) = self#default_exp() 
  method wExpression ((exp1,exp2) : (expression node * expression node)) : 'a walkAction = Children
end

(* from here down, it's to walk two trees at once *)

let doDoubleWalk walker combine startvisit children node1 node2 =
  let action = startvisit node1 node2 in
	match action with
	  Result(result) -> result
	| Children -> children walker node1 node2
	| ChildrenPost(fn) -> fn (children walker node1 node2)
	| CombineChildren(result) -> 
	  let result' = children walker node1 node2 in
		combine result result'
	| CombineChildrenPost(result,resfun) ->
	  let result' = children walker node1 node2 in 
		combine result (resfun result')

let rec walkExpression (walker : 'a doubleCabsWalker) (exp1 : expression node) (exp2 : expression node) : 'a =
  doWalk walker#best_of walker#wExpression (childrenDoubleExpression walker) (exp1,exp2)
 
and childrenDoubleExpression (walker : 'a doubleCabsWalker) (exp1,exp2) : 'a = 
  let best_ofs (lst : 'a list) : 'a = 
	lfoldl
	  (fun result ->
		fun eval ->
		  walker#best_of result eval) (walker#default_exp()) lst
  in
  let nothing_exp () : 'a = (* this should be part of the Children function*)
	let echildren = childrenExpression exp2 in 
	  lfoldl
		(fun result ->
		  fun child ->
			walker#best_of
			  result (walkExpression walker exp1 child)) (* FIXME: "onChildren??" *)
		(walker#default_exp()) echildren
  in
	match exp1.node,exp2.node with
	| GNU_BODY(_),_
	| _,GNU_BODY(_) -> failwith "Not implemented"
	| LABELADDR(_),_
	| VARIABLE(_),_
	| EXPR_PATTERN(_),_
	| NOTHING,_
	| CONSTANT(_),_ 
	| _,LABELADDR(_)
	| _,VARIABLE(_)
	| _,EXPR_PATTERN(_) 
	| _,CONSTANT(_) 
	| _,NOTHING -> nothing_exp ()
	| _,_ ->
	  let children1 = childrenExpression exp1 in
	  let children2 = childrenExpression exp2 in
	  let compare_to_all_children (exp1 : expression node) (lst : expression node list) : 'a = 
		lfoldl
		  (fun res ->
			fun child2 ->
			  walker#best_of res (walkExpression walker exp1 child2))
		  (walker#default_exp()) lst
	  in
		best_ofs 
		  [(compare_to_all_children exp1 children2);
		   (compare_to_all_children exp2 children1);
		   (lfoldl
			  (fun child ->
				fun res -> 
				  walker#best_of res
					(compare_to_all_children child children2))
			  (walker#default_exp()) children1)]
			
