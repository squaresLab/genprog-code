open Batteries
open Utils
open Cabs
open Globals

type 'a walkAction =
	Result of 'a
  | Children
  | CombineChildren of 'a

class virtual ['result_type] cabsWalker = object
  method virtual default_res : unit -> 'result_type
  method virtual combine : 'result_type -> 'result_type -> 'result_type
  method virtual wStatement : statement node -> 'result_type walkAction
  method virtual wDefinition : definition node -> 'result_type walkAction
  method virtual wExpression : expression node -> 'result_type walkAction
  method virtual wTreeNode : tree_node node -> 'result_type walkAction
  method virtual wDeclType : decl_type -> 'result_type walkAction
  method virtual wInitExpression :  init_expression -> 'result_type walkAction
  method virtual wName : name ->  'result_type walkAction

  method virtual walkTreenode : tree_node node -> 'result_type 
  method virtual walkStatement : statement node -> 'result_type 
  method virtual walkExpression : expression node -> 'result_type
  method virtual walkBlock : block -> 'result_type
  method virtual walkDefinition : definition node -> 'result_type
  method virtual walkSpecElem : spec_elem -> 'result_type
  method virtual walkSpecifier : specifier -> 'result_type
  method virtual walkDeclType : decl_type -> 'result_type
  method virtual walkInitExpression : init_expression -> 'result_type
  method virtual walkExpressionList : expression node list -> 'result_type 
  method virtual walkAttribute : attribute ->  'result_type 
  method virtual walkName : name -> 'result_type
  method virtual walkAttributes : attribute list -> 'result_type
  method virtual walkSingleNames : single_name list -> 'result_type
  method virtual walkInitWhat : initwhat -> 'result_type
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

class ['result_type] nopCabsWalker = object(self)
  inherit ['result_type] cabsWalker

  method default_res () = failwith "No default on a nopCabsWalker!"


  method combine res1 res2 = res1

  method wStatement s = Children
  method wDefinition d = Children
  method wExpression e = Children 
  method wTreeNode t = Children
  method wDeclType d = Children
  method wInitExpression i = Children
  method wName n = Children

  method walkTreenode tn =  
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
	let 
		childrenExpression exp =
	  match exp.node with
	  | MEMBEROF(e1,_)
	  | MEMBEROFPTR(e1,_)
	  | EXPR_ALIGNOF(e1)
	  | PAREN(e1)
	  | EXPR_SIZEOF(e1)
	  | UNARY(_,e1) -> self#walkExpression e1
	  | INDEX(e1,e2)
	  | BINARY(_,e1,e2) ->
		self#combine (self#walkExpression e1)
		  (self#walkExpression e2)
	  | QUESTION(e1,e2,e3) ->
		self#combine 
		  (self#walkExpression e1)
		  (self#combine
			 (self#walkExpression e2)
			 (self#walkExpression e3))
	  | CAST((spec,dt),ie) ->
		self#combine
		  (self#walkSpecifier spec)
		  (self#combine 
			 (self#walkDeclType dt)
			 (self#walkInitExpression ie))
	  | CALL(e1,elist) ->
		self#combine 
		  (self#walkExpression e1)
		  (self#walkExpressionList elist)
	  | COMMA(elist) -> self#walkExpressionList elist
	  | TYPE_SIZEOF(spec,dt)
	  | TYPE_ALIGNOF(spec,dt) ->
		self#combine (self#walkSpecifier spec) (self#walkDeclType dt)
	  | GNU_BODY(b) -> self#walkBlock b
	  | _ -> self#default_res()
	in
	  doWalk self#combine self#wExpression childrenExpression e

  method walkBlock block = failwith "Not implemented"
	
  method walkDefinition = failwith "Not implemented"

  method walkSpecElem spec_elem = 
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

  method walkSpecifier specifier = 
	lfoldl (* Maybe make this the same as the others? *)
	  (fun result ->
		fun specelem ->
		  self#combine result (self#walkSpecElem specelem)) (self#default_res()) specifier

  method walkDeclType dt =
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

  method walkInitExpression ie =
	let 
		childrenInitExpression ie =
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

  method walkExpressionList elist = 
	lfoldl (* Maybe make this the same as the others? *)
	  (fun result ->
		fun specelem ->
		  self#combine result (self#walkExpression specelem)) (self#default_res()) elist

  method walkAttribute attr =
	let _,elist = attr in 
	  self#walkExpressionList elist

  method walkName name = 

	let childrenName name = 
	  let _,dt,alist,_ = name in
		self#combine (self#walkDeclType dt)
		  (self#walkAttributes alist)
	in
	  doWalk self#combine self#wName childrenName name

  method walkAttributes alist =
	lfoldl (* Maybe make this the same as the others? *)
	  (fun result ->
		fun (_,elist) ->
		  self#combine result (self#walkExpressionList elist))
	  (self#default_res()) alist
	  
  method walkSingleNames sns =
	lfoldl (* Maybe make this the same as the others? *)
	  (fun result ->
		fun (spec,name) ->
		  self#combine (self#walkSpecifier spec)
			(self#walkName name))
	  (self#default_res()) sns

  method walkInitWhat iw =
	match iw with
      NEXT_INIT -> self#default_res()
	| INFIELD_INIT(_,iw) -> self#walkInitWhat iw
	| ATINDEX_INIT(e1,iw1) ->
	  self#combine (self#walkExpression e1) (self#walkInitWhat iw)
		
	| ATINDEXRANGE_INIT(e1,e2) ->
	  self#combine (self#walkExpression e1)
		(self#walkExpression e2)

  method treeWalk tree = 
	let str,tns = tree in
	  lfoldl
		(fun result ->
		  fun tn ->
			self#combine result (self#walkTreenode tn)) (self#default_res()) tns
end
