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
  method wTreenode (tn : 'tn_type) : 'tn_rt walkAction = Children
  method wTree (tree : 'tree_type) : 'result_type walkAction = Children
  method wSpecElem (se : 'se_type) : 'se_rt walkAction = Children
  method wSingleName (sn : 'single_name_type) : 'single_name_rt walkAction = Children
  method wNameGroup (ng : 'ng_type) : 'ng_rt walkAction = Children
  method wInitNameGroup (ing : 'ing_type) : 'ing_rt walkAction = Children
  method wInitName (inn : 'init_name_type) : 'init_name_rt walkAction = Children

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
  method virtual walkSingleName : 'single_name_type -> 'single_name_rt
  method virtual walkInitNameGroup : 'ing_type -> 'ing_rt
  method virtual walkInitName : 'init_name_type -> 'init_name_rt
  method virtual walkNameGroup : 'ng_type -> 'ng_rt

end

let rec childrenExpression (exp : expression node) : expression node list =
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
	| CALL(e1,elist) -> e1::elist
	| COMMA(elist) -> elist
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

  method childrenSpecifier spec =
	lfoldl
	  (fun sofar ->
		fun spec_elem ->
		  self#combine sofar (self#walkSpecElem spec_elem)) [] spec

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
	| GNU_BODY(b) -> self#walkBlock b
 	| CAST((spec1,dt1),ie1) -> self#combine (self#walkSpecifier spec1) (self#combine (self#walkDeclType dt1) (self#walkInitExpression ie1))
	| TYPE_SIZEOF(spec,dt)
	| TYPE_ALIGNOF(spec,dt) -> self#combine (self#walkSpecifier spec) (self#walkDeclType dt)
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
  method walkTreenode tn = doWalk self#combine self#wTreenode (fun _ -> failwith "Children treenode should never be called in doublewalker") tn
  method walkTree t = doWalk self#combine self#wTree self#childrenTree t
  method walkSingleName sn = doWalk self#combine self#wSingleName self#childrenSingleName sn
  method walkInitNameGroup ing = doWalk self#combine self#wInitNameGroup self#childrenInitNameGroup ing
  method walkInitName ing = doWalk self#combine self#wInitName self#childrenInitName ing
  method walkNameGroup ng = doWalk self#combine self#wNameGroup self#childrenNameGroup ng

end

class expressionChildren = object (self)
  inherit [expression node list] singleCabsWalker as super

  method combine elist1 elist2 = elist1 @ elist2
  method default_res () = []

  method walkExpression exp = [exp]
  method walkExpressionList elist = elist
  method childrenTreenode tn =
	match tn.node with
	  Exps(elist) -> elist
	| _ -> super#childrenTreenode tn

  method startWalk exp = 
	match exp.node with 
	| UNARY(_,exp)
	| PAREN(exp)
	| EXPR_SIZEOF(exp)
	| EXPR_ALIGNOF(exp) 
	| MEMBEROF(exp,_)
	| MEMBEROFPTR(exp,_) -> [exp]
	| INDEX(exp1,exp2)
	| BINARY(_,exp1,exp2) -> [exp1;exp2]
	| QUESTION(exp1,exp2,exp3) -> [exp1;exp2;exp3]
	| CAST((spec,dt),ie) -> 
	  self#combine (self#walkSpecifier spec)
		(self#combine (self#walkDeclType dt)
		   (self#walkInitExpression ie))
	| CALL(e1,elist) -> e1 :: elist
	| COMMA(elist) -> elist
	| TYPE_SIZEOF(spec,dt)
	| TYPE_ALIGNOF(spec,dt) -> 
	  self#combine (self#walkSpecifier spec) (self#walkDeclType dt)
	| GNU_BODY(b) -> self#walkBlock b
	| _ -> []
end


class statementChildren = object (self)
  inherit [statement node list] singleCabsWalker as super

  method combine elist1 elist2 = elist1 @ elist2
  method default_res () = []

  method walkStatement stmt = [stmt]
  method childrenTreenode tn =
	match tn.node with
	  Stmts(elist) -> elist
	| _ -> super#childrenTreenode tn

  method startWalk stmt = 
	match stmt.node with
  | COMPUTATION(exp,_)
  | RETURN(exp,_)
  | COMPGOTO(exp,_) -> self#walkExpression exp
  | BLOCK(b,_) -> self#walkBlock b
  | SEQUENCE(s1,s2,_) -> [s1;s2]
  | IF(e1,s1,s2,_) ->  (self#walkExpression e1) @ [s1;s2]
  | WHILE(exp,s1,_)
  | DOWHILE(exp,s1,_) 
  | SWITCH(exp,s1,_)
  | CASE(exp,s1,_) -> s1 :: (self#walkExpression exp)
  | FOR(fc,e1,e2,s1,_) ->
	s1 :: (match fc with
	  FC_EXP(e3) -> self#walkExpression e3
	| FC_DECL(def) -> self#walkDefinition def) @
	  (self#walkExpression e1) @ (self#walkExpression e2)
  | CASERANGE(e1,e2,s1,_) ->
	s1 ::((self#walkExpression e1) @ (self#walkExpression e2))
  | DEFAULT(s1,_)
  | LABEL(_,s1,_) -> [s1]
  | DEFINITION(def) -> self#walkDefinition def
  | ASM(attrs,_,ad,_) -> 
	let ado = lfoldl (fun sofar -> fun (_,_,e) -> sofar @ (self#walkExpression e)) [] in
	  (self#walkAttributes attrs) @ (match ad with 
		Some(det) -> (ado det.aoutputs) @ (ado det.ainputs)
	  | None -> [])
  | TRY_EXCEPT(b1,exp,b2,_) -> (self#walkBlock b1) @ (self#walkExpression exp) @ (self#walkBlock b2)
  | TRY_FINALLY(b1,b2,_) -> (self#walkBlock b1) @ (self#walkBlock b2)
  | _ -> []	  
end

class virtual ['a] childrenWalker = object(self)
  inherit ['a list] singleCabsWalker as super
  method combine elist1 elist2 = elist1 @ elist2
  method default_res () = []
  method virtual startWalk : 'a -> 'a list
end

class definitionChildren = object (self)
  inherit [definition node] childrenWalker as super

  method walkDefinition def = [def]
  method childrenTreenode tn =
	match tn.node with
	  Globals(elist) -> elist
	| _ -> super#childrenTreenode tn

  method startWalk def =
	match def.node with 
	  FUNDEF(sn,b,_,_) -> (self#walkSingleName sn) @ (self#walkBlock b)
	| DECDEF(ing,_) -> self#walkInitNameGroup ing
	| TYPEDEF(ng,_) -> self#walkNameGroup ng
	| ONLYTYPEDEF(spec,_) -> self#walkSpecifier spec
	| PRAGMA(exp,_) -> self#walkExpression exp
	| LINKAGE(_,_,dlist) -> dlist
	| _ -> []
end

class typeSpecifierChildren = object (self)
  inherit [typeSpecifier] childrenWalker as super

  method walkTypeSpecifier ts = [ts]

  method startWalk = function
  | Tstruct(_,None,attrs)
  | Tunion(_,None,attrs)
  | Tenum(_,None,attrs) -> self#walkAttributes attrs
  | Tstruct(_,Some(lst),attrs)
  | Tunion(_,Some(lst),attrs) ->
	lfoldl 
	  (fun sofar -> 
		fun (spec,lst2) ->
		  lfoldl
			(fun sofar ->
			  fun (nme,eno) ->
				match eno with 
				| Some(e) -> (self#walkExpression e) @ (self#walkName nme)
				| None ->self#walkName nme)
			(self#walkSpecifier spec) lst2) (self#walkAttributes attrs) lst
  | Tenum(_,Some(eis),attrs) -> 
	lfoldl
	  (fun sofar ->
		fun (_,e,_) -> (self#walkExpression e) @ sofar ) (self#walkAttributes attrs) eis
  | TtypeofE(exp) -> self#walkExpression exp
  | TtypeofT(spec,dt) -> (self#walkSpecifier spec) @ (self#walkDeclType dt)
  | _ -> []
end

class specElemChildren = object (self)
  inherit [spec_elem] childrenWalker as super

  method walkSpecElem se = [se]

  method startWalk = function
  | SpecAttr(attr) -> self#walkAttribute attr
  | SpecType(ts) -> self#walkTypeSpecifier ts
  | _ -> []
end

class specifierChildren = object (self)
  inherit [specifier] childrenWalker as super

  method walkSpecifier spec = [spec]
  method startWalk spec =
	lfoldl
	  (fun sofar ->
		fun spec_elem ->
		  match spec_elem with 
		  | SpecAttr(a) -> sofar @(self#walkAttribute a)
		  | SpecType(ts) -> sofar@(self#walkTypeSpecifier ts)
		  | _ -> sofar
	  ) [] spec
end

class singleNameChildren = object (self)
  inherit [single_name] childrenWalker as super

  method walkSingleName sn = [sn]
  method startWalk (spec,name) = (self#walkSpecifier spec) @ (self#walkName name)
end

class nameGroupChildren = object (self)
  inherit [name_group] childrenWalker as super

  method walkNameGroup ng = [ng]

  method startWalk (spec,names) = 
	lfoldl (fun sofar -> fun name -> sofar @ (self#walkName name))
	  (self#walkSpecifier spec) names

end

class nameChildren = object (self)
  inherit [name] childrenWalker as super

  method walkName name = [name]
  method startWalk ((_,dt,attrs,_) : name) =
	(self#walkDeclType dt) @ (self#walkAttributes attrs)
end

class attributeChildren = object (self)
  inherit [attribute] childrenWalker as super

  method walkAttribute attr = [attr]
  method startWalk ((_,exps) : attribute) = self#walkExpressionList exps
end

class initNameGroupChildren = object (self)
  inherit [init_name_group] childrenWalker as super

  method walkInitNameGroup ing = [ing]
  method startWalk (spec,ins) =
	lfoldl
	  (fun sofar ->
		fun (name,ie) ->
		  sofar @ (self#walkName name) @ (self#walkInitExpression ie)) (self#walkSpecifier spec) ins
end

class initNameChildren = object (self)
  inherit [init_name] childrenWalker as super

  method walkInitName ing = [ing]

  method startWalk (name,ie) = 
	(self#walkName name) @ (self#walkInitExpression ie)
end

class initExpressionChildren = object (self)
  inherit [init_expression] childrenWalker as super

  method walkInitExpression ie = [ie]

  method startWalk ie = 
	let rec walkInitWhat = function
	  | INFIELD_INIT(_,iw) -> walkInitWhat iw
	  | ATINDEX_INIT(exp,iw) -> (self#walkExpression exp) @ (walkInitWhat iw)
	  | ATINDEXRANGE_INIT(e1,e2) -> (self#walkExpression e1) @ (self#walkExpression e2)
	  | _ -> []
	in
	match ie with 
	| SINGLE_INIT(exp) -> self#walkExpression exp
	| COMPOUND_INIT(iwies) -> 
	  lfoldl
		(fun sofar ->
		  fun (iw,ie) ->
			ie :: (walkInitWhat iw)) [] iwies
	| _ -> []

end

class declTypeChildren = object (self)
  inherit [decl_type] childrenWalker as super

  method walkDeclType dt = [dt]

  method startWalk = function
  | PARENTYPE(attrs1,dt,attrs2) -> dt :: (self#walkAttributes attrs1) @ (self#walkAttributes attrs2)
  | ARRAY(dt,attrs,exp) -> dt :: (self#walkAttributes attrs) @(self#walkExpression exp)
  | PTR(attrs,dt) -> dt :: (self#walkAttributes attrs)
  | PROTO(dt,sns,_) -> 
	lfoldl
	  (fun sofar ->
		fun sn ->
		  sofar @ (self#walkSingleName sn)) [dt] sns
  | _ -> []
end

class blockChildren = object (self)
  inherit [block] childrenWalker as super

  method walkBlock b = [b]

  method startWalk b = 
	lfoldl 
	  (fun sofar ->
		fun s ->
		  sofar @ (self#walkStatement s)) [] b.bstmts
end

let expchildren = new expressionChildren
let stmtchildren = new statementChildren
let defchildren = new definitionChildren
let tschildren = new typeSpecifierChildren
let sechildren = new specElemChildren
let specchildren = new specifierChildren
let snchildren = new singleNameChildren
let ngchildren = new nameGroupChildren
let namechildren = new nameChildren
let attrchildren = new attributeChildren
let ingchildren = new initNameGroupChildren
let inchildren = new initNameChildren
let iechildren = new initExpressionChildren 
let dtchildren = new declTypeChildren
let blockchildren = new blockChildren

let compare_to_all_children val1 lst combine walk default = 
  lfoldl
	(fun sofar ->
	  fun child ->
	    combine sofar (walk (val1,child))) default lst

let gen_children val1 val2 children combine walkfun default = 
	let children1 = children val1 in
	let children2 = children val2 in
	  combine
		(compare_to_all_children val1 children2 combine walkfun default)
		(compare_to_all_children val2 children1 combine walkfun default)

let compare (val1 : 'a) (val2 : 'a) : 'a =
  let comp1 = Objsize.objsize val1 in 
  let comp2 = Objsize.objsize val2 in 
	if comp1 > comp2 then val1 else val2

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
  method virtual default_ts : unit -> 'ts_rt
  method virtual default_se : unit -> 'se_rt
  method virtual default_spec : unit -> 'spec_rt
  method virtual default_sn : unit -> 'single_name_rt
  method virtual default_ng : unit -> 'ng_rt
  method virtual default_name : unit -> 'name_rt
  method virtual default_attr : unit -> 'attr_rt
  method virtual default_ing : unit -> 'ing_rt

  method childrenDefinition (def1,def2) = gen_children def1 def2 defchildren#startWalk compare self#walkDefinition (self#default_def())
  method childrenExpression (exp1,exp2) = gen_children exp1 exp2 expchildren#startWalk compare self#walkExpression (self#default_exp())
  method childrenStatement (stmt1,stmt2) = gen_children stmt1 stmt2 stmtchildren#startWalk compare self#walkStatement (self#default_stmt())
  method childrenTypeSpecifier (ts1,ts2) = gen_children ts1 ts2 tschildren#startWalk compare self#walkTypeSpecifier (self#default_ts())
  method childrenSpecElem (se1,se2) = gen_children se1 se2 sechildren#startWalk compare self#walkSpecElem (self#default_se())
  method childrenSpec (se1,se2) = gen_children se1 se2 specchildren#startWalk compare self#walkSpecifier (self#default_spec())
  method childrenSingleName (sn1,sn2) = gen_children sn1 sn2 snchildren#startWalk compare self#walkSingleName (self#default_sn())
  method childrenNameGroup (ng1,ng2) = gen_children ng1 ng2 ngchildren#startWalk compare self#walkNameGroup (self#default_ng())
  method childrenName (name1,name2) = gen_children name1 name2 namechildren#startWalk compare self#walkName (self#default_name())
  method childrenAttribute (a1,a2) = gen_children a1 a2 attrchildren#startWalk compare self#walkAttribute (self#default_attr())
  method childrenInitNameGroup (ing1,ing2) = gen_children ing1 ing2 ingchildren#startWalk compare self#walkInitNameGroup (self#default_ing())
  method childrenInitName (in1,in2) = gen_children in1 in2 inchildren#startWalk compare self#walkInitName (self#default_init_name())
  method childrenInitExpression (ie1,ie2) = gen_children ie1 ie2 iechildren#startWalk compare self#walkInitExpression (self#default_ie())
  method childrenDeclType (dt1,dt2) = gen_children dt1 dt2 dtchildren#startWalk compare self#walkDeclType (self#default_dt())
  method childrenBlock (b1,b2) = gen_children b1 b2 blockchildren#startWalk compare self#walkBlock (self#default_block())
  method childrenTreenode ((tn1,tn2) : tree_node node * tree_node node) : 'tn_rt = self#default_tn ()

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
  method walkTree t = doWalk compare self#wTree self#childrenTree t
  method walkTreenode tn = doWalk compare self#wTreenode self#childrenTreenode tn
  method walkSingleName sn = doWalk compare self#wSingleName self#childrenSingleName sn
  method walkNameGroup ng = doWalk compare self#wNameGroup self#childrenNameGroup ng
  method walkInitNameGroup ing = doWalk compare self#wInitNameGroup self#childrenInitNameGroup ing
  method walkInitName iname = doWalk compare self#wInitName self#childrenInitName iname

end

