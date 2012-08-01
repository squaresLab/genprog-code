open Batteries
open Utils
open Cil
open Cprint
open Globals

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
	| ChildrenPost(fn) -> 
	  fn (children node)

class virtual ['result_type, 'varinfo_type, 'exp_type, 'lval_type, 'offset_type,
               'instr_type, 'stmt_type, 'block_type, 'typ_type,
               'attribute_type, 'attrparam_type, 'varinfo_rt,'exp_rt,
               'lval_rt,'offset_rt, 'instr_rt,'stmt_rt,'block_rt,
               'typ_rt, 'attribute_rt,'attrparam_rt] cilWalker =
object(self)
  (* NTS: because I don't intend to use this for anything but statement-level
     templates, I'm ommitting stuff like functions and globals *)
  method virtual default_res : unit -> 'result_type
  method virtual combine : 'result_type -> 'result_type -> 'result_type

  method wvarinfo (v : 'varinfo_type) : 'varinfo_rt walkAction = Children
  method wexpr (e : 'exp_type) : 'exp_rt walkAction = Children
  method wlval (l : 'lval_type) : 'lval_rt walkAction = Children
  method woffs (o : 'offset_type) : 'offset_rt walkAction = Children
  method winitoffs (o : 'offset_type) : 'offset_rt walkAction = Children
  method winst (i : 'instr_type) : 'instr_rt walkAction = Children
  method wstmt (s : 'stmt_type): 'stmt_rt walkAction = Children
  method wblock (b : 'block_type) : 'block_rt walkAction = Children
  method wtype (f : 'typ_type) : 'typ_rt walkAction = Children
  method vattr (f : 'attribute_type) : 'attribute_rt walkAction = Children
  method wattrparam (a : 'attrparam_type) : 'attrparam_rt walkAction = Children

  method enterScope () = ()
  method exitScope () = ()

  method virtual walkVarinfo: 'varinfo_type -> 'varinfo_rt
  method virtual walkVrbl: 'varinfo_type -> 'varinfo_rt
  method virtual walkExpr: 'exp_type -> 'exp_rt 
  method virtual walkLval: 'lval_type -> 'lval_rt
  method virtual walkOffset: 'offset_type -> 'offset_rt
  method virtual walkInstr: 'instr_type -> 'instr_rt
  method virtual walkStmt:  'stmt_type -> 'stmt_rt
  method virtual walkBlock:'block_type -> 'block_rt 
  method virtual walkType:    'typ_type -> 'typ_rt 
  method virtual valkAttr: 'attribute_type -> 'attribute_rt 
  method virtual walkAttrparam: 'attrparam_type -> 'attrparam_rt 

  method virtual childrenVarinfo: 'varinfo_type -> 'varinfo_rt
  method virtual childrenExpr: 'exp_type -> 'exp_rt 
  method virtual childrenLval: 'lval_type -> 'lval_rt
  method virtual childrenOffset: 'offset_type -> 'offset_rt
  method virtual childrenInstr: 'instr_type -> 'instr_rt
  method virtual childrenStmt: 'stmt_type -> 'stmt_rt
  method virtual childrenBlock: 'block_type -> 'block_rt 
  method virtual childrenType: 'typ_type -> 'typ_rt 
  method virtual childrenAttr: 'attribute_type -> 'attribute_rt 
  method virtual childrenAttrparam: 'attrparam_type -> 'attrparam_rt 
end

let walklist1 (start : 'a) (walker : 'b -> 'a) (combine : 'a -> 'a -> 'a) (lst : 'b list) = 
  lfoldl (fun best -> fun ele -> combine best (walker ele)) start lst

let walklist2 start walker val1 lst = 
  lfoldl (fun best -> fun ele -> compare best (walker (val1, ele))) start lst

let best_ofs lst combine default =
  lfoldl
	(fun result eval ->
	  combine result eval) default lst

class virtual ['a,  'varinfo_type, 'exp_type, 'lval_type, 'offset_type,
               'instr_type, 'stmt_type, 'block_type, 'typ_type,
               'attribute_type, 'attrparam_type] singleWalker = object(self)
  inherit ['a,  'varinfo_type, 'exp_type, 'lval_type, 'offset_type,
               'instr_type, 'stmt_type, 'block_type, 'typ_type,
               'attribute_type, 'attrparam_type,'a,'a,'a,'a,'a,'a,'a,'a,'a,'a] cilWalker

  method walkVarinfo varinfo = doWalk self#combine self#warinfo self#childrenvarinfo varinfo
  method walkExpr exp = doWalk self#combine self#wexpr self#childrenexpr exp
  method walkLval lval = doWalk self#combine self#wlval self#childrenlval lval 
  method walkOffset off = doWalk self#combine self#woffs self#childrenoffs off
  method walkInstr instr = doWalk self#combine self#winst self#childreninst instr
  method walkStmt stmt = doWalk self#combine self#wstmt self#childrenstmt stmt
  method walkBlock b = doWalk self#combine self#wblock self#childrenBlock b
  method walkType t = doWalk self#combine self#wtype self#childrentype t
  method walkAttr attr = doWalk self#combine self#wattr self#childrenattr attr
  method walkAttrparam ap = doWalk self#combine self#wattrparam self#childrenattrparam ap

  method walkExprs elist = 
	lfoldl
	  (fun res ->
		fun e ->
		  self#combine (self#walkExpr e) res) (self#default_res()) elist

end

class virtual ['a] singleCilWalker = object(self)
  inherit ['a,varinfo,exp,lval,offset,instr,stmt,block,fundec,global,typ,
           attribute,attrparam] singleWalker as super

  method childrenvarinfo varinfo =
    self#combine (self#walkType varinfo.vtype) (self#walkAttributes varinfo.vattrs)

  method childrenExpr (exp) : 'a =
    match exp with
  | Lval(lval)
  | AddrOf(lval)
  | StartOf(lval) -> self#walkLval lval
  | Const(CEnum(exp,_,_)) 
  | SizeOfE(exp)
  | AlignOfE(exp) -> self#walkExpr exp
  | SizeOf(t) 
  | AlignOf(t) -> self#walkType t
  | CastE(t,e)
  | UnOp(_,e,t) -> self#combine (self#walkExpr e) (self#walkType t) 
  | BinOp(_,e1,e2,t) -> self#combine (self#walkExpr exp) (self#combine (self#walkExpr exp) (self#walkType t))
  | _ -> self#default_res()

  method childrenLval lval = 
    let host,off = lval in
    let whost = 
      match host with 
      | Var(v) -> self#walkVarinfo v
      | Mem(e) -> self#walkExpr e 
    in
    let woff = self#walkOffset off in
      self#combine whost woff

  method childrenOffs (offset) =
    match off with 
    | Field(_(*FIXME fieldinfo *), o) -> self#walkOffset off
    | Index(e,o) -> self#combine (self#walkExpr e) (self#walkOffset off)

  method childrenInst (i) = 
    match i with
      Set(lval,exp,_) ->
        self#combine (self#walkLval lval) (self#walkExpr exp)
    | Call(None,e1,elist,_) -> self#walkExprs (e1 :: elist)
    | Call(Some(e1),e2,elist,_) -> self#walkExprs (e1 :: e2 :: elist)
    | Asm(attrs,_,outputs1,outputs2,_,_) ->
      let wattrs = self#walkAttributes attrs in
      let woutputs1 = 
        let lvals = lmap (fun (a,b,c) -> c) outputs1 in 
          walklist1 (self#default_res()) (self#walkLval) (self#combine) lvals
      in
      let woutputs2 = 
        let exps = lmap (fun (a,b,c) -> c) outputs2 in 
          self#walkExprs exps
      in
        self#combine wattrs (self#combine woutputs1 woutputs2)

  method childrenStmt stmt =
    match stmt.skind with
    | Instr(ilist) -> walklist1 (self#default_res()) (self#walkInstr) (self#combine) ilist
    | Return(Some(exp),_) -> self#walkExpr exp
    | If(exp,b1,b2,_) ->
      self#combine
        (self#walkExpr exp)
        (self#combine (self#walkBlock b1) (self#walkBlock b2))
    | Switch(exp,block,stmts,_) ->
      let wexp = self#walkExpr in
      let wblock = self#walkBlock block in
      let wstmts = walklist1 wexp (self#walkStmt) (self#combine) stmts
      in
        self#combine wblock wstmts
  | Block(b) | Loop(b,_,_,_) -> self#walkBlock b
  | TryFinally(b1,b2,_) -> self#combine (self#walkBlock b1) (self#walkBlock b2)
  | TryExcept(b1,(ilst,e),b2,_) ->
    let wb1 = self#walkBlock b1 in
    let wies = walklist1 (self#walkExpr e) (self#walkInstr) (self#combine) ilst in
    let wb2 = self#walkBlock b2 in
      self#combine wb1 (self#combine wies wb2)
  | _ -> self#default_res ()

  method childrenBlock b =
    walklist1 (self#walkAttributes b.battrs) (self#walkStmt) (self#combine) b.bstmts

  method childrenType typ =
    match typ with
  | TInt(_,attrs)
  | TFloat(_,attrs)
  | TBuiltin_va_list(attrs)
  | TVoid(attrs) -> self#walkAttributes attrs
  | TArray(t,None,attrs)
  | TPtr(t,attrs) ->
    self#combine (self#walkType t) (self#walkAttributes attrs)
  | TArray(t,Some(e),attrs) ->
    self#combine (self#walkType t)
      (self#combine (self#walkExpr e) (self#walkAttributes attrs))
  | _ -> self#default_res()
(*FIXME:  | TFun of typ * (string * typ * attributes) list option * bool * attributes
  | TNamed of typeinfo * attributes 
  | TComp of compinfo * attributes
  | TEnum of enuminfo * attributes*)

  method childrenAttr attr = self#default_res ()
  method childrenAttrparam = self#default_res ()
  method private walkAttributes (attrs : attribute list) : 'a = 
	walklist1 (self#default_res()) (self#walkAttr) (self#combine) attrs

end
(*
class virtual ['a] childrenWalker = object(self)
  inherit ['a list] singleCilWalker as super
  method combine elist1 elist2 = elist1 @ elist2
  method default_res () = []
  method virtual startWalk : 'a -> 'a list
end


class expressionChildren = object (self)
  inherit [expression node] childrenWalker as super

  method walkExpression exp = [exp]
  method walkExpressionList elist = elist
  method childrenTreenode tn =
	match (dn tn) with
	  Exps(elist) -> elist
	| _ -> super#childrenTreenode tn

  method startWalk exp = 
	match (dn exp) with 
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
  inherit [statement node] childrenWalker as super

  method walkStatement stmt = [stmt]
  method childrenTreenode tn =
	match (dn tn) with
	  Stmts(elist) -> elist
	| _ -> super#childrenTreenode tn

  method startWalk stmt = 
	match (dn stmt) with
  | COMPUTATION(exp,_)
  | RETURN(exp,_)
  | COMPGOTO(exp,_) -> self#walkExpression exp
  | BLOCK(b,_) -> []
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

class definitionChildren = object (self)
  inherit [definition node] childrenWalker as super

  method walkDefinition def = [def]
  method childrenTreenode tn =
	match (dn tn) with
	  Globals(elist) -> elist
	| _ -> super#childrenTreenode tn

  method startWalk def =
	match (dn def) with 
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

let compare (val1 : 'a) (val2 : 'a) : 'a = (* FIXME: this is so wrong *)
  let comp1 = Objsize.objsize val1 in 
  let comp2 = Objsize.objsize val2 in 
	if comp1 > comp2 then val1 else val2

class virtual ['result_type,'ts_rt,'se_rt,'spec_rt,'dt_rt,'ng_rt,'ing_rt,'name_rt,'init_name_rt,'single_name_rt,'directive_rt,'macro_rt,'def_rt,'block_rt,'stmt_rt,'exp_rt,'ie_rt,'attr_rt,'tn_rt] doubleCabsWalker = object(self)
  inherit
	['result_type, (typeSpecifier * typeSpecifier),(spec_elem * spec_elem),(specifier * specifier),decl_type * decl_type,
	 name_group * name_group, init_name_group * init_name_group, name * name, init_name * init_name, single_name * single_name,
	 directive node * directive node, macro * macro,
	 definition node * definition node, block * block, statement node * statement node, expression node * expression node, init_expression * init_expression,
	 attribute * attribute, tree_node node * tree_node node, tree * tree, 
	 'ts_rt,'se_rt,'spec_rt,'dt_rt,'ng_rt,'ing_rt,'name_rt,'init_name_rt,'single_name_rt,'directive_rt,'macro_rt,'def_rt,'block_rt,'stmt_rt,'exp_rt,'ie_rt,'attr_rt,'tn_rt] cabsWalker as super

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
  method virtual default_init_name : unit -> 'init_name_rt
  method virtual default_ie : unit -> 'ie_rt
  method virtual default_dt : unit -> 'dt_rt
  method virtual default_block : unit -> 'block_rt
  method virtual default_dir : unit -> 'directive_rt
  method virtual default_macro : unit -> 'macro_rt

  method childrenDefinition (def1,def2) = gen_children def1 def2 defchildren#startWalk compare self#walkDefinition (self#default_def())
  method childrenExpression (exp1,exp2) = gen_children exp1 exp2 expchildren#startWalk compare self#walkExpression (self#default_exp())
  method childrenStatement (stmt1,stmt2) = 
	gen_children stmt1 stmt2 stmtchildren#startWalk compare self#walkStatement (self#default_stmt())
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
  method childrenTreenode (tn1,tn2) = self#default_tn()
  method childrenDirective (d1,d2) : 'directive_rt = self#default_dir ()
  method childrenMacro (m1,m2) = self#default_macro ()

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
  method walkTreenode tn = doWalk compare self#wTreenode (fun _ -> failwith "Children treenode should never be called in ewalker") tn
  method walkSingleName sn = doWalk compare self#wSingleName self#childrenSingleName sn
  method walkNameGroup ng = doWalk compare self#wNameGroup self#childrenNameGroup ng
  method walkInitNameGroup ing = doWalk compare self#wInitNameGroup self#childrenInitNameGroup ing
  method walkInitName iname = doWalk compare self#wInitName self#childrenInitName iname
  method walkDirective dir = doWalk compare self#wDirective self#childrenDirective dir
  method walkMacro m = doWalk compare self#wMacro self#childrenMacro m
end

      *)
