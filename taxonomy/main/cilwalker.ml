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
               'instr_type, 'stmt_type, 'block_type, 'typ_type,  'ci_type,
               'varinfo_rt,'exp_rt,
               'lval_rt,'offset_rt, 'instr_rt,'stmt_rt,'block_rt,
               'typ_rt, 'ci_rt] cilWalker =
object(self)
  (* NTS: because I don't intend to use this for anything but statement-level
     templates, I'm ommitting stuff like functions and globals *)
  (* also: not handling attributes, because fuck them *)
  method virtual default_res : unit -> 'result_type
  method virtual combine : 'result_type -> 'result_type -> 'result_type

  method wVarinfo (v : 'varinfo_type) : 'varinfo_rt walkAction = Children
  method wExpr (e : 'exp_type) : 'exp_rt walkAction = Children
  method wLval (l : 'lval_type) : 'lval_rt walkAction = Children
  method wOffset (o : 'offset_type) : 'offset_rt walkAction = Children
  method wInstr (i : 'instr_type) : 'instr_rt walkAction = Children
  method wStmt (s : 'stmt_type): 'stmt_rt walkAction = Children
  method wBlock (b : 'block_type) : 'block_rt walkAction = Children
  method wType (f : 'typ_type) : 'typ_rt walkAction = Children
  method wCompinfo (ci : 'ci_type) : 'ci_rt walkAction = Children

  method enterScope () = ()
  method exitScope () = ()

  method virtual walkVarinfo: 'varinfo_type -> 'varinfo_rt
  method virtual walkExpr: 'exp_type -> 'exp_rt 
  method virtual walkLval: 'lval_type -> 'lval_rt
  method virtual walkOffset: 'offset_type -> 'offset_rt
  method virtual walkInstr: 'instr_type -> 'instr_rt
  method virtual walkStmt:  'stmt_type -> 'stmt_rt
  method virtual walkBlock:'block_type -> 'block_rt 
  method virtual walkType:    'typ_type -> 'typ_rt 
  method virtual walkCompinfo : 'ci_type -> 'ci_rt

  method virtual childrenVarinfo: 'varinfo_type -> 'varinfo_rt
  method virtual childrenExpr: 'exp_type -> 'exp_rt 
  method virtual childrenLval: 'lval_type -> 'lval_rt
  method virtual childrenOffset: 'offset_type -> 'offset_rt
  method virtual childrenInstr: 'instr_type -> 'instr_rt
  method virtual childrenStmt: 'stmt_type -> 'stmt_rt
  method virtual childrenBlock: 'block_type -> 'block_rt 
  method virtual childrenType: 'typ_type -> 'typ_rt 
  method virtual childrenCompinfo : 'ci_type -> 'ci_rt
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
               'instr_type, 'stmt_type, 'block_type, 'typ_type, 'ci_type] singleWalker = object(self)
  inherit ['a,  'varinfo_type, 'exp_type, 'lval_type, 'offset_type,
               'instr_type, 'stmt_type, 'block_type, 'typ_type, 'ci_type, 'a,
           'a,'a,'a,'a,'a,'a,'a,'a] cilWalker

  method walkVarinfo varinfo = doWalk self#combine self#wVarinfo self#childrenVarinfo varinfo
  method walkExpr exp = doWalk self#combine self#wExpr self#childrenExpr exp
  method walkLval lval = doWalk self#combine self#wLval self#childrenLval lval 
  method walkOffset off = doWalk self#combine self#wOffset self#childrenOffset off
  method walkInstr instr = doWalk self#combine self#wInstr self#childrenInstr instr
  method walkStmt stmt = doWalk self#combine self#wStmt self#childrenStmt stmt
  method walkBlock b = doWalk self#combine self#wBlock self#childrenBlock b
  method walkType t = doWalk self#combine self#wType self#childrenType t
  method walkCompinfo ci = doWalk self#combine self#wCompinfo self#childrenCompinfo ci

  method walkExprs elist = 
	lfoldl
	  (fun res ->
		fun e ->
		  self#combine (self#walkExpr e) res) (self#default_res()) elist

end

class virtual ['a] singleCilWalker = object(self)
  inherit ['a,varinfo,exp,lval,offset,instr,stmt,block,typ, compinfo] singleWalker as super

  method childrenVarinfo varinfo = self#walkType varinfo.vtype

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

  method childrenOffset (offset) =
    match offset with 
    | Field(f, o) -> 
      let wci = self#walkCompinfo f.fcomp in
      let wfi = self#combine (self#walkType f.ftype) wci in
        self#combine wfi (self#walkOffset o)
    | Index(e,o) -> self#combine (self#walkExpr e) (self#walkOffset o)
    | NoOffset -> self#default_res ()

  method childrenInstr (i) = 
    match i with
      Set(lval,exp,_) ->
        self#combine (self#walkLval lval) (self#walkExpr exp)
    | Call(None,e1,elist,_) -> self#walkExprs (e1 :: elist)
    | Call(Some(lval),e1,elist,_) -> 
      self#combine (self#walkLval lval) (self#walkExprs (e1 :: elist))
    | Asm(_,_,outputs1,outputs2,_,_) ->
      let woutputs1 = 
        let lvals = lmap (fun (a,b,c) -> c) outputs1 in 
          walklist1 (self#default_res()) (self#walkLval) (self#combine) lvals
      in
      let woutputs2 = 
        let exps = lmap (fun (a,b,c) -> c) outputs2 in 
          self#walkExprs exps
      in
        self#combine woutputs1 woutputs2

  method childrenStmt stmt =
    match stmt.skind with
    | Instr(ilist) -> walklist1 (self#default_res()) (self#walkInstr) (self#combine) ilist
    | Return(Some(exp),_) -> self#walkExpr exp
    | If(exp,b1,b2,_) ->
      self#combine
        (self#walkExpr exp)
        (self#combine (self#walkBlock b1) (self#walkBlock b2))
    | Switch(exp,block,stmts,_) ->
      let wexp = self#walkExpr exp in
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
    walklist1 (self#default_res ()) (self#walkStmt) (self#combine) b.bstmts

  method childrenType typ =
    match typ with
    | TFun(t, None,_,_)
    | TArray(t,None,_)
    | TPtr(t,_) -> self#walkType t
    | TArray(t,Some(e),_) ->
    self#combine (self#walkType t) (self#walkExpr e)
    | TFun(t,Some(lst),_,_) ->
    let wlst = 
    walklist1 (self#default_res())
      (fun (_,t,_) -> (self#walkType t)) (self#combine) lst
    in
    self#combine (self#walkType t) wlst 
  | TNamed(ti, _) -> self#walkType ti.ttype
  | TComp(ci,_) -> self#walkCompinfo ci
  | TEnum(ei,_) -> 
    let exps = lmap (fun (_,e,_) -> e) ei.eitems in
      walklist1 (self#default_res()) (self#walkExpr) (self#combine) exps
  | _ -> self#default_res()

  method childrenCompinfo ci = 
    walklist1 (* handling the fieldinfo list *)
      (self#default_res ())
      (fun f ->
        let wcomp = self#walkCompinfo f.fcomp in 
        let wtyp = self#walkType f.ftype in 
          self#combine wcomp wtyp)
      (self#combine) ci.cfields

end

class virtual ['a] childrenWalker = object(self)
  inherit ['a list] singleCilWalker as super
  method combine elist1 elist2 = elist1 @ elist2
  method default_res () = []
  method virtual startWalk : 'a -> 'a list
end
      

      
class varinfoChildren = object (self)
  inherit [varinfo] childrenWalker as super

  method walkVarinfo vi = [vi]

  method startWalk varinfo = self#walkType varinfo.vtype
end


class expressionChildren = object (self)
  inherit [exp] childrenWalker as super

  method walkExpr exp = [exp]

  method startWalk exp =
    match exp with
  | Lval(lval)
  | AddrOf(lval)
  | StartOf(lval) -> self#walkLval lval
  | SizeOf(typ)
  | AlignOf(typ) -> self#walkType typ
  | Const(CEnum(exp,_,(*enuminfo *) _))
  | SizeOfE(exp)
  | AlignOfE(exp) -> [exp]
  | UnOp(_,exp,typ)
  | CastE(typ,exp) -> self#combine [exp] (self#walkType typ)
  | BinOp(b,exp1,exp2,typ) ->
    self#combine (self#walkType typ) [exp1;exp2]
  | _ -> []

end


class lvalChildren = object (self)
  inherit [lval] childrenWalker as super

  method walkLval lval = [lval]

  method startWalk lval =
    let host,off = lval in 
    let whost = 
      match host with
        Var(vi) -> self#walkVarinfo vi
      | Mem(exp) -> self#walkExpr exp
    in
    let woff = self#walkOffset off in
      self#combine woff whost
end


class offsetChildren = object (self)
  inherit [offset] childrenWalker as super

  method walkOffset off = [off]

  method startWalk offset =
    match offset with
  | NoOffset -> []
  | Field(fieldinfo,offset) ->
    self#combine (self#walkOffset offset) (self#walkCompinfo fieldinfo.fcomp)
  | Index(exp,offset) -> self#combine (self#walkOffset offset) (self#walkExpr exp)
end


class instrChildren = object (self)
  inherit [instr] childrenWalker as super

  method walkInstr i = [i]

  method startWalk instr =
    match instr with
    Set(lval,exp,_) -> self#combine (self#walkLval lval) (self#walkExpr exp)
  | Call(None, exp1, elist, _) ->
    walklist1 (self#walkExpr exp1) (self#walkExpr) (self#combine) elist
  | Call(Some(lval), exp1, elist, _) ->
    walklist1 (self#combine (self#walkLval lval) (self#walkExpr exp1)) (self#walkExpr) (self#combine) elist
  | Asm(_,_,lst1,lst2,_,_) ->
    let lst1 = lmap (fun (_,_,c) -> c) lst1 in 
    let lst2 = lmap (fun (_,_,c) -> c) lst2 in 
    let wlst1 =  walklist1 (self#default_res()) (self#walkLval) (self#combine) lst1 in
      walklist1 (wlst1) (self#walkExpr) (self#combine) lst2
end

class statementChildren = object (self)
  inherit [stmt] childrenWalker as super

  method walkStmt stmt = [stmt]

  method walkBlock block = block.bstmts

  method startWalk stmt = 
    match stmt.skind with
  | Instr(ilist) ->
    walklist1 (self#default_res ()) (self#walkInstr) (self#combine) ilist
  | Return(Some(exp),_) -> self#walkExpr exp
  | If(exp,block1,block2,_) ->
    self#combine
      (self#walkExpr exp)
      (self#combine (self#walkBlock block1) (self#walkBlock block2))
  | Switch(exp,block,stmts,_) ->
    self#combine (self#walkExpr exp) (block.bstmts @ stmts)
  | Loop(block,_,_,_) 
  | Block(block) -> self#walkBlock block
  | TryFinally(block1,block2,_) ->
    self#combine (self#walkBlock block1) (self#walkBlock block2)
  | TryExcept(block1, (ilist,exp), block2, _) ->
    self#combine (block1.bstmts @ block2.bstmts)
      (walklist1 (self#walkExpr exp) (self#walkInstr) (self#combine) ilist)
  | _ -> []
end

class blockChildren = object (self)
  inherit [block] childrenWalker as super

  method walkBlock b = [b]

  method startWalk b = 
    walklist1 (self#default_res()) (self#walkStmt) (self#combine) b.bstmts
    
end

class typeChildren = object (self)
  inherit [typ] childrenWalker as super

  method walkType t = [t]

  method startWalk = function
  | TPtr(typ,_)
  | TArray(typ, None, _)
  | TFun(typ,None, _,_) -> [typ]
  | TFun(typ,Some(lst), _,_) ->
    let tlist = lmap (fun (a,b,c) -> b) lst in
      walklist1 [typ] (self#walkType) (self#combine) tlist
  | TNamed(t,_) -> [t.ttype]
  | TComp(ci, _) -> self#walkCompinfo ci
  | TArray(typ, Some(exp), _)  -> self#combine [typ] (self#walkExpr exp)
  | TEnum(ei, _) -> 
    let elist = lmap (fun (a,b,c) -> b) ei.eitems in
      walklist1 (self#default_res()) (self#walkExpr) (self#combine) elist
  | _ -> []
end

class compinfoChildren = object (self)
  inherit [compinfo] childrenWalker as super

  method walkCompinfo ci = [ci]
  method startWalk ci =
    lmap (fun fi -> fi.fcomp) ci.cfields
end

let varchildren = new varinfoChildren
let expchildren = new expressionChildren
let lvalchildren = new lvalChildren
let offsetchildren = new offsetChildren
let instrchildren = new instrChildren
let stmtchildren = new statementChildren
let blockchildren = new blockChildren
let typechildren = new typeChildren
let compinfochildren = new compinfoChildren

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


class virtual ['result_type, 'varinfo_rt,'exp_rt,
               'lval_rt,'offset_rt, 'instr_rt,'stmt_rt,'block_rt,
               'typ_rt, 'ci_rt ] doubleCabsWalker = object(self)
  inherit
	['result_type, (varinfo * varinfo), (exp * exp), (lval * lval), (offset * offset),
     (instr * instr), (stmt * stmt) , (block * block), (typ * typ), (compinfo * compinfo), 'varinfo_rt, 'exp_rt, 'lval_rt,'offset_rt, 'instr_rt,'stmt_rt,'block_rt,
     'typ_rt, 'ci_rt] cilWalker as super

  method virtual default_varinfo : unit -> 'varinfo_rt
  method virtual default_exp : unit -> 'exp_rt
  method virtual default_lval : unit -> 'lval_rt
  method virtual default_offset : unit -> 'offset_rt
  method virtual default_instr : unit -> 'instr_rt
  method virtual default_stmt : unit -> 'stmt_rt
  method virtual default_block : unit -> 'block_rt
  method virtual default_typ : unit -> 'typ_rt
  method virtual default_ci : unit -> 'ci_rt

  method childrenVarinfo (var1,var2) = 
    gen_children var1 var2 varchildren#startWalk self#compare self#walkVarinfo (self#default_varinfo())
  method childrenExpr (exp1,exp2) = gen_children exp1 exp2 expchildren#startWalk self#compare self#walkExpr (self#default_exp())
  method childrenLval (lval1,lval2) = gen_children lval1 lval2 lvalchildren#startWalk self#compare self#walkLval (self#default_lval())
  method childrenOffset (off1,off2) =
	gen_children off1 off2 offsetchildren#startWalk self#compare self#walkOffset (self#default_offset())
  method childrenInstr (i1,i2) =
	gen_children i1 i2 instrchildren#startWalk self#compare self#walkInstr (self#default_instr())
  method childrenStmt (stmt1, stmt2) = 
	gen_children stmt1 stmt2 stmtchildren#startWalk self#compare self#walkStmt (self#default_stmt())
  method childrenBlock (b1,b2) =
    gen_children b1 b2 blockchildren#startWalk self#compare self#walkBlock (self#default_block())
  method childrenType (t1,t2) =
    gen_children t1 t2 typechildren#startWalk self#compare self#walkType (self#default_typ())
  method childrenCompinfo (c1,c2) =
    gen_children c1 c2 compinfochildren#startWalk self#compare self#walkCompinfo (self#default_ci())

  method walkVarinfo vi = doWalk self#compare self#wVarinfo self#childrenVarinfo vi
  method walkExpr exp = doWalk self#compare self#wExpr self#childrenExpr exp
  method walkLval l = doWalk self#compare self#wLval self#childrenLval l
  method walkOffset o = doWalk self#compare self#wOffset self#childrenOffset o
  method walkInstr i = doWalk self#compare self#wInstr self#childrenInstr i
  method walkStmt stmt = doWalk self#compare self#wStmt self#childrenStmt stmt
  method walkBlock block = doWalk self#compare self#wBlock self#childrenBlock block
  method walkType t = doWalk self#compare self#wType self#childrenType t
  method walkCompinfo c = doWalk self#compare self#wCompinfo self#childrenCompinfo c
end
