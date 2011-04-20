open Batteries
open Utils
open Cabs
open Cprint
open Cabsvisit
open Difftypes

let dummyBlock = { blabels = []; battrs = [] ; bstmts = [] ; } 
let dummyLoc = {lineno = -1; 
				Cabs.filename = "";
				byteno = -1;
				ident = -1} 
let dummyExp = nd(NOTHING) 
let dummyStmt = nd(NOP(dummyLoc)) 
let dummyFC = FC_EXP(dummyExp) 
let dummyDef = { (nd(FUNDEF(([],("",JUSTBASE,[],dummyLoc)),dummyBlock,dummyLoc,dummyLoc))) with id = (-2) }
let dummyDt = JUSTBASE
let dummyIE = NO_INIT
let dummyDirective = nd(PREINCLUDE("",dummyLoc))

let getinfo node printer tl tl_ht node_ht =
  let str = printer tl in
(*  pprintf "Node: %d, node: %s, tl_str: %s\n" node.id (printer node) str; flush stdout;*)
  let tlint = typelabel str in
  let old_tl = ht_find tl_ht tlint (fun _ -> []) in
	hrep tl_ht tlint (node.id :: old_tl);
	node.typelabel <- tlint;
	node.tl_str <- str;
	hadd node_ht node.id node;
	DoChildren

let nodes_eq t1 t2 =
  (* if both their types and their labels are equal *) 
  t1.typelabel = t2.typelabel 


class typelabelVisitor typelabel_ht node_info = object(self)
  inherit nopCabsVisitor
  val node_info = node_info
  val typelabel_ht = typelabel_ht 

  method vexpr exp = 
	let dum = 
	  match dn exp with
	  | UNARY(uop,e1) -> UNARY(uop,dummyExp)
	  | BINARY(bop,e1,e2) -> BINARY(bop,dummyExp,dummyExp)
	  | QUESTION(e1,e2,e3) -> QUESTION(dummyExp,dummyExp,dummyExp)
	  | CALL(exp,elist) -> CALL(dummyExp,[]);
	  | COMMA(elist) -> COMMA([])
	  | CONSTANT(c) -> CONSTANT(c)
	  | PAREN(e1) -> PAREN(dummyExp)
	  | EXPR_SIZEOF(e1) -> EXPR_SIZEOF(dummyExp)
	  | EXPR_ALIGNOF(e1) -> EXPR_ALIGNOF(dummyExp)
	  | INDEX(e1,e2) -> INDEX(dummyExp,dummyExp)
	  | MEMBEROF(e1,str) -> MEMBEROF(dummyExp,str)
	  | MEMBEROFPTR(e1,str) -> MEMBEROFPTR(dummyExp,str)
	  | GNU_BODY(b) -> GNU_BODY(dummyBlock)
	  | TYPE_ALIGNOF(spec,dtype) -> TYPE_ALIGNOF([],dummyDt)
	  | TYPE_SIZEOF(spec,dtype) -> TYPE_SIZEOF([],dummyDt)
	  | CAST((spec,dtype),ie) -> CAST(([],dummyDt),dummyIE)
	  | e -> e
	in
	let dumExp = { exp with node = NODE(dum) } in
	let tl_str = "EXPRESSION: " ^ (Pretty.sprint ~width:80 (d_exp() dumExp)) in
	let tlint : int = typelabel tl_str in
	let old_tl = ht_find typelabel_ht tlint (fun _ -> []) in
	  hrep typelabel_ht tlint (exp.id :: old_tl);
	  exp.typelabel <- tlint;
	  exp.tl_str <- tl_str;
	  hadd node_info.exp_ht exp.id (exp,dumExp); 
	  DoChildren

  method vstmt stmt = 
	let dum =
	  match dn stmt with
		NOP(_) -> NOP(dummyLoc)
	  | STMTDIRECTIVE _ -> STMTDIRECTIVE(dummyDirective,dummyLoc)
	  | COMPUTATION(exp,_) -> COMPUTATION(dummyExp,dummyLoc)
	  | BLOCK(b,_) -> BLOCK(dummyBlock,dummyLoc)
	  | SEQUENCE(s1,s2,loc) -> SEQUENCE(dummyStmt,dummyStmt,dummyLoc)
	  | IF(exp,s1,s2,_) -> IF(dummyExp,dummyStmt,dummyStmt,dummyLoc)
	  | WHILE(exp,s1,_) -> WHILE(dummyExp,dummyStmt,dummyLoc)
	  | DOWHILE(exp,s1,_) -> DOWHILE(dummyExp,dummyStmt,dummyLoc)
	  | FOR(fc,exp1,exp2,s1,_) -> FOR(dummyFC,dummyExp,dummyExp,dummyStmt,dummyLoc)
	  | BREAK(_) -> BREAK(dummyLoc)
	  | CONTINUE(_) -> CONTINUE(dummyLoc)
	  | RETURN(exp,_) -> RETURN(dummyExp,dummyLoc)
	  | SWITCH(exp,s1,_) -> SWITCH(dummyExp,dummyStmt,dummyLoc)
	  | CASE(exp,s1,_) -> CASE(dummyExp,dummyStmt,dummyLoc)
	  | CASERANGE(e1,e2,s1,_) -> 
		CASERANGE(dummyExp,dummyExp,dummyStmt,dummyLoc)
	  | DEFAULT(s1,_) -> DEFAULT(dummyStmt,dummyLoc)
	  | LABEL(str,s1,_) -> LABEL(str,dummyStmt,dummyLoc)
	  | GOTO(str,_) -> GOTO(str,dummyLoc)
	  | COMPGOTO(exp,_) -> COMPGOTO(dummyExp,dummyLoc)
	  | DEFINITION(d) -> DEFINITION(dummyDef)
	  | ASM(attrs,strs,dets,loc) -> (* fixme: I'm too lazy for ASM *) dn stmt
	  | TRY_EXCEPT(b1,exp,b2,_) -> 
		TRY_EXCEPT(dummyBlock,dummyExp,dummyBlock,dummyLoc)
	  | TRY_FINALLY(b1,b2,_) -> 
		TRY_FINALLY(dummyBlock,dummyBlock,dummyLoc)
	in 
	let dumStmt = { stmt with node = NODE(dum) } in
	let tl_str = "STATEMENT: " ^ (Pretty.sprint ~width:80 (d_stmt () dumStmt)) in
	let tlint : int = typelabel tl_str in
	let old_tl = ht_find typelabel_ht tlint (fun _ -> []) in
	  hrep typelabel_ht tlint (stmt.id :: old_tl);
	  stmt.typelabel <- tlint;
	  stmt.tl_str <- tl_str;
	  hadd node_info.stmt_ht stmt.id (stmt,dumStmt); DoChildren

  method vdef def = 
	let dum = 
	  match dn def with (* TODO: names! *)
		FUNDEF(sn,b,_,_) -> FUNDEF(sn,dummyBlock,dummyLoc,dummyLoc)
	  | DECDEF(ing,_) -> DECDEF(ing,dummyLoc)
	  | TYPEDEF(ng,_) -> TYPEDEF(ng, dummyLoc)
	  | ONLYTYPEDEF(spec,_) -> ONLYTYPEDEF([],dummyLoc)
	  | PRAGMA(exp,_) -> PRAGMA(dummyExp,dummyLoc)
	  | LINKAGE(str,_,dlist) -> LINKAGE(str,dummyLoc,[])
	  | DIRECTIVE(d) -> DIRECTIVE(dummyDirective)
	in
	let dumDef = { def with node = NODE(dum) } in
	let tl_str = "DEFINITION: " ^ (Pretty.sprint ~width:80 (d_def() dumDef)) in
	let tlint : int = typelabel tl_str in
	let old_tl = ht_find typelabel_ht tlint (fun _ -> []) in
	  hrep typelabel_ht tlint (def.id :: old_tl);
	  def.typelabel <- tlint;
	  def.tl_str <- tl_str;
	  hadd node_info.def_ht def.id (def,dumDef); DoChildren

  method vtreenode tn =
	let dum =
	match dn tn with 
	| Globals(dlist) -> Globals([])
	| Stmts(slist) -> Stmts([])
	| Exps(elist) -> Exps([])
	in
	let dumTn = { tn with node = NODE(dum) } in
	let tl_str = "TREE_NODE: " ^ (Pretty.sprint ~width:80 (d_tree_node () dumTn)) in
	let tlint = typelabel tl_str in
	let old_tl = ht_find typelabel_ht tlint (fun _ -> []) in
	  hrep typelabel_ht tlint (tn.id :: old_tl);
	  tn.typelabel <- tlint;
	  tn.tl_str <- tl_str;
	  hadd node_info.tn_ht tn.id (tn,dumTn); DoChildren

end

let tree_to_diff_tree tree =
  let typelabel_ht = hcreate 10 in
  let node_info = new_tree_info () in
  let my_visit = new typelabelVisitor typelabel_ht node_info in
  let tree = visitTree my_visit tree in
	tree,typelabel_ht,node_info
(*let change_to_diff_tree change  = failwith "Doesn't work right now; fixme!" *)
(*  let convert_standard_eas sea = 
	let seas_tl_str,seas_tl_node = typelabel (CHANGE(sea)) in
	let children = 
	  match sea with
		(* the problem here is the positions, hm *)
	  | SInsert(nd1,Some(nd2),io) -> [| nd1; nd2 |]
	  | SInsert(nd1,None,io) -> [| nd1 |]
	  | SInsertTree(nd1,Some(nd2),io) -> [| nd1; nd2 |]
	  | SInsertTree(nd1,None,io) -> [| nd1 |]
	  | SMove(nd1,Some(nd2),io) -> [| nd1;nd2|]
	  | SMove(nd1,None,io) -> [| nd1 |]
	  | SDelete(nd) -> [| nd |] (* FIXME: not sure about this one *)
	  | SReplace(nd1,nd2) -> [| nd1;nd2 |]
	in
	  node seas_tl_str children seas_tl_node (CHANGE(sea))
  in 
  let children = Array.of_list (lmap convert_standard_eas change) in
  let tl_str,tl_node = typelabel (CHANGE_LIST(change)) in
	node tl_str children tl_node (CHANGE_LIST(change))
*)

  
