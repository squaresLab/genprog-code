open Batteries
open Utils
open Cabs
open Cprint
open Cabsvisit
open Difftypes

let dummyBlock = { blabels = []; battrs = [] ; bstmts = [] ; } 
let dummyLoc = {lineno = -1; 
				filename = "";
				byteno = -1;
				ident = -1} 
let dummyExp = nd(NOTHING) 
let dummyStmt = nd(NOP(dummyLoc)) 
let dummyFC = FC_EXP(dummyExp) 
let dummyDef = nd(FUNDEF(([],("",JUSTBASE,[],dummyLoc)),dummyBlock,dummyLoc,dummyLoc))

class typelabelVisitor typelabel_ht node_info = object(self)
  inherit nopCabsVisitor

  val node_info = node_info
  val typelabel_ht = typelabel_ht

  method vexpr exp =(* hadd tl_info.exp_ht exp.id exp; DoChildren*)
	let dum = 
	  match dn exp with
	  | UNARY(uop,e1) -> UNARY(uop,dummyExp)
	  | BINARY(bop,e1,e2) -> BINARY(bop,dummyExp,dummyExp)
	  | QUESTION(e1,e2,e3) -> QUESTION(dummyExp,dummyExp,dummyExp)
	  | CALL(exp,elist) -> CALL(dummyExp,[])
	  | COMMA(elist) -> COMMA([])
	  | CONSTANT(c) -> CONSTANT(c)
	  | PAREN(e1) -> PAREN(dummyExp)
	  | EXPR_SIZEOF(e1) -> EXPR_SIZEOF(dummyExp)
	  | EXPR_ALIGNOF(e1) -> EXPR_ALIGNOF(dummyExp)
	  | INDEX(e1,e2) -> INDEX(dummyExp,dummyExp)
	  | MEMBEROF(e1,str) -> MEMBEROF(dummyExp,str)
	  | MEMBEROFPTR(e1,str) -> MEMBEROFPTR(dummyExp,str)
	  | GNU_BODY(b) -> GNU_BODY(dummyBlock)
	  | _ -> dn exp
	in
	  getinfo exp (fun exp -> "EXPRESSION: " ^ (Pretty.sprint ~width:80 (d_exp() exp))) (nd dum) typelabel_ht node_info.exp_ht

	(* FIXME? 
	   | TYPE_ALIGNOF(spec,dtype) -> 
	   | TYPE_SIZEOF(spec,dtype) -> 
	   | CAST((spec,dtype),ie) -> 
	*)

  method vstmt stmt = 
	let dum = 
	  match dn stmt with
		NOP(_) -> NOP(dummyLoc)
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
	  | CASE(exp,s1,_) -> CASE(exp,dummyStmt,dummyLoc)
	  | CASERANGE(e1,e2,s1,_) -> CASERANGE(dummyExp,dummyExp,dummyStmt,dummyLoc)
	  | DEFAULT(s1,_) -> DEFAULT(dummyStmt,dummyLoc)
	  | LABEL(str,s1,_) -> LABEL(str,dummyStmt,dummyLoc)
	  | GOTO(str,_) -> GOTO(str,dummyLoc)
	  | COMPGOTO(exp,_) -> COMPGOTO(dummyExp,dummyLoc)
	  | DEFINITION(d) -> DEFINITION(dummyDef)
	  | ASM(attrs,strs,dets,loc) -> (* fixme: I'm too lazy for ASM *) dn stmt
	  | TRY_EXCEPT(b1,exp,b2,_) -> TRY_EXCEPT(dummyBlock,dummyExp,dummyBlock,dummyLoc)
	  | TRY_FINALLY(b1,b2,_) -> TRY_FINALLY(dummyBlock,dummyBlock,dummyLoc)
	in
	getinfo stmt (fun stmt -> "STATEMENT: " ^ (Pretty.sprint ~width:80 (d_stmt () stmt))) (nd dum) typelabel_ht node_info.stmt_ht
	  
  method vdef def = 
	let dum = 
	  match dn def with
	  FUNDEF(sn,b1,_,_) -> FUNDEF(sn,dummyBlock,dummyLoc,dummyLoc)
	| DECDEF(ing,_) -> DECDEF(ing,dummyLoc)
	| TYPEDEF(ng,_) -> TYPEDEF(ng,dummyLoc)
	| ONLYTYPEDEF(spec,_) -> ONLYTYPEDEF(spec,dummyLoc)
	| GLOBASM(str,_) -> GLOBASM(str,dummyLoc)
	| PRAGMA(exp,_) -> PRAGMA(dummyExp,dummyLoc)
	| LINKAGE(str,_,_) -> LINKAGE(str,dummyLoc,[])
	| _ -> dn def
	in
	getinfo def (fun def -> "DEFINITION: " ^ (Pretty.sprint ~width:80 (d_def () def))) (nd dum) typelabel_ht node_info.def_ht

  method vtreenode tn =
	let dum = 
	  match dn tn with 
	  | Globals(dlist) -> Globals([])
	  | Stmts(slist) -> Stmts([])
	  | Exps(elist) -> Exps([])
	  | Syntax(str) -> Syntax(str)
	in
	getinfo tn (fun tn -> "TREENODE: " ^ (Pretty.sprint ~width:80 (d_tree_node () tn))) (nd dum) typelabel_ht node_info.tn_ht

  method get_hts () = typelabel_ht, node_info

end

let tree_to_diff_tree tree tlht node_info =
  let coerce2 v = (v : typelabelVisitor :> cabsVisitor) in
  let myTl = new typelabelVisitor tlht node_info in
  let tree = visitTree (coerce2 myTl) tree in
	pprintf "TLinfo: \n"; 
	  (*	  hiter (fun k -> fun v -> pprintf "%s -> %d\n" k v) tl_ht;*)
	hiter (fun n -> fun exp -> pprintf "%d -> EXP: %s\n" n (Pretty.sprint ~width:80 (d_exp () exp))) node_info.exp_ht;
	hiter (fun n -> fun exp -> pprintf "%d -> STMT: %s\n" n (Pretty.sprint ~width:80 (d_stmt () exp))) node_info.stmt_ht;
	hiter (fun n -> fun exp -> pprintf "%d -> DEF: %s\n" n (Pretty.sprint ~width:80 (d_def () exp))) node_info.def_ht;
	hiter (fun n -> fun exp -> pprintf "%d -> TN: %s\n" n (Pretty.sprint ~width:80 (d_tree_node () exp))) node_info.tn_ht;
  tree

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

  
