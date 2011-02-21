open Batteries
open Utils
open Cabs
open Cprint
open Difftypes

(*************************************************************************)
(* Conversion: convert a code snippet/tree to the diff_tree_node nodes
 * we use for the actual generation of diffs, and back again for
 * sanity-checking output. *)


let typelabel node =
  match node with
  | DELETED -> "DELETED",node
  | TREE(tr) ->  Pretty.sprint ~width:80 (d_tree () tr), node
  | STMT(stmtn) -> Pretty.sprint ~width:80 (d_stmt () stmtn), node
  | EXP(expn) -> Pretty.sprint ~width:80 (d_exp () expn), node
  | DEF(defn) -> Pretty.sprint ~width:80 (d_def () defn), node
  | CHANGE(seas) -> failwith "No longer implemented; FIXME!"
(*	let str = match seas with
	  | SInsert(nd1,Some(nd2),io) -> Printf.sprintf "SInsert node-tl %d under node-tl %d" nd1.typelabel nd2.typelabel
	  | SInsert(nd1,None,io) -> Printf.sprintf "SInsert node-tl %d under node-tl -1" nd1.typelabel
	  | SInsertTree(nd1,Some(nd2),io) -> Printf.sprintf "SInsertTree node-tl %d under node-tl %d" nd1.typelabel nd2.typelabel
	  | SInsertTree(nd1,None,io) -> Printf.sprintf "SInsertTree node-tl %d under node-tl -1" nd1.typelabel
	  | SMove(nd1,Some(nd2),io) -> Printf.sprintf "SMove subtree rooted at node-tl %d under node-tl %d" nd1.typelabel nd2.typelabel
	  | SMove(nd1,None,io) -> Printf.sprintf "SMove subtree rooted at node-tl %d under node-tl -1" nd1.typelabel
	  | SDelete(nd) -> Printf.sprintf "SDelete subtree rooted at node-tl %d" nd.typelabel
	  | SReplace(nd1,nd2) -> Printf.sprintf "SReplace node-tl %d with node-tl %d" nd1.typelabel nd2.typelabel
	in
	  str,node*)
  | CHANGE_LIST(_) -> "CHANGE_LIST[]",CHANGE_LIST([])

let dummyBlock = { blabels = []; battrs = [] ; bstmts = [] ; } 
let dummyLoc = {lineno = -1; 
				filename = "";
				byteno = -1;
				ident = -1} 
let dummyExp = nd(NOTHING) 
let dummyStmt = nd(NOP(dummyLoc)) 
let dummyFC = FC_EXP(dummyExp) 
  
let rec convert_block b = Array.of_list (lmap convert_stmt b.bstmts)
and tl_stmt stmt = 
  let stmt_copy = copy stmt in 
  let dum,children = 
	match (dn stmt_copy) with
	  NOP(_) -> NOP(dummyLoc), [| |]
	| COMPUTATION(exp,_) -> COMPUTATION(dummyExp,dummyLoc), [| convert_exp exp |]
	| BLOCK(b,_) -> BLOCK(dummyBlock,dummyLoc), convert_block b 
	| SEQUENCE(s1,s2,loc) -> SEQUENCE(dummyStmt,dummyStmt,dummyLoc), [| convert_stmt s1; convert_stmt s2 |]
	| IF(exp,s1,s2,_) -> IF(dummyExp,dummyStmt,dummyStmt,dummyLoc), [| convert_exp exp; convert_stmt s1; convert_stmt s2 |]
	| WHILE(exp,s1,_) -> WHILE(dummyExp,dummyStmt,dummyLoc), [| convert_exp exp; convert_stmt s1 |]
	| DOWHILE(exp,s1,_) -> DOWHILE(dummyExp,dummyStmt,dummyLoc), [| convert_exp exp; convert_stmt s1 |]
	| FOR(fc,exp1,exp2,s1,_) -> 
	  let fc_child = match fc with
		  FC_EXP(exp3) -> convert_exp exp3
		| FC_DECL(def) -> convert_def def 
	  in
		FOR(dummyFC,dummyExp,dummyExp,dummyStmt,dummyLoc),
		[| fc_child; convert_exp exp1; convert_exp exp2; convert_stmt s1 |]
	| BREAK(_) -> BREAK(dummyLoc), [| |]
	| CONTINUE(_) -> CONTINUE(dummyLoc), [| |]
	| RETURN(exp,_) -> RETURN(dummyExp,dummyLoc), [| convert_exp exp |]
	| SWITCH(exp,s1,_) -> SWITCH(dummyExp,dummyStmt,dummyLoc),[| convert_exp exp; convert_stmt s1 |]
	| CASE(exp,s1,_) -> CASE(dummyExp,dummyStmt,dummyLoc), [| convert_exp exp; convert_stmt s1 |]
	| CASERANGE(e1,e2,s1,_) -> CASERANGE(dummyExp,dummyExp,dummyStmt,dummyLoc), [| convert_exp e1; convert_exp e2; convert_stmt s1 |]
	| DEFAULT(s1,_) -> DEFAULT(dummyStmt,dummyLoc), [| convert_stmt s1 |]
	| LABEL(str,s1,_) -> LABEL(str,dummyStmt,dummyLoc), [| convert_stmt s1 |]
	| GOTO(str,_) -> GOTO(str,dummyLoc), [|  |]
	| COMPGOTO(exp,_) -> COMPGOTO(dummyExp,dummyLoc), [| convert_exp exp |]
	| DEFINITION(d) -> DEFINITION(def_dum d), [| convert_def d |]
	| ASM(attrs,strs,dets,loc) -> 
	  let dummed_attrs = lmap attr_dum attrs in 
	  let dummed_dets = dets_dum dets in 
		ASM(dummed_attrs,[],dummed_dets,dummyLoc), [| |] (* FIXME *)
	  (* 		  Array.append (Array.concat (lmap attr_children alist)) (asm_det_children adetails)
	  *)
	| TRY_EXCEPT(b1,exp,b2,_) -> 
	  TRY_EXCEPT(dummyBlock,dummyExp,dummyBlock,dummyLoc),
	  Array.concat [convert_block b1 ; [| convert_exp exp |] ; convert_block b2 ]
	| TRY_FINALLY(b1,b2,_) -> TRY_FINALLY(dummyBlock,dummyBlock,dummyLoc),
	  Array.append (convert_block b1) (convert_block b2)
  in
	stmt_copy.node <- dum;
	typelabel (STMT(stmt_copy)),children
and convert_stmt stmt =
  let (stmt_tl_str,stmt_tl_node),children = tl_stmt stmt in
	node ~cabsid:stmt.id stmt_tl_str children stmt_tl_node (STMT(stmt))
and tl_exp exp = 
  let exp_copy = copy exp in
  let dum,children = 
	match (dn exp_copy) with
	  NOTHING -> NOTHING, [| |]
	| UNARY(uop,e1) -> UNARY(uop,dummyExp), [| convert_exp e1 |]
	| LABELADDR(str) -> LABELADDR(str), [| |]
	| BINARY(bop,e1,e2) -> BINARY(bop,dummyExp,dummyExp), [| convert_exp e1; convert_exp e2 |]
	| QUESTION(e1,e2,e3) -> QUESTION(dummyExp,dummyExp,dummyExp), [| convert_exp e1; convert_exp e2; convert_exp e3 |]
	| CAST((spec,dtype),ie) -> 
	  let dummed_specs = lmap spec_dum spec in 
	  let dummed_dt = dt_dum dtype in 
	  let dummed_IE = ie_dum ie in
		CAST((dummed_specs,dummed_dt),dummed_IE), Array.concat [(spec_children spec); (decl_children dtype); (ie_children ie)]
	| CALL(exp,elist) -> CALL(dummyExp,[]), Array.of_list (lmap convert_exp (exp :: elist))
	| COMMA(elist) -> COMMA([]), Array.of_list (lmap convert_exp elist)
	| CONSTANT(c) -> CONSTANT(c), [| |]
	| PAREN(e1) -> PAREN(dummyExp), [| convert_exp e1 |]
	| VARIABLE(str) -> VARIABLE(str), [| |]
	| EXPR_SIZEOF(e1) -> EXPR_SIZEOF(dummyExp), [| convert_exp e1 |]
	| TYPE_SIZEOF(spec,dtype) -> 
	  let dummed_specs = lmap spec_dum spec in 
	  let dummed_dt = dt_dum dtype in
		TYPE_SIZEOF(dummed_specs,dummed_dt), Array.append (spec_children spec) (decl_children dtype)
	| EXPR_ALIGNOF(e1) -> EXPR_ALIGNOF(dummyExp), [| convert_exp e1 |]
	| TYPE_ALIGNOF(spec,dtype) -> 
	  let dummed_specs = lmap spec_dum spec in 
	  let dummed_dt = dt_dum dtype in
		TYPE_ALIGNOF(dummed_specs, dummed_dt), Array.append (spec_children spec) (decl_children dtype)
	| INDEX(e1,e2) -> INDEX(dummyExp,dummyExp), [| convert_exp e1; convert_exp e2 |]
	| MEMBEROF(e1,str) -> MEMBEROF(dummyExp,str), [| convert_exp e1 |]
	| MEMBEROFPTR(e1,str) -> MEMBEROFPTR(dummyExp,str), [| convert_exp e1 |]
	| GNU_BODY(b) -> GNU_BODY(dummyBlock), convert_block b
	| EXPR_PATTERN(str) -> EXPR_PATTERN(str), [|  |] 
  in
	exp_copy.node <- dum;
	typelabel (EXP(exp_copy)),children
and convert_exp exp = 
  let (exp_tl_str,exp_tl_node), children = tl_exp exp in
	node ~cabsid:exp.id exp_tl_str children exp_tl_node (EXP(exp))
and def_dum def = 
  let dum = 
	match (dn def) with
	  FUNDEF(sn,b1,_,_) -> 
		FUNDEF(single_name_dum sn,dummyBlock,dummyLoc,dummyLoc)
	| DIRECTIVE(_) -> (dn def)
	| DECDEF(ing,_) -> DECDEF(ing_dum ing,dummyLoc)
	| TYPEDEF(ng,_) -> TYPEDEF(ng_dum ng,dummyLoc)
	| ONLYTYPEDEF(spec,_) -> ONLYTYPEDEF(lmap spec_dum spec,dummyLoc)
	| GLOBASM(str,_) -> GLOBASM("",dummyLoc)
	| PRAGMA(exp,_) -> PRAGMA(dummyExp,dummyLoc)
	| LINKAGE(str,_,_) -> LINKAGE("",dummyLoc,[])
  in def.node <- dum; def
and tl_def def = 
    let def_copy = copy def in 
  let dum,children = 
	match (dn def_copy) with
	  FUNDEF(sn,b1,_,_) -> 
		FUNDEF(single_name_dum sn,dummyBlock,dummyLoc,dummyLoc),
		Array.concat [ sn_children sn ; convert_block b1 ]
	| DIRECTIVE(_) -> (dn def_copy), [| |] (* FIXME: ignoring for now *)
	| DECDEF(ing,_) -> DECDEF(ing_dum ing,dummyLoc), ing_children ing  (* FIXME: is this ing_dum thing right? *)
	| TYPEDEF(ng,_) -> TYPEDEF(ng_dum ng,dummyLoc), ng_children ng
	| ONLYTYPEDEF(spec,_) -> ONLYTYPEDEF(lmap spec_dum spec,dummyLoc), spec_children spec
	| GLOBASM(str,_) -> GLOBASM(str,dummyLoc), [|  |]
	| PRAGMA(exp,_) -> PRAGMA(dummyExp,dummyLoc), [| convert_exp exp |]
	| LINKAGE(str,_,_) -> LINKAGE(str,dummyLoc,[]), [| |]
  in
	def_copy.node <- dum;
	 typelabel (DEF(def_copy)), children
and convert_def def = 
  let (def_tl_str,def_tl_node),children = tl_def def in
	node ~cabsid:def.id def_tl_str children def_tl_node (DEF(def))
and tn_children = function
  | Globals(defs) -> Array.of_list (lmap convert_def defs)
  | Stmts(ss) -> Array.of_list (lmap convert_stmt ss)
  | Exps(exps) -> Array.of_list (lmap convert_exp exps) 
  | Syntax(str) -> [|  |]
and attr_dum (str,elist) = (str,[]) 
and dets_dum = function
  | None -> None
  | Some(dets) -> 
	let dum_func (stropt,str,en) = (stropt,str,dummyExp) in
	  Some({aoutputs=lmap dum_func dets.aoutputs;
			ainputs=lmap dum_func dets.ainputs;
			aclobbers=dets.aclobbers})
and spec_dum se =
  match se with
  | SpecAttr(attr) -> SpecAttr(attr_dum attr)
  | SpecType(ts) -> SpecType(ts_dum ts)
  | _ -> se
and ts_dum ts =
  match ts with
  | Tstruct(str,Some(fgs),attrs) -> Tstruct(str,Some(lmap field_group_dum fgs), lmap attr_dum attrs)
  | Tunion(str,Some(fgs),attrs) -> Tunion(str,Some(lmap field_group_dum fgs), lmap attr_dum attrs)
  | Tstruct(str,None,attrs) -> Tstruct(str,None, lmap attr_dum attrs)
  | Tunion(str,None,attrs) -> Tunion(str,None, lmap attr_dum attrs)
  | Tenum(str,Some(eis),attrs) -> Tenum(str, Some(lmap enum_item_dum eis), lmap attr_dum attrs)
  | Tenum(str,None,attrs) -> Tenum(str,None, lmap attr_dum attrs)
  | TtypeofE(e) -> TtypeofE(dummyExp)
  | TtypeofT(spec,dt) -> TtypeofT(lmap spec_dum spec, dt_dum dt)
  | _ -> ts
and field_group_dum (spec, nes) = 
  let dum_func (name,enos) = name_dum name,  match enos with None -> None | Some(en) -> Some(dummyExp) in
	lmap spec_dum spec, lmap dum_func nes
and enum_item_dum (str,en,loc) = str,dummyExp,dummyLoc
and dt_dum = function
  | JUSTBASE -> JUSTBASE
  | PARENTYPE(attrs1,dt,attrs2) -> PARENTYPE(lmap attr_dum attrs1,dt_dum dt, lmap attr_dum attrs2)
  | ARRAY(dt,attrs,en) -> ARRAY(dt_dum dt,lmap attr_dum attrs,dummyExp)
  | PTR(attrs,dt) -> PTR(lmap attr_dum attrs, dt_dum dt)
  | PROTO(dt,sns,b) -> PROTO(dt_dum dt, lmap single_name_dum sns,b)
and single_name_dum (spec,name) = lmap spec_dum spec,name_dum name
and init_name_dum (name,ie) = name_dum name,ie_dum ie 
and ing_dum (spec,ins) = lmap spec_dum spec,lmap init_name_dum ins
and name_dum (str,dt,attrs,loc) = str,dt_dum dt,lmap attr_dum attrs,dummyLoc
and ng_dum (spec,names) = lmap spec_dum spec,lmap name_dum names
and iw_dum = function
NEXT_INIT -> NEXT_INIT
  | INFIELD_INIT(str,iw) -> INFIELD_INIT(str,iw_dum iw)
  | ATINDEX_INIT(e,iw) -> ATINDEX_INIT(dummyExp,iw_dum iw)
  | ATINDEXRANGE_INIT(e1,e2) -> ATINDEXRANGE_INIT(dummyExp,dummyExp)
and ie_dum = function
NO_INIT -> NO_INIT
  | SINGLE_INIT(e) -> SINGLE_INIT(dummyExp)
  | COMPOUND_INIT(lst) ->
	let dum_func (iw,ie) = iw_dum iw,ie_dum ie in
	  COMPOUND_INIT(lmap dum_func lst)
and attr_children attr = Array.of_list (lmap convert_exp (snd attr))
and spec_children specs =
  Array.concat 
	(lmap
	   (fun specn ->
		 match specn with
		   SpecAttr(attr) -> attr_children attr
		 | SpecType(tsn) -> 
		   begin
			 match tsn with
			 | Tstruct(_,Some(fgs), attrs)
			 | Tunion(_, Some(fgs), attrs) -> 
			   let fgcs = Array.concat (lmap field_group_children fgs) in
			   let attrsc = Array.concat (lmap attr_children attrs) in
				 Array.append fgcs attrsc
			 | Tenum(_,Some(eis), attrs) -> Array.concat ((lmap enum_item_children eis) @ (lmap attr_children attrs))
			 | TtypeofE(expn) -> [| convert_exp expn |]
			 | TtypeofT(spec,dtn) -> Array.append (spec_children spec) (decl_children dtn)
			 | _ -> [| |]
		   end
		 | _ -> [| |]) specs)
and field_group_children fg = 
  let sn, lst = fg in 
  let lst_children = 
	Array.concat
	  (lmap
		 (fun (name,expo) ->
		   let this = 
			 match expo with
			   Some(exp) -> [|convert_exp exp|]
			 | None -> [| |] 
		   in
			 Array.append (name_children name) this) lst) in
	Array.append lst_children  (spec_children sn)
and enum_item_children ei = 
  let str,enode,_ = ei in 
	[| convert_exp enode |]
and asm_det_children asmdet =
  match asmdet with
	Some({aoutputs=aoutputs;ainputs=ainputs;aclobbers=aclobbers}) ->
	  Array.of_list ((lmap (fun (sopt,s,exp) -> convert_exp exp) aoutputs) @
						(lmap (fun (sopt,s,exp) -> convert_exp exp) ainputs))
  | None -> [| |]
and decl_children dt =
  match dt with
  | JUSTBASE -> [| |]
  | PARENTYPE(alist1,decl,alist2) ->
	let a1s = Array.concat (lmap attr_children alist1) in 
	let a2s = Array.concat (lmap attr_children alist2) in 
	  Array.concat [a1s;a2s;decl_children decl]
  | ARRAY(decl,alist,exp) ->
	let aas = Array.concat (lmap attr_children alist) in 
	  Array.concat [decl_children decl;aas;[| convert_exp exp |]]
  | PTR(alist,decl) ->
	let aas = Array.concat (lmap attr_children alist) in 
	  Array.concat [aas;decl_children decl]
  | PROTO(decl,sns,b) -> 
	let dts = decl_children decl in 
	let snns = Array.concat (lmap sn_children sns) in 
	  Array.concat [dts;snns]
and ing_children ing =
  let (spec,ns) = ing in
  let specs = spec_children spec in 
  let inns = Array.concat (lmap init_name_children ns) in
	Array.concat [specs;inns]
and ng_children ng =
  let (spec,ns) = ng in
  let specs = spec_children spec in 
  let nns = Array.concat (lmap name_children ns) in 
	Array.concat [specs;nns]
and sn_children sn = 
  let (spec,name) = sn in
  let specs = spec_children spec in 
  let names = name_children name in 
	Array.concat [specs;names]
and ie_children ie =
  match ie with
  | NO_INIT -> [| |]
  | SINGLE_INIT(exp) -> [| convert_exp exp |]
  | COMPOUND_INIT(lst) -> 
	Array.concat
	  (lmap (fun(iw,ie) -> Array.concat [init_what_children iw; ie_children ie]) lst)
and name_children name = 
  let (str, dt, alist,_) = name in
	Array.append (decl_children dt) (Array.concat (lmap attr_children alist))
and init_name_children iname =
  let name,ie = iname in 
	Array.append (name_children name) (ie_children ie)
and init_what_children what =
  match what with
	NEXT_INIT -> [| |]
  | INFIELD_INIT(str,what2) -> init_what_children what2 
  | ATINDEX_INIT(expn,inode) -> Array.append [| convert_exp expn |] (init_what_children inode)
  | ATINDEXRANGE_INIT(e1,e2) -> [| convert_exp e1; convert_exp e2 |] 

and tree_to_diff_tree (tree : tree) : diff_tree_node = 
  let tree_tl_str, tree_tl_node = typelabel (TREE(fst tree,[])) in
  let children = Array.concat (lmap tn_children (snd tree)) in
	node ~cabsid:(-1) tree_tl_str children tree_tl_node (TREE(tree))

(* Now, apply treediff to the actual patches.  First, convert a patch to
   diff_tree_node representation.  This is actually pretty easy because the
   standardized edit actions mostly make use of diff_tree_nodes *)

let change_to_diff_tree change : diff_tree_node = failwith "Doesn't work right now; fixme!"
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

let cabslu = {lineno = -10; 
			  filename = "cabs loc unknown"; 
			  byteno = -10;
              ident = 0}

let process_tree tns = 
  lfoldl
	(fun res ->
	  fun tn ->
		match tn with 
		| Stmts(slist) -> res @ [Stmts([nd(BLOCK({blabels=[];battrs=[];bstmts=slist},cabslu))])]
		| Exps(elist) -> res@ [Exps([nd(COMMA(elist))])]
		| _ -> res @ [tn]) [] tns

  
