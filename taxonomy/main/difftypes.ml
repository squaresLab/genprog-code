open Batteries
open Ref
open Utils
open Cprint
open Cabs
open Cabsvisit

let tn_str tn = Pretty.sprint ~width:80 (d_tree_node () tn)
let def_str def = Pretty.sprint ~width:80 (d_def () def)
let stmt_str stmt = Pretty.sprint ~width:80 (d_stmt () stmt)
let exp_str exp = Pretty.sprint ~width:80 (d_exp () exp)

type parent_type =
	PTREE | PDEF | PSTMT | PEXP | FORINIT | PARENTTN | LOOPGUARD | CONDGUARD

type edit = 
	InsertTreeNode of tree_node node * int
  | ReorderTreeNode of tree_node node * int * int
  | ReplaceTreeNode of tree_node node * tree_node node * int
  | InsertDefinition of definition node * int * int * parent_type
  | ReplaceDefinition of definition node * definition node * int * int * parent_type
  | MoveDefinition of definition node * int * int * parent_type * parent_type
  | ReorderDefinition of definition node * int * int * int * parent_type
  | InsertStatement of statement node * int * int * parent_type
  | ReplaceStatement of statement node * statement node * int * int * parent_type
  | MoveStatement of statement node * int * int * parent_type * parent_type
  | ReorderStatement of statement node * int * int * int * parent_type
  | InsertExpression of expression node * int * int * parent_type
  | ReplaceExpression of expression node * expression node * int * int * parent_type
  | MoveExpression of expression node * int * int * parent_type * parent_type
  | ReorderExpression of expression node * int * int * int * parent_type
  | DeleteTN of tree_node node
  | DeleteDef of definition node
  | DeleteStmt of statement node
  | DeleteExp of expression node

type changes = edit list

let ptyp_str = function
  |	PTREE -> "PTREE"
  | PDEF -> "PDEF"
  | PSTMT -> "PSTMT"
  | PEXP -> "PEXP"
  | FORINIT -> "FORINIT"
  | PARENTTN -> "PARENTTN"
  | LOOPGUARD -> "LOOPGUARD"
  | CONDGUARD -> "CONDGUARD"

let print_edit = function
  | InsertTreeNode(tn,num) -> pprintf "Insert tree_node %s at %d\n" (Pretty.sprint ~width:80 (d_tree_node () tn)) num
  | ReorderTreeNode(tn,num1,num2) -> pprintf "Reorder treenode %s from %d to %d\n"  (Pretty.sprint ~width:80 (d_tree_node () tn)) num1 num2
  | ReplaceTreeNode(tn1,tn2,num1) -> 
	pprintf "Replace treenode %s with treenode %s at position %d\n" (tn_str tn1) (tn_str tn2) num1 
  | InsertDefinition(def,num1,num2,ptyp) -> 
	pprintf "Insert new definition %s to parent %d, position %d, type %s\n" 
	  (def_str def) num1 num2 (ptyp_str ptyp)
  | MoveDefinition(def,num1,num2,ptyp1,ptyp2) ->
	pprintf "Move definition %s to parent %d, position %d, from type %s to type %s\n"
	  (Pretty.sprint ~width:80 (d_def () def)) num1 num2 (ptyp_str ptyp1) (ptyp_str ptyp2)
  | ReorderDefinition(def,num1,num2,num3,ptyp) ->
	pprintf "Reorder definition %s at parent %d, from position %d to position %d, type %s\n"
	  (Pretty.sprint ~width:80 (d_def () def))  num1 num2 num3 (ptyp_str ptyp)
  | ReplaceDefinition(def1,def2,num1,num2,ptyp) ->
	pprintf "Replace definition %s with definition %s at parent %d, from position %d, type %s\n"
	  (def_str def1) (def_str def2) num1 num2 (ptyp_str ptyp)
  | InsertStatement(stmt,num1,num2,ptyp) ->
	pprintf "Insert statement %d %s to parent %d, position %d, type %s\n" 
	  stmt.id (Pretty.sprint ~width:80 (d_stmt () stmt)) num1 num2 (ptyp_str ptyp)
  | MoveStatement(stmt,num1,num2,ptyp1,ptyp2) ->
	pprintf "Move statement %s to parent %d, position %d, from type %s to type %s\n"
	  (Pretty.sprint ~width:80 (d_stmt () stmt))  num1 num2 (ptyp_str ptyp1) (ptyp_str ptyp2)
  | ReorderStatement(stmt,num1,num2,num3,ptyp) ->
	pprintf "Reorder statement %s at parent %d, from position %d to position %d, type %s\n"
	  (Pretty.sprint ~width:80 (d_stmt () stmt))  num1 num2 num3 (ptyp_str ptyp)
  | ReplaceStatement(stmt1,stmt2,num1,num2,ptyp) ->
	pprintf "Replace statement %s with statement %s at parent %d, from position %d, type %s\n"
	  (stmt_str stmt1) (stmt_str stmt2) num1 num2 (ptyp_str ptyp)
  | InsertExpression(exp,num1,num2,ptyp) ->
	pprintf "Insert expression %s to parent %d, position %d, type %s\n" 
	  (Pretty.sprint ~width:80 (d_exp () exp)) num1 num2 (ptyp_str ptyp)
  | MoveExpression(exp,num1,num2,ptyp1,ptyp2) ->
	pprintf "Move expression %s to parent %d, position %d, from type %s to type %s\n"
	  (Pretty.sprint ~width:80 (d_exp () exp)) num1 num2 (ptyp_str ptyp1) (ptyp_str ptyp2)
  | ReorderExpression(exp,num1,num2,num3,ptyp) ->
	pprintf "Reorder expression %s at parent %d, from position %d to position %d, type %s\n"
	  (Pretty.sprint ~width:80 (d_exp () exp)) num1 num2 num3 (ptyp_str ptyp)
  | ReplaceExpression(exp1,exp2,num1,num2,ptyp) ->
	pprintf "Replace expression %s with expression %s at parent %d, from position %d, type %s\n"
	  (exp_str exp1) (exp_str exp2) num1 num2 (ptyp_str ptyp)
  | DeleteTN(tn) -> pprintf "Delete TN %s\n" (tn_str tn)
  | DeleteDef(def) -> pprintf "Delete Def %s\n" (def_str def)
  | DeleteStmt(stmt) -> pprintf "Delete Stmt %s\n" (stmt_str stmt)
  | DeleteExp(exp) -> pprintf "Delete exp %s\n" (exp_str exp)

let typelabel_ht : (string, int) Hashtbl.t = hcreate 255 
let typelabel_counter = ref 0 

let typelabel (tlabel : string) : int =
  ht_find typelabel_ht tlabel (fun _ -> post_incr typelabel_counter)

type tree_info =
	{ exp_ht : (int, expression node) Hashtbl.t ;
	  stmt_ht : (int, statement node) Hashtbl.t ;
	  def_ht : (int, definition node) Hashtbl.t ;
	  tn_ht : (int, tree_node node) Hashtbl.t ;
	  parent_ht : (int, int list) Hashtbl.t 
	} 
let new_tree_info () = 
  { exp_ht = hcreate 10;
	stmt_ht = hcreate 10;
	def_ht = hcreate 10;
	tn_ht = hcreate 10;
	parent_ht = hcreate 10;
  } 


exception Found_It 
exception Found of int

let getinfo node printer tl tl_ht node_ht =
  let str = printer tl in
  pprintf "Node: %d, node: %s, tl_str: %s\n" node.id (printer node) str; flush stdout;
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

module type Mapper =
sig
  type retval

  val mapping_tn : tree_node node -> retval -> retval
  val mapping_def : definition node -> retval -> retval
  val mapping_stmt : statement node -> retval -> retval
  val mapping_exp : expression node -> retval -> retval
end

module LevelOrderTraversal =
  functor (S : Mapper ) ->
struct

  type pair_type = Pair of (S.retval -> S.retval) * (unit -> pair_type list) | Unit

  let nothing_fun = fun v -> v
  let mfun mapping children ele = Pair(mapping ele,children ele)
  let mnoth children ele = Pair(nothing_fun,children ele) 


  let rec mfuntn tn = mfun S.mapping_tn children_tn tn
  and mfundef def = mfun S.mapping_def children_def def
  and mfunstmt stmt = mfun S.mapping_stmt children_stmt stmt
  and mfunexp exp = mfun S.mapping_exp children_exp exp

  and children_tn tn () =
	match dn tn with
	  Globals(dlist) -> lmap mfundef dlist
	| Stmts(slist) -> lmap mfunstmt slist
	| Exps(elist) -> lmap mfunexp elist
	| Syntax(_) -> []

  and children_def def () =
	match dn def with 
	  FUNDEF(sn,b,_,_) -> [mnoth children_sn sn; mnoth children_block b]
	| DECDEF(ing,_) -> [mnoth children_ing ing]
	| TYPEDEF(ng,_) -> [mnoth children_ng ng]
	| ONLYTYPEDEF(spec,_) -> lmap (mnoth children_spec_elem) spec
	| PRAGMA(exp,_) -> [mfunexp exp]
	| LINKAGE(_,_,dlist) ->	lmap mfundef dlist
	| _ -> []

  and children_stmt stmt () =
	match dn stmt with
	| COMPGOTO(e1,_)
	| RETURN(e1,_) 
	| COMPUTATION(e1,_) -> [mfunexp e1]
	| BLOCK(b,_) -> [mnoth children_block b]
	| SEQUENCE(s1,s2,_) -> [mfunstmt s1;mfunstmt s2]
	| IF(e1,s1,s2,_) -> [mfunexp e1;mfunstmt s1; mfunstmt s2]
	| SWITCH(e1,s1,_)
	| CASE(e1,s1,_)
	| WHILE(e1,s1,_)
	| DOWHILE(e1,s1,_) -> [mfunexp e1;mfunstmt s1]
	| FOR(fc,e1,e2,s1,_) -> [mnoth children_fc fc;mfunexp e1;mfunexp e2;mfunstmt s1]
	| CASERANGE(e1,e2,s1,_) -> [mfunexp e1;mfunexp e2;mfunstmt s1]
	| LABEL(_,s1,_)
	| DEFAULT(s1,_) -> [mfunstmt s1]
	| DEFINITION(d) -> [mfundef d]
	| ASM(_,_,_,_) -> failwith "Not implemented"
	| TRY_EXCEPT(b1,e1,b2,_) -> [mnoth children_block b1;mfunexp e1;mnoth children_block b2]
	| TRY_FINALLY(b1,b2,_) -> [mnoth children_block b1;mnoth children_block b2]
	| _ -> []

  and children_exp exp () = 
	match dn exp with
	| PAREN(e1)
	| EXPR_SIZEOF(e1)
	| EXPR_ALIGNOF(e1)
	| MEMBEROF(e1,_)
	| MEMBEROFPTR(e1,_)
	| UNARY(_,e1) -> [mfunexp e1]
	| INDEX(e1,e2)
	| BINARY(_,e1,e2) -> [mfunexp e1;mfunexp e2]
	| QUESTION(e1,e2,e3) -> lmap mfunexp [e1;e2;e3]
	| CAST((spec,dt),ie) -> (lmap (mnoth children_spec_elem) spec) @ [mnoth children_dt dt; mnoth children_ie ie]
	| CALL(e1,elist) -> (mfunexp e1) :: lmap mfunexp elist
	| COMMA(elist) -> lmap mfunexp elist
	| TYPE_SIZEOF(spec,dt)
	| TYPE_ALIGNOF(spec,dt) -> (lmap (mnoth children_spec_elem) spec) @ [mnoth children_dt dt]
	| GNU_BODY(b) ->  [mnoth children_block b]
	| _ -> []

  and children_sn (spec,name) () =
	(lmap (mnoth children_spec_elem) spec) @ [mnoth children_name name]

  and children_block block () =
	(lmap mfunstmt block.bstmts) @ (lmap (mnoth children_attr) block.battrs)

  and children_ing (spec,ins) () =
	(lmap (mnoth children_spec_elem) spec) @ (lmap (mnoth children_in) ins)

  and children_ng (spec,names) () = 
	(lmap (mnoth children_spec_elem) spec) @ (lmap (mnoth children_name) names)

  and children_spec_elem se () = 
	match se with
	| SpecAttr(attr) -> [mnoth children_attr attr]
	| SpecType(ts) ->
	  begin
		match ts with
		| Tstruct(_,Some(fgs),attrs) 
		| Tunion(_,Some(fgs),attrs) -> (lmap (mnoth children_fg) fgs) @ (lmap (mnoth children_attr) attrs)
		| Tenum(_,Some(eis),attrs) ->  (lmap (mnoth children_ei) eis) @ (lmap (mnoth children_attr) attrs)
		| Tstruct(_,None,attrs) 
		| Tunion(_,None,attrs)
		| Tenum(_,None,attrs) -> lmap (mnoth children_attr) attrs
		| TtypeofE(exp) -> [mfunexp exp]
		| TtypeofT(spec,dt) -> (lmap (mnoth children_spec_elem) spec) @ [mnoth children_dt dt]
		| _ -> []
	  end
	| _ -> []

  and children_fc fc () = 
	match fc with
	| FC_EXP(exp) -> [mfunexp exp]
	| FC_DECL(def) -> [mfundef def]

  and children_dt dt () = 
	match dt with
	| PARENTYPE(attrs1,dt,attrs2) ->
	  (lmap (mnoth children_attr) attrs1) @ (mnoth children_dt dt) :: (lmap (mnoth children_attr) attrs2)
	| ARRAY(dt,attrs,exp) ->
	  (mnoth children_dt dt) :: (lmap (mnoth children_attr) attrs) @ [mfunexp exp]
	| PTR(attrs,dt) -> lmap (mnoth children_attr) attrs @ [mnoth children_dt dt]
	| PROTO(dt,sns,_) -> (mnoth children_dt dt) :: (lmap (mnoth children_sn) sns)
	| _ -> []

  and children_ie ie () = 
	match ie with
	| SINGLE_INIT(exp) -> [mfunexp exp]
	| COMPOUND_INIT(iwies) -> lmap (mnoth children_iwie) iwies
	| _ -> []

  and children_iwie iwie () =
	let iw,ie = iwie in 
	let rec children_iw iw () = 
	  match iw with
	  | INFIELD_INIT(_,iw) -> [mnoth children_iw iw]
	  | ATINDEX_INIT(e1,iw) -> mfunexp e1 :: [mnoth children_iw iw]
	  | ATINDEXRANGE_INIT(e1,e2) -> lmap mfunexp [e1;e2]
	  | _ -> []
	in
	let iws = mnoth children_iw iw in
	  iws :: [(mnoth children_ie ie)]

  and children_name (_,dt,attrs,_) () = 
	(mnoth children_dt dt) :: (lmap (mnoth children_attr) attrs)

  and children_attr (_,elist) () = lmap mfunexp elist 
  and children_in (name,ie) () = [mnoth children_name name; mnoth children_ie ie]
	
  and children_fg (spec,nenos) () = 
	(lmap (mnoth children_spec_elem) spec) @ 
	  (lflat (lmap (fun (n,eno) -> (mnoth children_name n) :: (match eno with None -> [] | Some(e) -> [mfunexp e])) nenos))

  and children_ei (_,exp,_) () = [mfunexp exp]
	
  and children_tree (t1 : tree) () = lmap mfuntn (snd t1)

  let q = Queue.create ()

  let traverse tree start = 
	Queue.add (Pair(nothing_fun,children_tree tree)) q ;
	let rec inner_traverse result = 
	  if Queue.is_empty q then result
	  else begin
		match Queue.take q with
		  Pair(mapping_x,children_x) ->
			liter (fun child -> Queue.add child q) (children_x());
			inner_traverse (mapping_x result)
		| Unit -> result
	  end
	in inner_traverse start
end

let parents1 : (int, (int * int * parent_type)) Hashtbl.t = hcreate 10
let parents2 : (int, (int * int * parent_type)) Hashtbl.t  = hcreate 10
let children1 : (int, int list) Hashtbl.t  = hcreate 10
let children2 : (int, int list) Hashtbl.t = hcreate 10
