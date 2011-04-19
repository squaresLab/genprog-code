open Batteries
open Set
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

(* FIXME: add parent info to the deletions! *)
(* Also, fix treediff so that the complete statement is moved, as it should be *)

type edit = 
	InsertTreeNode of tree_node node * int
  | ReorderTreeNode of tree_node node * int * int
  | ReplaceTreeNode of tree_node node * tree_node node * int
  | InsertDefinition of definition node * int * int * parent_type
  | ReplaceDefinition of definition node * definition node * int * int * parent_type
  | MoveDefinition of definition node * int * int * int * parent_type * parent_type
  | ReorderDefinition of definition node * int * int * int * parent_type
  | InsertStatement of statement node * int * int * parent_type
  | ReplaceStatement of statement node * statement node * int * int * parent_type
  | MoveStatement of statement node * int * int * int * parent_type * parent_type
  | ReorderStatement of statement node * int * int * int * parent_type
  | InsertExpression of expression node * int * int * parent_type
  | ReplaceExpression of expression node * expression node * int * int * parent_type
  | MoveExpression of expression node * int * int * int * parent_type * parent_type
  | ReorderExpression of expression node * int * int * int * parent_type
  | DeleteTN of tree_node node * int * parent_type
  | DeleteDef of definition node * int * parent_type
  | DeleteStmt of statement node * int * parent_type
  | DeleteExp of expression node * int * parent_type

type changes = (int * edit) list

let change_cntr = ref 0 
let change_ht : (int,edit) Hashtbl.t = hcreate 10
let new_change c = 
  let id = post_incr change_cntr in
	hadd change_ht id c; (id,c)

let ptyp_str = function
  |	PTREE -> "PTREE"
  | PDEF -> "PDEF"
  | PSTMT -> "PSTMT"
  | PEXP -> "PEXP"
  | FORINIT -> "FORINIT"
  | PARENTTN -> "PARENTTN"
  | LOOPGUARD -> "LOOPGUARD"
  | CONDGUARD -> "CONDGUARD"

let edit_str = function
  | InsertTreeNode(tn,num) -> Printf.sprintf "Insert tree_node %s at %d\n" (tn_str tn) num 
  | ReorderTreeNode(tn,num1,num2) -> Printf.sprintf "Reorder treenode %s from %d to %d\n" (tn_str tn) num1 num2
  | ReplaceTreeNode(tn1,tn2,num1) -> 
	Printf.sprintf "Replace treenode %s with treenode %s at position %d\n" (tn_str tn1) (tn_str tn2) num1 
  | InsertDefinition(def,num1,num2,ptyp) -> 
	Printf.sprintf "Insert new definition %s to parent %d, position %d, type %s\n" 
	  (def_str def) num1 num2 (ptyp_str ptyp)
  | MoveDefinition(def,num1,num2,num3,ptyp1,ptyp2) ->
	Printf.sprintf "Move definition %s to parent %d, position %d, from type %s to type %s\n"
	  (def_str def) num1 num2 (ptyp_str ptyp1) (ptyp_str ptyp2)
  | ReorderDefinition(def,num1,num2,num3,ptyp) ->
	Printf.sprintf "Reorder definition %s at parent %d, from position %d to position %d, type %s\n"
	  (def_str def)  num1 num2 num3 (ptyp_str ptyp)
  | ReplaceDefinition(def1,def2,num1,num2,ptyp) ->
	Printf.sprintf "Replace definition %s with definition %s at parent %d, from position %d, type %s\n"
	  (def_str def1) (def_str def2) num1 num2 (ptyp_str ptyp)
  | InsertStatement(stmt,num1,num2,ptyp) ->
	Printf.sprintf "Insert statement %d %s to parent %d, position %d, type %s\n" 
	  stmt.id (stmt_str stmt) num1 num2 (ptyp_str ptyp)
  | MoveStatement(stmt,num1,num2,num3,ptyp1,ptyp2) ->
	Printf.sprintf "Move statement %s to parent %d, position %d, from type %s to type %s\n"
	  (stmt_str stmt)  num1 num2 (ptyp_str ptyp1) (ptyp_str ptyp2)
  | ReorderStatement(stmt,num1,num2,num3,ptyp) ->
	Printf.sprintf "Reorder statement %s at parent %d, from position %d to position %d, type %s\n"
	  (stmt_str stmt)  num1 num2 num3 (ptyp_str ptyp)
  | ReplaceStatement(stmt1,stmt2,num1,num2,ptyp) ->
	Printf.sprintf "Replace statement %s with statement %s at parent %d, from position %d, type %s\n"
	  (stmt_str stmt1) (stmt_str stmt2) num1 num2 (ptyp_str ptyp)
  | InsertExpression(exp,num1,num2,ptyp) ->
	Printf.sprintf "Insert expression %s to parent %d, position %d, type %s\n" 
	  (exp_str exp) num1 num2 (ptyp_str ptyp)
  | MoveExpression(exp,num1,num2,num3,ptyp1,ptyp2) ->
	Printf.sprintf "Move expression %s to parent %d, position %d, from type %s to type %s\n"
	  (exp_str exp) num1 num2 (ptyp_str ptyp1) (ptyp_str ptyp2)
  | ReorderExpression(exp,num1,num2,num3,ptyp) ->
	Printf.sprintf "Reorder expression %s at parent %d, from position %d to position %d, type %s\n"
	  (exp_str exp) num1 num2 num3 (ptyp_str ptyp)
  | ReplaceExpression(exp1,exp2,num1,num2,ptyp) ->
	Printf.sprintf "Replace expression %s with expression %s at parent %d, from position %d, type %s\n"
	  (exp_str exp1) (exp_str exp2) num1 num2 (ptyp_str ptyp)
  | DeleteTN(tn,par,ptyp) -> Printf.sprintf "Delete TN %s from parent %d, type %s\n" (tn_str tn) par (ptyp_str ptyp)
  | DeleteDef(def,par,ptyp) -> Printf.sprintf "Delete Def %s from parent %d, type %s\n" (def_str def) par (ptyp_str ptyp)
  | DeleteStmt(stmt,par,ptyp) -> Printf.sprintf "Delete Stmt %s from parent %d, type %s\n" (stmt_str stmt) par (ptyp_str ptyp)
  | DeleteExp(exp,par,ptyp) -> Printf.sprintf "Delete exp %s from parent %d, type %s\n" (exp_str exp) par (ptyp_str ptyp)

let print_edit edit = pprintf "%s" (edit_str (snd edit))

let typelabel_ht : (string, int) Hashtbl.t = hcreate 255 
let typelabel_counter = ref 0 

let typelabel (tlabel : string) : int =
  ht_find typelabel_ht tlabel (fun _ -> post_incr typelabel_counter)

type tree_info =
	{ exp_ht : (int, (expression node * expression node)) Hashtbl.t ;
	  stmt_ht : (int, (statement node * statement node)) Hashtbl.t ;
	  def_ht : (int, (definition node * definition node)) Hashtbl.t ;
	  tn_ht : (int, (tree_node node * tree_node node)) Hashtbl.t ;
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

let dummyLoc = {lineno = -1; 
				filename = "";
				byteno = -1;
				ident = -1} 
let dummyBlock = { blabels = []; battrs = [] ; bstmts = [] ; } 
let dummyStmt = nd(NOP(dummyLoc)) 
let dummyDef = { (nd(FUNDEF(([],("",JUSTBASE,[],dummyLoc)),dummyBlock,dummyLoc,dummyLoc))) with id = (-2) }

class findDefVisitor ht = object
  inherit nopCabsVisitor

  val def_num = ref dummyDef
  val ht = ht 

  method vdef def = 
	let old_def = !def_num in
	  if !def_num.id == (-2) then (def_num := def; hadd ht def.id def) else hadd ht def.id !def_num; 
	  ChangeDoChildrenPost([def], (fun def -> def_num := old_def; def)) 

  method vstmt stmt = hadd ht stmt.id !def_num; DoChildren	
  method vexpr exp = hadd ht exp.id !def_num; DoChildren	
end

module FindStmtMapper =
struct
  type retval = statement node

  let stmt_ht = hcreate 10 
  let clear () = hclear stmt_ht

  let mapping_tn tn stmt_parent = hadd stmt_ht tn.id stmt_parent; stmt_parent
  let mapping_def def stmt_parent = hadd stmt_ht def.id stmt_parent; stmt_parent
  let mapping_stmt stmt stmt_parent = hadd stmt_ht stmt.id stmt; stmt
  let mapping_exp exp stmt_parent = hadd stmt_ht exp.id stmt_parent; stmt_parent

end

module FindDefMapper =
struct
  type retval = definition node

  let def_ht = hcreate 10 
  let clear () = hclear def_ht

  let mapping_tn tn def_parent = hadd def_ht tn.id def_parent; def_parent
  let mapping_def def def_parent = hadd def_ht def.id def; def
  let mapping_stmt stmt def_parent = hadd def_ht stmt.id def_parent; def_parent
  let mapping_exp exp def_parent = hadd def_ht exp.id def_parent; def_parent

end

module DefFindTraversal = LevelOrderTraversal(FindDefMapper)
module StmtFindTraversal = LevelOrderTraversal(FindStmtMapper)

let find_parents def_ht patch =
  let edits_ht = hcreate 10 in
  let edits_per_def = hcreate 10 in
	liter (fun (num,edit) ->
	  pprintf "adding edit %d: " num; print_edit (num,edit); pprintf "\n";
			 match edit with
			 | InsertDefinition(def,par,_,_) | ReplaceDefinition(_,def,par,_,_)
			 | MoveDefinition(def,par,_,_,_,_) | ReorderDefinition(def,par,_,_,_)	  
			 | DeleteDef (def,par,_) -> hadd edits_ht def.id par
			 | InsertStatement(stmt,par,_,_) | ReplaceStatement(_,stmt,par,_,_)
			 | MoveStatement(stmt,par,_,_,_,_) | ReorderStatement(stmt,par,_,_,_) 
			 | DeleteStmt (stmt,par,_) -> hadd edits_ht stmt.id par
			 | InsertExpression(exp,par,_,_) | ReplaceExpression(_,exp,par,_,_) 
			 | MoveExpression(exp,par,_,_,_,_) | ReorderExpression(exp,par,_,_,_)
			 | DeleteExp (exp,par,_) -> hadd edits_ht exp.id par
			 | _ -> failwith "Unexpected edit in Difftypes.find_parents")
	  patch;
	let add_ht defid edit =
	  let old = ht_find edits_per_def defid (fun _ -> []) in
		hrep edits_per_def defid (old@[edit])
	in
	let rec find_parent num = 
	  if hmem def_ht num then hfind def_ht num 
	  else find_parent (ht_find edits_ht num (fun _ -> failwith (Printf.sprintf "died in edits-hto find: %d\n" num)))
	in
	let defs = 
	  lmap (fun (num,edit) -> 
		match edit with
		| InsertDefinition(_,par,_,_) | ReplaceDefinition(_,_,par,_,_)
		| MoveDefinition(_,par,_,_,_,_) | ReorderDefinition(_,par,_,_,_)	  
		| DeleteDef (_,par,_)
		| InsertStatement(_,par,_,_) | ReplaceStatement(_,_,par,_,_)
		| MoveStatement(_,par,_,_,_,_) | ReorderStatement(_,par,_,_,_) 
		| DeleteStmt (_,par,_)
		| InsertExpression(_,par,_,_) | ReplaceExpression(_,_,par,_,_) 
		| MoveExpression(_,par,_,_,_,_) | ReorderExpression(_,par,_,_,_)
		| DeleteExp (_,par,_) -> 
		  let def = find_parent par in 
			add_ht def.id (num,edit); def
		| _ -> failwith "Unexepected edit in Difftypes.find_parents") patch in
	  lmap (fun def -> def,ht_find edits_per_def def.id (fun _ -> failwith "failed edits\n")) defs

type 'a lifted = STAR | MAYBE of 'a list | ATLEAST of 'a list | LNOTHING | UNUNIFIED of 'a list 
				 | PARTIALMATCH of 'a

and ops = Modify_value | Arithmetic | Bitwise | Logic | OnNumbers | OnBits | 
	Bop_op of bop_gen | Uop_op of uop_gen | Lifted_ops of ops lifted

and bop_gen =  Modify_assign | BitTruth | NotBitTruth | Shift
				| Bgen of ops | Bop of binary_operator | Bop_gen of bop_gen lifted

and uop_gen = Sizeof | Sign_modifier | Memory_operator | Not_operator | Alignof
			   | Pre_operator | Post_operator | Increment | Decrement | Uop of unary_operator   
			   | Ugen of ops | Uop_gen of uop_gen lifted

and exp_gen = EXPBASE of expression node
			   | ELIFTED of exp_gen lifted
			   | CONSTGEN of constant lifted
			   | UNARYOP of uop_gen * exp_gen
			   | BINOP of bop_gen * exp_gen * exp_gen
			   | QUESTOP of exp_gen * exp_gen * exp_gen
			   | CASTOP of (spec_gen * dt_gen) * ie_gen 
			   | CALLOP of exp_gen * exp_gen list
			   | COMMAOP of exp_gen list
			   | PARENOP of exp_gen
			   | EXPSIZEOFOP of exp_gen
			   | TYPESIZEOFOP of spec_gen * dt_gen
			   | EXPALIGNOFOP of exp_gen 
			   | TYPEALIGNOFOP of spec_gen * dt_gen
			   | INDEXOP of exp_gen * exp_gen
			   | MEMBEROFOP of exp_gen * string 
			   | MEMBEROFPTROP of exp_gen * string
			   | ADDROFEXP of exp_gen
			   | OPERATION of ops  * exp_gen
			   | SOMEMEMBER of exp_gen * string 
			   | VALUE of exp_gen 
			   | GNUGEN of block_gen

and spec_gen = Spec_list of se_gen list | Spec_lifted of spec_gen lifted | Spec_base of specifier

and se_gen = Spec_elem of spec_elem
			 | Se_attr of attr_gen
			 | Se_type of typeSpec_gen
			 | Se_lifted of se_gen lifted
			 | Se_CV of cvspec lifted
			 | Se_storage of storage lifted

and attr_gen = ATTRBASE of attribute | ATTRLIFTED of attr_gen lifted | ATTRGEN of string * exp_gen list
and storun = Struct | Union | Something
and typeSpec_gen = 
  | TSBASE of typeSpecifier | TSTYPEOFE of exp_gen | TSTYPEOFT of spec_gen * dt_gen
  | TSSORU of string * storun * fg_gen list option * attr_gen list 
  | TSLIFTED of typeSpec_gen lifted (* FIXME: no enums?? *)
and fg_gen = FGBASE of field_group | FGGEN of spec_gen * (name_gen * exp_gen option) list | FGLIFTED of fg_gen lifted
and sn_gen = SNBASE of single_name | SNGEN of spec_gen * name_gen | SNLIFTED of sn_gen lifted

and dt_gen = 
  | DTBASE of decl_type
  | DTLIFTED of dt_gen lifted
  | DTPAREN of attr_gen list * dt_gen * attr_gen list
  | DTARRAY of dt_gen * attr_gen list * exp_gen
  | DTPTR of attr_gen list * dt_gen
  | DTPROTO of dt_gen * sn_gen list 
  | DTCOMPLEX of dt_gen * attr_gen list

and ie_gen = 
  | IEBASE of init_expression
  | GENSINGLE of exp_gen
  | GENCOMPOUND of (iw_gen * ie_gen) list
  | IELIFTED of ie_gen lifted

and iw_gen = 
  | IWBASE of initwhat
  | IWINFIELD of string * iw_gen
  | IWATINDEX of exp_gen * iw_gen
  | IWATINDEXRANGE of exp_gen * exp_gen
  | IWLIFTED of iw_gen lifted
  | IWSOME of exp_gen * iw_gen

and stmt_gen = 
  | STMTBASE of statement node
  | SLIFTED of stmt_gen lifted
  | STMTCOMP of exp_gen
  | STMTBLOCK of block_gen
  | STMTSEQ of stmt_gen * stmt_gen
  | STMTIF of exp_gen * stmt_gen * stmt_gen
  | STMTFOR of fc_gen * exp_gen * exp_gen * stmt_gen
  | STMTLOOP of loop_type * exp_gen * stmt_gen
  | STMTCONTROL (* break or continue *)
  | STMTRET of exp_gen
  | STMTSWITCH of exp_gen * stmt_gen
  | STMTCASE of exp_gen * stmt_gen 
  | STMTCASERANGE of exp_gen * exp_gen * stmt_gen 
  | STMTDEFAULT of stmt_gen 
  | STMTLABEL of string * stmt_gen
  | STMTCOMPGOTO of exp_gen 
  | STMTDEF of def_gen (* FIXME: ommitting ASM for now *)
  | STMTTRYE of block_gen * exp_gen * block_gen
  | STMTTRYF of block_gen * block_gen 
and block_gen = Reg of stmt_gen list | BLKLIFTED of block_gen lifted | BLOCKBASE of block

and fc_gen = FCBASE of for_clause | FCLIFTED of fc_gen lifted | FCEXP of exp_gen | FCDECL of def_gen
and def_gen = DLIFTED of def_gen lifted
			  | DBASE of definition node
			  | DFUNDEF of sn_gen * block_gen
			  | DDECDEF of ing_gen
			  | DTYPEDEF of ng_gen
			  | DONLYTD of spec_gen
			  | DPRAGMA of exp_gen
			  | DLINK of string * def_gen list 
			  | DGENERICTYPE of spec_gen * name_gen list
			  | DGENERICDEC of spec_gen * name_gen

and loop_type = Any | While | DoWhile | AnyWhile	  
and ng_gen = NGBASE of name_group | NGGEN of spec_gen * name_gen list | NGLIFTED of ng_gen lifted
and name_gen = NAMEBASE of name | NAMEGEN of string * dt_gen * attr_gen list | NAMELIFTED of name_gen lifted
and ing_gen = INGBASE of init_name_group | INGGEN of spec_gen * in_gen list | INGLIFTED of ing_gen lifted
and in_gen = INBASE of init_name | INGEN of name_gen * ie_gen | INLIFTED of in_gen lifted

type tn_gen = 
  | TNLIFTED of tn_gen lifted
  |	GENDEFS of def_gen list
  | GENSTMTS of stmt_gen list
  | GENEXPS of exp_gen list
  | TNBASE of tree_node node

type tree_gen = TNS of tn_gen list | TREELIFTED of tree_gen lifted | TBASE of tree

type change_gen = (* potential FIXME: I lost the "which child" we're inserting
					 into because I think the context info is enough, but we may
					 want to put it back in? 
  |	InsertGen of dummy_gen 
  | MoveGen of dummy_gen 
  | DeleteGen of dummy_gen
  | ReplaceGen of dummy_gen * dummy_gen
  | ChangeLifted of change_gen lifted*)
  | ChangeBase of edit

type changes_gen = BASECHANGES of change_gen list | CHANGEATLEAST of change_gen list

(* types for generalized AST nodes *)
 
type guard = LOOP | EXPG | CATCH | CASEG | GUARDLIFTED of guard lifted

type old_context = 
	{
	  pdef : def_gen option;
	  pstmt : stmt_gen option;
	  pexp : exp_gen option;
	  sding :  int Set.t; (* fixme *)
	  gby : (guard * exp_gen) list;
	  ging : int Set.t; (* fixme *)
(*	  mutable renamed : (string,string) Map.t;*)
	}

type init_context = 
	{
	  parent_definition : definition node option;
	  parent_statement : statement node option;
	  parent_expression : expression node option;
	  surrounding : int Set.t; (* fixme *)
	  guarded_by: (guard * expression node) list;
	  guarding: int Set.t; (* fixme *)
	}

let make_icontext def s e sur gby ging = 
  {
	  parent_definition=def;
	  parent_statement=s;
	  parent_expression=e;
	  surrounding=sur;
	  guarded_by=gby;
	  guarding=ging;
(*	  alpha = Map.empty;*)
  }

let make_context def s e sur gby ging = 
  {
	  pdef=def;
	  pstmt=s;
	  pexp=e;
	  sding=sur;
	  gby=gby;
	  ging=ging;
(*	  renamed = Map.empty;*)
  }

type init_template = init_context * changes

let template_id = ref 0 
let new_template () = Ref.post_incr template_id

type template =
	{ template_id : int ;
	  filename : string;
	  def : definition node;
	  stmt : statement node;
	  edits : changes ;
	  names : StringSet.t ;
	  guards : (guard * expression node) Set.t ;
	  subgraphs : Pdg.subgraph list }

let print_template t = 
  pprintf "Template id: %d, fname: %s, def: %s, stmt: %s, edits: "
	t.template_id t.filename (def_str t.def) (stmt_str t.stmt);
  liter print_edit t.edits;
  pprintf "names: ";
  StringSet.iter (fun str -> pprintf "%s," str) t.names;
  pprintf "\n"; 
  pprintf "guards; ";
  Set.iter (fun (g,exp) -> pprintf "(";
	(match g with  LOOP -> pprintf "LOOP, "
	| EXPG -> pprintf "EXPG, "
	| CATCH -> pprintf "CATCH, "
	| CASEG -> pprintf "CASEG, " 
	| _ -> failwith "Unhandled guard type in print template");
	pprintf "%s)," (exp_str exp)) t.guards;
  pprintf "\n";
  pprintf "%d subgraphs" (llen t.subgraphs);
  liter
	(fun subgraph ->
	  pprintf "SEPSEPSEPSEP\n";
	  liter (fun ele -> Cfg.print_node ele.Pdg.cfg_node) subgraph;
	  pprintf "SEPSEPSEPSEP\n"
	) t.subgraphs;
  pprintf "Done printing template %d\n\b" t.template_id
