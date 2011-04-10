open Batteries
open Set
open Utils
open Cabsvisit
open Cabswalker
open Ttypes
open Difftypes
open Treediff
open Cfg
open Pdg
module C=Cabs

let vector_hash = hcreate 10

let hfind ht key msg = ht_find ht key (fun _ -> failwith msg)

(* OK, what are the interesting nodes for cabs? *)
(* everything rooted at statement, including expressions?  I think so. *)

type tIndex = {
  computation : int; if_ind : int; while_ind : int; dowhile  : int;
  for_ind : int; break : int; continue : int; return : int; 
  switch : int; case : int; caserange : int; default : int;
  label : int; goto : int; compgoto : int; definition : int;
  asm : int; try_except  : int; try_finally : int; uminus : int;
  uplus : int; unot : int; ubnot  : int; umemof  : int; uaddrof : int;
  upreincr : int; upredecr : int; uposincr : int; uposdecr : int;
  labeladdr : int; binary : int; question : int; cast : int; 
  call : int; comma : int; constant : int; paren : int; variable : int;
  expr_sizeof : int; type_sizeof : int; expr_alignof : int;
  type_alignof : int; index : int; memberof : int; memberofptr : int;
  gnu_body  : int; expr_pattern : int; badd : int; bsub : int; 
  bmul : int; bdiv : int; bmod : int; band : int; bor  : int;
  bband : int; bbor : int; bxor : int; bshl : int; bshr : int;
  beq : int; bne : int; blt : int; bgt : int; ble : int; bge : int;
  bassign : int; badd_assign : int; bsub_assign : int; bmul_assign : int;
  bdiv_assign : int; bmod_assign : int; bband_assign  : int;
  bbor_assign  : int; bxor_assign  : int; bshl_assign  : int;
  bshr_assign : int;
}

let i = {
  computation=0; if_ind=1; while_ind=2; dowhile=3; for_ind=4; break=5;
  continue=6; return=7; switch=8; case=9; caserange=10; default=11;
  label=12; goto=13; compgoto=14; definition=15; asm=16; try_except=17;
  try_finally=18; uminus=19; uplus=20; unot=21; ubnot=22; umemof=23;
  uaddrof=24; upreincr=25; upredecr=26; uposincr=27; uposdecr=28;
  labeladdr=29; binary=30; question=31; cast=32; call=33; comma=34; 
  constant=35; paren=36; variable=37; expr_sizeof=38; type_sizeof=39;
  expr_alignof=40; type_alignof=41; index=42; memberof=43; memberofptr=44;
  gnu_body=45; expr_pattern=46; badd=47; bsub=48; bmul=49; bdiv=50;
  bmod=51; band=52; bor=53; bband=54; bbor=55; bxor=56; bshl=57; bshr=58;
  beq=59; bne=60; blt=61; bgt=62; ble=63; bge=64; bassign=65; 
  badd_assign=66; bsub_assign=67; bmul_assign=68; bdiv_assign=69;
  bmod_assign=70; bband_assign=71; bbor_assign=72; bxor_assign=73;
  bshl_assign=74; bshr_assign=75;
}

let max_size = 75
(* we need to do everything in postorder *)
let array_incr array index =
  let currval = array.(index) in
	Array.set array index (currval + 1)
	     
let array_sum array1 array2 = (* for each i in array1, array1.(i) = array1.(i) + array2.(i) *)
  Array.iteri
	(fun index ->
	  fun val1 -> 
		Array.set array1 index (array1.(index) + array2.(index))) array1; array1
  
class vectorGenWalker = object(self)
  inherit [int Array.t] singleCabsWalker

  method default_res () = Array.make max_size 0
  method combine array1 array2 = array_sum array1 array2
  method wExpression exp =
	if not (hmem vector_hash (IntSet.singleton(exp.C.id))) then begin
	let exp_array = Array.make max_size 0 in
	let incr = array_incr exp_array in
	  (match C.dn exp with
	  | C.UNARY(uop,exp1) -> 
		let uop_index = 
		  match uop with
		  | C.MINUS -> i.uminus
		  | C.PLUS -> i.uplus 
		  | C.NOT -> i.unot
		  | C.BNOT -> i.ubnot
		  | C.MEMOF -> i.umemof 
		  | C.ADDROF -> i.uaddrof
		  | C.PREINCR -> i.upreincr
		  | C.PREDECR -> i.upredecr
		  | C.POSINCR -> i.uposincr
		  | C.POSDECR -> i.uposdecr 
		in
		  incr uop_index
	  | C.BINARY(bop,exp1,exp2) ->
		let bop_index = 
		  match bop with 
		  | C.ADD -> i.badd
		  | C.SUB -> i.bsub
		  | C.MUL -> i.bmul
		  | C.DIV -> i.bdiv
		  | C.MOD -> i.bmod
		  | C.AND -> i.band
		  | C.OR -> i.bor 
		  | C.BAND -> i.bband
		  | C.BOR -> i.bbor
		  | C.XOR -> i.bxor
		  | C.SHL -> i.bshl
		  | C.SHR -> i.bshr
		  | C.EQ -> i.beq
		  | C.NE -> i.bne
		  | C.LT -> i.blt
		  | C.GT -> i.bgt
		  | C.LE -> i.ble
		  | C.GE -> i.bge
		  | C.ASSIGN -> i.bassign
		  | C.ADD_ASSIGN -> i.badd_assign
		  | C.SUB_ASSIGN -> i.bsub_assign
		  | C.MUL_ASSIGN -> i.bmul_assign
		  | C.DIV_ASSIGN -> i.bdiv_assign
		  | C.MOD_ASSIGN -> i.bmod_assign
		  | C.BAND_ASSIGN -> i.bband_assign
		  | C.BOR_ASSIGN -> i.bbor_assign 
		  | C.XOR_ASSIGN -> i.bxor_assign
		  | C.SHL_ASSIGN -> i.bshl_assign
		  | C.SHR_ASSIGN -> i.bshr_assign 
		in
		  incr bop_index
	  | C.LABELADDR(str) -> incr i.labeladdr
	  | C.QUESTION(exp1,exp2,exp3) -> incr i.question
	  | C.CAST((spec,dt),ie) -> incr i.cast
	  | C.CALL(exp,elist) -> incr i.call
	  | C.CONSTANT(const) -> incr i.constant
	  (* GIANT QUESTION FIXME TODO: how to deal with info in variable names? *)
	  | C.VARIABLE(str) -> incr i.variable
	  | C.EXPR_SIZEOF(exp) -> incr i.expr_sizeof
	  | C.TYPE_SIZEOF(spec,dt) -> incr i.type_sizeof
	  | C.EXPR_ALIGNOF(exp) -> incr i.expr_alignof
	  | C.TYPE_ALIGNOF(spec,dt) -> incr i.type_alignof
	  | C.INDEX(e1,e2) -> incr i.index
	  | C.MEMBEROF(exp,str) -> incr i.memberof
	  | C.MEMBEROFPTR(exp,str) -> incr i.memberofptr
	  | C.GNU_BODY(b) -> incr i.gnu_body
	  | C.EXPR_PATTERN(str) -> incr i.expr_pattern
	  | _ -> ());
	  CombineChildrenPost(exp_array, 
						  (fun child_arrays -> 
							let exp_array = array_sum exp_array child_arrays in
							hadd vector_hash (IntSet.singleton(exp.C.id)) exp_array;
							exp_array))
	end else Result(hfind vector_hash (IntSet.singleton(exp.C.id)) "one")

  method wStatement stmt =
	if not (hmem vector_hash (IntSet.singleton (stmt.C.id))) then begin
	let stmt_array = Array.make max_size 0 in 
	let incr = array_incr stmt_array in
	  (match C.dn stmt with 
	  | C.COMPUTATION _ -> incr i.computation
	  | C.IF _ -> incr i.if_ind
	  | C.WHILE _ -> incr i.while_ind
	  | C.DOWHILE _ -> incr i.dowhile
	  | C.FOR _ -> incr i.for_ind
	  | C.BREAK _ -> incr i.break
	  | C.CONTINUE _ -> incr i.continue
	  | C.RETURN _ -> incr i.return
	  | C.SWITCH _ -> incr i.switch
	  | C.CASE _ -> incr i.case
	  | C.CASERANGE _ -> incr i.caserange
	  | C.DEFAULT _ -> incr i.default
	  | C.LABEL _ -> incr i.label
	  | C.GOTO _ -> incr i.goto
	  | C.COMPGOTO _ -> incr i.compgoto
	  | C.DEFINITION _ -> incr i.definition
	  | C.ASM _ ->  incr i.asm
	  | C.TRY_EXCEPT _ -> incr i.try_except
	  | C.TRY_FINALLY _ -> incr i.try_finally
	  | _ -> ()
	  );
	  CombineChildrenPost(stmt_array, 
						  (fun child_arrays -> 
							let stmt_array = array_sum stmt_array child_arrays in
							hadd vector_hash (IntSet.singleton(stmt.C.id)) stmt_array;
							stmt_array))
	end else Result(hfind vector_hash (IntSet.singleton(stmt.C.id)) "two")

  method wDefinition def = 
	if not (hmem vector_hash (IntSet.singleton (def.C.id))) then begin
	  ChildrenPost((fun array -> 
		hadd vector_hash (IntSet.singleton(def.C.id)) array; array))
	end else Result(hfind vector_hash (IntSet.singleton(def.C.id)) "three" )

  method wTreenode tn = 
	if not (hmem vector_hash (IntSet.singleton (tn.C.id))) then begin
	  ChildrenPost((fun array -> hadd vector_hash (IntSet.singleton(tn.C.id)) array; array))
	end else Result(hfind vector_hash (IntSet.singleton(tn.C.id)) "four")

end

let rec process_nodes sets window emitted =
  let emit () = 
	let set,array =
	  lfoldl
		(fun (sets,arrays) ->
		  fun (set,array) ->
			IntSet.union sets set,array_sum arrays array) (IntSet.empty,Array.make max_size 0) window in
	  hadd vector_hash set array; set,array
  in
  match sets with
	set :: sets ->
	  let setstr = IntSet.fold ( fun d -> fun str -> str^(Printf.sprintf "%d," d)) set "" in
	  let array = hfind vector_hash set ("set:"^setstr) in
	  let emitted,window = 
		if (llen window) == 3 then (emit()::emitted, List.tl window)
		else emitted,window
	  in
		process_nodes sets ((set,array) :: window) emitted
  | _ -> if (llen window) == 3 then emit() :: emitted else emitted 

let rec full_merge sets =
  let processed = process_nodes sets [] [] in
  let sets,arrays = List.split processed in 
	if (llen processed) >= 3 then arrays @ (full_merge sets)
	else arrays

class mergeWalker = object(self)
  inherit [int Array.t list] singleCabsWalker

  method default_res () = []
  method combine one two = one @ two

  method wExpression exp = 
	match exp.C.node with
	  C.MODSITE _ -> Result([])
	| C.NODE(node) -> begin
	  match node with
	  | C.CALL(exp,elist) ->
		let exps = lmap (fun exp -> IntSet.singleton exp.C.id) (exp::elist) in (* FIXME: do I really intend that cons? *)
		  CombineChildren(full_merge exps)
	  | C.COMMA(elist) ->
		let exps = lmap (fun exp -> IntSet.singleton exp.C.id) elist in
		  CombineChildren(full_merge exps)
	  | _ -> Children
	end

  method wBlock block = 
	let stmts = lmap (fun stmt -> IntSet.singleton stmt.C.id) block.C.bstmts in
	  CombineChildren(full_merge stmts)

  method wDefinition def =
	match def.C.node with
	  C.MODSITE _ -> Result([])
	| C.NODE(node) -> begin
	  match node with
	  | C.LINKAGE(_,_,dlist) -> (* FIXME: do we care about specifiers and such?  How is "adjacent" defined? *)
		let sets = lmap (fun def -> IntSet.singleton def.C.id) dlist in
		  CombineChildren(full_merge sets)
	  | _ -> Children
	end 

  method wTreenode tn = 
	match tn.C.node with
	  C.MODSITE _ -> Result([])
	| C.NODE(node) ->
	  let sets = 
		match node with
		| C.Globals(dlist) -> lmap (fun def -> IntSet.singleton def.C.id) dlist
		| C.Stmts(slist) -> lmap (fun stmt -> IntSet.singleton stmt.C.id) slist
		| C.Exps(elist) -> lmap (fun exp -> IntSet.singleton exp.C.id) elist
	  in
		CombineChildren(full_merge sets)

  method wTree (_,tns) = 
	let tn_sets = 
	  lmap (fun tn -> IntSet.singleton tn.C.id) tns in
	  CombineChildren(full_merge tn_sets) 

end

let mu (subgraph : Pdg.subgraph) = 
  pprintf "subgraph size: %d\n" (llen subgraph);
  (* this does both imaging and collection of vectors *)
  let cfg = lmap (fun p -> p.Pdg.cfg_node) subgraph in 
  let filtered = 
	lfilt
	  (fun cfg_node ->
		match cfg_node.cnode with
		  START | STOP | ENTRY -> false
		| _ -> true
	  ) cfg 
  in
  let rec get_stmts = function 
	| BASIC_BLOCK (slist) -> slist
	| CONTROL_FLOW(s,_) -> [s]
	| REGION_NODE (cns) -> 
	  lflat (lmap (fun (cn,_) -> get_stmts cn.cnode) cns)
	| _ -> []
  in
  let stmts = lflat (lmap (fun cn -> get_stmts cn.cnode) filtered) in
  let as_nums = 
	lmap (fun stmt -> IntSet.singleton stmt.C.id) stmts
  in
	full_merge as_nums

type changeIndex = {
  insertion : int; reorder : int; replace : int; move : int ; deletion : int;
  tree_node : int; definition : int; statement : int; expression : int;
  tree_parent : int; tn_parent : int; def_parent : int; stmt_parent : int;
  exp_parent : int; for_init : int; loop_guard : int ; cond_guard : int }

let ci = 
 { insertion=0; reorder=1; replace=2; move=3; deletion=4;
   tree_node=5; definition=6; statement=7; expression=8;
   tree_parent=9; tn_parent=10; def_parent=11; stmt_parent=12;
   exp_parent=13; for_init=14; loop_guard=15; cond_guard=16 }

let change_max = 17

let change_vec_ht = hcreate 10
let change_vectors (id,change) =
  let parent_type = function 
	| PTREE -> ci.tree_parent | PDEF -> ci.def_parent
	| PSTMT -> ci.stmt_parent | PEXP -> ci.exp_parent
	| FORINIT -> ci.for_init | PARENTTN -> ci.tn_parent
	| LOOPGUARD -> ci.loop_guard | CONDGUARD -> ci.cond_guard
  in
	ht_find change_vec_ht (IntSet.singleton id)
	  (fun _ ->
		let change_array = Array.make change_max 0 in
		let incr = array_incr change_array in
		  (match change with 
		  | InsertTreeNode _ ->
			incr ci.insertion; incr ci.tree_parent; incr ci.tree_node
		  | ReorderTreeNode _ ->
			incr ci.reorder; incr ci.tree_parent; incr ci.tree_node
		  | ReplaceTreeNode _ ->
			incr ci.replace; incr ci.tree_parent; incr ci.tree_node
		  | InsertDefinition(_,_,_,par) ->
			incr ci.insertion; incr (parent_type par); incr ci.definition
		  | ReplaceDefinition(_,_,_,_,par) ->
			incr ci.replace; incr (parent_type par); incr ci.definition
		  | MoveDefinition(_,_,_,par1,par2) ->
			incr ci.move; incr (parent_type par1); incr (parent_type par2); incr ci.definition
		  | ReorderDefinition(_,_,_,_,par) ->
			incr ci.reorder; incr (parent_type par); incr ci.definition
		  | InsertStatement(_,_,_,par) ->
			incr ci.insertion; incr (parent_type par); incr ci.statement
		  | ReplaceStatement(_,_,_,_,par) ->
			incr ci.replace; incr (parent_type par); incr ci.statement
		  | MoveStatement(_,_,_,par1,par2) ->
			incr ci.move; incr (parent_type par1); incr (parent_type par2); incr ci.statement
		  | ReorderStatement(_,_,_,_,par) ->
			incr ci.reorder; incr (parent_type par); incr ci.statement
		  | InsertExpression(_,_,_,par) ->
			incr ci.insertion; incr (parent_type par); incr ci.expression
		  | ReplaceExpression(_,_,_,_,par) ->
			incr ci.replace; incr (parent_type par); incr ci.expression
		  | MoveExpression(_,_,_,par1,par2) ->
			incr ci.move; incr (parent_type par1); incr (parent_type par2); incr ci.expression
		  | ReorderExpression(_,_,_,_,par) ->
			incr ci.reorder; incr (parent_type par); incr ci.expression
		  | DeleteTN _ -> incr ci.deletion; incr ci.tree_node
		  | DeleteDef _ -> incr ci.deletion; incr ci.definition
		  | DeleteStmt _ -> incr ci.deletion; incr ci.statement
		  | DeleteExp _ -> incr ci.deletion; incr ci.expression); change_array)

let vec_ast_ht = hcreate 10 

let change_asts (id,change) =
  ht_find vec_ast_ht (IntSet.singleton id) 
	(fun _ ->
	  match change with 
		InsertTreeNode(tn,_)
	  | ReorderTreeNode(tn,_,_)
	  | DeleteTN(tn,_) -> [hfind vector_hash (IntSet.singleton tn.C.id) "six"]
	  | ReplaceTreeNode(tn1,tn2,_) -> 
		let one = hfind vector_hash (IntSet.singleton tn1.C.id) "seven" in
		let two = hfind vector_hash (IntSet.singleton tn2.C.id) "eight" in
		  [one;two;array_sum one two]
	  | InsertDefinition(def,_,_,_)
	  | MoveDefinition(def,_,_,_,_)
	  | ReorderDefinition(def,_,_,_,_)   
	  | DeleteDef(def,_) -> [hfind vector_hash (IntSet.singleton def.C.id) "nine"]
	  | ReplaceDefinition(def1,def2,_,_,_) ->
		let one = hfind vector_hash (IntSet.singleton def1.C.id) "ten" in
		let two = hfind vector_hash (IntSet.singleton def2.C.id) "eleven" in
		  [one;two;array_sum one two]
	  | InsertStatement(stmt,_,_,_)
	  | MoveStatement(stmt,_,_,_,_)
	  | ReorderStatement(stmt,_,_,_,_) 
	  | DeleteStmt(stmt,_)-> [hfind vector_hash (IntSet.singleton stmt.C.id) "twelve"]
	  | ReplaceStatement(stmt1,stmt2,_,_,_) ->
		let one = hfind vector_hash (IntSet.singleton stmt1.C.id) "thirteen" in
		let two = hfind vector_hash (IntSet.singleton stmt2.C.id) "fourteen" in
		  [one;two;array_sum one two]
	  | InsertExpression(exp,_,_,_)
	  | MoveExpression(exp,_,_,_,_)
	  | ReorderExpression(exp,_,_,_,_)
	  | DeleteExp(exp,_) ->  [hfind vector_hash (IntSet.singleton exp.C.id) "fifteen" ]
	  | ReplaceExpression(exp1,exp2,_,_,_) ->
		let one = hfind vector_hash (IntSet.singleton exp1.C.id) "sixteen" in
		let two = hfind vector_hash (IntSet.singleton exp2.C.id) "seventeen" in
		  [one;two;array_sum one two]
	)

let rec process_changes sets window emitted =
  let emit () = 
	let set,array =
	  lfoldl
		(fun (sets,arrays) ->
		  fun (set,array) ->
			IntSet.union sets set,array_sum arrays array) (IntSet.empty,Array.make change_max 0) window in
	  hadd change_vec_ht set array; set,array
  in
  match sets with
	set :: sets ->
	  pprintf "set: [";
	  IntSet.iter (fun id -> pprintf "%d, " id) set;
	  pprintf "]\n";
	  let array = hfind change_vec_ht set "eighteen" in
	  let emitted,window = 
		if (llen window) == 3 then (emit()::emitted, List.tl window)
		else emitted,window
	  in
		process_changes sets ((set,array) :: window) emitted
  | _ -> if (llen window) == 3 then emit() :: emitted else emitted 

let rec full_change_merge sets =
  let processed = process_changes sets [] [] in
  let sets,arrays = List.split processed in 
	if (llen processed) >= 3 then arrays @ (full_change_merge sets)
	else arrays

(* a vector describing context can refer to:
   the entire AST of surrounding context.
   the characteristic vectors of the PDG of the entire AST of surrounding context
   the vectors of the syntax of the modification site
   the characteristic vectors of a subgraph in which a modification site is contained *)
(* what do the vectors match? From the paper, it's either (1) a complete AST
   subtree, (2) a sequence of contiguous statements, or (3) another semantic
   vector: a slice of another procedure *)

(* change: at some point, the distance between the first and the second tree
   might be relevant too.  The original clone detection stuff measures
   similarity...  That measure can't be the only thing, though, because it
   doesn't describe the actual change.  We need characteristic vectors for that.
*)

let vector_gen = new vectorGenWalker
let merge_gen = new mergeWalker

let full_info () = 
  let copy_over ht1 ht2 =
	hiter
	  (fun key ->
		fun value -> 
		  hadd ht1 key value) ht2
  in
  let exps1 = Hashtbl.copy t1_node_info.exp_ht in
  let stmts1 = Hashtbl.copy t1_node_info.stmt_ht in
  let defs1 = Hashtbl.copy t1_node_info.def_ht in
  let tns1 = Hashtbl.copy t1_node_info.tn_ht in
	copy_over exps1 t2_node_info.exp_ht;
	copy_over stmts1 t2_node_info.stmt_ht;
	copy_over defs1 t2_node_info.def_ht;
	copy_over tns1 t2_node_info.tn_ht;
	{exp_ht = exps1; stmt_ht = stmts1; def_ht = defs1;
	 tn_ht = tns1; parent_ht = hcreate 10 }

let get_ast_from_site modsite full_info = 
  if hmem full_info.exp_ht modsite then 
	let exp = hfind full_info.exp_ht modsite "nineteen" in
	  vector_gen#walkExpression exp :: merge_gen#walkExpression exp
  else if hmem full_info.stmt_ht modsite then 
	let stmt = hfind full_info.stmt_ht modsite "twenty" in
	  vector_gen#walkStatement stmt :: merge_gen#walkStatement stmt
  else if hmem full_info.def_ht modsite then 
	let def = hfind full_info.def_ht modsite "twenty-one" in
	  vector_gen#walkDefinition def :: merge_gen#walkDefinition def
  else
	let tn = hfind full_info.tn_ht modsite "twenty-two" in 
	  vector_gen#walkTreenode tn :: merge_gen#walkTreenode tn

let template_to_vectors tree1 tree2 modsites changes = 
  let full_info = full_info () in
	(* I think tree2 is will primarily be used for the changes, no? *)

	(* context first, from tree1.  Thought: do we want to merge change templates
	   over an entire file?  Doesn't seem like a bad idea.  Then another thing the
	   vectors can map to are sets of changes/contexts *)
	(* FIXME: figure out wtf process_nodes is doing, because it's not obvious it's right *)
	(* FIXME: debug interesting subgraphs *)
	pprintf "template to vectors go go go!\n"; flush stdout;
	hclear easy_access;
	hclear bb_map;
	let cfg1,tns1 = Cfg.ast2cfg tree1 in
	  if (llen cfg1) > 0 then begin
	  pprintf "cfg\n"; flush stdout;
	  let full_vecs1 = vector_gen#walkTree (fst tree1,tns1) in
		pprintf "full vecs 1a\n"; flush stdout;
		let full_vecs1 = full_vecs1 :: (merge_gen#walkTree (fst tree1, tns1)) in
		let pdg_nodes = Pdg.cfg2pdg cfg1 in
		  pprintf "made pdg1\n"; flush stdout;
		  let subgraphs = 
			lfilt
			  (fun subgraph -> not (List.is_empty subgraph))
			  (Pdg.interesting_subgraphs pdg_nodes)
		  in
			pprintf "computing modded\n"; flush stdout;
			let modded = 
			  lfilt (Pdg.contains_modsites modsites) subgraphs in
			  pprintf "computing mu over modded\n"; flush stdout;
			  hclear easy_access;
			  hclear bb_map;
			  let cfg2,tns2 = Cfg.ast2cfg tree2 in
				pprintf "full vecs 1\n"; flush stdout;
				let full_vecs2 = vector_gen#walkTree (fst tree2,tns2) in
				  pprintf "full vecs 2a\n"; flush stdout;
				  let full_vecs2 = full_vecs2 :: (merge_gen#walkTree (fst tree2,tns2)) in
					pprintf "walked tree\n"; flush stdout;
					let mod_pdg_vecs = lflat (lmap mu modded) in
					  pprintf "computing ast vecs\n"; flush stdout;
					  let mod_ast_vecs = 
						lflat (lmap (fun modsite -> get_ast_from_site modsite full_info) modsites) 
					  in
					  let context =  full_vecs1 @ full_vecs2 @ mod_pdg_vecs @ mod_ast_vecs in
						(* context (almost) done, now describe the change *) 
						pprintf "computing change_vecs\n"; flush stdout;
						let change_vecs = lmap change_vectors changes in
						let ids = lmap IntSet.singleton (fst (List.split changes)) in 
						  pprintf "computing merged change vecs\n"; flush stdout;
						  let merged_change_vecs = full_change_merge ids in
							pprintf "computing change asts\n"; flush stdout;
							let change_asts = lflat (lmap change_asts changes) in
							  (* can the distance just be the sum or harmonic mean of the distance between
								 the changes and the context? *)
							  context (*@ change_vecs @ merged_change_vecs @ change_asts*)
	  end else []
