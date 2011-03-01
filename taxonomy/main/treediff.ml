open Batteries
open Pretty
open Printf
open Ref
open Utils
open Map
open Cabs
open Cabsvisit
open Cprint
open Globals
open Diffparse
open Difftypes
open Convert
open Canon
open Cabswalker

let literi = List.iteri (* FIXME: put me in utils! *)
let lmapi = List.mapi

let check_comments strs = 
  lfoldl
	(fun (all_comment, unbalanced_beginnings,unbalanced_ends) ->
	   fun (diffstr : string) ->
		 let matches_comment_line = Str.string_match star_regexp diffstr 0 in
		 let matches_end_comment = try ignore(Str.search_forward end_comment_regexp diffstr 0); true with Not_found -> false in
		 let matches_start_comment = try ignore(Str.search_forward start_comment_regexp diffstr 0); true with Not_found -> false in
		   if matches_end_comment && matches_start_comment then 
			 (all_comment, unbalanced_beginnings, unbalanced_ends)
		   else 
			 begin
			   let unbalanced_beginnings,unbalanced_ends = 
				 if matches_end_comment && unbalanced_beginnings > 0 
				 then (unbalanced_beginnings - 1,unbalanced_ends) 
				 else if matches_end_comment then unbalanced_beginnings, unbalanced_ends + 1 
				 else  unbalanced_beginnings, unbalanced_ends
			   in
			   let unbalanced_beginnings = if matches_start_comment then unbalanced_beginnings + 1 else unbalanced_beginnings in 
				 all_comment && matches_comment_line, unbalanced_beginnings,unbalanced_ends
			 end)
	(true, 0,0) strs

(*************************************************************************)

(* XDiff algorithm: mostly taken from cdiff/the original paper, except
 * where Wes modified it to fix their bugs *)

exception Found_It 
exception Found_Node of diff_tree_node 
exception Found of int

let typelabel (tlabel : string) : int =
  ht_find typelabel_ht tlabel (fun _ -> post_incr typelabel_counter)

type tree_info =
	{ exp_ht : (int, expression node) Hashtbl.t ;
	  stmt_ht : (int, statement node) Hashtbl.t ;
	  def_ht : (int, definition node) Hashtbl.t ;
	  tn_ht : (int, tree_node node) Hashtbl.t
	} 
let new_tree_info () = 
  { exp_ht = hcreate 10;
	stmt_ht = hcreate 10;
	def_ht = hcreate 10;
	tn_ht = hcreate 10;
  } 

class dumifyVisitor () = object(self)
  inherit nopCabsVisitor

  val tl_info = new_tree_info ()

  method vexpr exp = 
	ChangeDoChildrenPost(exp,(fun exp -> hadd tl_info.exp_ht exp.id exp; dummyExp))

  method vstmt stmt = 
	let dum = match stmt.node with
	  NOP(_) -> NOP(dummyLoc)
	| COMPUTATION(exp,_) -> COMPUTATION(exp,dummyLoc)
	| BLOCK(b,_) -> BLOCK(dummyBlock,dummyLoc), convert_block b 
	| SEQUENCE(s1,s2,loc) -> SEQUENCE(dummyStmt,dummyStmt,dummyLoc)
	| IF(exp,s1,s2,_) -> IF(exp,dummyStmt,dummyStmt,dummyLoc)
	| WHILE(exp,s1,_) -> WHILE(exp,dummyStmt,dummyLoc)
	| DOWHILE(exp,s1,_) -> DOWHILE(exp,dummyStmt,dummyLoc)
	| FOR(fc,exp1,exp2,s1,_) -> 
		FOR(fc,exp1,exp2,dummyStmt,dummyLoc),
	| BREAK(_) -> BREAK(dummyLoc)
	| CONTINUE(_) -> CONTINUE(dummyLoc)
	| RETURN(exp,_) -> RETURN(exp,dummyLoc)
	| SWITCH(exp,s1,_) -> SWITCH(exp,dummyStmt,dummyLoc)
	| CASE(exp,s1,_) -> CASE(exp,dummyStmt,dummyLoc)
	| CASERANGE(e1,e2,s1,_) -> CASERANGE(e1,e2,dummyStmt,dummyLoc)
	| DEFAULT(s1,_) -> DEFAULT(dummyStmt,dummyLoc), [| convert_stmt s1 |]
	| LABEL(str,s1,_) -> LABEL(str,dummyStmt,dummyLoc), [| convert_stmt s1 |]
	| GOTO(str,_) -> GOTO(str,dummyLoc), [|  |]
	| COMPGOTO(exp,_) -> COMPGOTO(exp,dummyLoc), [| convert_exp exp |]
	| DEFINITION(d) -> DEFINITION(def_dum d)
	| ASM(attrs,strs,dets,loc) -> failwith "I'm too lazy for ASM"
(*	  let dummed_attrs = lmap attr_dum attrs in 
	  let dummed_dets = dets_dum dets in 
		ASM(dummed_attrs,[],dummed_dets,dummyLoc) *)
	| TRY_EXCEPT(b1,exp,b2,_) -> 
	  TRY_EXCEPT(dummyBlock,exp,dummyBlock,dummyLoc)
	| TRY_FINALLY(b1,b2,_) -> TRY_FINALLY(dummyBlock,dummyBlock,dummyLoc)
	in
	  stmt.node <- dum;
	  hadd tl_info.stmt_ht stmt.id stmt; DoChildren
	  
  method vdef def = 
	ChangeDoChildrenPost([def],(fun defs -> lmap (fun def -> hadd tl_info.def_ht def.id def; dummyDef) defs)) 

  method vblock b = ChangeDoChildrenPost(b,(fun b -> dummyBlock))

  method vtreenode tn =
	let dum = 
	  match tn.node with 
	  | Globals(dlist) -> Globals([])
	  | Stmts(slist) -> Stmts([])
	  | Exps(elist) -> Exps([])
	  | Syntax(str) -> Syntax(str)
	in
	  ChangeDoChildrenPost(tn,(fun tn -> tn.node <- dum; hadd tl_info.tn_ht tn.id tn; tn))

  method get_tl_info () = tl_info 

end

let getinfo node printer ht tl_ht node_ht =
  let tl = hfind ht node.id in
  let str = printer tl in
  pprintf "Node: %d, node: %s, tl_str: %s\n" node.id (printer node) str; flush stdout;
  let tlint = typelabel str in
  let old_tl = 
	if hmem tl_ht tlint then hfind tl_ht tlint else [] in
	hrep tl_ht tlint (node.id :: old_tl);
	node.typelabel <- tlint;
	node.tl_str <- str;
	hadd node_ht node.id node;
	DoChildren

class typelabelVisitor tl_info = 
  object(self)
  inherit nopCabsVisitor

  val tl_info = tl_info
  val typelabel_ht = hcreate 10 
  val node_info = new_tree_info ()

  method vexpr exp = 
	getinfo exp (fun exp -> "EXPRESSION: " ^ (Pretty.sprint ~width:80 (d_exp () exp))) tl_info.exp_ht typelabel_ht node_info.exp_ht

  method vstmt stmt = 
	getinfo stmt (fun stmt -> "STATEMENT: " ^ (Pretty.sprint ~width:80 (d_stmt () stmt))) tl_info.stmt_ht typelabel_ht node_info.stmt_ht

  method vdef def = 
	getinfo def (fun def -> "DEFINITION: " ^ (Pretty.sprint ~width:80 (d_def () def))) tl_info.def_ht typelabel_ht node_info.def_ht

  method vtreenode tn = 
	getinfo tn (fun tn -> "TREENODE: " ^ (Pretty.sprint ~width:80 (d_tree_node () tn))) tl_info.tn_ht typelabel_ht node_info.tn_ht

  method get_hts () = typelabel_ht, node_info

end

type pair_type = Pair of (unit -> unit) * (unit -> pair_type list) | Unit

let nothing_fun = fun () -> ()
let mfun mapping children ele = Pair(mapping ele,children ele)
let mnoth children ele = Pair(nothing_fun,children ele) 

class virtual levelOrderTraversal = object(self)

  method mfuntn tn = mfun self#mapping_tn self#children_tn tn
  method mfundef def = mfun self#mapping_def self#children_def def
  method mfunstmt stmt = mfun self#mapping_stmt self#children_stmt stmt
  method mfunexp exp = mfun self#mapping_exp self#children_exp exp

  method virtual mapping_tn : tree_node node -> unit -> unit
  method virtual mapping_def : definition node -> unit -> unit
  method virtual mapping_stmt : statement node -> unit -> unit
  method virtual mapping_exp : expression node -> unit -> unit

  method children_tn tn () =
	match tn.node with
	  Globals(dlist) -> lmap self#mfundef dlist
	| Stmts(slist) -> lmap self#mfunstmt slist
	| Exps(elist) -> lmap self#mfunexp elist
	| Syntax(_) -> []

  method children_def def () =
	match def.node with 
	  FUNDEF(sn,b,_,_) -> [mnoth self#children_sn sn; mnoth self#children_block b]
	| DECDEF(ing,_) -> [mnoth self#children_ing ing]
	| TYPEDEF(ng,_) -> [mnoth self#children_ng ng]
	| ONLYTYPEDEF(spec,_) -> lmap (mnoth self#children_spec_elem) spec
	| PRAGMA(exp,_) -> [self#mfunexp exp]
	| LINKAGE(_,_,dlist) ->	lmap self#mfundef dlist
	| _ -> []

  method children_stmt stmt () =
	match stmt.node with
	| COMPGOTO(e1,_)
	| RETURN(e1,_) 
	| COMPUTATION(e1,_) -> [self#mfunexp e1]
	| BLOCK(b,_) -> [mnoth self#children_block b]
	| SEQUENCE(s1,s2,_) -> [self#mfunstmt s1;self#mfunstmt s2]
	| IF(e1,s1,s2,_) -> [self#mfunexp e1;self#mfunstmt s1; self#mfunstmt s2]
	| SWITCH(e1,s1,_)
	| CASE(e1,s1,_)
	| WHILE(e1,s1,_)
	| DOWHILE(e1,s1,_) -> [self#mfunexp e1;self#mfunstmt s1]
	| FOR(fc,e1,e2,s1,_) -> [mnoth self#children_fc fc;self#mfunexp e1;self#mfunexp e2;self#mfunstmt s1]
	| CASERANGE(e1,e2,s1,_) -> [self#mfunexp e1;self#mfunexp e2;self#mfunstmt s1]
	| LABEL(_,s1,_)
	| DEFAULT(s1,_) -> [self#mfunstmt s1]
	| DEFINITION(d) -> [self#mfundef d]
	| ASM(_,_,_,_) -> failwith "Not implemented"
	| TRY_EXCEPT(b1,e1,b2,_) -> [mnoth self#children_block b1;self#mfunexp e1;mnoth self#children_block b2]
	| TRY_FINALLY(b1,b2,_) -> [mnoth self#children_block b1;mnoth self#children_block b2]
	| _ -> []

  method children_exp exp () = 
	match exp.node with
	| PAREN(e1)
	| EXPR_SIZEOF(e1)
	| EXPR_ALIGNOF(e1)
	| MEMBEROF(e1,_)
	| MEMBEROFPTR(e1,_)
	| UNARY(_,e1) -> [self#mfunexp e1]
	| INDEX(e1,e2)
	| BINARY(_,e1,e2) -> [self#mfunexp e1;self#mfunexp e2]
	| QUESTION(e1,e2,e3) -> lmap self#mfunexp [e1;e2;e3]
	| CAST((spec,dt),ie) -> (lmap (mnoth self#children_spec_elem) spec) @ [mnoth self#children_dt dt; mnoth self#children_ie ie]
	| CALL(e1,elist) -> (self#mfunexp e1) :: lmap self#mfunexp elist
	| COMMA(elist) -> lmap self#mfunexp elist
	| TYPE_SIZEOF(spec,dt)
	| TYPE_ALIGNOF(spec,dt) -> (lmap (mnoth self#children_spec_elem) spec) @ [mnoth self#children_dt dt]
	| GNU_BODY(b) ->  [mnoth self#children_block b]
	| _ -> []

  method children_sn (spec,name) () =
	(lmap (mnoth self#children_spec_elem) spec) @ [mnoth self#children_name name]

  method children_block block () =
	(lmap self#mfunstmt block.bstmts) @ (lmap (mnoth self#children_attr) block.battrs)

  method children_ing (spec,ins) () =
	(lmap (mnoth self#children_spec_elem) spec) @ (lmap (mnoth self#children_in) ins)

  method children_ng (spec,names) () = 
	(lmap (mnoth self#children_spec_elem) spec) @ (lmap (mnoth self#children_name) names)

  method children_spec_elem se () = 
	match se with
	| SpecAttr(attr) -> [mnoth self#children_attr attr]
	| SpecType(ts) ->
	  begin
		match ts with
		| Tstruct(_,Some(fgs),attrs) 
		| Tunion(_,Some(fgs),attrs) -> (lmap (mnoth self#children_fg) fgs) @ (lmap (mnoth self#children_attr) attrs)
		| Tenum(_,Some(eis),attrs) ->  (lmap (mnoth self#children_ei) eis) @ (lmap (mnoth self#children_attr) attrs)
		| Tstruct(_,None,attrs) 
		| Tunion(_,None,attrs)
		| Tenum(_,None,attrs) -> lmap (mnoth self#children_attr) attrs
		| TtypeofE(exp) -> [self#mfunexp exp]
		| TtypeofT(spec,dt) -> (lmap (mnoth self#children_spec_elem) spec) @ [mnoth self#children_dt dt]
		| _ -> []
	  end
	| _ -> []

  method children_fc fc () = 
	match fc with
	| FC_EXP(exp) -> [self#mfunexp exp]
	| FC_DECL(def) -> [self#mfundef def]

  method children_dt dt () = 
	match dt with
	| PARENTYPE(attrs1,dt,attrs2) ->
	  (lmap (mnoth self#children_attr) attrs1) @ (mnoth self#children_dt dt) :: (lmap (mnoth self#children_attr) attrs2)
	| ARRAY(dt,attrs,exp) ->
	  (mnoth self#children_dt dt) :: (lmap (mnoth self#children_attr) attrs) @ [self#mfunexp exp]
	| PTR(attrs,dt) -> lmap (mnoth self#children_attr) attrs @ [mnoth self#children_dt dt]
	| PROTO(dt,sns,_) -> (mnoth self#children_dt dt) :: (lmap (mnoth self#children_sn) sns)
	| _ -> []

  method children_ie ie () = 
	match ie with
	| SINGLE_INIT(exp) -> [self#mfunexp exp]
	| COMPOUND_INIT(iwies) -> lmap (mnoth self#children_iwie) iwies
	| _ -> []

  method children_iwie iwie () =
	let iw,ie = iwie in 
	let rec children_iw iw () = 
	  match iw with
	  | INFIELD_INIT(_,iw) -> [mnoth children_iw iw]
	  | ATINDEX_INIT(e1,iw) -> self#mfunexp e1 :: [mnoth children_iw iw]
	  | ATINDEXRANGE_INIT(e1,e2) -> lmap self#mfunexp [e1;e2]
	  | _ -> []
	in
	let iws = mnoth children_iw iw in
	  iws :: [(mnoth self#children_ie ie)]

  method children_name (_,dt,attrs,_) () = 
	(mnoth self#children_dt dt) :: (lmap (mnoth self#children_attr) attrs)

  method children_attr (_,elist) () = lmap self#mfunexp elist 
  method children_in (name,ie) () = [mnoth self#children_name name; mnoth self#children_ie ie]
	
  method children_fg (spec,nenos) () = 
	(lmap (mnoth self#children_spec_elem) spec) @ 
	  (lflat (lmap (fun (n,eno) -> (mnoth self#children_name n) :: (match eno with None -> [] | Some(e) -> [self#mfunexp e])) nenos))

  method children_ei (_,exp,_) () = [self#mfunexp exp]
	
  method children_tree (t1 : tree) () = lmap self#mfuntn (snd t1)

  method traverse tree = 
	let q = Queue.create () in 
	  Queue.add (Pair(nothing_fun,self#children_tree tree)) q ;
	  while not (Queue.is_empty q) do
		match Queue.take q with
		  Pair(mapping_x,children_x) ->
			liter (fun child -> Queue.add child q) (children_x());
			mapping_x ()
		| Unit -> ()
	  done

end

let in_map_domain m t = Map.mem t m 
let in_map_range m t = Map.exists_f (fun k -> fun v -> v == t) m 
let map_size m = Enum.count (Map.enum m) 

let match_frag_list list1 list2 matchfun m m' = 
  let array1 = Array.of_list list1 in 
  let array2 = Array.of_list list2 in 
  let xc = Array.length array1 in 
  let yc = Array.length array2 in 
	for i = 0 to pred (min xc yc) do
	  matchfun array1.(i) array2.(i) m m'
	done

let rec match_fragment_def def1 def2 m m' =
  if not (in_map_domain m def1.id) &&
	not (in_map_range m def2.id) &&
	def2.typelabel == def2.typelabel then begin
	  m' := Map.add def1.id def2.id !m' ;
	  match def1.node,def2.node with
		FUNDEF(sn1,b1,_,_),FUNDEF(sn2,b2,_,_) -> 
		  match_fragment_sn sn1 sn2 m m'; 
		  match_fragment_block b1 b2 m m'
	  | DECDEF(ing1,_),DECDEF(ing2,_) -> match_fragment_ing ing1 ing2 m m'
	  | TYPEDEF(ng1,_),TYPEDEF(ng2,_) -> match_fragment_ng ng1 ng2 m m'
	  | ONLYTYPEDEF(spec1,_),ONLYTYPEDEF(spec2,_) -> 
		match_frag_list spec1 spec2 match_fragment_spec_elem m m'
	  | PRAGMA(exp1,_),PRAGMA(exp2,_) -> match_fragment_exp exp1 exp2 m m'
	  | LINKAGE(_,_,dlist1),LINKAGE(_,_,dlist2) -> 
		match_frag_list dlist1 dlist2 match_fragment_def m m'
	  | _,_ -> ()
	end
and match_fragment_stmt (stmt1 : statement node) (stmt2 : statement node) m m' =
  if not (in_map_domain m stmt1.id) &&
	not (in_map_range m stmt2.id) &&
	stmt1.typelabel == stmt2.typelabel then begin
	  m' := Map.add stmt1.id stmt2.id !m' ;
	  match stmt1.node,stmt2.node with
	  | COMPGOTO(e1,_),COMPGOTO(e2,_)
	  | RETURN(e1,_),RETURN(e2,_)
	  | COMPUTATION(e1,_),COMPUTATION(e2,_) -> match_fragment_exp e1 e2 m m'
	  | BLOCK(b1,_),BLOCK(b2,_) -> match_fragment_block b1 b2 m m'
	  | SEQUENCE(s1,s2,_),SEQUENCE(s3,s4,_) -> 
		match_fragment_stmt s1 s3 m m';
		match_fragment_stmt s2 s3 m m'
	  | IF(e1,s1,s2,_),IF(e2,s3,s4,_) ->
		match_fragment_exp e1 e2 m m';
		match_fragment_stmt s1 s3 m m';
		match_fragment_stmt s2 s3 m m'
	  | CASE(e1,s1,_),CASE(e2,s2,_)
	  | SWITCH(e1,s1,_),SWITCH(e2,s2,_)
	  | WHILE(e1,s1,_),WHILE(e2,s2,_)
	  | DOWHILE(e1,s1,_),DOWHILE(e2,s2,_) ->
		match_fragment_exp e1 e2 m m';
		match_fragment_stmt s1 s2 m m'
	  | FOR(fc1,e1,e2,s1,_),FOR(fc2,e3,e4,s2,_) ->
		match_fragment_fc fc1 fc2 m m';
		match_fragment_exp e1 e3 m m';
		match_fragment_exp e2 e4 m m';
		match_fragment_stmt s1 s2 m m'
	  | CASERANGE(e1,e2,s1,_),CASERANGE(e3,e4,s2,_) ->
		match_fragment_exp e1 e3 m m';
		match_fragment_exp e2 e4 m m';
		match_fragment_stmt s1 s2 m m'
	  | DEFINITION(d1),DEFINITION(d2) -> match_fragment_def d1 d2 m m'
	  | TRY_EXCEPT(b1,e1,b2,_),TRY_EXCEPT(b3,e2,b4,_) ->
		match_fragment_block b1 b3 m m';
		match_fragment_exp e1 e2 m m';
		match_fragment_block b2 b4 m m'
	  | TRY_FINALLY(b1,b2,_),TRY_FINALLY(b3,b4,_) -> 
		match_fragment_block b1 b3 m m';
		match_fragment_block b2 b4 m m'
	  | DEFAULT(s1,_),DEFAULT(s2,_)
	  | LABEL(_,s1,_),LABEL(_,s2,_) -> match_fragment_stmt s1 s2 m m'
	  | ASM(_,slist1,_,_),ASM(_,slist2,_,_) -> failwith "FIXME"
	  | _,_ -> ()
	end
and match_fragment_exp exp1 exp2 m m' =
  if not (in_map_domain m exp1.id) &&
	not (in_map_range m exp2.id) &&
	exp1.typelabel == exp2.typelabel then begin
	  m' := Map.add exp1.id exp2.id !m' ;
	  match exp1.node,exp2.node with
	  | EXPR_ALIGNOF(e1),EXPR_ALIGNOF(e2)
	  | EXPR_SIZEOF(e1),EXPR_SIZEOF(e2)
	  | PAREN(e1),PAREN(e2)
	  | MEMBEROF(e1,_),MEMBEROF(e2,_)
	  | MEMBEROFPTR(e1,_),MEMBEROFPTR(e2,_)
	  | UNARY(_,e1),UNARY(_,e2) -> match_fragment_exp e1 e2 m m'
	  | INDEX(e1,e2),INDEX(e3,e4)
	  | BINARY(_,e1,e2),BINARY(_,e3,e4) ->
		match_fragment_exp e1 e3 m m';
		match_fragment_exp e2 e4 m m'
	  | QUESTION(e1,e2,e3),QUESTION(e4,e5,e6) ->
		match_fragment_exp e1 e4 m m';
		match_fragment_exp e2 e5 m m';
		match_fragment_exp e3 e6 m m'
	  | CALL(e1,elist1),CALL(e2,elist2) ->
		match_fragment_exp e1 e2 m m';
		match_frag_list elist1 elist2 match_fragment_exp m m'
	  | COMMA(elist1),COMMA(elist2) ->
		match_frag_list elist1 elist2 match_fragment_exp m m'
	  | CAST((spec1,dt1),ie1),CAST((spec2,dt2),ie2) -> 
		match_frag_list spec1 spec2 match_fragment_spec_elem m m';
		match_fragment_dt dt1 dt2 m m';
		match_fragment_ie ie1 ie2 m m'
	  | TYPE_SIZEOF(spec1,dt1),TYPE_SIZEOF(spec2,dt2)
	  | TYPE_ALIGNOF(spec1,dt1),TYPE_ALIGNOF(spec2,dt2) ->
		match_frag_list spec1 spec2 match_fragment_spec_elem m m';
		match_fragment_dt dt1 dt2 m m'
	  | GNU_BODY(b1),GNU_BODY(b2) -> match_fragment_block b1 b2 m m'
	  | _,_ -> ()
	end
and match_fragment_tn tn1 tn2 m m' =
  if not (in_map_domain m tn1.id) &&
	not (in_map_range m tn2.id) &&
	tn1.typelabel == tn2.typelabel then begin
	  m' := Map.add tn1.id tn2.id !m' ;
	  match tn1.node,tn2.node with
	  | Globals(dlist1),Globals(dlist2) -> 
		match_frag_list dlist1 dlist2 match_fragment_def m m'
	  | Stmts(slist1),Stmts(slist2) ->
		match_frag_list slist1 slist2 match_fragment_stmt m m'
	  | Exps(elist1),Exps(elist2) -> 
		match_frag_list elist1 elist2 match_fragment_exp m m'
	  | _,_ -> ()
	end 
and match_fragment_sn sn1 sn2 m m' =
  let (spec1,name1),(spec2,name2) = sn1, sn2 in 
	match_frag_list spec1 spec2 match_fragment_spec_elem m m';
	match_fragment_name name1 name2 m m'
and match_fragment_block b1 b2 m m' = 
  match_frag_list b1.bstmts b2.bstmts match_fragment_stmt m m';
  match_frag_list b1.battrs b2.battrs match_fragment_attr m m'
and match_fragment_ing ing1 ing2 m m' = 
  let (spec1,ins1),(spec2,ins2) = ing1, ing2 in
	match_frag_list spec1 spec2 match_fragment_spec_elem m m';
	match_frag_list ins1 ins2 match_fragment_init_name m m'
and match_fragment_ng ng1 ng2 m m' = 
  let (spec1,names1),(spec2,names2) = ng1,ng2 in
	match_frag_list spec1 spec2 match_fragment_spec_elem m m';
	match_frag_list names1 names2 match_fragment_name m m'
and match_fragment_spec_elem se1 se2 m m' = 
  match se1,se2 with
  | SpecAttr(attr1),SpecAttr(attr2) -> match_fragment_attr attr1 attr2 m m'
  | SpecType(ts1),SpecType(ts2) -> match_fragment_typespec ts1 ts2 m m'
  | _,_ -> ()
and match_fragment_typespec se1 se2 m m' = 
  match se1,se2 with
  | Tstruct(_,Some(fgs1),attrs1), Tstruct(_,Some(fgs2),attrs2)
  | Tunion(_,Some(fgs1),attrs1), Tunion(_,Some(fgs2),attrs2) ->
	match_frag_list fgs1 fgs2 match_fragment_fg m m';
	match_frag_list attrs1 attrs2 match_fragment_attr m m'
  | Tenum(_,None,attrs1), Tenum(_,None,attrs2)
  | Tstruct(_,None,attrs1), Tstruct(_,None,attrs2)
  | Tunion(_,None,attrs1), Tunion(_,None,attrs2) ->
	match_frag_list attrs1 attrs2 match_fragment_attr m m'
  | Tenum(_,Some(eis1),attrs1),Tenum(_,Some(eis2),attrs2) ->
	match_frag_list eis1 eis2 match_fragment_ei m m';
	match_frag_list attrs1 attrs2 match_fragment_attr m m'
  | TtypeofE(e1),TtypeofE(e2) -> match_fragment_exp e1 e2 m m'
  | TtypeofT(spec1,dt1),TtypeofT(spec2,dt2) ->
	match_frag_list spec1 spec2 match_fragment_spec_elem m m';
	match_fragment_dt dt1 dt2 m m'
  | _,_ -> ()
and match_fragment_fc fc1 fc2 m m' = 
  match fc1,fc2 with
	FC_EXP(e1),FC_EXP(e2) -> match_fragment_exp e1 e2 m m'
  | FC_DECL(d1),FC_DECL(d2) -> match_fragment_def d1 d2 m m'
  | _,_ -> ()
and match_fragment_dt dt1 dt2 m m' = 
  match dt1,dt2 with
  | PARENTYPE(attrs1,dt1,attrs2),PARENTYPE(attrs3,dt2,attrs4) ->
	match_frag_list attrs1 attrs3 match_fragment_attr m m';
	match_fragment_dt dt1 dt2 m m';
	match_frag_list attrs2 attrs4 match_fragment_attr m m'
  | ARRAY(dt1,attrs1,e1),ARRAY(dt2,attrs2,e2) ->
	match_fragment_dt dt1 dt2 m m';
	match_frag_list attrs1 attrs2 match_fragment_attr m m';
	match_fragment_exp e1 e2 m m'
  | PTR(attrs1,dt1),PTR(attrs2,dt2) ->
	match_frag_list attrs1 attrs2 match_fragment_attr m m';
	match_fragment_dt dt1 dt2 m m'
  | PROTO(dt1,sns1,_),PROTO(dt2,sns2,_) ->
	match_fragment_dt dt1 dt2 m m';
	match_frag_list sns1 sns2 match_fragment_sn m m'
  | _,_ -> ()
and match_fragment_ie ie1 ie2 m m' = 
  match ie1,ie2 with
  | SINGLE_INIT(e1),SINGLE_INIT(e2) -> match_fragment_exp e1 e2 m m'
  | COMPOUND_INIT(iwies1),COMPOUND_INIT(iwies2) ->
	match_frag_list iwies1 iwies2 match_fragment_iwie m m'
  | _,_ -> ()
and match_fragment_iwie iwie1 iwie2 m m' =
  let (iw1,ie1),(iw2,ie2) = iwie1, iwie2 in
	match_fragment_init_what iw1 iw2 m m';
	match_fragment_ie ie1 ie2 m m'
and match_fragment_init_what iw1 iw2 m m' =
  match iw1,iw2 with
  | INFIELD_INIT(_,iw1),INFIELD_INIT(_,iw2) -> match_fragment_init_what iw1 iw2 m m'
  | ATINDEX_INIT(e1,iw1), ATINDEX_INIT(e2,iw2) ->
	match_fragment_exp e1 e2 m m';
	match_fragment_init_what iw1 iw2 m m'
  | ATINDEXRANGE_INIT(e1,e2),ATINDEXRANGE_INIT(e3,e4) ->
	match_fragment_exp e1 e3 m m';
	match_fragment_exp e2 e4 m m'
  | _,_ -> ()
and match_fragment_name name1 name2 m m' = 
  let (_,dt1,attrs1,_),(_,dt2,attrs2,_) = name1,name2 in 
	match_fragment_dt dt1 dt2 m m';
	match_frag_list attrs1 attrs2 match_fragment_attr m m'
and match_fragment_attr attr1 attr2 m m' = 
  let (_,elist1),(_,elist2) = attr1,attr2 in 
	match_frag_list elist1 elist2 match_fragment_exp m m'
and match_fragment_init_name in1 in2 m m' = 
  let (name1,ie1),(name2,ie2) = in1, in2 in
	match_fragment_name name1 name2 m m';
	match_fragment_ie ie1 ie2 m m'
and match_fragment_fg fg1 fg2 m m' = 
  let (spec1,lst1),(spec2,lst2) = fg1, fg2 in
	match_frag_list spec1 spec2 match_fragment_spec_elem m m';
	let match_neno neno1 neno2 m m' =
	  let (name1,eno1),(name2,eno2) = neno1,neno2 in
		match_fragment_name name1 name2 m m';
		match eno1,eno2 with
		  Some(e1),Some(e2) -> match_fragment_exp e1 e2 m m'
		| _,_ -> ()
	in
	  match_frag_list lst1 lst2 match_neno m m'
and match_fragment_ei ei1 ei2 m m' = 
  let (_,e1,_),(_,e2,_) = ei1,ei2 in
	match_fragment_exp e1 e2 m m'

let nodes_in_tree_equal_to node tlht nodeht = 
  let equal_to_tl = try hfind tlht node.typelabel with _ -> [] in
	lmap (fun id -> hfind nodeht id) equal_to_tl


let mapping node nodeht tlht matchfun m =
  if in_map_domain !m node.id then () else
	begin
	  let y = nodes_in_tree_equal_to node tlht nodeht in
	  let m'' = ref Map.empty in 
		liter
		  (fun yi ->
			if not (in_map_range !m yi.id) then begin
			  let m' = ref Map.empty in 
				matchfun node yi !m m' ;
				if map_size !m' > map_size !m'' then begin
				  m'' := !m'
				end 
			end 
		  ) y ;
		m := Map.union !m !m'' 
	end

class mappingTraversal t2_tl_ht t2_node_info = object(self)
  inherit levelOrderTraversal as super

  val m = ref Map.empty
  val t2_tl_ht = t2_tl_ht
  val t2_node_info = t2_node_info 


  method mapping_tn tn () = mapping tn t2_node_info.tn_ht t2_tl_ht match_fragment_tn m
  method mapping_def def () = mapping def t2_node_info.def_ht t2_tl_ht match_fragment_def m
  method mapping_stmt stmt () = mapping stmt t2_node_info.stmt_ht t2_tl_ht match_fragment_stmt m
  method mapping_exp exp () = mapping exp t2_node_info.exp_ht t2_tl_ht match_fragment_exp m

  method get_map () = !m
end

type parent_type =
	PTREE | PDEF | PSTMT | PEXP | FORINIT | PARENTTN | LOOPGUARD | CONDGUARD

class getParentsWalker = object(self)
  inherit [unit] singleCabsWalker

  val info = hcreate 10
  val parent = ref (-1)
  val position = ref (-1)
  val typ = ref PTREE

  method default_res () = ()
  method combine unit1 unit2 = ()

  method wExpression exp = 
	hadd info exp.id (!parent,!position,!typ);
	pprintf "Exp: %d, Parent: %d\n" exp.id !parent;
	let tempparent = !parent in 
	let tempposition = !position in
	let temptyp = !typ in
	  parent := exp.id; typ := PEXP;
	  ChildrenPost(fun _ -> parent := tempparent; position := tempposition; typ := temptyp)

  method wStatement stmt = 
	pprintf "Stmt: %d, Parent: %d\n" stmt.id !parent;
	hadd info stmt.id (!parent,!position,!typ);
	let tempparent = !parent in 
	let tempposition = !position in
	let temptyp = !typ in
	  parent := stmt.id; typ := PSTMT;
	  ChildrenPost(fun _ -> parent := tempparent; position := tempposition; typ := temptyp)

  method wDefinition def = 
	pprintf "Def: %d, Parent: %d\n" def.id (-1);
	hadd info def.id (!parent,!position,!typ);
	let tempparent = !parent in 
	let tempposition = !position in
	let temptyp = !typ in
	  parent := def.id; typ := PDEF; 
	  ChildrenPost(fun _ -> parent := tempparent; position := tempposition; typ := temptyp)

  method wTreenode tn = 
	pprintf "TN: %d, str: %s Parent: %d\n" tn.id (Pretty.sprint ~width:80 (d_tree_node () tn)) (-1);
	hadd info tn.id (-1,!position,PTREE);
	let tempparent = !parent in 
	let tempposition = !position in
	let temptyp = !typ in
	  parent := tn.id; typ := PARENTTN; position := 0;
	  ChildrenPost(fun _ -> parent := tempparent; position := tempposition; typ := temptyp)

  method childrenExpression exp = 
	let walklist start lst =
	  literi
		(fun index ->
		  fun exp ->
			position := index + start;
			self#walkExpression exp) lst
	in
	  position := 0;
	  match exp.node with
	  | UNARY(_,e1) -> walklist 1 [e1]
	  | BINARY(_,e1,e2) -> walklist 1 [e1;e2]
	  | QUESTION(e1,e2,e3) -> walklist 0 [e1;e2;e3]
	  | CAST((spec,dt),ie) -> 
		self#walkSpecifier spec; incr position;
		self#walkDeclType dt; incr position;
		self#walkInitExpression ie
	  | CALL(e1,elist) -> walklist 0 [e1];incr position; self#walkExpressions elist
	  | COMMA(elist) ->self#walkExpressions elist
	  | MEMBEROF(e1,_)
	  | MEMBEROFPTR(e1,_)
	  | EXPR_SIZEOF(e1)
	  | EXPR_ALIGNOF(e1)
	  | PAREN(e1) -> walklist 0 [e1]
	  | TYPE_SIZEOF(spec,dt)
	  | TYPE_ALIGNOF(spec,dt) ->
		self#walkSpecifier spec;incr position;self#walkDeclType dt
	  | INDEX(e1,e2) -> walklist 0 [e1;e2]
	  | GNU_BODY(b) -> self#walkBlock b
	  | _ -> ()

  method childrenStatement stmt = 
	let walklist start lst =
	  literi
		(fun index ->
		  fun stmt ->
			position := index + start;
			self#walkStatement stmt) lst
	in
	  position := 0;
	  match stmt.node with
	  | COMPGOTO(e1,_)
	  | RETURN(e1,_)
	  | COMPUTATION(e1,_) -> self#walkExpression e1
	  | BLOCK(b,_) -> self#walkBlock b
	  | SEQUENCE(s1,s2,_) -> walklist 0 [s1;s2]
	  | IF(e1,s1,s2,_) ->
		let temp = !typ in
		  typ := CONDGUARD;
		  self#walkExpression e1; 
		  typ := temp;
		  walklist 1 [s1;s2]
	  | CASE(e1,s1,_)
	  | SWITCH(e1,s1,_) -> 
		let temp = !typ in
		  typ := CONDGUARD;self#walkExpression e1;incr position; self#walkStatement s1;typ := temp
	  | WHILE(e1,s1,_)
	  | DOWHILE(e1,s1,_) ->
		let temp = !typ in
		  typ := LOOPGUARD;self#walkExpression e1;
		  incr position;typ := temp;self#walkStatement s1
	  | FOR(fc,e1,e2,s1,_) -> 
		let temp = !typ in 
		  typ := FORINIT;
		  (match fc with 
			FC_EXP(e) -> self#walkExpression e
		  | FC_DECL(d) -> self#walkDefinition d);
		  incr position;typ := LOOPGUARD;self#walkExpression e1; 
		  incr position;self#walkExpression e2; 
		  incr position;typ := temp;self#walkStatement s1
	  | CASERANGE(e1,e2,s1,_) ->
		self#walkExpression e1; incr position; self#walkExpression e2; incr position; self#walkStatement s1
	  | DEFAULT(s1,_) -> self#walkStatement s1
	  | LABEL(_,s1,_) -> incr position; self#walkStatement s1
	  | DEFINITION(d) -> self#walkDefinition d
	  | ASM(_,_,_,_) -> failwith "ASM not handled in getparents walker"
	  | TRY_EXCEPT(b1,e1,b2,_) ->
		self#walkBlock b1; incr position; 
		let temp = !typ in
		  typ := CONDGUARD;self#walkExpression e1; incr position; typ := temp; self#walkBlock b2
	  | TRY_FINALLY(b1,b2,_) ->
		self#walkBlock b1; incr position; incr position; self#walkBlock b2
	  | _ -> ()	  

  method childrenDefinition def = 
	position := 0;
	match def.node with
	  FUNDEF(sn,b,_,_) -> self#walkSingleName sn; incr position; self#walkBlock b
	| DECDEF(ing,_) -> self#walkInitNameGroup ing
	| TYPEDEF(ng,_) -> self#walkNameGroup ng
	| ONLYTYPEDEF(spec, _) -> self#walkSpecifier spec
	| PRAGMA(exp,_) -> self#walkExpression exp
	| LINKAGE(_,_,dlist) -> position := 2; self#walkDefinitions dlist
	| _ -> ()

(*  method childrenTreenode tn = 
	match tn.node with 
	| Globals(dlist) ->
	  literi (fun index -> fun def -> hadd info def.id (tn.id,index,PARENTTN)) dlist
	| Stmts(slist) ->
	  literi (fun index -> fun stmt -> hadd info stmt.id (tn.id,index,PARENTTN)) slist
	| Exps(elist) ->
	  literi (fun index -> fun exp -> hadd info exp.id (tn.id,index,PARENTTN)) elist
	| _ -> ()*)

  method get_info () = info
end

let tree_mapping t1 t2_tl_info t2_node_info = 
  let map_traversal = new mappingTraversal t2_tl_info t2_node_info in
	map_traversal#traverse t1; map_traversal#get_map()

let node_that_maps_to mapping parent = 
  try
	Map.iter (fun k -> fun v -> if v == parent then raise (Found(k))) mapping ;
	None
  with Found(a) -> Some(a)

let mapsto m x y = Map.mem x m && ((Map.find x m) == y)
  
type edits = 
	NewTreeNode of tree_node node * int
  | ReorderTreeNode of tree_node node * int * int
  | InsertDefinition of definition node * int * int * parent_type
  | MoveDefinition of definition node * int * int * parent_type * parent_type
  | ReorderDefinition of definition node * int * int * int * parent_type
  | InsertStatement of statement node * int * int * parent_type
  | MoveStatement of statement node * int * int * parent_type * parent_type
  | ReorderStatement of statement node * int * int * int * parent_type
  | InsertExpression of expression node * int * int * parent_type
  | MoveExpression of expression node * int * int * parent_type * parent_type
  | ReorderExpression of expression node * int * int * int * parent_type

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
  | NewTreeNode(tn,num) -> pprintf "Insert tree_node %s at %d\n" (Pretty.sprint ~width:80 (d_tree_node () tn)) num
  | ReorderTreeNode(tn,num1,num2) -> pprintf "Reorder treenode %s from %d to %d\n"  (Pretty.sprint ~width:80 (d_tree_node () tn)) num1 num2
  | InsertDefinition(def,num1,num2,ptyp) -> 
	pprintf "Insert new definition %s to parent %d, position %d, type %s\n" 
	  (Pretty.sprint ~width:80 (d_def () def)) num1 num2 (ptyp_str ptyp)
  | MoveDefinition(def,num1,num2,ptyp1,ptyp2) ->
	pprintf "Move definition %s to parent %d, position %d, from type %s to type %s\n"
	  (Pretty.sprint ~width:80 (d_def () def)) num1 num2 (ptyp_str ptyp1) (ptyp_str ptyp2)
  | ReorderDefinition(def,num1,num2,num3,ptyp) ->
	pprintf "Reorder definition %s at parent %d, from position %d to position %d, type %s\n"
	  (Pretty.sprint ~width:80 (d_def () def))  num1 num2 num3 (ptyp_str ptyp)
  | InsertStatement(stmt,num1,num2,ptyp) ->
	pprintf "Insert new statement %s to parent %d, position %d, type %s\n" 
	  (Pretty.sprint ~width:80 (d_stmt () stmt)) num1 num2 (ptyp_str ptyp)
  | MoveStatement(stmt,num1,num2,ptyp1,ptyp2) ->
	pprintf "Move statement %s to parent %d, position %d, from type %s to type %s\n"
	  (Pretty.sprint ~width:80 (d_stmt () stmt))  num1 num2 (ptyp_str ptyp1) (ptyp_str ptyp2)
  | ReorderStatement(stmt,num1,num2,num3,ptyp) ->
	pprintf "Reorder statement %s at parent %d, from position %d to position %d, type %s\n"
	  (Pretty.sprint ~width:80 (d_stmt () stmt))  num1 num2 num3 (ptyp_str ptyp)
  | InsertExpression(exp,num1,num2,ptyp) ->
	pprintf "Insert new expression %s to parent %d, position %d, type %s\n" 
	  (Pretty.sprint ~width:80 (d_exp () exp)) num1 num2 (ptyp_str ptyp)
  | MoveExpression(exp,num1,num2,ptyp1,ptyp2) ->
	pprintf "Move expression %s to parent %d, position %d, from type %s to type %s\n"
	  (Pretty.sprint ~width:80 (d_exp () exp)) num1 num2 (ptyp_str ptyp1) (ptyp_str ptyp2)
  | ReorderExpression(exp,num1,num2,num3,ptyp) ->
	pprintf "Reorder expression %s at parent %d, from position %d to position %d, type %s\n"
	  (Pretty.sprint ~width:80 (d_exp () exp)) num1 num2 num3 (ptyp_str ptyp)


class markVisited ht = object(self)
  inherit nopCabsVisitor

  val ht = ht 

  method vexpr exp = hadd ht exp.id (); DoChildren

  method vstmt stmt = hadd ht stmt.id (); DoChildren

  method vdef def = hadd ht def.id (); DoChildren

  method vtreenode tn = hadd ht tn.id (); DoChildren

end

class genDiffTraversal t1 t2 mapping parents1 parents2 = object(self)
  inherit levelOrderTraversal

  val t1 = t1
  val t2 = t2
  val m = mapping
  val parents1 = parents1
  val parents2 = parents2
  val s = ref []
  val handled_ht = hcreate 10

  method get_script () = lrev (!s)

  method private parent_of_t1 x = hfind parents1 x
  method private parent_of_t2 x = hfind parents2 x
  method private handled x = hmem handled_ht x

  method mapping_tn tn () =
	if not (self#handled tn.id) then begin
	  hadd handled_ht tn.id ();
	  let handled = new markVisited handled_ht in
	  if not (in_map_range m tn.id) then begin
		let _,xposition,_ = self#parent_of_t2 tn.id in
		  ignore(visitTreeNode handled tn);
		  s := (NewTreeNode(tn,xposition)) :: !s
	  end
	  else begin
		let Some(x) = node_that_maps_to m tn.id in
		   let _,xposition,_ = self#parent_of_t1 x in
		   let _,yposition,_ = self#parent_of_t2 tn.id in 
			 if xposition <> yposition then begin
			   ignore(visitTreeNode handled tn);
			   s := (ReorderTreeNode(tn,xposition,yposition)) :: !s
			 end
	  end
	end

  method mapping_def def ()  = 
	if not (self#handled def.id) then begin
	  let handled = new markVisited handled_ht in
	  hadd handled_ht def.id ();
	  if not (in_map_range m def.id) then begin
		let yparent,yposition,ytype = self#parent_of_t2 def.id in 
		let insert_parent = 
		  match (node_that_maps_to m yparent) with
		  | Some(xx) -> xx 
		  | None     -> yparent 
		in
		  ignore(visitCabsDefinition handled def);
		  s := (InsertDefinition(def,insert_parent,yposition,ytype)) :: !s
	  end else begin
		match (node_that_maps_to m def.id) with
		| None -> failwith "generate_script: error: no node that maps to this def\n" 
		| Some(x) -> 
		  let xparent,xposition,xtype = self#parent_of_t1 x in
		  let yparent,yposition,ytype = self#parent_of_t2 def.id in 
			if not (mapsto m xparent yparent) then begin
			  let move_parent = 
				match (node_that_maps_to m yparent) with
				  Some(xx) -> xx
				| None -> yparent 
			  in
				ignore(visitCabsDefinition handled def);
				s := (MoveDefinition(def,move_parent,yposition,xtype,ytype)) :: !s
			end else if xposition <> yposition then begin
			  ignore(visitCabsDefinition handled def);
			  s := (ReorderDefinition(def,xparent,xposition,yposition,ytype)) :: !s
			end
	  end
	end
  method mapping_stmt stmt () =
	if not (self#handled stmt.id) then begin
	  hadd handled_ht stmt.id ();
	  let handled = new markVisited handled_ht in
	  if not (in_map_range m stmt.id) then begin
		let yparent,yposition,ytype = self#parent_of_t2 stmt.id in 
		let insert_parent,typ = 
		  match (node_that_maps_to m yparent) with
		  | Some(xx) -> xx,ytype (* FIXME: double-check this *)
		  | None     -> yparent,ytype
		in
		let handled = new markVisited handled_ht in
		  ignore(visitCabsStatement handled stmt);
		  pprintf "Inserting statement\n"; flush stdout;
		  s := (InsertStatement(stmt,insert_parent,yposition,typ)) :: !s
	  end
	  else begin
		match (node_that_maps_to m stmt.id) with
		| None -> failwith "generate_script: error: no node that maps to this stmt!\n" 
		| Some(x) -> 
		  let xparent,xposition,xtype = self#parent_of_t1 x in
		  let yparent,yposition,ytype = self#parent_of_t2 stmt.id in 
			if not (mapsto m xparent yparent) then begin
			  let move_parent = 
				match (node_that_maps_to m yparent) with
				| Some(xx) -> xx
				| None     -> yparent 
			  in
				ignore(visitCabsStatement handled stmt);
				s := (MoveStatement(stmt,move_parent,yposition,xtype,ytype)) :: !s
			end else if xposition <> yposition then begin
			  ignore(visitCabsStatement handled stmt);
			  s := (ReorderStatement(stmt,xparent,xposition,yposition,ytype)) :: !s
			end
	  end
	end

  method mapping_exp exp () =
	if not (self#handled exp.id) then begin
	  hadd handled_ht exp.id (); 
	  let handled = new markVisited handled_ht in
	  if not (in_map_range m exp.id) then begin
		let yparent,yposition,ytype = self#parent_of_t2 exp.id in 
		let insert_parent,typ = 
		  match (node_that_maps_to m yparent) with
		  | Some(xx) -> xx,ytype (* FIXME double_check this *)
		  | None     -> yparent,ytype
		in
		  ignore(visitCabsExpression handled exp);
		  s := (InsertExpression(exp,insert_parent,yposition,typ)) :: !s
	  end else begin
		match (node_that_maps_to m exp.id) with
		| None -> failwith "generate_script: error: no node that maps to this expression!\n" 
		| Some(x) -> 
		  let xparent,xposition,xtype = self#parent_of_t1 x in
		  let yparent,yposition,ytype = self#parent_of_t2 exp.id in 
			pprintf "Handling exp %d, maps to: %d, str: %s\n" exp.id x (Pretty.sprint ~width:80 (d_exp () exp));
			pprintf "xparent: %d, yparent: %d\n" xparent yparent;
			if not (mapsto m xparent yparent) then begin
			  let move_parent = 
				match (node_that_maps_to m yparent) with
				| Some(xx) -> xx
				| None     -> yparent 
			  in (* Is it possible to recognize insertion of entire trees, to avoid the usual stupidity? *)
				ignore(visitCabsExpression handled exp);
				s := (MoveExpression(exp,move_parent,yposition,xtype,ytype)) :: !s
			end else if xposition <> yposition then begin
			  ignore(visitCabsExpression handled exp);
			  s := (ReorderExpression(exp,xparent,xposition,yposition,ytype)) :: !s
			end
	  end
	end

end
  
let new_tree_to_diff_tree tree  =
  let coerce1 v = (v : dumifyVisitor :> cabsVisitor) in
  let coerce2 v = (v : typelabelVisitor :> cabsVisitor) in
  let myDum = new dumifyVisitor() in
  let tree_copy = copy tree in
	ignore(visitTree (coerce1 myDum) tree_copy);
	let tl_info = myDum#get_tl_info() in
	let myTl = new typelabelVisitor tl_info in
	let tree = visitTree (coerce2 myTl) tree in
	let tl_ht,node_info = myTl#get_hts() in
	  pprintf "TLinfo: \n"; 
(*	  hiter (fun k -> fun v -> pprintf "%s -> %d\n" k v) tl_ht;*)
	  hiter (fun n -> fun exp -> pprintf "%d -> EXP: %s\n" n (Pretty.sprint ~width:80 (d_exp () exp))) node_info.exp_ht;
	  hiter (fun n -> fun exp -> pprintf "%d -> STMT: %s\n" n (Pretty.sprint ~width:80 (d_stmt () exp))) node_info.stmt_ht;
	  hiter (fun n -> fun exp -> pprintf "%d -> DEF: %s\n" n (Pretty.sprint ~width:80 (d_def () exp))) node_info.def_ht;
	  hiter (fun n -> fun exp -> pprintf "%d -> TN: %s\n" n (Pretty.sprint ~width:80 (d_tree_node () exp))) node_info.tn_ht;
	  tree,myTl#get_hts ()

let new_gen_diff t1 t2 = 
  pprintf "Tree 1:\n";
  let t1,(tl_ht1,node_info1) = new_tree_to_diff_tree t1 in
  pprintf "Tree 2:\n";
  let t2,(tl_ht2,node_info2) = new_tree_to_diff_tree t2 in
  pprintf "Done making diff trees\n";
  let parents1 = new getParentsWalker in
	parents1#walkTree t1;
	let info1 = parents1#get_info() in
	let parents2 = new getParentsWalker in
	  parents2#walkTree t2;
	  let info2 = parents2#get_info() in
	  let map_traversal = new mappingTraversal tl_ht2 node_info2 in
		map_traversal#traverse t1; 
		let map = map_traversal#get_map() in
		  pprintf "Map: \n"; 
			Map.iter
			  (fun id1 -> fun id2 -> pprintf "%d -> %d\n" id1 id2) map;
			flush stdout;
			pprintf "Making a gendiff traversal\n"; flush stdout;
		let gendiff = new genDiffTraversal t1 t2 map info1 info2 in
		  pprintf "Traversing!\n"; flush stdout;
		  gendiff#traverse t2;
		  pprintf "Done traversing.\n"; flush stdout;
		  let script = gendiff#get_script() in
			liter print_edit script

let in_map_domain m t =
  try 
    NodeMap.iter (fun (a,_) -> 
      if a == t then raise Found_It
    ) m ;
    false
  with Found_It -> true 

(* returns true if (_,t) is in m *) 
let in_map_range m t =
  try 
    NodeMap.iter (fun (_,a) -> 
      if a == t then raise Found_It
    ) m ;
    false
  with Found_It -> true 

let deleted_node = {
  nid = -1;
  children = [| |] ;
  typelabelF = -1 ;
  tl_strF = "deleted";
  tl_node = DELETED ;
  original_node = DELETED;
} 

let rec cleanup_tree t =
  Array.iter (fun child ->
    cleanup_tree child
  ) t.children; 
  let lst = Array.to_list t.children in
  let lst = List.filter (fun child ->
    child.typelabelF <> -1
  ) lst in
  t.children <- Array.of_list lst 

let delete node =
  node.nid <- -1 ; 
  node.children <- [| |] ; 
  node.typelabelF <- -1 


let find_node_that_maps_to m y =
  try 
    NodeMap.iter (fun (a,b) -> 
      if b.nid = y.nid then raise (Found_Node(a))
    ) m ;
    None
  with Found_Node(a) -> Some(a)  


(* return a set containing all nodes in t equal to n *) 
let rec nodes_in_tree_equal_to t n = 
  let sofar = ref 
    (if nodes_eqF t n then NodeSet.singleton t else NodeSet.empty)
  in 
  Array.iter (fun child ->
    sofar := NodeSet.union !sofar (nodes_in_tree_equal_to child n) 
  ) t.children ; 
  !sofar 

let map_size m = NodeMap.cardinal m 

let level_order_traversal t callback =
  let q = Queue.create () in 
  Queue.add t q ; 
  while not (Queue.is_empty q) do
    let x = Queue.take q in 
    Array.iter (fun child ->
      Queue.add child q
    ) x.children ; 
    callback x ; 
  done 

let parent_of tree some_node =
  try 
    level_order_traversal tree (fun p ->
      Array.iter (fun child ->
        if child.nid = some_node.nid then
          raise (Found_Node(p) )
      ) p.children 
    ) ;
    None
  with Found_Node(n) -> Some(n) 

let parent_of_nid tree some_nid =
  try 
    level_order_traversal tree (fun p ->
      Array.iter (fun child ->
        if child.nid = some_nid then
          raise (Found_Node(p) )
      ) p.children 
    ) ;
    None
  with Found_Node(n) -> Some(n) 

let node_in_tree tree some_nid = 
  try
	level_order_traversal tree ( fun p ->
	  if p.nid == some_nid then raise (Found_Node(p))
	) ; false
  with Found_Node(n) -> true

let position_of (parent : diff_tree_node option) child =
  match parent with
  | None -> None
  | Some(parent) -> 
    let result = ref None in 
    Array.iteri (fun i child' ->
      if child.nid = child'.nid then
        result := Some(i) 
    ) parent.children ;
    !result 

let position_of_nid (parent : diff_tree_node option) child_nid =
  match parent with
  | None -> None
  | Some(parent) -> 
    let result = ref None in 
    Array.iteri (fun i child' ->
      if child_nid = child'.nid then
        result := Some(i) 
    ) parent.children ;
    !result 

(* This is the DiffX algorithm, taken verbatim from their paper *) 
let rec mapping t1 t2 =
  let m = ref NodeMap.empty in 
  level_order_traversal t1 (fun x -> 
	if in_map_domain !m x then 
      () (* skip current node *)
    else begin
      let y = nodes_in_tree_equal_to t2 x in 
      let m'' = ref NodeMap.empty in 
      NodeSet.iter (fun yi ->
        if not (in_map_range !m yi) then begin
          let m' = ref NodeMap.empty in 
          match_fragment x yi !m m' ;
          if map_size !m' > map_size !m'' then begin
            m'' := !m'
          end 
        end 
      ) y ;
      m := NodeMap.union !m !m'' 
    end 
  ) ;
  !m 

(* still taken verbatim from their paper *) 
and match_fragment x y (m : NodeMap.t) (m' : NodeMap.t ref) = 
  if (not (in_map_domain m x)) &&
     (not (in_map_range m y)) &&
     (nodes_eqF x y) then begin
    m' := NodeMap.add (x,y) !m' ;
    let xc = Array.length x.children in 
    let yc = Array.length y.children in 
    for i = 0 to pred (min xc yc) do
      match_fragment x.children.(i) y.children.(i) m m'
    done 
  end 
(* This algorithm is not taken directly from their paper, because the
 * version in their paper has bugs! *) 

let generate_script t1 t2 m = 
  let s = ref [] in 
	level_order_traversal t2 
	  (fun y -> 
		 if not (in_map_range m y) then begin
		   let yparent = parent_of t2 y in 
		   let ypos = position_of yparent y in
			 match yparent with
			 | None -> 
				 s := (Insert(y.nid,noio yparent,ypos)) :: !s 
			 | Some(yparent) -> begin
				 let xx = find_node_that_maps_to m yparent in
				   match xx with
				   | Some(xx) -> s := (Insert(y.nid,Some(xx.nid),ypos)) :: !s 
				   | None     -> s := (Insert(y.nid,Some(yparent.nid),ypos)) :: !s 
					   (* in the None case, our yParent was moved over, so this works
						  inductively *) 
			   end 
		 end else begin
		   match find_node_that_maps_to m y with
		   | None -> 
			   pprintf "generate_script: error: no node that maps to!\n" 
		   | Some(x) -> 
			   begin
				 let xparent = parent_of t1 x in
				 let yparent = parent_of t2 y in 
				 let yposition = position_of yparent y in 
				 let xposition = position_of xparent x in 
				   match xparent, yparent with
				   | Some(xparent), Some(yparent) -> 
					   if not (NodeMap.mem (xparent,yparent) m) then begin 
						 let xx = find_node_that_maps_to m yparent in
						   match xx with
						   | Some(xx) -> s := (Move(x.nid,Some(xx.nid),yposition)) :: !s 
						   | None     -> s := (Move(x.nid,Some yparent.nid,yposition)) :: !s
					   end else if xposition <> yposition then 
						 s := (Move(x.nid,Some xparent.nid,yposition)) :: !s
					   else () (* they're the same, don't need to be renamed *)
				   | _, _ -> () (* well, no parents implies no parents in the mapping *) 
					   (* s := (Move(x,yparent,None)) :: !s *)
			   end 
		 end 
	  ) ;
	level_order_traversal t1 
	  (fun x ->
		 if not (in_map_domain m x) then 
		   s := (Delete(x.nid)) :: !s
	  ) ;
	List.rev !s

(*************************************************************************)
(* applying a generated diff; mostly unecessary for taxonomy purposes,
 * but included for completeness/testing *)

(* Apply a single edit operation to a file. This version if very fault
 * tolerant because we're expecting our caller (= a delta-debugging script)
 * to be throwing out parts of the diff script in an effort to minimize it.
 * So this is 'best effort'. *) 

let apply_diff m ast1 ast2 s =  
    match s with

    (* delete sub-tree rooted at node x *)
    | Delete(nid) -> 
      let node = node_of_nid nid in 
      delete node 

    (* insert node x as pth child of node y *) 
    | Insert(xid,yopt,ypopt) -> begin
      let xnode = node_of_nid xid in 
      (match yopt with
      | None -> printf "apply: error: insert to root?"  
      | Some(yid) -> 
        let ynode = node_of_nid yid in 
        (* let ynode = corresponding m ynode in  *)
        let ypos = match ypopt with
        | Some(x) -> x
        | None -> 0 
        in 
        (* Step 1: remove children of X *) 
        xnode.children <- [| |] ; 

        (* Step 2: remove X from its parent *)
        let xparent1 = parent_of ast1 xnode in 
        let xparent2 = parent_of ast2 xnode in 
        (match xparent1, xparent2 with
        | Some(parent), _ 
        | _, Some(parent) -> 
          let plst = Array.to_list parent.children in
          let plst = List.map (fun child ->
            if child.nid = xid then
              deleted_node
            else
              child
          ) plst in
          parent.children <- Array.of_list plst
        | _, _ -> ()
          (* this case is fine, and typically comes up when we are
          Inserting the children of a node that itself was Inserted over *)
        ) ;

        (* Step 3: put X as p-th child of Y *) 
        let len = Array.length ynode.children in 
        let before = Array.sub ynode.children 0 ypos in
        let after  = Array.sub ynode.children ypos (len - ypos) in 
        let result = Array.concat [ before ; [| xnode |] ; after ] in 
        ynode.children <- result;
      ) 
    end 

    (* move subtree rooted at node x to as p-th child of node y *) 
    | Move(xid,yopt,ypopt) -> begin 
      let xnode = node_of_nid xid in 
      (match yopt with
      | None -> printf "apply: error: %s: move to root?\n"  
            (edit_action_to_str s) 
      | Some(yid) -> 
        let ynode = node_of_nid yid in 
        (* let ynode = corresponding m ynode in *)
        let ypos = match ypopt with
        | Some(x) -> x
        | None -> 0 
        in 
        (* Step 1: remove X from its parent *)
        let xparent1 = parent_of ast1 xnode in 
        let xparent2 = parent_of ast2 xnode in 
        (match xparent1, xparent2 with
        | Some(parent), _ 
        | _, Some(parent) -> 
          let plst = Array.to_list parent.children in
          let plst = List.map (fun child ->
            if child.nid = xid then
              deleted_node
            else
              child
          ) plst in
          parent.children <- Array.of_list plst ; 
        | None, None -> 
          printf "apply: error: %s: no x parent\n" 
            (edit_action_to_str s) 
        ) ;
        (* Step 2: put X as p-th child of Y *) 
        let len = Array.length ynode.children in 
        let before = Array.sub ynode.children 0 ypos in
        let after  = Array.sub ynode.children ypos (len - ypos) in 
        let result = Array.concat [ before ; [| xnode |] ; after ] in 
        ynode.children <- result 
      ) 
    end 


(*************************************************************************)
 (* "main" and test functions *)

	  
(* Generate a set of difference between two trees. Write the textual
 * diff script to 'diff_out', write the data files and hash tables to
 * 'data_out'. *) 

let gendiff t1 t2 ?(print=false) ?(diff_out=IO.stdnull) ?(data_out=IO.stdnull) name = 
  let data_ht = hcreate 255 in 
  let m = mapping t1 t2 in 
    if !debug_bl then begin
	  verbose := true;
	NodeMap.iter 
	  (fun (a,b) ->
		let stra = if !verbose then 
			begin
			  let node = node_of_nid a.nid in 
			  let tl = node.typelabelF in
			  let n_str = Printf.sprintf "%2d: %d " a.nid tl in
				n_str ^ node.tl_strF
			end 
		  else Printf.sprintf "%2d" a.nid
		in
		let strb = if !verbose then 
			begin
			  let node = node_of_nid b.nid in 
			  let tl = node.typelabelF in
			  let n_str = Printf.sprintf "%2d: %d " b.nid tl in
				n_str ^ node.tl_strF
			end 
		  else Printf.sprintf "%2d" b.nid
		in
		  printf "diff: \t\t %s %s\n" stra strb
	  ) m ;
	printf "Diff: \ttree t1\n" ; 
	print_tree t1 ; 
	printf "Diff: \ttree t2\n" ; 
	print_tree t2 ; 
	printf "diff: \tgenerating script\n" ; flush stdout ; 
    end;
	let s = generate_script t1 t2 m in 
	  hadd data_ht name (m,t1,t2) ; 
	  if !debug_bl then begin
		printf "diff: \tscript: %d\n" (llen s) ; flush stdout ; 
		liter (fun ea ->
		  fprintf diff_out "%s %s\n" name (edit_action_to_str ea) ;
		  printf "Script: %s %s\n" name (edit_action_to_str ea)
		) s  ;
		Marshal.to_channel data_out data_ht [] ; 
		Marshal.to_channel data_out node_id_to_diff_tree_node [] ; 
	  end;
	  s

(* Apply a (partial) diff script. *) 
let usediff name diff_in data_in file_out = 
  let data_ht = Marshal.from_channel data_in in 
  let copy_ht local global = 
    hiter (fun a b -> hadd global a b) local
  in
	let node_id_to_diff_tree_node' = Marshal.from_channel data_in in 
	  copy_ht node_id_to_diff_tree_node' node_id_to_diff_tree_node ; 

	  let patch_ht = Hashtbl.create 255 in
	  let add_patch fname ea = (* preserves order, fwiw *) 
		let sofar = try Hashtbl.find patch_ht fname with _ -> [] in
		  Hashtbl.replace patch_ht fname (sofar @ [ea]) 
	  in 

	  let num_to_io x = if x < 0 then None else Some(x) in 
		(try while true do
		   let line = input_line diff_in in
			 Scanf.sscanf line "%s %s (%d,%d,%d)" 
			   (fun fname ea a b c -> 
				  let it = match String.lowercase ea with 
					| "insert" -> Insert(a, num_to_io b, num_to_io c) 
					| "move" ->   Move(a, num_to_io b, num_to_io c)
					| "delete" -> Delete(a) 
					| _ -> failwith ("invalid patch: " ^ line)
				  in add_patch fname it 
			   ) 
		 done with End_of_file -> ()
		) ; 

		let patches = try Hashtbl.find patch_ht name with _ -> [] in
		  pprintf "Patches length: %d\n" (llen patches); flush stdout;
		  if patches <> [] then begin
			let m, t1, t2 = Hashtbl.find data_ht name in 
			  printf "/* Tree t1:\n" ; 
			  print_tree t1; 
			  printf "*/\n" ; 
			  printf "/* Tree t2:\n" ; 
			  print_tree t2; 
			  printf "*/\n" ; 
			  verbose := true;
			  List.iter (fun ea ->
						   printf "// %s\n" ( edit_action_to_str ea ) ; 
						   apply_diff m t1 t2 ea
						) patches ; 
			  verbose := false;
			  cleanup_tree t1 ; 
			  print_diffed_tree t1
		  end else pprintf "No patch found for this tree pair, skipping\n"

let tree_diff_cabs  old_file_tree new_file_tree diff_name = 
  let old_file_tree = process_tree old_file_tree in
  let new_file_tree = process_tree new_file_tree in
  let f1 =  ((diff_name^"1"), old_file_tree) in
  let f2 =  ((diff_name^"2"), new_file_tree) in 
  let t1 = tree_to_diff_tree f1 in
  let t2 = tree_to_diff_tree f2 in
  let m = mapping t1 t2 in
    if !debug_bl then begin
	  verbose := true;
	  printf "Diff: \ttree t1\n" ; 
	  print_tree t1 ; 
	  printf "Diff: \ttree t2\n" ; 
	  print_tree t2 ; 
	  NodeMap.iter 
		(fun (a,b) ->
		  let stra = if !verbose then 
			  begin
				let node = node_of_nid a.nid in 
				let tl = node.typelabelF in
				let n_str = Printf.sprintf "%2d: %d " a.nid tl in
				  n_str ^ node.tl_strF
			  end 
			else Printf.sprintf "%2d" a.nid
		  in
		  let strb = if !verbose then 
			  begin
				let node = node_of_nid b.nid in 
				let tl = node.typelabelF in
				let n_str = Printf.sprintf "%2d: %d " b.nid tl in
				  n_str ^ node.tl_strF
			  end 
			else Printf.sprintf "%2d" b.nid
		  in
			printf "diff: \t\t %s %s\n" stra strb
		) m ;
    end;
	
  let diff = gendiff t1 t2 diff_name in
  let diff' = standardize_diff diff in
(*  let alpha = alpha_rename diff' in*)
  let alpha = diff' in
    if !debug_bl then begin
	verbose := true;
	pprintf "Standard diff: \n";
	print_standard_diff diff';
	pprintf "Alpha-renamed diff: \n";
	print_standard_diff alpha;
	flush stdout
    end;
	t1,diff', alpha

let tree_diff_change f1 f2 name = 
  let t1 = change_to_diff_tree f1 in 
  let t2 = change_to_diff_tree f2 in
  let diff = gendiff t1 t2 name in
  let diff' = standardize_diff diff in
  let alpha = alpha_rename diff' in
    if !debug_bl then begin
	pprintf "Standard diff: \n";
	print_standard_diff diff';
	pprintf "Alpha-renamed diff: \n";
	print_standard_diff alpha;
	flush stdout;
    end;
	diff', alpha
	
let apply name =
  let data_in = open_in_bin name in 
  let diff_in = open_in (name ^ ".diff") in
  let file_out = stdout in 
	usediff name diff_in data_in file_out 

  

(*************************************************************************)
(* functions called from the outside to generate the diffs we
 * ultimately care about, as well as testing drivers.  *)

(* diff_name is string uniquely IDing this diff *)
let test_new_mapping files = 
  pprintf "Test template!\n"; flush stdout;
  let syntactic = 
	lmap
	  (fun file -> 
		let strs = File.lines_of file in 
		let oldf,newf = 
		  Enum.fold
			(fun (olds,news) ->
			  fun str ->
				if Str.string_match plus_regexp str 0 then
				  olds,(news@ [String.lchop str])
				else if Str.string_match minus_regexp str 0 then
				  (olds@ [(String.lchop str)]),news
				else (olds@[str]),(news@[str])
			) ([],[]) strs
		in
		let all_comment_old,unbalanced_beginnings_old,unbalanced_ends_old = check_comments oldf in
		let all_comment_new, unbalanced_beginnings_new,unbalanced_ends_new = check_comments newf in
		let oldf'' = 
		  if unbalanced_beginnings_old > 0 || all_comment_old then oldf @ ["*/"] else oldf in
		let newf'' = 
		  if unbalanced_beginnings_new > 0 || all_comment_new then newf @ ["*/"] else newf in
		let oldf''' = 
		  if unbalanced_ends_old > 0 || all_comment_old then "/*" :: oldf'' else oldf'' in
		let newf''' = 
		  if unbalanced_ends_new > 0 || all_comment_new then "/*" :: newf'' else newf'' in
	   let foldstrs strs = lfoldl (fun accum -> fun str -> accum^"\n"^str) "" strs in
		 (foldstrs oldf'''),(foldstrs newf'''))
	  files
  in
	liter
	  (fun (diff1,diff2) ->
		let old_file_tree,new_file_tree = 
		  process_tree (fst (Diffparse.parse_from_string diff1)), process_tree (fst (Diffparse.parse_from_string diff2)) in
		  pprintf "dumping parsed cabs1: ";
		  dumpTree defaultCabsPrinter Pervasives.stdout ("",old_file_tree);
		  pprintf "end dumped to stdout\n"; flush stdout;
		  pprintf "dumping parsed cabs2: ";
		  dumpTree defaultCabsPrinter Pervasives.stdout ("",new_file_tree);
		  pprintf "end dumped to stdout\n"; flush stdout;
		  new_gen_diff (diff1,old_file_tree)  (diff2,new_file_tree)
(*		  let t1,(t1_tl_ht,t1_node_info) = new_tree_to_diff_tree  in
		  pprintf "New tree 2:\n"; flush stdout;
		  let t2,(t2_tl_ht,t2_node_info) = new_tree_to_diff_tree in
		  let mapping = tree_mapping t1 t2_tl_ht t2_node_info in
			Map.iter
			  (fun id1 -> fun id2 -> pprintf "%d -> %d\n" id1 id2) mapping*)
	  ) syntactic

let test_diff_cabs files =
  let diff1 = List.hd files in
  let diff2 = List.hd (List.tl files) in
  let old_file_tree, new_file_tree =
	 fst (Diffparse.parse_file diff1), fst (Diffparse.parse_file diff2) in
	Printf.printf "tree1:\n";
	dumpTree defaultCabsPrinter (Pervasives.stdout) (diff1, old_file_tree);
	Printf.printf "\ntree2:\n";
	dumpTree defaultCabsPrinter (Pervasives.stdout) (diff2, new_file_tree);
	Printf.printf "\n\n"; flush stdout;
	pprintf "Generating a diff:\n";
	let tree,patch,_ = tree_diff_cabs old_file_tree new_file_tree "test_generate" in 
		pprintf "Printing standardized patch:\n";
	  verbose := true;
		print_standard_diff patch; 
(*	pprintf "\n\nTesting, using the diff:\n";
	apply "test_generate";*)
		pprintf "diff use testing turned off for brokenness\n"; flush stdout;
	pprintf "\n\n Done in test_diff\n\n"; flush stdout

let test_diff_change files =
  (*  FIXME: this test function explicitly throws away the alpha-renamed tree *)
  pprintf "Testing diffs on changes.  Step 1: parse files\n"; flush stdout;
  let parsed = lmap 
	(fun file -> pprintf "Parsing: %s\n" file; flush stdout; 
	  let parsed = fst (Diffparse.parse_file file) in
		pprintf "dumping parsed cabs: ";
		dumpTree defaultCabsPrinter Pervasives.stdout (file,parsed);
		pprintf "end dumped to stdout\n"; flush stdout;
		(file, parsed))
	files 
  in
  let rec cabs_diff_pairs = function
      (f1,hd1)::(f2,hd2)::tl -> pprintf "Diffing cabs for %s with %s\n" f1 f2; flush stdout;
		let _,diff,_ = tree_diff_cabs hd1 hd2 "test_diff_change" in
		let restdiff = cabs_diff_pairs tl in
		  diff :: restdiff
	| [(f2,hd2)] -> pprintf "Warning: odd-length snippet list in test_diff_change: %s\n" f2; flush stdout; []
	| [] -> []
  in
	pprintf "Step 2: diff pairs of files\n"; flush stdout;
  let diffs = cabs_diff_pairs parsed in 
	pprintf "Step 2a: printing diffs from pairs of files\n"; flush stdout;
	verbose := true;
	liter (fun x -> pprintf "A DIFF:\n\n"; print_standard_diff x; pprintf "END A DIFF\n\n"; flush stdout) diffs; flush stdout;
	verbose := false;
  let rec diff_diff_pairs = function 
      hd1::hd2::tl -> (fst (tree_diff_change hd1 hd2 "test_diff_change")) :: diff_diff_pairs tl
	| [hd2] -> pprintf "Warning: odd-length diff list in test_diff_change\n"; flush stdout; []
	| [] -> []
  in
	pprintf "Step 3: diff pairs of diffs\n"; flush stdout;
  let diff_diffs = diff_diff_pairs diffs in 
	verbose := true;
	pprintf "Step 4: printing diff diffs\n"; flush stdout;
	liter (fun x -> pprintf "A DIFF:\n\n"; print_standard_diff x; pprintf "END A DIFF\n\n"; flush stdout) diff_diffs;
	pprintf "Done testing change diffing\n"; flush stdout
