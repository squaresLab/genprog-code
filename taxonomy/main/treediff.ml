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

(*************************************************************************)

(* XDiff algorithm: mostly taken from cdiff/the original paper, except
 * where Wes modified it to fix their bugs *)

exception Found_It 
exception Found_Node of diff_tree_node 

let typelabel (tlabel : string) : int =
  ht_find typelabel_ht tlabel (fun _ -> post_incr typelabel_counter)

let exp_tl_ht = hcreate 10
let stmt_tl_ht = hcreate 10
let def_tl_ht = hcreate 10
let tn_tl_ht = hcreate 10

class dumifyVisitor = object(self)
  inherit nopCabsVisitor

  method vexpr exp = 
	ChangeDoChildrenPost(exp,(fun exp -> hadd exp_tl_ht exp.id exp; dummyExp))

  method vstmt stmt = 
	(*ChangeDoChildrenPost(stmt,(fun stmt -> hadd stmt_tl_ht stmt.id stmt; dummyStmt)) FIXME*) DoChildren
	  
  method vdef def = DoChildren
(*	ChangeDoChildrenPost(def,(fun def -> hadd def_tl_ht def.id def; dummyDef)) FIXME *)

  method vblock b = ChangeDoChildrenPost(b,(fun b -> dummyBlock))

  method vtreenode tn =
	let dum = 
	  match tn.node with 
	  | Globals(dlist) -> Globals([])
	  | Stmts(slist) -> Stmts([])
	  | Exps(elist) -> Exps([])
	  | Syntax(str) -> Syntax(str)
	in
	  ChangeDoChildrenPost(tn,(fun tn -> tn.node <- dum; hadd tn_tl_ht tn.id tn; tn))

end

let getinfo node printer ht tl_ht =
  let tl = hfind ht node.id in
  let str = printer tl in
  let tlint = typelabel str in
  let old_tl = 
	if hmem tl_ht tlint then hfind tl_ht tlint else [] in
	hrep tl_ht tlint (node.id :: old_tl);
	node.typelabel <- tlint;
	node.tl_str <- str;
	DoChildren

let typelabel_ht = hcreate 10 

class typelabelVisitor () = 
  let _ = hclear typelabel_ht in
  object(self)
  inherit nopCabsVisitor

  method vexpr exp = 
	getinfo exp (fun exp -> Pretty.sprint ~width:80 (d_exp () exp)) exp_tl_ht typelabel_ht

  method vstmt stmt = 
	getinfo stmt (fun stmt -> Pretty.sprint ~width:80 (d_stmt () stmt)) stmt_tl_ht typelabel_ht

  method vdef def = 
	getinfo def (fun def -> Pretty.sprint ~width:80 (d_def () def)) def_tl_ht typelabel_ht

  method vtreenode tn = 
	getinfo tn (fun tn -> Pretty.sprint ~width:80 (d_tree_node () tn)) tn_tl_ht typelabel_ht

end

let new_tree_to_diff_tree tree =
  let myDum = new dumifyVisitor in
  let myTl = new typelabelVisitor () in
  let tree_copy = copy tree in
	ignore(visitTree myDum tree_copy);
	let tree = visitTree myTl tree in
	tree,typelabel_ht

(* returns true if (t,_) is in m *) 
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

type pair_type = Pair of (unit -> unit) * (unit -> pair_type list) | Unit

let tree_mapping t1 t2 = 
  let in_map_domain m t = Map.mem t m in
  let in_map_range m t = Map.exists_f (fun k -> fun v -> v == t) m in
  let map_size m = Enum.count (Map.enum m) in
  let match_frag_list list1 list2 matchfun m m' = 
	let array1 = Array.of_list list1 in 
	let array2 = Array.of_list list2 in 
	let xc = Array.length array1 in 
	let yc = Array.length array2 in 
	  for i = 0 to pred (min xc yc) do
		matchfun array1.(i) array2.(i) m m'
	  done
  in
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
  in
  let m = ref Map.empty in
  let nodes_in_tree_equal_to_tn t tn = failwith "Not implemented" in
  let nodes_in_tree_equal_to_def t tn = failwith "Not implemented" in
  let nodes_in_tree_equal_to_stmt t tn = failwith "Not implemented" in
  let nodes_in_tree_equal_to_exp t tn = failwith "Not implemented" in
  let mapping_tn tn () = 
	if in_map_domain !m tn.id then () else
	  begin
		let y = nodes_in_tree_equal_to_tn t2 tn in
		let m'' = ref Map.empty in 
		  Set.iter
			 (fun yi ->
			   if not (in_map_range !m yi.id) then begin
				 let m' = ref Map.empty in 
				   match_fragment_tn tn yi !m m' ;
				   if map_size !m' > map_size !m'' then begin
					 m'' := !m'
				   end 
			   end 
			 ) y ;
		  m := Map.union !m !m'' 
	  end
  in
  let mapping_def def () = 
	if in_map_domain !m def.id then () else
	  begin
		let y = nodes_in_tree_equal_to_def t2 def in
		let m'' = ref Map.empty in 
		  Set.iter
			 (fun (yi : definition node) ->
			   if not (in_map_range !m yi.id) then begin
				 let m' = ref Map.empty in 
				   match_fragment_def def yi !m m' ;
				   if map_size !m' > map_size !m'' then begin
					 m'' := !m'
				   end 
			   end 
			 ) y ;
		  m := Map.union !m !m'' 
	  end
  in
  let mapping_stmt stmt () = 
	if in_map_domain !m stmt.id then () else
	  begin
		let y = nodes_in_tree_equal_to_stmt t2 stmt in
		let m'' = ref Map.empty in 
		  Set.iter
			 (fun yi ->
			   if not (in_map_range !m yi.id) then begin
				 let m' = ref Map.empty in 
				   match_fragment_stmt stmt yi !m m' ;
				   if map_size !m' > map_size !m'' then begin
					 m'' := !m'
				   end 
			   end 
			 ) y ;
		  m := Map.union !m !m'' 
	  end
  in
  let mapping_exp exp () = 
	if in_map_domain !m exp.id then () else
	  begin
		let y = nodes_in_tree_equal_to_exp t2 exp in
		let m'' = ref Map.empty in 
		  Set.iter
			 (fun yi ->
			   if not (in_map_range !m yi.id) then begin
				 let m' = ref Map.empty in 
				   match_fragment_exp exp yi !m m' ;
				   if map_size !m' > map_size !m'' then begin
					 m'' := !m'
				   end 
			   end 
			 ) y ;
		  m := Map.union !m !m'' 
	  end
  in
  let nothing_fun = fun () -> () in
  let mnoth children ele = Pair(nothing_fun,children ele) in
  let mfun mapping children ele = Pair(mapping ele,children ele) in
  let rec mfundef def = mfun mapping_def children_def def
  and mfunstmt stmt = mfun mapping_stmt children_stmt stmt
  and mfunexp exp = mfun mapping_exp children_exp exp
  and children_tn tn () =
	match tn.node with
	  Globals(dlist) -> lmap mfundef dlist
	| Stmts(slist) -> lmap mfunstmt slist
	| Exps(elist) -> lmap mfunexp elist
	| Syntax(_) -> []
  and children_def def () : pair_type list = 
	match def.node with 
	  FUNDEF(sn,b,_,_) -> [mnoth children_sn sn; mnoth children_block b]
	| DECDEF(ing,_) -> [mnoth children_ing ing]
	| TYPEDEF(ng,_) -> [mnoth children_ng ng]
	| ONLYTYPEDEF(spec,_) -> lmap (mnoth children_spec_elem) spec
	| PRAGMA(exp,_) -> [mfunexp exp]
	| LINKAGE(_,_,dlist) ->	lmap mfundef dlist
	| _ -> []
  and children_stmt stmt () =
	match stmt.node with
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
	match exp.node with
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
  and children_tree () = 
	let _,tns = t1 in 
	  lmap (fun tn -> mfun mapping_tn children_tn tn) tns
  in
  let q = Queue.create () in 
	Queue.add (Pair(nothing_fun,children_tree)) q ;
	while not (Queue.is_empty q) do
	  match Queue.take q with
		Pair(mapping_x,children_x) ->
		  liter (fun child -> Queue.add child q) (children_x());
		  mapping_x ()
	  | Unit -> ()
	done

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
