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

let literi = List.iteri
let lmapi = List.mapi

let check_comments strs = 
  lfoldl
	(fun (all_comment, unbalanced_beginnings,unbalanced_ends) ->
	   fun (diffstr : string) ->
		 let matches_comment_line = Str.string_match star_regexp diffstr 0 in
		 let matches_end_comment = does_match end_comment_regexp diffstr in
		 let matches_start_comment = does_match start_comment_regexp diffstr in
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

let in_map_domain m t = Map.exists_f (fun (k,_) -> fun (v,_) -> k == t) m
let in_map_range m t = Map.exists_f (fun (k,_) -> fun (v,_) -> v == t) m 
let map_size m = Enum.count (Map.enum m) 

let match_array list1 list2 matchfun m m' = 
  let array1 = Array.of_list list1 in 
  let array2 = Array.of_list list2 in 
  let xc = Array.length array1 in 
  let yc = Array.length array2 in 
  let res = ref m' in
	for i = 0 to pred (min xc yc) do
	  res := matchfun array1.(i) array2.(i) m !res
	done; !res

let match_list list1 list2 matchfun m m' = 
  let lst = ref list2 in 
  let nodes_in_lst_equal_to node =
	let sofar = ref [] in
	let lst' = ref [] in
	  liter (fun n -> if nodes_eq n node then sofar := n :: !sofar else lst' := n :: !lst') !lst;
	  lst := !lst'; !sofar 
  in
	lfoldl
	  (fun m' ->
		fun xi ->
		  let y = nodes_in_lst_equal_to xi in
		  let m'' = 
			lfoldl
			  (fun m'' ->
				fun yi ->
				  if not (in_map_range m' yi.id) then begin
					let m''' = matchfun xi yi m' (Map.empty) in
					  if map_size m'' > map_size m''' then
						m'' else m'''
				  end else m''
			  ) (Map.empty) y 
		  in
			Map.union m' m'') m' list1

(* OK: the first map (m) is for reference: what's been matched so far.  The
   second map (m') is essentially the return value *)

let rec match_fragment_def def1 def2 m m' =
  if not (in_map_domain m def1.id) &&
	not (in_map_range m def2.id) &&
	def2.typelabel == def2.typelabel then begin
	  let m' = Map.add (def1.id,def_str def1) (def2.id,def_str def2) m' in
		match (dn def1),(dn def2) with
		  FUNDEF(sn1,b1,_,_),FUNDEF(sn2,b2,_,_) -> 
			let m' = match_fragment_sn sn1 sn2 m m' in
			  match_fragment_block b1 b2 m m'
		| DECDEF(ing1,_),DECDEF(ing2,_) -> match_fragment_ing ing1 ing2 m m'
		| TYPEDEF(ng1,_),TYPEDEF(ng2,_) -> match_fragment_ng ng1 ng2 m m'
		| ONLYTYPEDEF(spec1,_),ONLYTYPEDEF(spec2,_) -> 
		  match_array spec1 spec2 match_fragment_spec_elem m m' 
		| PRAGMA(exp1,_),PRAGMA(exp2,_) -> match_fragment_exp exp1 exp2 m m'
		| LINKAGE(_,_,dlist1),LINKAGE(_,_,dlist2) -> 
		  match_list dlist1 dlist2 match_fragment_def m m'
		| _,_ -> m'
	end else m'
and match_fragment_stmt (stmt1 : statement node) (stmt2 : statement node) m m' =
  pprintf "matching fragment stmt1: %s, stmt2: %s\n" (stmt_str stmt1) (stmt_str stmt2); 
  if not (in_map_domain m stmt1.id) &&
	not (in_map_range m stmt2.id) &&
	stmt1.typelabel == stmt2.typelabel then begin
	  let m' = Map.add (stmt1.id,stmt_str stmt1) (stmt2.id,stmt_str stmt2) m' in
	  match dn stmt1,dn stmt2 with
	  | COMPGOTO(e1,_),COMPGOTO(e2,_)
	  | RETURN(e1,_),RETURN(e2,_)
	  | COMPUTATION(e1,_),COMPUTATION(e2,_) -> pprintf "matching computation %s and %s\n" (exp_str e1) (exp_str e2); match_fragment_exp e1 e2 m m'
	  | BLOCK(b1,_),BLOCK(b2,_) -> match_fragment_block b1 b2 m m'
	  | SEQUENCE(s1,s2,_),SEQUENCE(s3,s4,_) -> 
		let m' = match_fragment_stmt s1 s3 m m' in
		match_fragment_stmt s2 s3 m m'
	  | IF(e1,s1,s2,_),IF(e2,s3,s4,_) ->
		let m' = match_fragment_exp e1 e2 m m' in
		let m' = match_fragment_stmt s1 s3 m m' in
		match_fragment_stmt s2 s3 m m'
	  | CASE(e1,s1,_),CASE(e2,s2,_)
	  | SWITCH(e1,s1,_),SWITCH(e2,s2,_)
	  | WHILE(e1,s1,_),WHILE(e2,s2,_)
	  | DOWHILE(e1,s1,_),DOWHILE(e2,s2,_) ->
		let m' = match_fragment_exp e1 e2 m m' in
		match_fragment_stmt s1 s2 m m'
	  | FOR(fc1,e1,e2,s1,_),FOR(fc2,e3,e4,s2,_) ->
		let m' = match_fragment_fc fc1 fc2 m m' in
		let m' = match_fragment_exp e1 e3 m m' in
		let m' = match_fragment_exp e2 e4 m m' in
		  match_fragment_stmt s1 s2 m m'
	  | CASERANGE(e1,e2,s1,_),CASERANGE(e3,e4,s2,_) ->
		let m' = match_fragment_exp e1 e3 m m' in
		let m' = match_fragment_exp e2 e4 m m' in
		  match_fragment_stmt s1 s2 m m'
	  | DEFINITION(d1),DEFINITION(d2) -> match_fragment_def d1 d2 m m'
	  | TRY_EXCEPT(b1,e1,b2,_),TRY_EXCEPT(b3,e2,b4,_) ->
		let m' = match_fragment_block b1 b3 m m' in
		let m' = match_fragment_exp e1 e2 m m' in
		  match_fragment_block b2 b4 m m'
	  | TRY_FINALLY(b1,b2,_),TRY_FINALLY(b3,b4,_) -> 
		let m' = match_fragment_block b1 b3 m m' in
		  match_fragment_block b2 b4 m m'
	  | DEFAULT(s1,_),DEFAULT(s2,_)
	  | LABEL(_,s1,_),LABEL(_,s2,_) -> match_fragment_stmt s1 s2 m m'
	  | ASM(_,slist1,_,_),ASM(_,slist2,_,_) -> failwith "FIXME"
	  | _,_ -> m'
	end else m'
and match_fragment_exp exp1 exp2 m m' =
  pprintf "in match_fragment_exp: %s and %s\n" (exp_str exp1) (exp_str exp2);
  if not (in_map_domain m exp1.id) &&
	not (in_map_range m exp2.id) &&
	exp1.typelabel == exp2.typelabel then 
	begin
	  let m' = Map.add (exp1.id,exp_str exp1) (exp2.id,exp_str exp2) m' in
	  match dn exp1,dn exp2 with
	  | EXPR_ALIGNOF(e1),EXPR_ALIGNOF(e2)
	  | EXPR_SIZEOF(e1),EXPR_SIZEOF(e2)
	  | PAREN(e1),PAREN(e2)
	  | MEMBEROF(e1,_),MEMBEROF(e2,_)
	  | MEMBEROFPTR(e1,_),MEMBEROFPTR(e2,_)
	  | UNARY(_,e1),UNARY(_,e2) -> match_fragment_exp e1 e2 m m'
	  | INDEX(e1,e2),INDEX(e3,e4)
	  | BINARY(_,e1,e2),BINARY(_,e3,e4) ->
		let m' = match_fragment_exp e1 e3 m m' in
		match_fragment_exp e2 e4 m m'
	  | QUESTION(e1,e2,e3),QUESTION(e4,e5,e6) ->
		let m' = match_fragment_exp e1 e4 m m' in
		let m' = match_fragment_exp e2 e5 m m' in
		  match_fragment_exp e3 e6 m m'
	  | CALL(e1,elist1),CALL(e2,elist2) ->
		pprintf "matching call e1: %s, e2: %s\n" (exp_str e1) (exp_str e2); 
		let m' = match_fragment_exp e1 e2 m m' in
		match_list elist1 elist2 match_fragment_exp m m'
	  | COMMA(elist1),COMMA(elist2) ->
		match_list elist1 elist2 match_fragment_exp m m'
	  | CAST((spec1,dt1),ie1),CAST((spec2,dt2),ie2) -> 
		let m' = match_array spec1 spec2 match_fragment_spec_elem m m' in 
		let m' = match_fragment_dt dt1 dt2 m m' in
		  match_fragment_ie ie1 ie2 m m'
	  | TYPE_SIZEOF(spec1,dt1),TYPE_SIZEOF(spec2,dt2)
	  | TYPE_ALIGNOF(spec1,dt1),TYPE_ALIGNOF(spec2,dt2) ->
		let m' = match_array spec1 spec2 match_fragment_spec_elem m m' in 
		  match_fragment_dt dt1 dt2 m m'
	  | GNU_BODY(b1),GNU_BODY(b2) -> match_fragment_block b1 b2 m m'
	  | _,_ -> m'
	end else m'
and match_fragment_tn tn1 tn2 m m' =
  pprintf "in match_fragment_tn\n"; flush stdout;
  if not (in_map_domain m tn1.id) &&
	not (in_map_range m tn2.id) &&
	tn1.typelabel == tn2.typelabel then begin
	  pprintf "%d and %d match!\n" tn1.id tn2.id; flush stdout;
	  let m' = Map.add (tn1.id,tn_str tn1) (tn2.id,tn_str tn2) m' in
	  match dn tn1,dn tn2 with
	  | Globals(dlist1),Globals(dlist2) -> 
		match_list dlist1 dlist2 match_fragment_def m m'
	  | Stmts(slist1),Stmts(slist2) ->
		match_list slist1 slist2 match_fragment_stmt m m'
	  | Exps(elist1),Exps(elist2) -> 
		match_list elist1 elist2 match_fragment_exp m m'
	  | _,_ -> m'
	end 
  else (pprintf "not match? \n"; flush stdout; m' )
and match_fragment_sn sn1 sn2 m m' =
  let (spec1,name1),(spec2,name2) = sn1, sn2 in 
  let m' = match_array spec1 spec2 match_fragment_spec_elem m m' in 
	match_fragment_name name1 name2 m m'
and match_fragment_block b1 b2 m m' =
  let m' = match_list b1.bstmts b2.bstmts match_fragment_stmt m m' in
  match_array b1.battrs b2.battrs match_fragment_attr m m' 
and match_fragment_ing ing1 ing2 m m' = 
  let (spec1,ins1),(spec2,ins2) = ing1, ing2 in
  let m' = match_array spec1 spec2 match_fragment_spec_elem m m' in
	match_array ins1 ins2 match_fragment_init_name m m' 
and match_fragment_ng ng1 ng2 m m' = 
  let (spec1,names1),(spec2,names2) = ng1,ng2 in
  let m' = match_array spec1 spec2 match_fragment_spec_elem m m' in 
	match_array names1 names2 match_fragment_name m m'
and match_fragment_spec_elem se1 se2 m m' = 
  match se1,se2 with
  | SpecAttr(attr1),SpecAttr(attr2) -> match_fragment_attr attr1 attr2 m m'
  | SpecType(ts1),SpecType(ts2) -> match_fragment_typespec ts1 ts2 m m'
  | _,_ -> m'
and match_fragment_typespec se1 se2 m m' = 
  match se1,se2 with
  | Tstruct(_,Some(fgs1),attrs1), Tstruct(_,Some(fgs2),attrs2)
  | Tunion(_,Some(fgs1),attrs1), Tunion(_,Some(fgs2),attrs2) ->
	let m' = match_array fgs1 fgs2 match_fragment_fg m m' in
	match_array attrs1 attrs2 match_fragment_attr m m' 
  | Tenum(_,None,attrs1), Tenum(_,None,attrs2)
  | Tstruct(_,None,attrs1), Tstruct(_,None,attrs2)
  | Tunion(_,None,attrs1), Tunion(_,None,attrs2) ->
	match_array attrs1 attrs2 match_fragment_attr m m' 
  | Tenum(_,Some(eis1),attrs1),Tenum(_,Some(eis2),attrs2) ->
	let m' = match_array eis1 eis2 match_fragment_ei m m' in
	  match_array attrs1 attrs2 match_fragment_attr m m' 
  | TtypeofE(e1),TtypeofE(e2) -> match_fragment_exp e1 e2 m m'
  | TtypeofT(spec1,dt1),TtypeofT(spec2,dt2) ->
	let m' = match_array spec1 spec2 match_fragment_spec_elem m m' in
	match_fragment_dt dt1 dt2 m m'
  | _,_ -> m'
and match_fragment_fc fc1 fc2 m m' = 
  match fc1,fc2 with
	FC_EXP(e1),FC_EXP(e2) -> match_fragment_exp e1 e2 m m'
  | FC_DECL(d1),FC_DECL(d2) -> match_fragment_def d1 d2 m m'
  | _,_ -> m'
and match_fragment_dt dt1 dt2 m m' = 
  match dt1,dt2 with
  | PARENTYPE(attrs1,dt1,attrs2),PARENTYPE(attrs3,dt2,attrs4) ->
	let m' = match_array attrs1 attrs3 match_fragment_attr m m' in
	let m' = match_fragment_dt dt1 dt2 m m' in
	  match_array attrs2 attrs4 match_fragment_attr m m' 
  | ARRAY(dt1,attrs1,e1),ARRAY(dt2,attrs2,e2) ->
	let m' = match_fragment_dt dt1 dt2 m m' in
	let m' = match_array attrs1 attrs2 match_fragment_attr m m' in 
	  match_fragment_exp e1 e2 m m'
  | PTR(attrs1,dt1),PTR(attrs2,dt2) ->
	let m' = match_array attrs1 attrs2 match_fragment_attr m m' in 
	  match_fragment_dt dt1 dt2 m m'
  | PROTO(dt1,sns1,_),PROTO(dt2,sns2,_) ->
	let m' = match_fragment_dt dt1 dt2 m m' in
	  match_array sns1 sns2 match_fragment_sn m m' 
  | _,_ -> m'
and match_fragment_ie ie1 ie2 m m' = 
  match ie1,ie2 with
  | SINGLE_INIT(e1),SINGLE_INIT(e2) -> match_fragment_exp e1 e2 m m'
  | COMPOUND_INIT(iwies1),COMPOUND_INIT(iwies2) ->
	match_array iwies1 iwies2 match_fragment_iwie m m'
  | _,_ -> m'
and match_fragment_iwie iwie1 iwie2 m m' =
  let (iw1,ie1),(iw2,ie2) = iwie1, iwie2 in
  let m' = match_fragment_init_what iw1 iw2 m m' in
	match_fragment_ie ie1 ie2 m m'
and match_fragment_init_what iw1 iw2 m m' =
  match iw1,iw2 with
  | INFIELD_INIT(_,iw1),INFIELD_INIT(_,iw2) -> match_fragment_init_what iw1 iw2 m m'
  | ATINDEX_INIT(e1,iw1), ATINDEX_INIT(e2,iw2) ->
	let m' = match_fragment_exp e1 e2 m m' in
	match_fragment_init_what iw1 iw2 m m'
  | ATINDEXRANGE_INIT(e1,e2),ATINDEXRANGE_INIT(e3,e4) ->
	let m' = match_fragment_exp e1 e3 m m' in
	match_fragment_exp e2 e4 m m'
  | _,_ -> m'
and match_fragment_name name1 name2 m m' = 
  let (_,dt1,attrs1,_),(_,dt2,attrs2,_) = name1,name2 in 
  let m' = match_fragment_dt dt1 dt2 m m' in
	match_array attrs1 attrs2 match_fragment_attr m m'
and match_fragment_attr attr1 attr2 m m' = 
  let (_,elist1),(_,elist2) = attr1,attr2 in 
	match_list elist1 elist2 match_fragment_exp m m'
and match_fragment_init_name in1 in2 m m' = 
  let (name1,ie1),(name2,ie2) = in1, in2 in
  let m' = match_fragment_name name1 name2 m m' in
	match_fragment_ie ie1 ie2 m m'
and match_fragment_fg fg1 fg2 m m' = 
  let (spec1,lst1),(spec2,lst2) = fg1, fg2 in
  let m' = match_array spec1 spec2 match_fragment_spec_elem m m' in 
	let match_neno neno1 neno2 m m' =
	  let (name1,eno1),(name2,eno2) = neno1,neno2 in
	  let m' = match_fragment_name name1 name2 m m' in
		match eno1,eno2 with
		  Some(e1),Some(e2) -> match_fragment_exp e1 e2 m m'
		| _,_ -> m'
	in
	  match_array lst1 lst2 match_neno m m' 
and match_fragment_ei ei1 ei2 m m' = 
  let (_,e1,_),(_,e2,_) = ei1,ei2 in
	match_fragment_exp e1 e2 m m'

let nodes_in_tree_equal_to node tlht nodeht = 
  let equal_to_tl = try hfind tlht node.typelabel with _ -> [] in
	lmap (fun id -> hfind nodeht id) equal_to_tl

let mapping node nodeht tlht matchfun m =
  if in_map_domain m node.id then m else
	begin
	  let y = nodes_in_tree_equal_to node tlht nodeht in
	  let m'' = 
		lfoldl
		  (fun m'' ->
			fun yi ->
			  if not (in_map_range m yi.id) then 
				let m' = matchfun node yi m (Map.empty) in 
				  if map_size m' > map_size m'' then m' else m''
			  else m''
		  ) (Map.empty) y
	  in
		Map.union m m'' 
	end

let t1_tl_ht = hcreate 10
let t1_node_info = new_tree_info ()
let t2_tl_ht = hcreate 10
let t2_node_info = new_tree_info ()

module Mapping =
struct

  type retval = (int * string, int * string) Map.t

  let mapping_tn tn m = 
	pprintf "tn node info length: %d, t2_tl_ht length: %d\n" (hlen t2_node_info.tn_ht) (hlen t2_tl_ht); 
	mapping tn t2_node_info.tn_ht t2_tl_ht match_fragment_tn m
  let mapping_def def m = mapping def t2_node_info.def_ht t2_tl_ht match_fragment_def m
  let mapping_stmt stmt m = 
	pprintf "mapping stmt: %s\n" (stmt_str stmt); flush stdout;
	mapping stmt t2_node_info.stmt_ht t2_tl_ht match_fragment_stmt m
  let mapping_exp exp m = mapping exp t2_node_info.exp_ht t2_tl_ht match_fragment_exp m
end

module TreeTraversal = LevelOrderTraversal(Mapping)

let add_lst_ht ht key ele =
  let v = ht_find ht key (fun _ -> []) in
	hrep ht key (ele :: v)

class getParentsWalker c2pht p2cht = object(self)
  inherit [unit] singleCabsWalker

  val child_to_parent = c2pht
  val parent_to_child = p2cht
  val parent = ref (-1)
  val position = ref (-1)
  val typ = ref PTREE

  method default_res () = ()
  method combine unit1 unit2 = ()

  method wExpression exp = 
	hadd child_to_parent exp.id (!parent,!position,!typ);
	add_lst_ht parent_to_child !parent exp.id;
	let tempparent = !parent in 
	let tempposition = !position in
	let temptyp = !typ in
	  parent := exp.id; typ := PEXP;
	  ChildrenPost(fun _ -> parent := tempparent; position := tempposition; typ := temptyp)

  method wStatement stmt = 
	hadd child_to_parent stmt.id (!parent,!position,!typ);
	add_lst_ht parent_to_child !parent stmt.id;
	let tempparent = !parent in 
	let tempposition = !position in
	let temptyp = !typ in
	  parent := stmt.id; typ := PSTMT;
	  ChildrenPost(fun _ -> parent := tempparent; position := tempposition; typ := temptyp)

  method wDefinition def = 
	hadd child_to_parent def.id (!parent,!position,!typ);
	add_lst_ht parent_to_child !parent def.id;
	let tempparent = !parent in 
	let tempposition = !position in
	let temptyp = !typ in
	  parent := def.id; typ := PDEF; 
	  ChildrenPost(fun _ -> parent := tempparent; position := tempposition; typ := temptyp)

  method wTreenode tn = 
	hadd child_to_parent tn.id (-1,!position,PTREE);
	add_lst_ht parent_to_child (-1) tn.id;
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
	  match dn exp with
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
	  match dn stmt with
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
	match dn def with
	  FUNDEF(sn,b,_,_) -> self#walkSingleName sn; incr position; self#walkBlock b
	| DECDEF(ing,_) -> self#walkInitNameGroup ing
	| TYPEDEF(ng,_) -> self#walkNameGroup ng
	| ONLYTYPEDEF(spec, _) -> self#walkSpecifier spec
	| PRAGMA(exp,_) -> self#walkExpression exp
	| LINKAGE(_,_,dlist) -> position := 2; self#walkDefinitions dlist
	| _ -> ()

end

let node_that_maps_to mapping parent = 
  try
	Map.iter (fun (k,_) -> fun (v,_) -> if v == parent then raise (Found(k))) mapping ;
	None
  with Found(a) -> Some(a)

let mapsto m x y = 
  try
	Map.iter
	  (fun (k,_) ->
		fun (v,_) ->
		  if k == x && v == y then raise Found_It) m; false
  with Found_It -> true
  

class markVisited ht = object(self)
  inherit nopCabsVisitor

  val ht = ht 

  method vexpr exp = hadd ht exp.id (); DoChildren

  method vstmt stmt = hadd ht stmt.id (); DoChildren

  method vdef def = hadd ht def.id (); DoChildren

  method vtreenode tn = hadd ht tn.id (); DoChildren

end

class constructInsert handled_ht map parents2 (startparenty,startparentx) = object(self)
  inherit nopCabsVisitor

  val handled_ht = handled_ht 
  val m = map
  val parents2 = parents2
  val parents = let ret = hcreate 10 in hadd ret startparenty startparentx; ret

  method vexpr exp = 
	pprintf "in vexpr: %s\n" (exp_str exp); flush stdout;
	if not (hmem handled_ht exp.id) then begin
	  pprintf "not handled\n"; flush stdout;
	  if not (in_map_range m exp.id) then begin
		pprintf "ConstructInsert Handling exp %d, str: %s\n" exp.id (Pretty.sprint ~width:80 (d_exp () exp));
		let yparent,yposition,ytype = hfind parents2 exp.id in 
		let insert_parent,typ = 
		  match (node_that_maps_to m yparent) with
		  | Some(xx) -> xx,ytype (* FIXME double_check this *)
		  | None     -> yparent,ytype
		in
		  if hmem parents yparent && (hfind parents yparent) == insert_parent then begin
			hadd parents exp.id exp.id; 
			pprintf "adding exp %d to handled\n" exp.id;
			hadd handled_ht exp.id (); DoChildren
		  end else begin
			pprintf "Whackadoodle vexp\n";
			ChangeTo({exp with node = newmod ()})
		  end
	  end else (pprintf "in map range already\n"; ChangeTo({exp with node = newmod ()}))
	end else (pprintf "already handled\n"; SkipChildren)

  method vstmt stmt = 
	if not (hmem handled_ht stmt.id) then begin
	  if not (in_map_range m stmt.id) then begin
		pprintf "ConstructInsert parent length: %d Handling stmt %d, str: %s\n" (hlen parents) stmt.id (Pretty.sprint ~width:80 (d_stmt () stmt));
		hiter (fun k -> fun v -> pprintf "parent: %d\n" k) parents;
		let yparent,yposition,ytype = hfind parents2 stmt.id in 
		  pprintf "yparent: %d\n" yparent; 
		let insert_parent,typ = 
		  match (node_that_maps_to m yparent) with
		  | Some(xx) -> xx,ytype (* FIXME double_check this *)
		  | None     -> yparent,ytype
		in
		  pprintf "insert_parent: %d\n" insert_parent; flush stdout;
		  if hmem parents yparent && (hfind parents yparent) == insert_parent then begin
			hadd handled_ht stmt.id (); 
			hadd parents stmt.id stmt.id; DoChildren
		  end else begin
			if not (hmem handled_ht yparent) then pprintf "Not in handled\n";
			if insert_parent != yparent then pprintf "insert parent != yparent\n";
			pprintf "Whackadoodle vstmt\n";
			pprintf "two\n"; 
			ChangeTo([{stmt with node = newmod ()}])
		  end
	  end else (pprintf "three\n"; ChangeTo([{stmt with node = newmod ()}]))
	end else (pprintf "four\n"; SkipChildren)

  method vdef def = 
	if not (hmem handled_ht def.id) then begin
	  if not (in_map_range m def.id) then begin
		pprintf "ConstructInsert Handling def %d, str: %s\n" def.id (Pretty.sprint ~width:80 (d_def () def));
		let yparent,yposition,ytype = hfind parents2 def.id in 
		let insert_parent,typ = 
		  match (node_that_maps_to m yparent) with
		  | Some(xx) -> xx,ytype (* FIXME double_check this *)
		  | None     -> yparent,ytype
		in
		  if hmem parents yparent && (hfind parents yparent) == insert_parent then begin
			hadd parents def.id def.id; 
			hadd handled_ht def.id (); DoChildren
		  end else begin
			pprintf "Whackadoodle vdef\n";
			ChangeTo([{def with node = newmod ()}])
		  end
	  end else (ChangeTo([{def with node = newmod ()}]))
	end else SkipChildren

  method vtreenode tn = pprintf "vtreenode: %d\n" tn.id; hadd parents tn.id (-1); DoChildren

end

let mapping = ref (Map.empty)

module GenDiffTraversal =
struct
  type retval = edit list

  let handled_ht = hcreate 10

  let parent_of_t1 x = hfind parents1 x
  let parent_of_t2 x = hfind parents2 x
  let handled x = hmem handled_ht x

  let mapping_tn tn edits =
	if not (handled tn.id) then begin
	  hadd handled_ht tn.id ();
(*	  let handled = new markVisited handled_ht in*)
	  if not (in_map_range !mapping tn.id) then begin
		let _,xposition,_ = parent_of_t2 tn.id in
(*		  ignore(visitTreeNode handled tn);*)
		  (InsertTreeNode(tn,xposition)) :: edits
	  end
	  else begin
		let Some(x) = node_that_maps_to !mapping tn.id in
		   let _,xposition,_ = parent_of_t1 x in
		   let _,yposition,_ = parent_of_t2 tn.id in 
			 if xposition <> yposition then begin
(*			   ignore(visitTreeNode handled tn);*)
			    (ReorderTreeNode(tn,xposition,yposition)) :: edits
			 end else edits
	  end
	end else edits

  let mapping_def def edits = 
	if not (handled def.id) then begin
	  pprintf "adding %d to handled\n" def.id;
	  hadd handled_ht def.id ();
	  if not (in_map_range !mapping def.id) then begin
		(* insert! *)
		let yparent,yposition,ytype = parent_of_t2 def.id in 
		let insert_parent = 
		  match (node_that_maps_to !mapping yparent) with
		  | Some(xx) -> xx 
		  | None     -> yparent 
		in
		  pprintf "yparent is: %d\n" yparent;
		  let construct = new constructInsert handled_ht !mapping parents2 (yparent,insert_parent) in
		let [def'] = visitCabsDefinition construct (copy def) in
		  pprintf "Handling insert of def %s\n" (def_str def'); 
		   (InsertDefinition(def',insert_parent,yposition,ytype)) :: edits
	  end else begin
		match (node_that_maps_to !mapping def.id) with
		| None -> failwith "generate_script: error: no node that maps to this def\n" 
		| Some(x) -> 
		  let xparent,xposition,xtype = parent_of_t1 x in
		  let yparent,yposition,ytype = parent_of_t2 def.id in 
			if not (mapsto !mapping xparent yparent) then begin
			  let move_parent = 
				match (node_that_maps_to !mapping yparent) with
				  Some(xx) -> xx
				| None -> yparent 
			  in
			  ignore(visitCabsDefinition (new markVisited handled_ht) def);
				(MoveDefinition(def,move_parent,yposition,xtype,ytype)) :: edits
			end else if xposition <> yposition then begin
			  ignore(visitCabsDefinition (new markVisited handled_ht) def);
			  (ReorderDefinition(def,xparent,xposition,yposition,ytype)) :: edits
			end else edits
	  end
	end else edits

  let mapping_stmt stmt edits =
	if not (handled stmt.id) then begin
	  pprintf "adding %d, stmt %s to handled\n" stmt.id (stmt_str stmt);
	  if not (in_map_range !mapping stmt.id) then begin
		let yparent,yposition,ytype = parent_of_t2 stmt.id in 
		  pprintf "parent to construct_insert: %d\n" yparent; 
		let insert_parent,typ = 
		  match (node_that_maps_to !mapping yparent) with
		  | Some(xx) -> xx,ytype (* FIXME: double-check this *)
		  | None     -> yparent,ytype
		in
		let construct = new constructInsert handled_ht !mapping parents2 (yparent,insert_parent) in
		let [stmt'] = visitCabsStatement construct (copy stmt) in
		  pprintf "done construct insert stmt\n"; flush stdout;
		  pprintf "Handling insert of stmt %s\n" (stmt_str stmt'); 
	  hadd handled_ht stmt.id ();
		  (InsertStatement(stmt,insert_parent,yposition,typ)) :: edits
	  end
	  else begin
	  hadd handled_ht stmt.id ();
		match (node_that_maps_to !mapping stmt.id) with
		| None -> failwith "generate_script: error: no node that maps to this stmt!\n" 
		| Some(x) -> 
		  let xparent,xposition,xtype = parent_of_t1 x in
		  let yparent,yposition,ytype = parent_of_t2 stmt.id in 
			if not (mapsto !mapping xparent yparent) then begin
			  let move_parent = 
				match (node_that_maps_to !mapping yparent) with
				| Some(xx) -> xx
				| None     -> yparent 
			  in
			  ignore(visitCabsStatement (new markVisited handled_ht) stmt);
				(MoveStatement(stmt,move_parent,yposition,xtype,ytype)) :: edits
			end else if xposition <> yposition then begin
			  ignore(visitCabsStatement (new markVisited handled_ht) stmt);
			   (ReorderStatement(stmt,xparent,xposition,yposition,ytype)) :: edits
			end else edits
	  end
	end else edits

  let mapping_exp exp edits =
	if not (handled exp.id) then begin
	  pprintf "adding %d to handled\n" exp.id;
	  if not (in_map_range !mapping exp.id) then begin
		pprintf "Handling exp %d, str: %s\n" exp.id (Pretty.sprint ~width:80 (d_exp () exp));
		let yparent,yposition,ytype = parent_of_t2 exp.id in 
		let insert_parent,typ = 
		  match (node_that_maps_to !mapping yparent) with
		  | Some(xx) -> xx,ytype (* FIXME double_check this *)
		  | None     -> yparent,ytype
		in
 		let construct = new constructInsert handled_ht !mapping parents2 (yparent,insert_parent) in
		let exp' = visitCabsExpression construct (copy exp) in
		  hadd handled_ht exp.id (); 
		  pprintf "Handling insert of exp %s\n" (exp_str exp'); 
		  pprintf "Inserting expression\n"; flush stdout;
		  (InsertExpression(exp',insert_parent,yposition,typ)) :: edits
	  end else begin
		match (node_that_maps_to !mapping exp.id) with 
		| None -> failwith "generate_script: error: no node that maps to this expression!\n" 
		| Some(x) -> 
		  let xparent,xposition,xtype = parent_of_t1 x in
		  let yparent,yposition,ytype = parent_of_t2 exp.id in 
			pprintf "Handling exp %d, str: %s, maps to: %d\n" exp.id (Pretty.sprint ~width:80 (d_exp () exp)) x;
			pprintf "xparent: %d, yparent: %d\n" xparent yparent;
			if not (mapsto !mapping xparent yparent) then begin
			  let move_parent = 
				match (node_that_maps_to !mapping yparent) with
				| Some(xx) -> xx
				| None     -> yparent 
			  in (* Is it possible to recognize insertion of entire trees, to avoid the usual stupidity? *)
				ignore(visitCabsExpression (new markVisited handled_ht) exp);
				(MoveExpression(exp,move_parent,yposition,xtype,ytype)) :: edits
			end else if xposition <> yposition then begin
			  ignore(visitCabsExpression (new markVisited handled_ht) exp);
			   (ReorderExpression(exp,xparent,xposition,yposition,ytype)) :: edits
			end else edits
	  end
	end else edits

end

module DeleteTraversal =
struct
  type retval = edit list

  let mapping_tn tn edits = 
	if not (in_map_domain !mapping tn.id) then begin
	  let parent,_,_ = hfind parents1 tn.id in
		(DeleteTN(tn, parent)) :: edits
	end else edits 

  let mapping_def def edits =
 	if not (in_map_domain !mapping def.id) then begin
	  let parent,_,_ = hfind parents1 def.id in
		(DeleteDef(def, parent)) :: edits
	end else edits 

  let mapping_stmt stmt edits = 
	if not (in_map_domain !mapping stmt.id) then begin
	  let parent,_,_ = hfind parents1 stmt.id in
		(DeleteStmt(stmt,parent)) :: edits
	end else edits 

  let mapping_exp exp edits = 
	if not (in_map_domain !mapping exp.id) then begin
	  let parent,_,_ = hfind parents1 exp.id in
		(DeleteExp(exp,parent)) :: edits
	end else edits
end

module GenDiff = LevelOrderTraversal(GenDiffTraversal)
module Deletions = LevelOrderTraversal(DeleteTraversal)

let gendiff t1 t2 = 
  pprintf "Done making diff trees\n";
  let p1 = new getParentsWalker parents1 children1 in
	p1#walkTree t1;
	let p2 = new getParentsWalker parents2 children2 in
	  p2#walkTree t2;
	  let map = TreeTraversal.traverse t1 (Map.empty) in
		pprintf "Map: \n"; 
		Map.iter
		  (fun (id1,str1) -> fun (id2,str2) -> pprintf "%d,%s -> %d,%s\n" id1 str1 id2 str2) map;
		flush stdout;
		pprintf "End Map!\n"; 
		mapping := map;
		let regscript = GenDiff.traverse t2 [] in 
		let script = lrev (Deletions.traverse t2 regscript) in
		  pprintf "Done with script!\n"; flush stdout;
		  liter print_edit script; script

(*let rec cleanup_tree t = FIXME: do I still need this? 
  Array.iter (fun child ->
    cleanup_tree child
  ) t.children; 
  let lst = Array.to_list t.children in
  let lst = List.filter (fun child ->
    child.typelabelF <> -1
  ) lst in
  t.children <- Array.of_list lst *)

(*************************************************************************)
(* applying a generated diff; mostly unecessary for taxonomy purposes,
 * but included for completeness/testing *)

(* Apply a single edit operation to a file. This version if very fault
 * tolerant because we're expecting our caller (= a delta-debugging script)
 * to be throwing out parts of the diff script in an effort to minimize it.
 * So this is 'best effort'. *) 
(*
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

*)
(*************************************************************************)
 (* "main" and test functions *)

(* Apply a (partial) diff script. *) 
(*let usediff name diff_in data_in file_out = 
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
*)

(*************************************************************************)
(* functions called from the outside to generate the diffs we
 * ultimately care about, as well as testing drivers.  *)

(* diff_name is string uniquely IDing this diff *)
let test_mapping files = 
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
		  (*process_tree FIXME: what was this? *) (fst (Diffparse.parse_from_string diff1)), (*process_tree*) (fst (Diffparse.parse_from_string diff2)) in
		  pprintf "dumping parsed cabs1: ";
		  dumpTree defaultCabsPrinter Pervasives.stdout ("",old_file_tree);
		  pprintf "end dumped to stdout\n"; flush stdout;
		  pprintf "dumping parsed cabs2: ";
		  dumpTree defaultCabsPrinter Pervasives.stdout ("",new_file_tree);
		  pprintf "end dumped to stdout\n"; flush stdout;
		  ignore(gendiff (diff1,old_file_tree)  (diff2,new_file_tree))
	  ) syntactic

let tree_diff_cabs old_file_tree new_file_tree diff_name = 
(*  let old_file_tree = process_tree old_file_tree in
  let new_file_tree = process_tree new_file_tree in*)
  let f1 =  ((diff_name^"1"), old_file_tree) in
  let f2 =  ((diff_name^"2"), new_file_tree) in 
  pprintf "Tree 1:\n";
  let t1 = tree_to_diff_tree f1 t1_tl_ht t1_node_info in
  pprintf "Tree 2:\n";
	let t2 = tree_to_diff_tree f2 t2_tl_ht t2_node_info in
  let script = gendiff t1 t2 in
  let diff' = standardize_diff script in
  let alpha = alpha_rename diff' in
    if !debug_bl then begin
	pprintf "Standard diff: \n";
	liter print_edit diff';
	pprintf "Alpha-renamed diff: \n";
	liter print_edit alpha;
	flush stdout
    end;
	diff', alpha
