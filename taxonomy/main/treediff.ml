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
  if (llen list1) == (llen list2) then begin
	List.fold_left2
	  (fun map ->
		fun ele1 ->
		  fun ele2 ->
			let m' = matchfun ele1 ele2 map (Map.empty) in 
			  Map.union map m'
	  ) m' list1 list2
  end else begin
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
  end
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
  if not (in_map_domain m stmt1.id) &&
	not (in_map_range m stmt2.id) &&
	stmt1.typelabel == stmt2.typelabel then begin
	  let m' = Map.add (stmt1.id,stmt_str stmt1) (stmt2.id,stmt_str stmt2) m' in
		match dn stmt1,dn stmt2 with
		| COMPGOTO(e1,_),COMPGOTO(e2,_)
		| RETURN(e1,_),RETURN(e2,_)
		| COMPUTATION(e1,_),COMPUTATION(e2,_) -> match_fragment_exp e1 e2 m m'
		| BLOCK(b1,_),BLOCK(b2,_) -> 
		  match_fragment_block b1 b2 m m'
		| SEQUENCE(s1,s2,_),SEQUENCE(s3,s4,_) -> 
		  let m' = match_fragment_stmt s1 s3 m m' in
			match_fragment_stmt s2 s3 m m'
		| IF(e1,s1,s2,_),IF(e2,s3,s4,_) ->
		  let m' = match_fragment_exp e1 e2 m m' in
		  let m' = match_fragment_stmt s1 s3 m m' in
			match_fragment_stmt s2 s4 m m'
		| CASE(e1,s1,_),CASE(e2,s2,_) ->
		  let m' = match_fragment_exp e1 e2 m m' in
			match_fragment_stmt s1 s2 m m'
		| SWITCH(e1,s1,_),SWITCH(e2,s2,_) ->
		  let m' = match_fragment_exp e1 e2 m m' in
			match_fragment_stmt s1 s2 m m'
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
  if not (in_map_domain m tn1.id) &&
	not (in_map_range m tn2.id) &&
	tn1.typelabel == tn2.typelabel then begin
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
  else m'
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
	lmap (fun id -> fst (hfind nodeht id)) equal_to_tl

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

module Mapping =
struct

  type retval = (int * string, int * string) Map.t

  let t2_node_info = ref (new_tree_info ())
  let t2_tl_ht = ref (hcreate 10)

  let set_vals node_info ht = 
	t2_node_info := node_info; t2_tl_ht := ht
	  
  let mapping_tn tn m = 
	mapping tn !t2_node_info.tn_ht !t2_tl_ht match_fragment_tn m
  let mapping_def def m = mapping def !t2_node_info.def_ht !t2_tl_ht match_fragment_def m
  let mapping_stmt stmt m = 
	mapping stmt !t2_node_info.stmt_ht !t2_tl_ht match_fragment_stmt m
  let mapping_exp exp m = mapping exp !t2_node_info.exp_ht !t2_tl_ht match_fragment_exp m
end

module TreeTraversal = LevelOrderTraversal(Mapping)

let combine_parent_maps map1 map2 = 
  let new_map = ref (Map.empty) in
  let one_fun map = 
	Map.iter
	  (fun key ->
		fun lst ->
		  let old_val = try Map.find key !new_map with Not_found -> [] in
			new_map := Map.add key (old_val @ lst) !new_map)
	  map in
	one_fun map1; one_fun map2; !new_map

class getParentsWalker = object(self)
(* FIXME: the positional info here is completely broken, debug it something! *)
  inherit [(int, (int * int * parent_type)) Map.t * (int, int list) Map.t] singleCabsWalker

  val parent = ref (-1)
  val position = ref (-1)
  val typ = ref PTREE

  method default_res () = Map.empty, Map.empty
  method combine (c2p1,p2c1) (c2p2,p2c2) = 
	Map.union c2p1 c2p2, combine_parent_maps p2c1 p2c2 

  method wExpression exp = 
	let child = Map.add exp.id (!parent,!position,!typ) Map.empty in
	let parents = Map.add !parent [exp.id] (Map.empty) in
	let tempparent = !parent in 
	let tempposition = !position in
	let temptyp = !typ in
	  parent := exp.id; typ := PEXP;
	  CombineChildrenPost((child,parents),
						  (fun res -> parent := tempparent; position := tempposition; typ := temptyp; res))

  method wStatement stmt = 
	let child = Map.add stmt.id (!parent,!position,!typ) Map.empty in
	let parents = Map.add !parent [stmt.id] Map.empty in
	let tempparent = !parent in 
	let tempposition = !position in
	let temptyp = !typ in
	  parent := stmt.id; typ := PSTMT;
	  CombineChildrenPost((child,parents), (fun res -> parent := tempparent; position := tempposition; typ := temptyp; res))

  method wDefinition def = 
	let child = Map.add def.id (!parent,!position,!typ) Map.empty in
	let parents = Map.add !parent [def.id] Map.empty in
	let tempparent = !parent in 
	let tempposition = !position in
	let temptyp = !typ in
	  parent := def.id; typ := PDEF; 
	  CombineChildrenPost((child,parents), (fun res -> parent := tempparent; position := tempposition; typ := temptyp; res))

  method wTreenode tn = 
	let child = Map.add tn.id (!parent,!position,!typ) Map.empty in
	let parents = Map.add !parent [tn.id] Map.empty in
	let tempparent = !parent in 
	let tempposition = !position in
	let temptyp = !typ in
	  parent := tn.id; typ := PARENTTN; position := 0;
	  CombineChildrenPost((child,parents), 
						  (fun res -> parent := tempparent; position := tempposition; typ := temptyp; res))

  method childrenExpression exp = 
	let walklist start lst =
	  fst 
		(lfoldl
		   (fun (maps,index) ->
			 fun exp ->
			   position := index;
			   self#combine maps
				 (self#walkExpression exp),
			   index+1) (self#default_res(),start) lst)
	in
	  position := 0;
	  match dn exp with
	  | UNARY(_,e1) -> walklist 1 [e1]
	  | BINARY(_,e1,e2) -> walklist 1 [e1;e2]
	  | QUESTION(e1,e2,e3) -> walklist 0 [e1;e2;e3]
	  | CAST((spec,dt),ie) -> 
		let maps1 = self#walkSpecifier spec in
		  incr position;
		  let maps2 = self#walkDeclType dt in
			incr position;
			let maps3 = self#walkInitExpression ie in
			  self#combine maps1 (self#combine maps2 maps3)
	  | CALL(e1,elist) -> walklist !position (e1::elist)
	  | COMMA(elist) ->self#walkExpressions elist
	  | MEMBEROF(e1,_)
	  | MEMBEROFPTR(e1,_)
	  | EXPR_SIZEOF(e1)
	  | EXPR_ALIGNOF(e1)
	  | PAREN(e1) -> self#walkExpression e1
	  | TYPE_SIZEOF(spec,dt)
	  | TYPE_ALIGNOF(spec,dt) ->
		let maps1 = self#walkSpecifier spec in
		  incr position;
		  self#combine maps1 (self#walkDeclType dt)
	  | INDEX(e1,e2) -> walklist 0 [e1;e2]
	  | GNU_BODY(b) -> self#walkBlock b
	  | _ -> self#default_res()

  method childrenStatement stmt = 
	let walklist start lst =
	  fst
		(lfoldl
		   (fun (maps,index) ->
			 fun stmt ->
			   position := index;
			   self#combine maps
				 (self#walkStatement stmt),
			   index+1) (self#default_res(),start) lst)
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
		  let maps1 = self#walkExpression e1 in
			typ := temp;
			self#combine maps1 (walklist 1 [s1;s2])
	  | CASE(e1,s1,_)
	  | SWITCH(e1,s1,_) -> 
		let temp = !typ in
		  typ := CONDGUARD;
		  let maps1 = self#walkExpression e1 in 
			incr position; 
			let maps2 = self#combine maps1 (self#walkStatement s1) in
			  typ := temp; maps2
	  | WHILE(e1,s1,_)
	  | DOWHILE(e1,s1,_) ->
		let temp = !typ in
		  typ := LOOPGUARD;
		  let maps1 = self#walkExpression e1 in
		  incr position;typ := temp;
			self#combine maps1 (self#walkStatement s1)
	  | FOR(fc,e1,e2,s1,_) -> 
		let temp = !typ in 
		  typ := FORINIT;
		  let maps1 = match fc with 
			  FC_EXP(e) -> self#walkExpression e
			| FC_DECL(d) -> self#walkDefinition d
		  in
			incr position;typ := LOOPGUARD;
			let maps2 = self#walkExpression e1; in
			  incr position;
			  let maps3 = self#walkExpression e2 in
				incr position;typ := temp;
				self#combine maps1 (self#combine maps2 (self#combine maps3 (self#walkStatement s1)))
	  | CASERANGE(e1,e2,s1,_) ->
		let maps1 =	self#walkExpression e1 in
		  incr position; 
		  let maps2 = self#walkExpression e2 in
			incr position; 
			self#combine maps1 (self#combine maps2 (self#walkStatement s1))
	  | DEFAULT(s1,_) -> self#walkStatement s1
	  | LABEL(_,s1,_) -> incr position; self#walkStatement s1
	  | DEFINITION(d) -> self#walkDefinition d
	  | ASM(_,_,_,_) -> failwith "ASM not handled in getparents walker"
	  | TRY_EXCEPT(b1,e1,b2,_) ->
		let maps1 = self#walkBlock b1 in
		  incr position; 
		let temp = !typ in
		  typ := CONDGUARD;
		  let maps2 = self#walkExpression e1 in incr position; typ := temp; 
			self#combine maps1 (self#combine maps2 (self#walkBlock b2))
	  | TRY_FINALLY(b1,b2,_) ->
		let maps1 = self#walkBlock b1 in
		  incr position; incr position; 
		  self#combine maps1 (self#walkBlock b2)
	  | _ -> self#default_res()

  method childrenDefinition def = 
	position := 0;
	match dn def with
	  FUNDEF(sn,b,_,_) -> 
		let maps1 = self#walkSingleName sn in
		  incr position; 
		  self#combine maps1 (self#walkBlock b)
	| DECDEF(ing,_) -> self#walkInitNameGroup ing
	| TYPEDEF(ng,_) -> self#walkNameGroup ng
	| ONLYTYPEDEF(spec, _) -> self#walkSpecifier spec
	| PRAGMA(exp,_) -> self#walkExpression exp
	| LINKAGE(_,_,dlist) -> position := 2; self#walkDefinitions dlist
	| _ -> self#default_res()

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
  


let map = ref (Map.empty)

class markVisited ht = object(self)
  inherit nopCabsVisitor
 
  val ht = ht 
 
  method vexpr exp = hadd ht exp.id (); DoChildren

  method vstmt stmt = hadd ht stmt.id (); DoChildren

  method vdef def = hadd ht def.id (); DoChildren

  method vtreenode tn = hadd ht tn.id (); DoChildren

end
module GenDiffTraversal =
struct
  type retval = edit list

  let parents1 = ref Map.empty 
  let parents2 = ref Map.empty 
  let children1 = ref Map.empty 
  let children2 = ref Map.empty 
  let info = ref (new_tree_info ())

  let set_vals p1 c1 p2 c2 i =
	parents1 :=  p1; parents2 := p2;
	children1 := c1; children2 := c2;
	info := i

  let handled_ht = hcreate 10

  let parent_of_t1 x = Map.find x !parents1
  let parent_of_t2 x = Map.find x !parents2
  let handled x = hmem handled_ht x


  let is_insert id = 
	if not (in_map_range !map id) then begin
	  let yparent,yposition,ytype = parent_of_t2 id in 
	  let insert_parent,typ = 
		match (node_that_maps_to !map yparent) with
		| Some(xx) -> xx,ytype (* FIXME double_check this *)
		| None     -> yparent,ytype
	  in 
		true,insert_parent,yposition,typ
	end else false,-1,-1,PEXP

  let is_move id = 
	match (node_that_maps_to !map id) with 
	| None -> failwith "generate_script: error: no node that maps to this expression!\n" 
	| Some(x) -> 
	  let xparent,xposition,xtype = parent_of_t1 x in
	  let yparent,yposition,ytype = parent_of_t2 id in 
	  let ism = not (mapsto !map xparent yparent) in
	  let move_parent = 
		match (node_that_maps_to !map yparent) with
		| Some(xx) -> xx
		| None     -> yparent 
	  in
		ism,move_parent,xparent,xposition,yposition,xtype,ytype


  let mapping_tn tn edits =
	if not (handled tn.id) then begin
	  hadd handled_ht tn.id ();
	  if not (in_map_range !map tn.id) then begin
		let _,xposition,_ = parent_of_t2 tn.id in
		  (InsertTreeNode(snd (hfind !info.tn_ht tn.id),xposition)) :: edits
	  end
	  else begin
		let Some(x) = node_that_maps_to !map tn.id in
		let _,xposition,_ = parent_of_t1 x in
		let _,yposition,_ = parent_of_t2 tn.id in 
		  if xposition <> yposition then begin
			let mark = new markVisited handled_ht in 
			  ignore(visitTreeNode mark tn);
			  (ReorderTreeNode(tn,xposition,yposition)) :: edits
		  end else edits 
	  end
	end else edits

  let mapping_def def edits = 
	let edit = 
	  if not (handled def.id) then begin
		hadd handled_ht def.id ();
		let is_insert,insert_parent,yposition,typ = is_insert def.id in
		  if is_insert then
			[InsertDefinition(snd (hfind !info.def_ht def.id),insert_parent,yposition,typ)]
		  else begin
			let def = fst (hfind !info.def_ht def.id) in
			let is_move,move_parent,xparent,xposition,yposition,xtype,ytype = is_move def.id in
			  if is_move || xposition <> yposition then begin
				let mark = new markVisited handled_ht in 
				  ignore(visitCabsDefinition mark def);
			  end;
			  if is_move then
				[MoveDefinition(def,move_parent,xparent,yposition,xtype,ytype)]
			  else if xposition <> yposition then 
				[ReorderDefinition(def,xparent,xposition,yposition,ytype)]
			  else []
		  end
	  end else []
	in edit @ edits

  let mapping_stmt stmt edits =
	let edit = 
	  if not (handled stmt.id) then begin
		hadd handled_ht stmt.id ();
		let is_insert,insert_parent,yposition,typ = is_insert stmt.id in 
		  if is_insert then
			[InsertStatement(snd (hfind !info.stmt_ht stmt.id),insert_parent,yposition,typ)]
		  else begin
			let stmt = fst (hfind !info.stmt_ht stmt.id) in
			let is_move,move_parent,xparent,xposition,yposition,xtype,ytype = is_move stmt.id in
			  if is_move || xposition <> yposition then begin
				let mark = new markVisited handled_ht in 
				  ignore(visitStatement mark stmt); 
			  end;
			  if is_move then
				[MoveStatement(stmt,move_parent,xparent,yposition,xtype,ytype)]
			  else if xposition <> yposition then
				[ReorderStatement(stmt,xparent,xposition,yposition,ytype)]
			  else []
		  end
	  end else [] in edit @ edits

  let mapping_exp exp edits =
	let edit = 
	  if not (handled exp.id) then begin
		hadd handled_ht exp.id (); 
		let is_insert,insert_parent,yposition,typ = is_insert exp.id in
		  if is_insert then 
			[InsertExpression(snd (hfind !info.exp_ht exp.id),insert_parent,yposition,typ)]
		  else begin
			let exp = fst (hfind !info.exp_ht exp.id) in
			let is_move,move_parent,xparent,xposition,yposition,xtype,ytype = is_move exp.id in
			  if is_move || xposition <> yposition then begin
				let mark = new markVisited handled_ht in 
				  ignore(visitCabsExpression mark exp); 
			  end;
			  if is_move then
				[MoveExpression(exp,move_parent,xparent,yposition,xtype,ytype)]
			  else if xposition <> yposition then 
				[ReorderExpression(exp,xparent,xposition,yposition,ytype)]
			  else []
		  end
	  end else [] in edit @ edits

end

module DeleteTraversal =
struct
  type retval = edit list

  let parents1 = ref Map.empty 

  let set_vals p1 = parents1 := p1

  let mapping_tn tn edits = 
	if not (in_map_domain !map tn.id) then 
	  (DeleteTN(tn, -1, PTREE)) :: edits
	else edits 

  let mapping_def def edits =
 	if not (in_map_domain !map def.id) then
	  let parent,_,ptyp = Map.find def.id !parents1 in
		(DeleteDef(def, parent,ptyp)) :: edits
	else edits 

  let mapping_stmt stmt edits = 
	if not (in_map_domain !map stmt.id) then 
	  let parent,_,ptyp = Map.find stmt.id !parents1 in
		(DeleteStmt(stmt,parent,ptyp)) :: edits
	else edits 

  let mapping_exp exp edits = 
	if not (in_map_domain !map exp.id) then 
	  let parent,_,ptyp = Map.find exp.id !parents1 in
		(DeleteExp(exp,parent,ptyp)) :: edits
	else edits
end

module GenDiff = LevelOrderTraversal(GenDiffTraversal)
module Deletions = LevelOrderTraversal(DeleteTraversal)

class numPrinter = object
  inherit nopCabsVisitor

  method vexpr exp = pprintf "EXP: ((%d:%s))\n" exp.id (exp_str exp); DoChildren
  method vstmt stmt = pprintf "STMT: ((%d:%s))\n" stmt.id (stmt_str stmt); DoChildren
  method vdef def = pprintf "DEF: ((%d:%s))\n" def.id (def_str def); DoChildren
  method vtreenode tn = pprintf "TN: ((%d:%s))\n" tn.id (tn_str tn); DoChildren
end
 
let full_info info1 info2 = 
  let copy_over ht1 ht2 =
	hiter
	  (fun key ->
		fun value -> 
		  hadd ht1 key value) ht2
  in
  let exps1 = Hashtbl.copy info1.exp_ht in
  let stmts1 = Hashtbl.copy info1.stmt_ht in
  let defs1 = Hashtbl.copy info1.def_ht in
  let tns1 = Hashtbl.copy info1.tn_ht in
	copy_over exps1 info2.exp_ht;
	copy_over stmts1 info2.stmt_ht;
	copy_over defs1 info2.def_ht;
	copy_over tns1 info2.tn_ht;
	{exp_ht = exps1; stmt_ht = stmts1; def_ht = defs1;
	 tn_ht = tns1; parent_ht = hcreate 10 }

let gendiff t1 t2 = 
  let printer = new numPrinter in
  let t1,t1_tl_ht,t1_node_info = tree_to_diff_tree t1 in
(*	pprintf "tree 1:\n";
	ignore(visitTree printer t1);*)
  let t2,t2_tl_ht,t2_node_info = tree_to_diff_tree t2 in
  let parent_walker = new getParentsWalker in
  let parents1,children1 = parent_walker#walkTree t1 in
  let parents2,children2 = parent_walker#walkTree t2 in
  let combined = full_info t1_node_info t2_node_info in
	GenDiffTraversal.set_vals parents1 children1 parents2 children2 combined;
	DeleteTraversal.set_vals parents1;
	Mapping.set_vals t2_node_info t2_tl_ht;
	map := (TreeTraversal.traverse t1 (Map.empty));
(*	pprintf "MAPPING: \n"; 
	Map.iter
	  (fun (id1,str1) ->
		fun (id2,str2) ->
		  pprintf "id1: %d ---> id2: %d\n" id1 id2)
	  !map;
	pprintf "DONE MAPPING\n"; flush stdout;*)
	let regscript = GenDiff.traverse t2 [] in 
	  lmap new_change (lrev (Deletions.traverse t1 regscript)), combined,children1

let filter_tree_to_defs patch tree1 =
  FindDefMapper.clear();
  let def_ht = hcreate 10 in
  let defvisit = new findDefVisitor def_ht in
	ignore(visitTree defvisit tree1);
(*  lmap
	(fun (def,edits) ->
	  pprintf "Defnum: %d, def: %s, edits: " def.id (def_str def);
	  liter print_edit edits;
	  def,edits) *)
  (find_parents def_ht patch)

(*************************************************************************)
(* functions called from the outside to generate the diffs we
 * ultimately care about, as well as testing drivers.  *)

(* diff_name is string uniquely IDing this diff *)
let test_mapping files = 
  let file_strs = 
	lmap 
	  (fun file -> 
		let strs = List.of_enum (File.lines_of file) in
		  lfoldl (fun strs -> fun str -> strs^"\n"^str) "" strs) files 
  in
  let rec group files = 
	match files with
	  one :: two :: rest -> (one,two) :: group rest
	| _ -> [] in
  let syntactic = group file_strs in
	lflat
	  (lmap
	  (fun (diff1,diff2) ->
		let old_file_tree,new_file_tree = 
		  fst (Diffparse.parse_from_string diff1), fst (Diffparse.parse_from_string diff2) in
		  (* pprintf "dumping parsed cabs1: "; dumpTree defaultCabsPrinter
			 Pervasives.stdout ("",old_file_tree); pprintf "end dumped to
			 stdout\n"; flush stdout; pprintf "dumping parsed cabs2: "; dumpTree
			 defaultCabsPrinter Pervasives.stdout ("",new_file_tree); pprintf "end
			 dumped to stdout\n"; flush stdout;*)
		let patch,info,children1 = gendiff (diff1,old_file_tree)  (diff2,new_file_tree) in
		let diff' = standardize_diff children1 patch info in
		let filtered_tree : (definition node * ((int * edit) list)) list = filter_tree_to_defs diff' (diff1,old_file_tree) in
		  pprintf "diff length: %d\n" (llen diff'); flush stdout;
		  liter (fun (_,edit) -> pprintf "%s\n" (edit_str edit)) diff';
		  pprintf "DONE PRINTING SCRIPT\n"; flush stdout;
		  lmap (fun filt -> diff1,filt,info) filtered_tree
	  ) syntactic)

let tree_diff_cabs diff1 diff2 diff_name = 
  pprintf "TREEONE\n";
  let old_file_tree, new_file_tree = 
	fst (Diffparse.parse_from_string diff1), (*process_tree*) fst (Diffparse.parse_from_string diff2) in
  pprintf "TREETWO\n";		
  let patch,info,children1 = gendiff ("",old_file_tree) ("",new_file_tree) in
	liter (fun (_,edit) -> pprintf "%s\n" (edit_str edit)) patch;
	pprintf "DONE PRINTING SCRIPT\n"; flush stdout;
	let diff' = patch in (*standardize_diff children1 patch info in*)
	  pprintf "TREETHREE\n";		
	  let filtered_tree : (definition node * ((int * edit) list)) list = filter_tree_to_defs diff' (diff1,old_file_tree) in
		pprintf "TREEFOUR\n";		
		lmap
		  (fun (defs,edits) -> defs,edits,info) filtered_tree
