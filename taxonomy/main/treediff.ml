open Batteries
open Pretty
open Printf
open Utils
open Globals
open Cabs
open Cprint
open Diffparse

(* This makes a deep copy of an arbitrary Ocaml data structure *) 
let copy (x : 'a) = 
  let str = Marshal.to_string x [] in
  (Marshal.from_string str 0 : 'a) 

(*
 * We convert to a very generic tree data structure (below) for the
 * purposes of doing the DiffX structural difference algorithm. Then we
 * convert back later after applying the diff script. 
 *)
type dummyNode = 
  | TREE of tree_node node list 
  | FORCLAUSE of for_clause node
  | ASMDET of asm_details option node 
  | ATTR of attribute node
  | DECLT of decl_type node
  | ING of init_name_group node
  | NG of name_group node
  | SN of single_name node
  | SPECS of specifier node
  | SPEC of spec_elem node
  | IE of init_expression node
  | STMT of statement node
  | EXP of expression node
  | BLK of block node
  | TREENODE of tree_node node
  | DEF of definition node
  | NAME of name node
  | INITNAME of init_name node
  | WHAT of initwhat node
  | TYPESPEC of typeSpecifier node
  | FIELDGROUP of field_group node
  | ENUMITEM of enum_item node

type diff_tree_node = {
  mutable nid : int ; (* unique per node *)
  mutable children : diff_tree_node array ;
  mutable typelabel : int ; 
  (* two nodes that represent the same C statement will have the same
     typelabel. "children" are not considered for calculating typelabels,
     so 'if (x<y) { foo(); }' and 'if (x<y) { bar(); }' have the
     same typelabels, but their children (foo and bar) will not.  *) 
} 

let print_tree n = 
  let rec print n depth = 
    pprintf "%*s%02d (tl = %02d) (%d children)\n" 
      depth "" 
      n.nid n.typelabel
      (Array.length n.children) ;
    Array.iter (fun child ->
      print child (depth + 2)
    ) n.children
  in
  print n 0 

let deleted_node = {
  nid = -1;
  children = [| |] ;
  typelabel = -1 ;
} 

let rec cleanup_tree t =
  Array.iter (fun child ->
    cleanup_tree child
  ) t.children; 
  let lst = Array.to_list t.children in
  let lst = List.filter (fun child ->
    child.typelabel <> -1
  ) lst in
  t.children <- Array.of_list lst 

let delete node =
  node.nid <- -1 ; 
  node.children <- [| |] ; 
  node.typelabel <- -1 

let node_counter = ref 0 

let new_node typelabel = 
  let nid = !node_counter in
  incr node_counter ;
  { nid = nid ;
    children = [| |] ; 
    typelabel = typelabel ;
  }  

let nodes_eq t1 t2 =
  (* if both their types and their labels are equal *) 
  t1.typelabel = t2.typelabel 

module OrderedNode =
  struct
    type t = diff_tree_node
    let compare x y = compare x.nid y.nid
  end
module OrderedNodeNode =
  struct
    type t = diff_tree_node * diff_tree_node
    let compare (a,b) (c,d) = 
      let r1 = compare a.nid c.nid in
      if r1 = 0 then
        compare b.nid d.nid
      else
        r1 
  end

module NodeSet = Set.Make(OrderedNode)
module NodeMap = Set.Make(OrderedNodeNode)

exception Found_It 
exception Found_Node of diff_tree_node 

(* returns true if (t,_) is in m *) 
let in_map_domain m t =
  try 
    NodeMap.iter (fun (a,_) -> 
      if a.nid = t.nid then raise Found_It
    ) m ;
    false
  with Found_It -> true 

(* returns true if (_,t) is in m *) 
let in_map_range m t =
  try 
    NodeMap.iter (fun (_,a) -> 
      if a.nid = t.nid then raise Found_It
    ) m ;
    false
  with Found_It -> true 

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
    (if nodes_eq t n then NodeSet.singleton t else NodeSet.empty)
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
     (nodes_eq x y) then begin
    m' := NodeMap.add (x,y) !m' ;
    let xc = Array.length x.children in 
    let yc = Array.length y.children in 
    for i = 0 to pred (min xc yc) do
      match_fragment x.children.(i) y.children.(i) m m'
    done 
  end 

type edit_action = 
  | Insert of int * (int option) * (int option)
  | Move   of int * (int option) * (int option)
  | Delete of int 

let noio no = match no with
  | Some(n) -> Some(n.nid)
  | None -> None 

let io_to_str io = match io with
  | Some(n) -> Printf.sprintf "%d" n
  | None -> "-1" 

let edit_action_to_str ea = match ea with
  | Insert(n,no,io) -> Printf.sprintf "Insert (%d,%s,%s)" n (io_to_str no)
    (io_to_str io)
  | Move(n,no,io) -> Printf.sprintf "Move (%d,%s,%s)" n (io_to_str no) 
    (io_to_str io)
  | Delete(n) -> Printf.sprintf "Delete (%d,0,0)" n
  
(* This algorithm is not taken directly from their paper, because the
 * version in their paper has bugs! *) 
let generate_script t1 t2 m = 
  let s = ref [] in 
  level_order_traversal t2 (fun y -> 
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
      | Some(x) -> begin
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

        | _, _ -> (* well, no parents implies no parents in the mapping *) 
           ()
           (* s := (Move(x,yparent,None)) :: !s *)
      end 
    end 
  ) ;
  level_order_traversal t1 (fun x ->
    if not (in_map_domain m x) then 
      s := (Delete(x.nid)) :: !s
  ) ;
  List.rev !s

(*************************************************************************)

let typelabel_ht = hcreate 255 
let inv_typelabel_ht = hcreate 255 
let typelabel_counter = ref 0 

let ast_stmt_id_to_node_id = hcreate 255 
let node_id_to_ast_stmt = hcreate 255 
let node_id_to_node = hcreate 255 

let node_of_nid x = hfind node_id_to_node x 

(* determine the 'typelabel' of a CIL Stmt -- basically, turn 
 *  if (x<y) { foo(); }
 * into:
 *  if (x<y) { }
 * and then hash it. 
 *) 

(* Apply a single edit operation to a file. This version if very fault
 * tolerant because we're expecting our caller (= a delta-debugging script)
 * to be throwing out parts of the diff script in an effort to minimize it.
 * So this is 'best effort'. *) 

let apply_diff m ast1 ast2 s =  
  try 
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
        ynode.children <- result 
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
  with e -> 
    printf "apply: exception: %s: %s\n" (edit_action_to_str s) 
    (Printexc.to_string e) ; exit 1 

let dummyBlock = nd({ blabels = []; battrs = [] ; bstmts = [] ; }  )
let dummyLoc = {lineno = -1; 
				filename = "";
				byteno = -1;
				ident = -1}
let dummyExp = nd(NOTHING)
let dummyStmt = nd(NOP(dummyLoc))
let dummyDt = nd(JUSTBASE)
let dummyName = nd("",dummyDt,[],dummyLoc)
let dummyIng = nd(nd([]),[])
let dummyNg = nd(nd([]),[])
let dummyIE = nd(NO_INIT)

let convert_tree (tree : tree node) : diff_tree_node = 
  let rec fc_dum fc =
	match (dn fc) with
	| FC_EXP(_) -> nd(FC_EXP(dummyExp))
	| FC_DECL(def) -> nd(FC_DECL(def_dum def))
  and fc_tl s =
	let dum = fc_dum s in
	  FORCLAUSE(dum), Pretty.sprint ~width:80 (d_fc () dum)
  and fc_children fc =
	match (dn fc) with
	  FC_EXP(exp) -> [| convert_exp exp |]
	| FC_DECL(def) -> [| convert_def def |]
  and type_spec_dum tc = 
	match (dn tc) with 
	| Tstruct(_) -> nd(Tstruct("",None,[]))
	| Tunion(_) -> nd(Tunion("",None,[]))
	| Tenum(_) -> nd(Tenum("",None,[]))
	| TtypeofE(_) -> nd(TtypeofE(dummyExp))
	| TtypeofT(_) -> nd(TtypeofT(nd([]),dummyDt))
	| _ -> tc
  and type_spec_tl tc = 
	let dum = type_spec_dum tc in
	  TYPESPEC(dum), Pretty.sprint ~width:80 (d_type_spec () dum)
  and type_spec_children tc =
	match (dn tc) with
	| Tstruct(_,Some(lst1),lst2) -> Array.of_list ((lmap convert_field_group lst1) @ (lmap convert_attr lst2))
	| Tunion(_,Some(lst1),lst2) -> Array.of_list ((lmap convert_field_group lst1) @ (lmap convert_attr lst2))
	| Tenum(_,Some(lst1),lst2) -> Array.of_list ((lmap convert_enum_item lst1) @ (lmap convert_attr lst2))
	| TtypeofE(exp) -> [| convert_exp exp |]
	| TtypeofT(spec, dtn) -> [| convert_spec spec; convert_dt dtn |]
	| _ -> [| |]
  and field_group_tl fg = 
	let dum = field_group_dum fg in 
	  FIELDGROUP(dum), Pretty.sprint ~width:80 (d_field_group () dum) 
  and field_group_dum fg = nd(nd([]), [])
  and field_group_children fg = 
	let sn, lst = dn fg in 
	  Array.of_list (lfoldl (fun accum -> fun (name,expo) -> 
							   let this = 
								 match expo with
								   Some(exp) -> [convert_exp exp; convert_name name]
								 | None -> [convert_name name]
							   in
								 accum @ this) [(convert_spec sn)] lst)
  and enum_item_children ei = 
	let str,enode,_ = dn ei in 
	  [| convert_exp enode |]
  and enum_item_dum ei = nd("",dummyExp,dummyLoc) (* FIXME: strings? Oh stuff and bother *)
  and enum_item_tl ei = 
	let dum = enum_item_dum ei in 
	  ENUMITEM(dum), Pretty.sprint ~width:80 (d_enum_item () dum)
  and asm_det_dum asmdet =
	match (dn asmdet) with
	  Some(_) -> nd(Some({aoutputs=[];ainputs=[];aclobbers=[]}))
	| None -> nd(None)
  and asm_det_tl a = 
	let dum = asm_det_dum a in
	  ASMDET(dum), Pretty.sprint ~width:80 (d_asm_det () dum)
  and asm_det_children asmdet =
	match (dn asmdet) with
	  Some({aoutputs=aoutputs;ainputs=ainputs;aclobbers=aclobbers}) ->
		Array.of_list ((lmap (fun (sopt,s,exp) -> convert_exp exp) aoutputs) @
						 (lmap (fun (sopt,s,exp) -> convert_exp exp) ainputs))
	| None -> [| |]
  and attr_dum a = nd(fst (dn a),(lmap (fun _ -> dummyExp) (snd (dn a))))
  and attr_tl a =
	let dum = attr_dum a in
	  ATTR(dum), Pretty.sprint ~width:80 (d_attr () (dum))
  and attr_children attr = 
	let (s,elist) = dn attr in
	  Array.of_list (lmap convert_exp elist)
		(* FIXME: does this make sense, or should I just return JUSTBASE for
		   all decltypes? *)
  and dt_dum dt =
	match (dn dt) with
	| JUSTBASE -> nd(JUSTBASE)
	| PARENTYPE(alist1,declt,alist2) -> nd(PARENTYPE([], nd(JUSTBASE), []))
	| ARRAY(declt,alist,exp) -> nd(ARRAY(nd(JUSTBASE), [], dummyExp))
	| PTR(alist,declt) -> nd(PTR([], nd(JUSTBASE)))
	| PROTO(decl,sns,b) -> nd(PROTO(nd(JUSTBASE), [], false))
  and dt_tl dt =
	let dum = dt_dum dt in
	  DECLT(dum), Pretty.sprint ~width:80 (d_decl_type () dum)
  and dt_children dt =
	match (dn dt) with
	| JUSTBASE -> [| |]
	| PARENTYPE(alist1,decl,alist2) ->
		Array.of_list ((lmap convert_attr alist1) @ ((convert_dt decl) :: (lmap convert_attr alist2)))
	| ARRAY(decl,alist,exp) ->
		Array.of_list ((convert_dt decl) :: ((lmap convert_attr alist) @ [(convert_exp exp)]))
	| PTR(alist,decl) ->
		Array.of_list ((lmap convert_attr alist) @ [(convert_dt decl)])
	| PROTO(decl,sns,b) -> 
		Array.of_list ((convert_dt decl) :: (lmap convert_sn sns))
  and name_dum name =
	let str,dtn,alist,_ = dn name in
	  nd("",dt_dum dtn,[],dummyLoc)
  and init_name_dum ing = 
	let name,ie = dn ing in
	  nd(name_dum name,ie_dum ie)
  and ing_dum ing = 
	let (spec,ns) = (dn ing) in
	  nd (spec_dum spec, [])
  and ing_tl ing = 
	let dum = ing_dum ing in
	  ING(dum), Pretty.sprint ~width:80 (d_init_name_group () dum) 
  and ing_children ing =
	let (spec,ns) = dn ing in
	  Array.of_list ((convert_spec spec) :: (lmap convert_init_name ns))
  and ng_dum ng =
	let (spec,ns) = dn ng in
	  nd(spec_dum spec, [])
  and ng_tl (ng : name_group node) =
	let dum = ng_dum ng in
	  NG(dum), Pretty.sprint ~width:80 (d_name_group () dum) 
  and ng_children ng =
	let (spec,ns) = dn ng in
	  Array.of_list ((convert_spec spec) :: (lmap convert_name ns))
  and sn_dum sn = 
	let (spec,name) = (dn sn) in
	  nd (spec_dum spec, name_dum name)
  and sn_tl sn = 
	let dum = sn_dum sn in
	  SN(dum), Pretty.sprint ~width:80 (d_single_name () dum)
  and sn_children sn = 
	let (spec,name) = dn sn in
	  [| convert_spec spec; convert_name name |]
  and spec_elem_dum se = 
	match (dn se) with
	| SpecAttr(attr) -> nd(SpecAttr(attr_dum attr))
	| SpecType(tspecn) -> nd(SpecType(type_spec_dum tspecn))
	| _ -> se
  and spec_dum specelems = nd(lmap spec_elem_dum (dn specelems))
  and spec_tl (s : specifier node) = 
	let dum = spec_dum s in
	  SPECS(dum), Pretty.sprint ~width:80 (d_specifier () dum)
  and spec_children specelems = 
	Array.of_list (lmap convert_spec_elem (dn specelems))
  and ie_dum ie =
	match (dn ie) with
	| NO_INIT -> nd(NO_INIT)
	| SINGLE_INIT(exp) -> nd(SINGLE_INIT(dummyExp))
	| COMPOUND_INIT(lst) -> nd(COMPOUND_INIT([]))
  and ie_tl ie = 
	let dum = ie_dum ie in
	  IE(dum), Pretty.sprint ~width:80 (d_init_expression () dum)
  and ie_children ie =
	match (dn ie) with
	| NO_INIT -> [| |]
	| SINGLE_INIT(exp) -> [| convert_exp exp |]
	| COMPOUND_INIT(lst) -> 
		Array.of_list 
		  (lfoldl (fun accum -> 
					 (fun(iw,ie) -> [convert_init_what iw; convert_ie ie] @ accum))
			 [] lst)
  and stmt_tl s = 
	let dum = stmt_dum s in
	  STMT(dum), Pretty.sprint ~width:80 (d_stmt () dum)
  and stmt_dum stmt =
	match (dn stmt) with
	  NOP(_) -> nd(NOP(dummyLoc))
	| COMPUTATION(_) -> nd(COMPUTATION(dummyExp,dummyLoc))
	| BLOCK(_) -> nd(BLOCK(dummyBlock,dummyLoc))
	| SEQUENCE(_) -> nd(SEQUENCE(dummyStmt,dummyStmt,dummyLoc))
	| IF(_) -> nd(IF(dummyExp,dummyStmt,dummyStmt,dummyLoc))
	| WHILE(_) -> nd(WHILE(dummyExp,dummyStmt,dummyLoc))
	| DOWHILE(_) -> nd(DOWHILE(dummyExp,dummyStmt,dummyLoc))
	| FOR(fc,_,_,_,_) -> nd(FOR(fc_dum fc,dummyExp,dummyExp,dummyStmt,dummyLoc))
	| BREAK(_) -> nd(BREAK(dummyLoc))
	| CONTINUE(_) -> nd(CONTINUE(dummyLoc))
	| RETURN(_) -> nd(RETURN(dummyExp,dummyLoc))
	| SWITCH(_) -> nd(SWITCH(dummyExp,dummyStmt,dummyLoc))
	| CASE(_) -> nd(CASE(dummyExp,dummyStmt,dummyLoc))
	| CASERANGE(_) -> nd(CASERANGE(dummyExp,dummyExp,dummyStmt,dummyLoc))
	| DEFAULT(_) -> nd(DEFAULT(dummyStmt,dummyLoc))
	| LABEL(str,_,_) -> nd(LABEL(str,dummyStmt,dummyLoc))
	| GOTO(str,_) -> nd(GOTO(str,dummyLoc))
	| COMPGOTO(_) -> nd(COMPGOTO(dummyExp,dummyLoc))
	| DEFINITION(d) -> nd(DEFINITION(def_dum d))
	| ASM(_) -> nd(ASM([],[],nd(None),dummyLoc))
	| TRY_EXCEPT(_) -> nd(TRY_EXCEPT(dummyBlock,dummyExp,dummyBlock,dummyLoc))
	| TRY_FINALLY(_) -> nd(TRY_FINALLY(dummyBlock,dummyBlock,dummyLoc))
  and exp_tl e =
	let dum = exp_dum e in
	  EXP(dum), Pretty.sprint ~width:80 (d_exp () dum)
  and exp_dum exp =
	match (dn exp) with
	  NOTHING -> nd(NOTHING)
	| UNARY(uop,_) -> nd(UNARY(uop,dummyExp))
	| LABELADDR(str) -> nd(LABELADDR(str))
	| BINARY(bop,_,_) -> nd(BINARY(bop,dummyExp,dummyExp))
	| QUESTION(_) -> nd(QUESTION(dummyExp,dummyExp,dummyExp))
	| CAST((spec,dtype),ie) -> nd(CAST((nd([]),dummyDt),dummyIE))
	| CALL(_) -> nd(CALL(dummyExp,[]))
	| COMMA(_) -> nd(COMMA([]))
	| CONSTANT(c) -> nd(CONSTANT(c) (* Maybe? *))
	| PAREN(_) -> nd(PAREN(dummyExp))
	| VARIABLE(str) -> nd(VARIABLE(str))
	| EXPR_SIZEOF(_) -> nd(EXPR_SIZEOF(dummyExp))
	| TYPE_SIZEOF(spec,dtype) -> nd(TYPE_SIZEOF(nd([]),dummyDt))
	| EXPR_ALIGNOF(_) -> nd(EXPR_ALIGNOF(dummyExp))
	| TYPE_ALIGNOF(spec,decl_type) -> nd(TYPE_ALIGNOF(nd([]),dummyDt))
	| INDEX(_) -> nd(INDEX(dummyExp,dummyExp))
	| MEMBEROF(_,str) -> nd(MEMBEROF(dummyExp,str))
	| MEMBEROFPTR(_,str) -> nd(MEMBEROFPTR(dummyExp,str))
	| GNU_BODY(_) -> nd(GNU_BODY(dummyBlock))
	| EXPR_PATTERN(str) -> nd(EXPR_PATTERN(str))
		(* I have no idea if the following is right *)
  and block_tl block = 
	let dum = block_dum block in
	  BLK(dum), Pretty.sprint ~width:80 (d_block () dum)
  and block_dum block = nd({(dn block) with battrs=[];bstmts=[]} )
  and def_tl def =
	let dum = def_dum def in
	  DEF(dum), Pretty.sprint ~width:80 (d_def () dum)
  and def_dum def =
	match (dn def) with
	  FUNDEF(_) -> nd(FUNDEF(nd(nd([]),dummyName),dummyBlock,dummyLoc,dummyLoc))
	| DECDEF(_) -> nd(DECDEF(dummyIng,dummyLoc))
	| TYPEDEF(_) -> nd(TYPEDEF(dummyNg,dummyLoc))
	| ONLYTYPEDEF(_) -> nd(ONLYTYPEDEF(nd([]),dummyLoc))
	| GLOBASM(str,_) -> nd(GLOBASM(str,dummyLoc))
	| PRAGMA(_) -> nd(PRAGMA(dummyExp,dummyLoc))
	| LINKAGE(str,_,_) -> nd(LINKAGE(str,dummyLoc,[]))
  and spec_elem_tl sen = 
	let dum = spec_elem_dum sen in
	  SPEC(dum), Pretty.sprint ~width:80 (d_spec_elem () dum)
  and spec_elem_children sen =
	match (dn sen) with
	| SpecAttr(attr) -> [| convert_attr attr |]
	| SpecType(tspecn) -> [| convert_type_spec tspecn |]
	| _ -> [| |]
  and name_tl name =
	let dum = name_dum name in 
	  NAME(dum), Pretty.sprint ~width:80 (d_name () dum)
  and name_children name = 
	let (str, dt, alist,_) =  (dn name) in
	  Array.of_list ((convert_dt dt) :: (lmap convert_attr alist))
  and init_name_tl iname =
	let name_dum = init_name_dum iname in 
	  INITNAME(name_dum), Pretty.sprint ~width:80 (d_init_name () name_dum)
  and init_name_children iname =
	let name,ie = dn iname in 
	  [| convert_name name; convert_ie ie |]
  and init_what_dum (what : initwhat node) = 
	match (dn what) with 
      NEXT_INIT -> what 
	| INFIELD_INIT(str,_) -> nd(INFIELD_INIT(str, nd(NEXT_INIT))) (* I don't know if this is sensible *)
		(* FIXME: how have I been handling strings, in general? *)
	| ATINDEX_INIT(_) -> nd(ATINDEX_INIT(dummyExp,nd(NEXT_INIT)))
	| ATINDEXRANGE_INIT(_) -> nd(ATINDEXRANGE_INIT(dummyExp,dummyExp)) (* FIXME: make a new dummyExp every time? *)
  and init_what_tl (what : initwhat node) =
	let dum = init_what_dum what in 
	  WHAT(dum), Pretty.sprint ~width:80 (d_init_what () dum)
  and init_what_children (what : initwhat node) =
	match (dn what) with
	  NEXT_INIT -> [| |]
	| INFIELD_INIT(str,what2) -> [| convert_init_what what2 |]
	| ATINDEX_INIT(expn,inode) -> [| convert_exp expn; convert_init_what inode |]
	| ATINDEXRANGE_INIT(e1,e2) -> [| convert_exp e1; convert_exp e2 |]
  and tree_node_tl def =
	let dum = tree_node_dum def in
	  TREENODE(dum), Pretty.sprint ~width:80 (d_tree_node () dum)
  and tree_node_dum tn =
	match (dn tn) with
	  (* FIXME: is this right, what we're doing here? *)
	| Globals(defs) -> nd(Globals([]))
	| Stmts(ss) -> nd(Stmts([]))
	| Exps(exps) -> nd(Exps([]) )
	| Syntax(str) -> nd(Syntax(str))
  and def_children def =
	match (dn def) with
	  FUNDEF(sn,b,_,_) -> [| convert_sn sn; convert_block b |]
	| DECDEF(ing,_) -> [| convert_ing ing |]
	| TYPEDEF(ng,_) -> [| convert_ng ng |]
	| ONLYTYPEDEF(spec,_) -> [| convert_spec spec |]
	| GLOBASM(str,_) -> [| |]
	| PRAGMA(exp,_) -> [| convert_exp exp |]
	| LINKAGE(str,_,defs) -> Array.of_list (lmap convert_def defs)
  and exp_children exp =
	match (dn exp) with
	| NOTHING -> [| |]
	| UNARY(_,exp) -> [| convert_exp exp |]
	| LABELADDR(str) -> [| |]
	| BINARY(bop,exp1,exp2) -> [| convert_exp exp1; convert_exp exp2 |]
	| QUESTION(exp1,exp2,exp3) ->  [| convert_exp exp1; convert_exp exp2; convert_exp exp3 |]
	| CAST((spec,decl),ie) -> [| convert_spec spec; convert_dt decl; convert_ie ie |]
	| CALL(exp,elist) -> Array.of_list (lmap convert_exp (exp :: elist))
	| COMMA(elist) -> Array.of_list (lmap convert_exp elist)
	| CONSTANT(c) -> [| |]
	| PAREN(exp) -> [| convert_exp exp |]
	| VARIABLE(str) -> [| |]
	| EXPR_SIZEOF(exp) -> [| convert_exp exp |]
	| TYPE_SIZEOF(spec,decl) -> [| convert_spec spec; convert_dt decl |]
	| EXPR_ALIGNOF(exp) -> [| convert_exp exp |]
	| TYPE_ALIGNOF(spec,decl) -> [| convert_spec spec; convert_dt decl |]
	| INDEX(exp1,exp2) -> [| convert_exp exp1; convert_exp exp2 |]
	| MEMBEROF(exp,str) ->  [| convert_exp exp |]
	| MEMBEROFPTR(exp,str) -> [| convert_exp exp |]
	| GNU_BODY(b) -> [| convert_block b |]
	| EXPR_PATTERN(str) -> [| |]
  and stmt_children stmt =
	match (dn stmt) with
	| COMPUTATION(exp,loc) -> [| convert_exp exp |]
	| BLOCK(b,loc) -> [| convert_block b |]
	| SEQUENCE(s1,s2,loc) -> [| convert_stmt s1; convert_stmt s2 |]
	| IF(exp,s1,s2,loc) -> [| convert_exp exp; convert_stmt s1; convert_stmt s2 |]
	| WHILE(exp,s1,loc) -> [| convert_exp exp; convert_stmt s1 |]
	| DOWHILE(exp,s1,loc) -> [| convert_exp exp; convert_stmt s1 |]
	| FOR(fc,exp1,exp2,s1,loc) -> [| convert_fc fc; convert_exp exp1; convert_exp exp2; convert_stmt s1|]
	| RETURN(e1,loc) -> [| convert_exp e1 |]
	| SWITCH(e1,s1,loc) -> [| convert_exp e1; convert_stmt s1 |]
	| CASE(e1,s1,loc) ->  [| convert_exp e1; convert_stmt s1 |]
	| CASERANGE(e1,e2,s1,loc) -> [| convert_exp e1; convert_exp e2; convert_stmt s1 |]
	| DEFAULT(s1,loc) -> [| convert_stmt s1|]
	| LABEL(str,s1,loc) -> [| convert_stmt s1 |]
	| GOTO(str,loc) -> [| |]
	| COMPGOTO(exp,loc) -> [| convert_exp exp |]
	| DEFINITION(def) -> [| convert_def def |]
	| ASM(alist,slist,adetails,loc) -> Array.of_list ((List.map convert_attr alist) @ [(convert_asm_det adetails)])
	| TRY_EXCEPT(b1,exp,b2,loc) -> [| convert_block b1; convert_exp exp; convert_block b2 |]
	| TRY_FINALLY(b1,b2,loc) -> [| convert_block b1; convert_block b2 |]
	| _ -> [| |]
  and tree_node_children tn =
	match (dn tn) with
	| Globals(defs) -> Array.of_list (lmap convert_def defs)
	| Stmts(ss) -> Array.of_list (lmap convert_stmt ss)
	| Exps(exps) -> Array.of_list (lmap convert_exp exps)
	| Syntax(str) -> [| |]
  and block_children block =
	let {blabels=blabels;battrs=battrs;bstmts=bstmts} = (dn block) in
	  Array.of_list ((lmap convert_attr battrs) @ (lmap convert_stmt bstmts))
  and tree_children tree = 
	Array.of_list (lmap convert_tree_node (snd (dn tree)))
  and convert_node (nodeid : int) (dum, tlabel : dummyNode * string) (children: diff_tree_node array) : diff_tree_node =
	let tl = ht_find typelabel_ht tlabel (fun x -> incr typelabel_counter;
											hadd inv_typelabel_ht !typelabel_counter dum ; 
											!typelabel_counter) in
	let n = new_node tl in
	  n.children <- children; 
	  hadd ast_stmt_id_to_node_id nodeid n.nid ;
	  hadd node_id_to_ast_stmt n.nid nodeid ;
	  hadd node_id_to_node n.nid n ;
	  n
  and convert_field_group node = convert_node node.id (field_group_tl node) (field_group_children node)
  and convert_enum_item node = convert_node node.id (enum_item_tl node) (enum_item_children node)
  and convert_spec_elem node = convert_node node.id (spec_elem_tl node) (spec_elem_children node)
  and convert_name node = convert_node node.id (name_tl node) (name_children node)
  and convert_init_name node = convert_node node.id (init_name_tl node) (init_name_children node)
  and convert_init_what (node : initwhat node) = convert_node node.id (init_what_tl node) (init_what_children node)
  and convert_asm_det node = convert_node node.id (asm_det_tl node) (asm_det_children node) 
  and convert_attr node = convert_node node.id (attr_tl node) (attr_children node)
  and convert_fc node = convert_node node.id (fc_tl node) (fc_children node)
  and convert_dt node = convert_node node.id (dt_tl node) (dt_children node)
  and convert_def node = convert_node node.id (def_tl node) (def_children node)
  and convert_ing node = convert_node node.id (ing_tl node) (ing_children node)
  and convert_ng node = convert_node node.id (ng_tl node) (ng_children node)
  and convert_sn node = convert_node node.id (sn_tl node) (sn_children node)
  and convert_stmt node = convert_node node.id (stmt_tl node) (stmt_children node)
  and convert_tree_node node = convert_node node.id (tree_node_tl node) (tree_node_children node)
  and convert_exp node = convert_node node.id (exp_tl node) (exp_children node)
  and convert_block node = convert_node node.id (block_tl node) (block_children node)
  and convert_spec node = convert_node node.id (spec_tl node) (spec_children node)
  and convert_ie node = convert_node node.id (ie_tl node) (ie_children node)
  and convert_type_spec node = convert_node node.id (type_spec_tl node) (type_spec_children node)
  in
  let dum = lmap tree_node_dum (snd (dn tree)) in 
	convert_node (tree.id) (TREE(dum), Pretty.sprint ~width:80 (d_tree () (fst (dn tree), dum))) (tree_children tree)

(* Generate a set of difference between two Cil files. Write the textual
 * diff script to 'diff_out', write the data files and hash tables to
 * 'data_out'. *) 

let gendiff f1 f2 diff_out data_out = 
  printf "diff: processing f1\n" ; flush stdout ; 
  let t1 = convert_tree f1 in
    printf "diff: processing f2\n" ; flush stdout ; 
	let t2 = convert_tree f2 in

	let data_ht = hcreate 255 in 
      printf "diff: \tmapping\n" ; flush stdout ; 
      let m = mapping t1 t2 in 
		NodeMap.iter (fun (a,b) ->
						printf "diff: \t\t%2d %2d\n" a.nid b.nid 
					 ) m ; 
		printf "Diff: \ttree t1\n" ; 
		print_tree t1 ; 
		printf "Diff: \ttree t2\n" ; 
		print_tree t2 ; 
		printf "diff: \tgenerating script\n" ; flush stdout ; 
		let s = generate_script t1 t2 m in 
          printf "diff: \tscript: %d\n" 
			(llen s) ; flush stdout ; 
          List.iter (fun ea ->
					   fprintf diff_out "%s %s\n" "FIXME" (edit_action_to_str ea) ;
					   printf "Script: %s %s\n" "FIXME" (edit_action_to_str ea)
					) s  ;
          hadd data_ht "FIXME" (m,t1,t2) ; 
		  Marshal.to_channel data_out data_ht [] ; 
		  Marshal.to_channel data_out inv_typelabel_ht [] ; 
		  Marshal.to_channel data_out f1 [] ;
		  Marshal.to_channel data_out node_id_to_node [] ; 
		  () 

(* Apply a (partial) diff script. *) 
let usediff diff_in data_in file_out = 
  let data_ht = Marshal.from_channel data_in in 
  let inv_typelabel_ht' = Marshal.from_channel data_in in 
  let copy_ht local global = 
    hiter (fun a b -> hadd global a b) local
  in
	copy_ht inv_typelabel_ht' inv_typelabel_ht ; 
	let f1 = Marshal.from_channel data_in in 
	let node_id_to_node' = Marshal.from_channel data_in in 
	  copy_ht node_id_to_node' node_id_to_node ; 

	  let patch_ht = Hashtbl.create 255 in
	  let add_patch fname ea = (* preserves order, fwiw *) 
		let sofar = try Hashtbl.find patch_ht fname with _ -> [] in
		  Hashtbl.replace patch_ht fname (sofar @ [ea]) 
	  in 

	  let num_to_io x = if x < 0 then None else Some(x) in 


		(try while true do
		   let line = input_line diff_in in
			 Scanf.sscanf line "%s %s (%d,%d,%d)" (fun fname ea a b c -> 
													 let it = match String.lowercase ea with 
													   | "insert" -> Insert(a, num_to_io b, num_to_io c) 
													   | "move" ->   Move(a, num_to_io b, num_to_io c)
													   | "delete" -> Delete(a) 
													   | _ -> failwith ("invalid patch: " ^ line)
													 in add_patch fname it 
												  ) 
		 done with End_of_file -> ()
		   (* printf "// %s\n" (Printexc.to_string e) *)
		) ; 

		let myprint glob = failwith "Not implemented"
(*		  ignore (Pretty.fprintf file_out "%a\n" dn_global glob)*)
		in 
(*
		  iterGlobals f1 (fun g1 ->
							match g1 with
							| GFun(fd1,l) -> begin
								let name = fd1.svar.vname in
								let patches = try Hashtbl.find patch_ht name with _ -> [] in
								  (*
									printf "// %s: %d patches\n" name (List.length patches) ; 
								  *)
								  if patches <> [] then begin
									let m, t1, t2 = Hashtbl.find data_ht name in 
									  printf "/* Tree t1:\n" ; 
									  print_tree t1; 
									  printf "*/\n" ; 
									  printf "/* Tree t2:\n" ; 
									  print_tree t2; 
									  printf "*/\n" ; 
									  List.iter (fun ea ->
												   printf "// %s\n" ( edit_action_to_str ea ) ; 
												   apply_diff m t1 t2 ea
												) patches ; 

									  cleanup_tree t1 ; 
									  let output_fundec = ast_to_fundec fd1 t1 in 

										myprint (GFun(output_fundec,l)) ; 
								  end else 
									myprint g1 
							  end

							| _ -> myprint g1 
						 ) ; *)

		  () 

let label_prefix = ref "" 

let node_number = ref 0


let generate f1 f2 = 
  let diffname = "FIXME" ^ ".diff" in 
  let diff_out = open_out diffname in 
  let data_out = open_out_bin "FIXME" in 
    gendiff f1 f2 diff_out data_out ;
    close_out diff_out ; 
    close_out data_out

let apply =
  let data_in = open_in_bin ("FIXME") in 
  let diff_in = open_in "FIXME"(* diff*) in 
  let file_out = stdout in 

	usediff diff_in data_in file_out 

(* process_diff takes the syntactic diff returned by svn diff and
 * splits it into old_file and new file, parses them, and them diffs
 * them to produce tree-based representations of the changes suitable
 * for comparison to other diffs *)

let tree_diff tree1 tree2 = failwith "Not implemented" 

let process_diff (syntactic : string list) =
  let old_file_str,new_file_str = 
	lfoldl
	  (fun (oldf,newf) ->
		 fun str ->
		   if Str.string_match at_regexp str 0 then oldf,newf
		   else if Str.string_match plus_regexp str 0 then oldf,(String.lchop str)^"\n"^newf
		   else if Str.string_match minus_regexp str 0 then (String.lchop str)^"\n"^oldf,newf
		   else (str^"\n"^oldf),(str^"\n"^newf)
	  ) ("","") syntactic 
  in 
  let old_file_tree,new_file_tree = (* will the diff files be backwards?  Double-check! *)
	  (fst (Diffparse.parse_from_string old_file_str)),
	  (fst (Diffparse.parse_from_string new_file_str))
	in
	  tree_diff old_file_tree new_file_tree
