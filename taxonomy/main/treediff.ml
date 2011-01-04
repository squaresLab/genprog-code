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
  | TREE of tree 
  | STMT of statement node
  | EXP of expression node
  | TREENODE of tree_node node
  | DEF of definition node

type diff_tree_node = {
  mutable nid : int ; (* unique per node *)
  mutable children : diff_tree_node array ;
  mutable typelabel : int ; 
  (* two nodes that represent the same C statement will have the same
     typelabel. "children" are not considered for calculating typelabels,
     so 'if (x<y) { foo(); }' and 'if (x<y) { bar(); }' have the
     same typelabels, but their children (foo and bar) will not.  *) 
} 

let typelabel_ht = hcreate 255 
let inv_typelabel_ht = hcreate 255 
let typelabel_counter = ref 0 

let node_id_to_ast_node = hcreate 255 
let node_id_to_diff_tree_node = hcreate 255 

let print_tree n = 
  let rec print n depth = 
    pprintf "%*s%02d (tl = %02d, str: %s) (%d children)\n" 
      depth "" 
      n.nid n.typelabel
	  (hfind inv_typelabel_ht n.typelabel)
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

let node_of_nid x = hfind node_id_to_diff_tree_node x 

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

let verbose = ref true

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

let io_to_str_verb io = match io with
  | Some(n) -> 
	  let node = node_of_nid n in 
	  let tl = node.typelabel in
	  let n_str = Printf.sprintf "%d: " n in
	  n_str ^ (hfind inv_typelabel_ht tl)
  | None -> "-1" 

let edit_action_to_str ea = match ea with
  | Insert(n,no,io) -> 
	  Printf.sprintf "Insert (%s,%s,%s)" 
		(if !verbose then io_to_str_verb (Some(n)) else Printf.sprintf "%d" n) (if !verbose then io_to_str_verb no else io_to_str no)
		(io_to_str io)
  | Move(n,no,io) -> 
	  Printf.sprintf "Move (%s,%s,%s)" 
		(if !verbose then io_to_str_verb (Some(n)) else Printf.sprintf "%d" n) 
		(if !verbose then io_to_str_verb no else io_to_str no) 
		(io_to_str io)
  | Delete(n) -> 
	  Printf.sprintf "Delete (%s,0,0)"
 		(if !verbose then io_to_str_verb (Some(n)) else Printf.sprintf "%d" n) 

  
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

let dummyBlock = { blabels = []; battrs = [] ; bstmts = [] ; }
let dummyLoc = {lineno = -1; 
				filename = "";
				byteno = -1;
				ident = -1}
let dummyExp = nd(NOTHING)
let dummyStmt = nd(NOP(dummyLoc))
let dummyDt = JUSTBASE
let dummyName = ("",dummyDt,[],dummyLoc)
let dummyIng = ([],[])
let dummyNg = ([],[])
let dummyIE = NO_INIT
let dummyFC = FC_EXP(dummyExp)

let convert_tree (tree : tree) : diff_tree_node = 
  let rec fc_children fc =
	match fc with
	  FC_EXP(exp) -> [| convert_exp exp |]
	| FC_DECL(def) -> [| convert_def def |]
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
	| FOR(_) -> nd(FOR(dummyFC,dummyExp,dummyExp,dummyStmt,dummyLoc))
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
	| ASM(_) -> nd(ASM([],[],None,dummyLoc))
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
	| CAST((spec,dtype),ie) -> nd(CAST(([],dummyDt),dummyIE))
	| CALL(_) -> nd(CALL(dummyExp,[]))
	| COMMA(_) -> nd(COMMA([]))
	| CONSTANT(c) -> nd(CONSTANT(c))
	| PAREN(_) -> nd(PAREN(dummyExp))
	| VARIABLE(str) -> nd(VARIABLE(str))
	| EXPR_SIZEOF(_) -> nd(EXPR_SIZEOF(dummyExp))
	| TYPE_SIZEOF(spec,dtype) -> nd(TYPE_SIZEOF([],dummyDt))
	| EXPR_ALIGNOF(_) -> nd(EXPR_ALIGNOF(dummyExp))
	| TYPE_ALIGNOF(spec,decl_type) -> nd(TYPE_ALIGNOF([],dummyDt))
	| INDEX(_) -> nd(INDEX(dummyExp,dummyExp))
	| MEMBEROF(_,str) -> nd(MEMBEROF(dummyExp,str))
	| MEMBEROFPTR(_,str) -> nd(MEMBEROFPTR(dummyExp,str))
	| GNU_BODY(_) -> nd(GNU_BODY(dummyBlock))
	| EXPR_PATTERN(str) -> nd(EXPR_PATTERN(str))
		(* I have no idea if the following is right *)
  and def_tl def =
	let dum = def_dum def in
	  DEF(dum), Pretty.sprint ~width:80 (d_def () dum)
  and def_dum def =
	match (dn def) with
	  FUNDEF(_) -> nd(FUNDEF(([],dummyName),dummyBlock,dummyLoc,dummyLoc))
	| DECDEF(_) -> nd(DECDEF(dummyIng,dummyLoc))
	| TYPEDEF(_) -> nd(TYPEDEF(dummyNg,dummyLoc))
	| ONLYTYPEDEF(_) -> nd(ONLYTYPEDEF([],dummyLoc))
	| GLOBASM(str,_) -> nd(GLOBASM(str,dummyLoc))
	| PRAGMA(_) -> nd(PRAGMA(dummyExp,dummyLoc))
	| LINKAGE(str,_,_) -> nd(LINKAGE(str,dummyLoc,[]))
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
  and exp_children exp =
	match (dn exp) with
	| UNARY(_,exp) -> [| convert_exp exp |]
	| BINARY(bop,exp1,exp2) -> [| convert_exp exp1; convert_exp exp2 |]
	| QUESTION(exp1,exp2,exp3) ->  [| convert_exp exp1; convert_exp exp2; convert_exp exp3 |]
	| CAST((spec,decl),ie) -> Array.concat [(spec_children spec); (decl_children decl); (ie_children ie)]
	| CALL(exp,elist) -> Array.of_list (lmap convert_exp (exp :: elist))
	| COMMA(elist) -> Array.of_list (lmap convert_exp elist)
	| PAREN(exp) -> [| convert_exp exp |]
	| EXPR_SIZEOF(exp) -> [| convert_exp exp |]
	| TYPE_SIZEOF(spec,decl) -> Array.append (spec_children spec) (decl_children decl)
	| EXPR_ALIGNOF(exp) -> [| convert_exp exp |]
	| TYPE_ALIGNOF(spec,decl) ->  Array.append (spec_children spec) (decl_children decl)
	| INDEX(exp1,exp2) -> [| convert_exp exp1; convert_exp exp2 |]
	| MEMBEROF(exp,str) ->  [| convert_exp exp |]
	| MEMBEROFPTR(exp,str) -> [| convert_exp exp |]
	| GNU_BODY(b) -> Array.of_list (lmap convert_stmt b.bstmts)
	| _ -> [| |]
  and stmt_children stmt =
	match (dn stmt) with
	| COMPUTATION(exp,loc) -> [| convert_exp exp |]
	| BLOCK(b,loc) -> Array.of_list (lmap convert_stmt b.bstmts) 
	| SEQUENCE(s1,s2,loc) -> [| convert_stmt s1; convert_stmt s2 |]
	| IF(exp,s1,s2,loc) -> [| convert_exp exp; convert_stmt s1; convert_stmt s2 |]
	| WHILE(exp,s1,loc) -> [| convert_exp exp; convert_stmt s1 |]
	| DOWHILE(exp,s1,loc) -> [| convert_exp exp; convert_stmt s1 |]
	| FOR(fc,exp1,exp2,s1,loc) -> Array.append (fc_children fc) [| convert_exp exp1; convert_exp exp2; convert_stmt s1|]
	| RETURN(e1,loc) -> [| convert_exp e1 |]
	| SWITCH(e1,s1,loc) -> [| convert_exp e1; convert_stmt s1 |]
	| CASE(e1,s1,loc) ->  [| convert_exp e1; convert_stmt s1 |]
	| CASERANGE(e1,e2,s1,loc) -> [| convert_exp e1; convert_exp e2; convert_stmt s1 |]
	| DEFAULT(s1,loc) -> [| convert_stmt s1|]
	| LABEL(str,s1,loc) -> [| convert_stmt s1 |]
	| COMPGOTO(exp,loc) -> [| convert_exp exp |]
	| DEFINITION(def) -> [| convert_def def |]
	| ASM(alist,slist,adetails,loc) -> 
		Array.append (Array.concat (lmap attr_children alist)) (asm_det_children adetails)
	| TRY_EXCEPT(b1,exp,b2,loc) -> Array.of_list ((lmap convert_stmt b1.bstmts) @ ( convert_exp exp :: (lmap convert_stmt b2.bstmts)))
	| TRY_FINALLY(b1,b2,loc) -> Array.of_list ((lmap convert_stmt b1.bstmts) @ (lmap convert_stmt b2.bstmts))
	| _ -> [| |]
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
	  FUNDEF(sn,b,_,_) ->  Array.of_list (lmap convert_stmt b.bstmts)
	| DECDEF(ing,_) -> ing_children ing 
	| TYPEDEF(ng,_) -> ng_children ng
	| ONLYTYPEDEF(spec,_) -> spec_children spec 
	| GLOBASM(str,_) -> [| |]
	| PRAGMA(exp,_) -> [| convert_exp exp |]
	| LINKAGE(str,_,defs) -> Array.of_list (lmap convert_def defs)
  and tree_node_children tn =
	match (dn tn) with
	| Globals(defs) -> Array.of_list (lmap convert_def defs)
	| Stmts(ss) -> Array.of_list (lmap convert_stmt ss)
	| Exps(exps) -> Array.of_list (lmap convert_exp exps)
	| Syntax(str) -> [| |]
  and tree_children tree = 
	Array.of_list (lmap convert_tree_node (snd tree))
  and convert_node node (dum, tlabel : dummyNode * string) (children: diff_tree_node array) : diff_tree_node =
	let tl = ht_find typelabel_ht tlabel (fun x -> incr typelabel_counter;
											hadd inv_typelabel_ht !typelabel_counter tlabel ; 
											!typelabel_counter) in
	let n = new_node tl in
	  n.children <- children; 
	  hadd node_id_to_ast_node n.nid node ;
	  hadd node_id_to_diff_tree_node n.nid n ;
	  n
  and convert_def node = convert_node (DEF(node)) (def_tl node) (def_children node)
  and convert_stmt node = convert_node (STMT(node)) (stmt_tl node) (stmt_children node)
  and convert_tree_node node = convert_node (TREENODE(node)) (tree_node_tl node) (tree_node_children node)
  and convert_exp node = convert_node (EXP(node)) (exp_tl node) (exp_children node)
  in
  let dum = (fst tree, lmap tree_node_dum (snd tree)) in 
	convert_node (TREE(tree)) (TREE(dum), Pretty.sprint ~width:80 (d_tree () dum)) (tree_children tree)

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
          liter (fun ea ->
				   fprintf diff_out "%s\n" (edit_action_to_str ea) ;
				   printf "Script: %s\n" (edit_action_to_str ea)
				) s  ;
		  
          hadd data_ht "FIXME" (m,t1,t2) ; 
		  Marshal.to_channel data_out data_ht [] ; 
		  Marshal.to_channel data_out inv_typelabel_ht [] ; 
		  Marshal.to_channel data_out f1 [] ;
		  Marshal.to_channel data_out node_id_to_diff_tree_node [] ; 
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

let generate name f1 f2 = 
  let diffname = name ^ ".diff" in 
  let diff_out = open_out diffname in 
  let data_out = open_out_bin name in 
    gendiff f1 f2 diff_out data_out ;
    close_out diff_out ; 
    close_out data_out

let apply name =
  let data_in = open_in_bin name in 
  let diff_in = open_in (name ^ ".diff") in
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
	 fst (Diffparse.parse_from_string old_file_str),
	  fst (Diffparse.parse_from_string new_file_str)
	in
	  generate "test_generate" ("diff2", old_file_tree) ("diff1", new_file_tree)

let test_diff diff1 diff2 =
  let old_file_tree, new_file_tree =
	fst (Diffparse.parse_file diff1), fst (Diffparse.parse_file diff2) in
	Printf.printf "tree1:\n";
	dumpTree defaultCabsPrinter (Pervasives.stdout) ("foo",old_file_tree);
	Printf.printf "\ntree2:\n";
	dumpTree defaultCabsPrinter (Pervasives.stdout) ("foo",new_file_tree);
	Printf.printf "\n\n"; flush stdout;
	generate "test_generate" (diff1, old_file_tree) (diff2, new_file_tree)
