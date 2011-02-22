open Batteries
open Ref
open Utils
open Cprint
open Cabs

(* Conversion: We convert to a very generic tree data structure
 * (below) for the purposes of doing the DiffX structural difference
 * algorithm. *)

type diff_tree_node = {
  mutable nid : int ; (* unique per node *)
  mutable children : diff_tree_node array ;
  mutable typelabel : int ; 
  tl_str : string ;
  tl_node : dummyNode ;
  original_node : dummyNode ;
  (* two nodes that represent the same C statement will have the same
     typelabel. "children" are not considered for calculating typelabels,
     so 'if (x<y) { foo(); }' and 'if (x<y) { bar(); }' have the
     same typelabels, but their children (foo and bar) will not.  *) 
} 

and edit_action = 
  | Insert of int * (int option) * (int option)
  | Move   of int * (int option) * (int option)
  | Delete of int 

and change = 
  | SInsert of (int * dummyNode) * (int * dummyNode) option * int option
  | SInsertTree of (int * dummyNode) * (int * dummyNode) option * int option
  | SMove of (int * dummyNode) * (int * dummyNode) option * int option
  | SDelete of (int * dummyNode)
  | SReplace of (int * dummyNode) * (int * dummyNode)

and changes = change list 

and dummyNode = 
  | DELETED
  | TREE of tree 
  | STMT of statement node
  | EXP of expression node
  | DEF of definition node
  | CHANGE of change
  | CHANGE_LIST of changes
  | TREENODE of tree_node node

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

let typelabel_ht : (string, int) Hashtbl.t = hcreate 255 
let typelabel_counter = ref 0 
  
let node_id_to_diff_tree_node : (int, diff_tree_node) Hashtbl.t = hcreate 255 
let cabs_id_to_diff_tree_node : (int, diff_tree_node) Hashtbl.t = hcreate 255

let node_counter = ref 0 

let new_node typelabel str tl_node original_node = 
  let nid = !node_counter in
  incr node_counter ;
  { nid = nid ;
    children = [| |] ; 
    typelabel = typelabel ;
	tl_str = str ;
	tl_node = tl_node;
	original_node = original_node;
  }  

let node_of_nid x = hfind node_id_to_diff_tree_node x 

let node ?cabsid:(cabsid=(-1)) (tlabel : string) children (tl_node : dummyNode) (orig_node : dummyNode) = (* FIXME: I think the inv_typelabel_ht should have the dummyNode, not the original *)
  let tl_int = ht_find typelabel_ht tlabel (fun x -> pre_incr typelabel_counter) in
  let n = new_node tl_int tlabel tl_node orig_node in
	n.children <- children;
	hadd node_id_to_diff_tree_node n.nid n;
	if cabsid > -1 then 
	  hadd cabs_id_to_diff_tree_node cabsid n;
	n

let verbose = ref false

let noio = function
  | Some(n) -> Some(n.nid)
  | None -> None 

let io_to_str io = match io with
  | Some(n) -> Printf.sprintf "%d" n
  | None -> "-1" 

let io_to_str_verb = function
  | Some(n) -> 
	  let node = node_of_nid n in 
	  let tl = node.typelabel in
	  let n_str = Printf.sprintf "%d: %d" n tl in
	  n_str ^ node.tl_str
  | None -> "-1" 

let edit_action_to_str = function
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

let print_tree n = 
  let rec print n depth = 
    pprintf "%*s%02d (tl = %02d, str: %s) (%d children)\n" 
	  depth "" 
	  n.nid n.typelabel n.tl_str
	  (Array.length n.children) ;
    Array.iter (fun child ->
	  print child (depth + 2)
    ) n.children
  in
	print n 0 

let print_tree_str n = 
  let rec print n depth = 
	let str1 = 
    Printf.sprintf "%*s%02d (tl = %02d, str: %s) (%d children)\n" 
	  depth "" 
	  n.nid n.typelabel n.tl_str
	  (Array.length n.children) 
	in
	  Array.fold_left 
		(fun str -> 
		  fun child ->
			str ^ (print child (depth + 2))
		) str1 n.children
  in
	print n 0

let print_diffed_tree n = (* FIXME: this is kind of broken but whatever *)
  let rec print n depth = 
    pprintf "%*s%02d (tl = %02d, str: %s) (%d children)\n" 
      depth "" 
      n.nid n.typelabel n.tl_str
      (Array.length n.children) ;
    Array.iter (fun child ->
      print (hfind node_id_to_diff_tree_node child.nid) (depth + 2)
    ) n.children
  in
  print (hfind node_id_to_diff_tree_node n.nid) 0 

let rec dummy_node_to_str = function
  | DELETED -> "DELETED"
  | TREE(t) -> Pretty.sprint ~width:80 (d_tree () t)
  | STMT(s) -> Pretty.sprint ~width:80 (d_stmt () s)
  | EXP(e) -> Pretty.sprint ~width:80 (d_exp () e) 
  | DEF(def) -> Pretty.sprint ~width:80 (d_def () def) 
  | CHANGE(c) -> Printf.sprintf "%s\n" (standard_eas_to_str c)
  | CHANGE_LIST(cs) -> lfoldl (fun res -> fun c -> res^ Printf.sprintf "%s," (standard_eas_to_str c)) "" cs

and standard_eas_to_str = function
  | SInsert((dt1id,dt1),Some(dtoid,dto),io) -> 
	Printf.sprintf "SInsert node (%d: %s) at parent (%d: %s) at position %s\n"  
	  dt1id (dummy_node_to_str dt1)
	  dtoid (dummy_node_to_str dto)
	  (io_to_str io)
  | SInsert((dt1id,dt1),None,io) -> 
	Printf.sprintf "SInsert node (%d: %s) at None at position %s\n" 
	  dt1id (dummy_node_to_str dt1)
	  (io_to_str io)
  | SInsertTree((dt1id,dt1),Some(dtoid,dto),io) ->
	Printf.sprintf "SInsertTree subtree (%d: %s) at parent (%d: %s) at position %s\n" 
	  dt1id (dummy_node_to_str dt1) dtoid
	  (dummy_node_to_str dto)
	  (io_to_str io)
  | SInsertTree((dt1id,dt1),None,io) ->
	Printf.sprintf "SInsertTree subtree (%d: %s) at None at position %s\n" 
	  dt1id (dummy_node_to_str dt1)
	  (io_to_str io)
  | SMove((dt1id,dt1),Some(dtoid,dto),io) ->
	Printf.sprintf "SMove subtree (%d: %s) at parent (%d: %s) at position %s\n" 
	  dt1id (dummy_node_to_str dt1) dtoid
	  (dummy_node_to_str dto)
	  (io_to_str io)
  | SMove((dt1id,dt1),None,io) ->
	Printf.sprintf "SMove subtree (%d: %s) at None at position %s\n" 
	  dt1id (dummy_node_to_str dt1)
	  (io_to_str io)
  | SDelete(dtnid,dtn) ->
	  Printf.sprintf "SDelete subtree (%d: %s)\n"
		dtnid
	  (dummy_node_to_str dtn)
  | SReplace((dt1id,dt1),(dt2id,dt2)) ->
    let str1 = dummy_node_to_str dt1 in 
	let str2 = dummy_node_to_str dt2 in 
	Printf.sprintf "SReplace subtree (%d: %s) with subtree (%d: %s)\n" 
	  dt1id str1 dt2id str2

let print_dummy_node = function
  | DELETED -> pprintf "DELETED\n";
  | TREE(t) -> dumpTree defaultCabsPrinter (Pervasives.stdout) t
  | STMT(s) -> dumpStmt defaultCabsPrinter (Pervasives.stdout) 0 s
  | EXP(e) -> dumpExpression defaultCabsPrinter (Pervasives.stdout) 0 e
  | DEF(def) -> dumpDefinition defaultCabsPrinter (Pervasives.stdout) def
  | CHANGE(c) -> pprintf "%s\n" (standard_eas_to_str c)
  | CHANGE_LIST(cs) -> liter (fun c -> pprintf "%s," (standard_eas_to_str c)) cs; pprintf "\n" 


(* debugging stuff *)

let print_diff_tree_node node = 
  Printf.sprintf "%d" node.nid
let print_diff_tree_node_o node =
  match node with
	None -> "None"
  | Some(node) -> print_diff_tree_node node

let print_standard_diff patch = liter (fun x -> pprintf "%s\n" (standard_eas_to_str x)) patch

let insert_node_of_nid x = 
  let node = hfind node_id_to_diff_tree_node x in
  let node' = copy node in
	node'.children <- [| |]; node'
