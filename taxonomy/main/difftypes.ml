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
  | SInsert of diff_tree_node * diff_tree_node option * int option
  | SInsertTree of diff_tree_node * diff_tree_node option * int option
  | SMove of diff_tree_node * diff_tree_node option * int option
  | SDelete of diff_tree_node
  | SReplace of diff_tree_node * diff_tree_node

and changes = change list 

and dummyNode = 
  | DELETED
  | TREE of tree 
  | STMT of statement node
  | EXP of expression node
  | TREENODE of tree_node node
  | DEF of definition node
  | STRING of string
  | CHANGE of change
  | CHANGE_LIST of changes

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
	if cabsid > -1 then begin
	  pprintf "Adding cabs node %d (tlabel: %s) to tree, diff_tree_node n.nid: %d\n" cabsid tlabel n.nid;
	  hadd cabs_id_to_diff_tree_node cabsid n;
	end;
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

let standard_eas_to_str = function
  | SInsert(dt1,Some(dto),io) -> 
	Printf.sprintf "SInsert node %s at parent %s at position %s\n"  (Printf.sprintf "%d" dt1.nid) 
	  (Printf.sprintf "%d" dto.nid)
	  (io_to_str io)
  | SInsert(dt1,None,io) -> 
	Printf.sprintf "SInsert node %s at None at position %s\n" 
	  (Printf.sprintf "%d" dt1.nid)
	  (io_to_str io)
  | SInsertTree(dt1,Some(dto),io) ->
	Printf.sprintf "SInsertTree subtree %s at parent %s at position %s\n" 
	  (if !verbose then print_tree_str dt1 else Printf.sprintf "%d" dt1.nid) 
	  (Printf.sprintf "%d" dto.nid)
	  (io_to_str io)
  | SInsertTree(dt1,None,io) ->
	Printf.sprintf "SInsertTree subtree %s at None at position %s\n" 
	(if !verbose then print_tree_str dt1 else Printf.sprintf "%d" dt1.nid)
	  (io_to_str io)
  | SMove(dt1,Some(dto),io) ->
	Printf.sprintf "SMove subtree %s at parent %s at position %s\n" 
	  (if !verbose then print_tree_str dt1 else Printf.sprintf "%d" dt1.nid) 
	  (Printf.sprintf "%d" dto.nid)
	  (io_to_str io)
  | SMove(dt1,None,io) ->
	Printf.sprintf "SMove subtree %s at None at position %s\n" 
	  (if !verbose then print_tree_str dt1 else Printf.sprintf "%d" dt1.nid) 
	  (io_to_str io)
  | SDelete(dtn) ->
	  Printf.sprintf "SDelete subtree %s\n"
 		(if !verbose then print_tree_str dtn else Printf.sprintf "%d" dtn.nid) 
  | SReplace(dt1,dt2) ->
	Printf.sprintf "SReplace subtree %s with subtree %s\n" 
	  (if !verbose then print_tree_str dt1 else Printf.sprintf "%d" dt1.nid) 
	  (if !verbose then print_tree_str dt2 else Printf.sprintf "%d" dt2.nid)

let standard_eas_to_str2 = function
  | SInsert(dt1,Some(dto),io) -> 
	Printf.sprintf "SInsert node %s under parent %s at position %s"  (Printf.sprintf "%d" dt1.nid) (Printf.sprintf "%d" dto.nid) (io_to_str io)
  | SInsert(dt1,None,io) -> 
	Printf.sprintf "SInsert node %s under None at position %s)" (Printf.sprintf "%d" dt1.nid) (io_to_str io)
  | SInsertTree(dt1,Some(dto),io) ->
	Printf.sprintf "SInsertTree subtree %s under parent %s at position %s" (Printf.sprintf "%d" dt1.nid) (Printf.sprintf "%d" dto.nid) (io_to_str io)
  | SInsertTree(dt1,None,io) ->
	Printf.sprintf "SInsertTree subtree %s under None at position %s" (Printf.sprintf "%d" dt1.nid) (io_to_str io)
  | SMove(dt1,Some(dto),io) ->
	Printf.sprintf "SMove subtree %s under parent %s at position %s" (Printf.sprintf "%d" dt1.nid) (Printf.sprintf "%d" dto.nid) (io_to_str io)
  | SMove(dt1,None,io) ->
	Printf.sprintf "SMove subtree %s under None at position %s" (Printf.sprintf "%d" dt1.nid) (io_to_str io)
  | SDelete(dtn) ->
	  Printf.sprintf "SDelete subtree %s" (Printf.sprintf "%d" dtn.nid) 
  | SReplace(dt1,dt2) ->
	Printf.sprintf "SReplace subtree %s with subtree %s" (Printf.sprintf "%d" dt1.nid) (Printf.sprintf "%d" dt2.nid)

let print_dummy_node = function
  | DELETED -> pprintf "DELETED\n";
  | TREE(t) -> dumpTree defaultCabsPrinter (Pervasives.stdout) t
  | STMT(s) -> dumpStmt defaultCabsPrinter (Pervasives.stdout) 0 s
  | EXP(e) -> dumpExpression defaultCabsPrinter (Pervasives.stdout) 0 e
  | TREENODE(tn) -> dumpTreeNode defaultCabsPrinter (Pervasives.stdout) tn
  | DEF(def) -> dumpDefinition defaultCabsPrinter (Pervasives.stdout) def
  | STRING(str) -> pprintf "STRING: %s\n" str
  | CHANGE(c) -> pprintf "%s\n" (standard_eas_to_str2 c)
  | CHANGE_LIST(cs) -> liter (fun c -> pprintf "%s," (standard_eas_to_str2 c)) cs; pprintf "\n" 

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
