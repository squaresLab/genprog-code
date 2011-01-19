open Batteries
open Pretty
open Printf
open Utils
open Globals
open Map
open Cabs
open Cabsvisit
open Cprint
open Diffparse
open Difftypes
open Convert
open Canon

(*************************************************************************)

(* XDiff algorithm: mostly taken from cdiff/the original paper, except
 * where Wes modified it to fix their bugs *)

exception Found_It 
exception Found_Node of diff_tree_node 

let deleted_node = {
  nid = -1;
  children = [| |] ;
  typelabel = -1 ;
  tl_str = "deleted";
  tl_node = DELETED ;
  original_node = DELETED;
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

let devnull = open_out "/dev/null"

let gendiff t1 t2 ?(print=false) ?(diff_out=devnull) ?(data_out=devnull) name = 
  let data_ht = hcreate 255 in 
  let m = mapping t1 t2 in 
(*	NodeMap.iter 
	  (fun (a,b) ->
		let stra = if !verbose then 
			begin
			  let node = node_of_nid a.nid in 
			  let tl = node.typelabel in
			  let n_str = Printf.sprintf "%2d: %d" a.nid tl in
				n_str ^ node.tl_str
			end 
		  else Printf.sprintf "%2d" a.nid
		in
		let strb = if !verbose then 
			begin
			  let node = node_of_nid b.nid in 
			  let tl = node.typelabel in
			  let n_str = Printf.sprintf "%2d: %d" b.nid tl in
				n_str ^ node.tl_str
			end 
		  else Printf.sprintf "%2d" b.nid
		in
		  printf "diff: \t\t%s %s\n" stra strb
	  ) m ;
	printf "Diff: \ttree t1\n" ; 
	print_tree t1 ; 
	printf "Diff: \ttree t2\n" ; 
	print_tree t2 ; *)
	printf "diff: \tgenerating script\n" ; flush stdout ; 
	let s = generate_script t1 t2 m in 
	  hadd data_ht name (m,t1,t2) ; 
	  if print then begin
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
(*  hclear typelabel_ht;
  typelabel_counter := 0;
  hclear cabs_stmt_id_to_node_id;
  hclear node_id_to_cabs_stmt;
  hclear node_id_to_diff_tree_node;
  node_counter := 0; FIXME: I think I don't actually want to do this *)
  let f1 =  ((diff_name^"1"), old_file_tree) in
  let f2 =  ((diff_name^"2"), new_file_tree) in 
  let t1 = tree_to_diff_tree f1 in
  let t2 = tree_to_diff_tree f2 in
  let diff = gendiff t1 t2 diff_name in
  let diff' = standardize_diff diff in
  let alpha = alpha_rename diff' in
	verbose := true;
	pprintf "Standard diff: \n";
	print_standard_diff diff';
	pprintf "Alpha-renamed diff: \n";
	print_standard_diff alpha;
	flush stdout;
	diff'

let tree_diff_change f1 f2 name = 
  let t1 = change_to_diff_tree f1 in 
  let t2 = change_to_diff_tree f2 in
  let diff = gendiff t1 t2 name in
  let diff' = standardize_diff diff in
  let alpha = alpha_rename diff' in
	pprintf "Standard diff: \n";
	print_standard_diff diff';
	pprintf "Alpha-renamed diff: \n";
	print_standard_diff alpha;
	flush stdout;
	diff'
	
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
	let patch = tree_diff_cabs old_file_tree new_file_tree "test_generate" in 
(*		pprintf "Printing standardized patch:\n";
		print_standard_diff patch; *)
(*	pprintf "\n\nTesting, using the diff:\n";
	apply "test_generate";*)
		pprintf "diff use testing turned off for brokenness\n"; flush stdout;
	pprintf "\n\n Done in test_diff\n\n"; flush stdout

let test_diff_change files =
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
		let diff = (tree_diff_cabs hd1 hd2 "test_diff_change") in
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
      hd1::hd2::tl -> (tree_diff_change hd1 hd2 "test_diff_change") :: diff_diff_pairs tl
	| [hd2] -> pprintf "Warning: odd-length diff list in test_diff_change\n"; flush stdout; []
	| [] -> []
  in
	pprintf "Step 3: diff pairs of diffs\n"; flush stdout;
  let diff_diffs = diff_diff_pairs diffs in 
	verbose := true;
	pprintf "Step 4: printing diff diffs\n"; flush stdout;
	liter (fun x -> pprintf "A DIFF:\n\n"; print_standard_diff x; pprintf "END A DIFF\n\n"; flush stdout) diff_diffs;
	pprintf "Done testing change diffing\n"; flush stdout
