open Batteries
open Utils
open Pretty
open Printf
open Map
open Cil
open Global

(* Toggled if an exception is thrown in applydiff,
 * meaning that the line being removed by minimization
 * is necessary. *)
let applydiff_exception = ref false

let verbose = ref false

(**** For doing line numbers of nodes... ****)
class statementLinesPrinter = object
  inherit nopCilVisitor
    (* Visit each expression, etc... print current location's line *)
  method vexpr e =
    Printf.printf "Line %d\n" !currentLoc.line; DoChildren
  method vinst i =
    Printf.printf "Line %d\n" !currentLoc.line; DoChildren
  method vblock b =
    Printf.printf "Line %d\n" !currentLoc.line; DoChildren
end

let my_stmt_line_printer = new statementLinesPrinter

let last_good_line = ref 0

(* Called by lineRangeVisitor to get the line numbers from the original file. *)
let lineRangeMethod ht id currentLoc = begin
    let (lr,_) = (Hashtbl.find ht id) in
    let theLines = ref lr in
    let my_line = (* !currentLoc.line *)
      if ((String.length (currentLoc.file))!=0 && (String.get (currentLoc.file) 0)!='/') then last_good_line := currentLoc.line;
      !last_good_line
    in 
    theLines := (my_line :: !theLines);
    Hashtbl.replace ht id (!theLines,currentLoc.file)
end

class lineRangeVisitor id ht = object
  inherit nopCilVisitor
  method vexpr e =
    lineRangeMethod ht id !currentLoc;
    DoChildren
  method vinst i =
    lineRangeMethod ht id !currentLoc;
    DoChildren
  method vblock b =
    lineRangeMethod ht id !currentLoc;
    DoChildren
end

let my_line_range_visitor = new lineRangeVisitor

(* This makes a deep copy of an arbitrary Ocaml data structure *) 
let copy (x : 'a) = 
  let str = Marshal.to_string x [] in
  (Marshal.from_string str 0 : 'a) 

type node_id = int 

(*
 * We convert to a very generic tree data structure (below) for the
 * purposes of doing the DiffX structural difference algorithm. Then we
 * convert back later after applying the diff script. 
 *)
type tree_node = {
  mutable nid : node_id ; (* unique per node *)
  mutable children : int array ;
  mutable typelabel : int ; 
  (* two nodes that represent the same C statement will have the same
     typelabel. "children" are not considered for calculating typelabels,
     so 'if (x<y) { foo(); }' and 'if (x<y) { bar(); }' have the
     same typelabels, but their children (foo and bar) will not.  *) 

} 

let typelabel_ht = Hashtbl.create 255 
let inv_typelabel_ht = Hashtbl.create 255 
let typelabel_counter = ref 0 


  (* Records the filename and line beginning/ending lines of a node *)
let verbose_node_info = Hashtbl.create 255
let node_id_to_cil_stmt : (int, Cil.stmt) Hashtbl.t = Hashtbl.create 255
  (* Intermediary steps for verbose_node_info *)
let node_id_to_line_list_fn = Hashtbl.create 255

let node_of_nid node_map x = IntMap.find x node_map

let print_tree node_map (n : tree_node) = 
  let rec print n depth = 
    printf "%*s%02d (tl = %02d) (%d children)\n" 
      depth "" 
      n.nid n.typelabel
      (Array.length n.children) ;
    Array.iter (fun child ->
      let child = node_of_nid node_map child in
      print child (depth + 2)
    ) n.children
  in
  print n 0 

let deleted_node = {
  nid = -1;
  children = [| |] ;
  typelabel = -1 ;
} 

let init_map () = IntMap.add (-1) deleted_node (IntMap.empty)

let rec cleanup_tree node_map t =
  let node_map =
	Array.fold_left
	  (fun node_map ->
		fun child ->
		  let child = node_of_nid node_map child in
			cleanup_tree node_map child
	  ) node_map (t.children)
  in
  let lst = Array.to_list t.children in
  let lst = List.filter (fun child ->
	let child = node_of_nid node_map child in
    child.typelabel <> -1
  ) lst in
  t.children <- Array.of_list lst;
	IntMap.add (t.nid) t node_map

let delete node_map node =
  let nid = node.nid in 
  node.nid <- -1 ; 
  node.children <- [| |] ; 
  node.typelabel <- -1 ;
  IntMap.add nid node node_map

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
    type t = tree_node
    let compare x y = compare x.nid y.nid
  end
module OrderedNodeNode =
  struct
    type t = tree_node * tree_node
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
exception Found_Node of tree_node 

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
let rec nodes_in_tree_equal_to node_map t n = 
  let sofar = ref 
    (if nodes_eq t n then NodeSet.singleton t else NodeSet.empty)
  in 
  Array.iter (fun child ->
	let child = node_of_nid node_map child in
    sofar := NodeSet.union !sofar (nodes_in_tree_equal_to node_map child n) 
  ) t.children ; 
  !sofar 

let map_size m = NodeMap.cardinal m 

let level_order_traversal node_map t callback =
  let q = Queue.create () in 
  Queue.add t q ; 
  while not (Queue.is_empty q) do
    let x = Queue.take q in 
    Array.iter (fun child ->
	  let child = node_of_nid node_map child in
      Queue.add child q
    ) x.children ; 
    callback x ; 
  done 

let parent_of node_map tree some_node =
  try 
    level_order_traversal node_map tree (fun p ->
      Array.iter (fun child ->
		let child = node_of_nid node_map child in 
          if child.nid = some_node.nid then
			raise (Found_Node(p) )
      ) p.children 
    ) ;
    None
  with Found_Node(n) -> Some(n) 

let parent_of_nid node_map tree some_nid =
  try 
    level_order_traversal node_map tree (fun p ->
      Array.iter (fun child ->
		let child = node_of_nid node_map child in
        if child.nid = some_nid then
          raise (Found_Node(p) )
      ) p.children 
    ) ;
    None
  with Found_Node(n) -> Some(n) 

let position_of node_map (parent : tree_node option) child =
  match parent with
  | None -> None
  | Some(parent) -> 
    let result = ref None in 
    Array.iteri (fun i child' ->
	  let child' = node_of_nid node_map child' in 
      if child.nid = child'.nid then
        result := Some(i) 
    ) parent.children ;
    !result 

let position_of_nid node_map (parent : tree_node option) child_nid =
  match parent with
  | None -> None
  | Some(parent) -> 
    let result = ref None in 
    Array.iteri (fun i child' ->
	  let child' = node_of_nid node_map child' in
      if child_nid = child'.nid then
        result := Some(i) 
    ) parent.children ;
    !result 

(* This is the DiffX algorithm, taken verbatim from their paper *) 
let rec mapping node_map t1 t2 =
  let t1 = node_of_nid node_map t1 in 
  let t2 = node_of_nid node_map t2 in
  let m = ref NodeMap.empty in 
  level_order_traversal node_map t1 (fun x -> 
    if in_map_domain !m x then
      () (* skip current node *)
    else begin
      let y = nodes_in_tree_equal_to node_map t2 x in 
      let m'' = ref NodeMap.empty in 
      NodeSet.iter (fun yi ->
        if not (in_map_range !m yi) then begin
          let m' = ref NodeMap.empty in 
          match_fragment node_map x yi !m m' ;
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
and match_fragment node_map x y (m : NodeMap.t) (m' : NodeMap.t ref) = 
  if (not (in_map_domain m x)) &&
     (not (in_map_range m y)) &&
     (nodes_eq x y) then begin
    m' := NodeMap.add (x,y) !m' ;
    let xc = Array.length x.children in 
    let yc = Array.length y.children in 
    for i = 0 to pred (min xc yc) do
      match_fragment node_map (node_of_nid node_map x.children.(i)) (node_of_nid node_map y.children.(i)) m m'
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
  | Some(n) -> sprintf "%d" n
  | None -> "-1" 

let edit_action_to_str ea = match ea with
  | Insert(n,no,io) -> sprintf "Insert (%d,%s,%s)" n (io_to_str no)
    (io_to_str io)
  | Move(n,no,io) -> sprintf "Move (%d,%s,%s)" n (io_to_str no) 
    (io_to_str io)
  | Delete(n) -> sprintf "Delete (%d,0,0)" n

(* Prints an edit action more verbosely - with line numbers. *)
(* TODO: Derive line numbers for child inserts (or make a best guess...) *)
let edit_action_to_str_verbose () = 

(*

match ea with
  | Insert(n,no,io) -> sprintf "Insert\n";
    let info = Hashtbl.find n 
    Printf.printf "%d\n" num;
    let s_id = Hashtbl.find node_id_to_cil_stmt_id num in
    sprintf "Node %d: %d\n" n (Hashtbl.find stmtids_to_lines s_id)
  | Move(n,no,io) -> sprintf "Move (%d,%s,%s)" n (io_to_str no) 
    (io_to_str io)
  | Delete(n) -> sprintf "Delete (%d,0,0)" n

    *)
(*
 Hashtbl.iter (fun x y -> Printf.printf "Node %d is Stmt %d\n" x y) node_id_to_cil_stmt_id ;
 Hashtbl.iter (fun x y -> Printf.printf "Stmt %d is Node %d\n" x y) cil_stmt_id_to_node_id ;
*)
(*
 Hashtbl.iter (fun x y -> Printf.printf "Node %d:\n" x;
                Pretty.printf "%a\n" dn_stmt y; ()) node_id_to_cil_stmt;
*)
Hashtbl.iter (fun x (fn,min,max) ->
                  Printf.printf "Node %d: %s %d %d\n" x fn min max) verbose_node_info
  
(* This algorithm is not taken directly from their paper, because the
 * version in their paper has bugs! *) 
let generate_script node_map t1 t2 m = 
  let s = ref [] in 
  level_order_traversal node_map t2 (fun y -> 
    if not (in_map_range m y) then begin
      let yparent = parent_of node_map t2 y in 
      let ypos = position_of node_map yparent y in
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
      | None -> printf "generate_script: error: no node that maps to!\n"
      | Some(x) -> begin
        let xparent = parent_of node_map t1 x in
        let yparent = parent_of node_map t2 y in 
        let yposition = position_of node_map yparent y in 
        let xposition = position_of node_map xparent x in 
        match xparent, yparent with
        | Some(xparent), Some(yparent) -> 
          if not (NodeMap.mem (xparent,yparent) m) then begin 
            let xx = find_node_that_maps_to m yparent in
            match xx with
            | Some(xx) -> s := (Move(x.nid,Some(xx.nid),yposition)) :: !s 
            | None     -> s := (Move(x.nid,Some yparent.nid,yposition)) :: !s
          end 
	  else if xposition <> yposition then 
            s := (Move(x.nid,Some xparent.nid,yposition)) :: !s

        | _, _ -> (* well, no parents implies no parents in the mapping *) 
           ()
           (* s := (Move(x,yparent,None)) :: !s *)
      end 
    end 
  ) ;
  level_order_traversal node_map t1 (fun x ->
    if not (in_map_domain m x) then begin
      s := (Delete(x.nid)) :: !s
    end
  ) ;
  List.rev !s

(*************************************************************************)
let dummyBlock = { battrs = [] ; bstmts = [] ; }  
let dummyLoc = { line = 0 ; file = "" ; byte = 0; } 

(* determine the 'typelabel' of a CIL Stmt -- basically, turn 
 *  if (x<y) { foo(); }
 * into:
 *  if (x<y) { }
 * and then hash it. 
 *) 
let stmt_to_typelabel (s : Cil.stmt) = 
  let convert_label l = match l with
    | Label(s,loc,b) -> Label(s,dummyLoc,b) 
    | Case(e,loc) -> Case(e,dummyLoc)
    | Default(loc) -> Default(dummyLoc)
  in 
  let labels = List.map convert_label s.labels in
  let convert_il il = 
    List.map (fun i -> match i with
      | Set(lv,e,loc) -> Set(lv,e,dummyLoc)
      | Call(None,Lval(Var(vi),o),el,loc) when vi.vname = "log_error_write" -> 
		Call(None,Lval(Var(vi),o),[],dummyLoc) 
      | Call(lvo,e,el,loc) -> Call(lvo,e,el,dummyLoc) 
      | Asm(a,b,c,d,e,loc) -> Asm(a,b,c,d,e,dummyLoc)
    ) il 
  in
  let skind = match s.skind with
    | Instr(il)  -> Instr(convert_il il) 
    | Return(eo,l) -> Return(eo,dummyLoc) 
    | Goto(sr,l) -> Goto(sr,dummyLoc) 
    | Break(l) -> Break(dummyLoc) 
    | Continue(l) -> Continue(dummyLoc) 
    | If(e,b1,b2,l) -> If(e,dummyBlock,dummyBlock,l)
    | Switch(e,b,sl,l) -> Switch(e,dummyBlock,[],l) 
    | Loop(b,l,so1,so2) -> Loop(dummyBlock,l,None,None) 
    | Block(block) -> Block(dummyBlock) 
    | TryFinally(b1,b2,l) -> TryFinally(dummyBlock,dummyBlock,dummyLoc) 
    | TryExcept(b1,(il,e),b2,l) ->
      TryExcept(dummyBlock,(convert_il il,e),dummyBlock,dummyLoc) 
  in
  let it = (labels, skind) in 
  let s' = { s with skind = skind ; labels = labels } in 
  let doc = dn_stmt () s' in 
  let str = Pretty.sprint ~width:80 doc in 
  if Hashtbl.mem typelabel_ht str then begin 
    Hashtbl.find typelabel_ht str , it
  end else begin
    let res = !typelabel_counter in
    incr typelabel_counter ; 
    Hashtbl.add typelabel_ht str res ; 
    Hashtbl.add inv_typelabel_ht res it ; 
    res , it
  end 

let wrap_block b = mkStmt (Block(b))

(* Builds a (file,minline,maxline) tuple and adds it to the verbose HT *)
let build_node_tuple id =
  if (Hashtbl.mem node_id_to_line_list_fn id) then begin 
    let (lr,f) = (Hashtbl.find node_id_to_line_list_fn id) in
    if (List.length lr)!=0 then begin
      let lineRange = ref lr in
      lineRange := (List.sort ?cmp:(Some(fun x y -> x - y)) !lineRange);
      let min = (List.hd !lineRange) in
      let max = (List.nth !lineRange ((List.length !lineRange)-1)) in
      Hashtbl.add verbose_node_info id (f,min,max)
     end
     else
      Hashtbl.add verbose_node_info id (f,0,0)
  end


(* the bitch of this is that all these convert-to-ast functions now need to
   return both the id and the new node map (where before, state was our
   friend *)

let fundec_to_ast node_map (f:Cil.fundec) =
  let node_map = ref node_map in
  let rec stmt_to_node s =
  let tl, (labels,skind) = stmt_to_typelabel s in
  let n = new_node tl in 
  (* now just fill in the children *) 
  let children = 
	match s.skind with
    | Instr _  | Return _ | Goto _ 
    | Break _  | Continue _  -> [| |]
    | If(e,b1,b2,l)  ->
	  [| stmt_to_node (wrap_block b1) ;stmt_to_node (wrap_block b2) |]
    | Switch(e,b,sl,l) -> 
       [| stmt_to_node (wrap_block b) |]
    | Loop(b,l,so1,so2) -> 
       [| stmt_to_node (wrap_block b) |] 
    | TryFinally(b1,b2,l) -> 
       [| stmt_to_node (wrap_block b1) ; stmt_to_node (wrap_block b2) |] 
    | TryExcept(b1,(il,e),b2,l) ->
       [| stmt_to_node (wrap_block b1) ; stmt_to_node (wrap_block b2) |] 
    | Block(block) -> 
       (* Printf.printf "HELLO!\n"; *)
       let children = List.map stmt_to_node block.bstmts in
       Array.of_list children 
  in
  n.children <- children ;
  let emptyLineList = ref [] in
  Hashtbl.add node_id_to_line_list_fn n.nid (!emptyLineList,"");
	ignore(visitCilStmt (my_line_range_visitor n.nid node_id_to_line_list_fn) s) ;
  build_node_tuple n.nid;
  Hashtbl.add node_id_to_cil_stmt n.nid s;
  node_map := IntMap.add n.nid n !node_map ;

  let s' = { s with skind = skind ; labels = labels } in 
	ignore (if !verbose then begin
	  (* I'm relatively confident that this line won't print anything to stdout *)
	  ignore(Pretty.printf "diff:  %3d = %3d = @[%a@]\n" n.nid tl dn_stmt s'); ()
	end) ;
  flush stdout ; 
  n.nid
  in
  let b = wrap_block f.sbody in 
	stmt_to_node b , !node_map

(* convert a very abstract tree node into a CIL Stmt *) 
let rec node_to_stmt node_map n = 
  let children = Array.map (fun child ->
	let child = node_of_nid node_map child in
    node_to_stmt node_map child 
  ) n.children in 
  let labels, skind = Hashtbl.find inv_typelabel_ht n.typelabel in 
  let require x = 
    if Array.length children = x then ()
    else begin
      printf "// node_to_stmt: warn: wanted %d children, have %d\n" 
        x (Array.length children) ;
        (*
      let doc = d_stmt () (mkStmt skind) in
      let str = Pretty.sprint ~width:80 doc in 
      printf "/* %s */\n" str ; 
      *)
    end
  in 
  let block x = 

    if x >= Array.length children then dummyBlock 
    else match children.(x).skind with
    | Block(b) -> b
    | _ -> begin 
      printf "// node_to_stmt: warn: wanted child %d to be a block\n" x ;
      (*
      let doc = d_stmt () (mkStmt skind) in
      let str = Pretty.sprint ~width:80 doc in 
      printf "/* %s */\n" str ; 
      *)
      dummyBlock 
    end 
  in
  let stmt = mkStmt begin
    match skind with
    | Instr _  
    | Return _ 
    | Goto _ 
    | Break _   
    | Continue _  
    -> skind
    | If(e,b1,b2,l)  -> require 2 ; If(e,block 0,block 1,l)  
    | Switch(e,b,sl,l) -> require 1 ; Switch(e,block 0,sl,l) 
    | Loop(b,l,so1,so2) -> require 1 ; Loop(block 0,l,so1,so2) 
    | TryFinally(b1,b2,l) -> require 2 ; TryFinally(block 0,block 1,l) 
    | TryExcept(b1,(il,e),b2,l) -> require 2; TryExcept(block 0,(il,e),block 1,l) 
    | Block _ -> Block(mkBlock (Array.to_list children)) 
  end 
  in
  stmt.labels <- labels ;
  stmt 

let ast_to_fundec node_map (f:Cil.fundec) n =
  let stmt = node_to_stmt node_map n in 
	match stmt.skind with 
  | Block(b) -> { f with sbody = b ; } 
  | _ -> 
    printf "fundec_to_ast: error: wanted child to be a block\n" ;
    failwith "fundec_to_ast" 

let corresponding m y =
  match find_node_that_maps_to m y with
  | Some(x) -> x
  | None -> y

     
(* Generate a set of difference between two Cil files. Write the textual
 * diff script to 'diff_out', write the data files and hash tables to
 * 'data_out'. *) 

let gendiff f1 f2 =
  let f1ht = Hashtbl.create 255 in 
  let f2ht = Hashtbl.create 255 in 
  (* save all of file 1's function names in f1ht *)
  let _ = 
	iterGlobals f1 
	  (fun g1 ->
		match g1 with
		| GFun(fd,l) -> Hashtbl.add f1ht fd.svar.vname fd 
		| _ -> () 
	  ) 
  in 
  let _ = 
	iterGlobals f2
	  (fun g1 ->
		match g1 with
		| GFun(fd,l) -> Hashtbl.add f2ht fd.svar.vname fd 
		| _ -> () 
	  ) 
  in 
 (* data_ht contains pairs of fundecs, one from f1, one from f2, and a mapping
	between them as generated by the diffX algorithm *)
  let data_ht = Hashtbl.create 255 in 
 (* the node_map serves the same purpose as the old nid_to_node hashtable did,
	but not statefully *)
  let node_map =
	foldGlobals f2 
	  (fun node_map ->
		fun g2 ->
		  match g2 with
		  | GFun(fd2,l) when Hashtbl.mem f1ht fd2.svar.vname -> begin
			let name = fd2.svar.vname in
			let fd1 = Hashtbl.find f1ht name in
			  (* convert both functions to generic tree structures *)
			let _ = 
			  if !verbose then (printf "diff: processing f1 %s\n" name ; flush stdout );
			in
			let t1,map1 = fundec_to_ast node_map fd1 in
			let _ = 
			  if !verbose then (printf "diff: processing f2 %s\n" name ; flush stdout);
			in
			let t2,map2 = fundec_to_ast map1 fd2 in 
			let _ = 
			  if !verbose then (printf "diff: \tmapping\n" ; flush stdout);
			in
			  (* construct the diffX mapping between them *)
			let m = mapping map2 t1 t2 in 
			(* debug output *)
			let _ = 
			  if !verbose then begin
				NodeMap.iter (fun (a,b) ->
				  if !verbose then
					(printf "diff: \t\t%2d %2d\n" a.nid b.nid);
				) m ; 
				printf "Diff: \ttree t1\n" ; 
				print_tree map2 (node_of_nid map2 t1) ; 
				printf "Diff: \ttree t2\n" ; 
				print_tree map2 (node_of_nid map2 t2) ; 
				printf "diff: \tgenerating script\n" ; flush stdout ;
			  end
			in
			(* use the mapping to generate the actual diff script *)
			let s = generate_script map2 (node_of_nid map2 t1) (node_of_nid map2 t2) m in 
			(* print it out, save it, etc etc *)
			let _ = 
			  if !verbose then 
				(printf "diff: \tscript: %d\n" (List.length s) ; flush stdout) ; 
			in
			  if (llen s) > 0 then
				Hashtbl.add data_ht name (s,mapping,t1,t2);
			  map2
		  end 
    | _ -> node_map
	) (init_map ()) 
  in
	data_ht, f1ht, f2ht

let tree_diff_cil diff1 diff2 =
  Cil.initCIL () ;
  let tempfile = "temp.c" in
	File.write_lines tempfile diff1;
	let f1 = Frontc.parse tempfile () in
	  File.write_lines tempfile diff2;
	  let f2 = Frontc.parse tempfile () in
		Cfg.computeFileCFG f1 ; 
		Cfg.computeFileCFG f2 ; 
		let data_ht,f1ht,f2ht = gendiff f1 f2 in
		  f1,f2,data_ht,f1ht,f2ht
	  
