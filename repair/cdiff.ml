(** Cdiff implements the diffX algorithm to produce the structural diff between
    two Cil ASTs; it can also produce new Cil ASTs by applying a (potentially
    partial) cdiff edit script to an existing AST *)
open Pretty
open Printf
open Cil
open Global

type node_id = int 

(** We convert a CIL AST to a very generic data structure for the purposes of
    performing the DiffX structural difference algorithm; we convert back later.
    This structure is sufficiently generic that it could easily be adapted to non-C
    representations *)
type tree_node = {
  mutable nid : node_id ; (* unique per node *)
  mutable children : int array ;
  mutable typelabel : int ; 
(** two nodes that represent the same C statement will have the same
    typelabel. "children" are not considered for calculating typelabels, so 'if
    (x<y) foo(); ' and 'if (x<y) \{ bar(); \} ' have the same typelabels, but
    their children (foo and bar) will not.  *)
} 

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

let typelabel_ht = Hashtbl.create 255 
let inv_typelabel_ht = Hashtbl.create 255 
let typelabel_counter = ref 0 


let node_of_nid node_map x = IntMap.find x node_map

let deleted_node = {
  nid = -1;
  children = [| |] ;
  typelabel = -1 ;
} 

(** the node_map is maps node IDs (integers) to cdiff tree nodes. The generation
    of the edit script also produces the node map, which is saved as part of a
    minimizableObject's structural signature *)
let init_map () = IntMap.add (-1) deleted_node (IntMap.empty)

(** cleanup_tree node_map tree does a little cleanup after a diff script has
    been (perhaps sloppily, given our use case) applied to a tree *)
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

(* if both their types and their labels are equal *) 
let nodes_eq t1 t2 = t1.typelabel = t2.typelabel 

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
let in_map_domain (m : NodeMap.t) (t : tree_node) =
  try 
    NodeMap.iter (fun (a,_) -> 
      if a.nid = t.nid then raise Found_It
    ) m ;
    false
  with Found_It -> true 

(* returns true if (_,t) is in m *) 
let in_map_range (m : NodeMap.t) (t : tree_node) =
  try 
    NodeMap.iter (fun (_,a) -> 
      if a.nid = t.nid then raise Found_It
    ) m ;
    false
  with Found_It -> true 

let find_node_that_maps_to (m : NodeMap.t) (y : tree_node) =
  try 
    NodeMap.iter (fun (a,b) -> 
      if b.nid = y.nid then raise (Found_Node(a))
    ) m ;
    None
  with Found_Node(a) -> Some(a)  

(* return a set containing all nodes in t equal to n *) 
let rec nodes_in_tree_equal_to 
    (node_info : tree_node IntMap.t) (t : tree_node) (n : tree_node) = 
  let sofar = ref 
    (if nodes_eq t n then NodeSet.singleton t else NodeSet.empty)
  in 
    Array.iter (fun child ->
      let child = node_of_nid node_info child in
        sofar := NodeSet.union !sofar (nodes_in_tree_equal_to node_info child n) 
    ) t.children ; 
    !sofar 

let map_size (m : NodeMap.t) = NodeMap.cardinal m 

(* level_order_traversal does a breadth-first walk of a tree *)
let level_order_traversal 
    (node_info : tree_node IntMap.t) (t : tree_node) callback =
  let q = Queue.create () in 
    Queue.add t q ; 
    while not (Queue.is_empty q) do
      let x = Queue.take q in 
        Array.iter (fun child ->
          let child = node_of_nid node_info child in
            Queue.add child q
        ) x.children ; 
        callback x ; 
    done 

let parent_of (node_info : tree_node IntMap.t) 
    (tree : tree_node) (some_node : tree_node) =
  try 
    level_order_traversal node_info tree (fun p ->
      Array.iter (fun child ->
        let child = node_of_nid node_info child in 
          if child.nid = some_node.nid then
            raise (Found_Node(p) )
      ) p.children 
    ) ;
    None
  with Found_Node(n) -> Some(n) 

let parent_of_nid (node_info : tree_node IntMap.t) 
    (tree : tree_node) (some_nid : node_id) =
  try 
    level_order_traversal node_info tree (fun p ->
      Array.iter (fun child ->
        let child = node_of_nid node_info child in
          if child.nid = some_nid then
            raise (Found_Node(p) )
      ) p.children 
    ) ;
    None
  with Found_Node(n) -> Some(n) 

let position_of (node_info : tree_node IntMap.t) 
    (parent : tree_node option) (child : tree_node) =
  match parent with
  | None -> None
  | Some(parent) -> 
    let result = ref None in 
      Array.iteri (fun i child' ->
        let child' = node_of_nid node_info child' in 
          if child.nid = child'.nid then
            result := Some(i) 
      ) parent.children ;
      !result 

let position_of_nid (node_info : tree_node IntMap.t)
    (parent : tree_node option) (child_nid : node_id) =
  match parent with
  | None -> None
  | Some(parent) -> 
    let result = ref None in 
      Array.iteri (fun i child' ->
        let child' = node_of_nid node_info child' in
          if child_nid = child'.nid then
            result := Some(i) 
      ) parent.children ;
      !result 

(** mapping node_info tree1 tree2 maps tree1 to tree2, matching nodes that
    (presumably) do not change between them, using the algorithm taken verbatim
    from the DiffX paper. The trees are represented by the ids of their root
    nodes.  Returns a node map, which is a misleadingly named type, as it's a
    set of pairs of node IDs (corresponding to a mapping between those nodes
    between tree1 and tree2) *)
let rec mapping (node_info :  tree_node IntMap.t) 
    (t1 : node_id) (t2 : node_id) : NodeMap.t =
  let t1 = node_of_nid node_info t1 in 
  let t2 = node_of_nid node_info t2 in
  let m = ref NodeMap.empty in 
    level_order_traversal node_info t1 (fun x -> 
      if in_map_domain !m x then ()
      else begin
        let y = nodes_in_tree_equal_to node_info t2 x in 
        let m'' = ref NodeMap.empty in 
          NodeSet.iter (fun yi ->
            if not (in_map_range !m yi) then begin
              let m' = ref NodeMap.empty in 
                match_fragment node_info x yi !m m' ;
                if map_size !m' > map_size !m'' then
                  m'' := !m'
            end 
          ) y ;
          m := NodeMap.union !m !m'' 
      end 
    ) ;
    !m 

(* still taken verbatim from their paper *) 
and match_fragment node_info x y (m : NodeMap.t) (m' : NodeMap.t ref) = 
  if (not (in_map_domain m x)) &&
    (not (in_map_range m y)) &&
    (nodes_eq x y) then begin
      m' := NodeMap.add (x,y) !m' ;
      let xc = Array.length x.children in 
      let yc = Array.length y.children in 
        for i = 0 to pred (min xc yc) do
          match_fragment node_info 
            (node_of_nid node_info x.children.(i)) 
            (node_of_nid node_info y.children.(i)) m m'
        done 
    end 
    
(** {b generate_script} node_map tree1 tree2 generates the edit script between
    tree1 and tree2 using a node_map that maps node_ids to tree_node
    representations and the mapping generated by the function mapping (above).
    Returns a list of edit operations.  This is not taken directly from the
    DiffX paper, because the version in the DiffX paper's pseudocode has
    (unspecified by Wes, who wrote the code initially) bugs *)
let generate_script (node_info : tree_node IntMap.t) (t1 : tree_node) 
    (t2 : tree_node) (m : NodeMap.t) : edit_action list = 
  let s = ref [] in 
    level_order_traversal node_info t2 (fun y -> 
      if not (in_map_range m y) then begin
        let yparent = parent_of node_info t2 y in 
        let ypos = position_of node_info yparent y in
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
          let xparent = parent_of node_info t1 x in
          let yparent = parent_of node_info t2 y in 
          let yposition = position_of node_info yparent y in 
          let xposition = position_of node_info xparent x in 
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
            | _, _ -> ()
        end 
      end 
    ) ;
    level_order_traversal node_info t1 (fun x ->
      if not (in_map_domain m x) then begin
        s := (Delete(x.nid)) :: !s
      end
    ) ;
    List.rev !s

let dummyBlock = { battrs = [] ; bstmts = [] ; }  
let dummyLoc = { line = 0 ; file = "" ; byte = 0; } 

(** stmt_to_typelabel generates the 'typelabel' of a CIL construct -- basically
    by turning if (x<y) foo();  into: if (x<y) ()and then hashing it.  See
    the DiffX paper for more detail on the definition of typelabel.  *)
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

(** {b fundec_to_ast} node_info cil_fundec and related helper functions converts
    a Cil function definition to the abstract tree datatype used by this
    implementation of the DiffX algorithm.  Returns the converted node and an
    updated node_info (which maps tree node ids to their tree_node representations *)
let fundec_to_ast (node_info : tree_node IntMap.t) (f:Cil.fundec) =
  let node_info = ref node_info in
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
      node_info := IntMap.add n.nid n !node_info ;
      n.nid
  in
  let b = wrap_block f.sbody in 
    stmt_to_node b , !node_info

(** {b node_to_stmt} node_info node converts the abstract tree node back into a
    CIL Stmt *)
let rec node_to_stmt (node_info : tree_node IntMap.t) (n : tree_node) : Cil.stmt = 
  let children = Array.map (fun child ->
    let child = node_of_nid node_info child in
      node_to_stmt node_info child 
  ) n.children in 
  let labels, skind = Hashtbl.find inv_typelabel_ht n.typelabel in 
  let require x = 
    if Array.length children = x then ()
    else begin
      printf "// node_to_stmt: warn: wanted %d children, have %d\n" 
        x (Array.length children) ;
    end
  in 
  let block x = 
    if x >= Array.length children then dummyBlock 
    else match children.(x).skind with
    | Block(b) -> b
    | _ -> begin 
      printf "// node_to_stmt: warn: wanted child %d to be a block\n" x ;
      dummyBlock 
    end 
  in
  let stmt = mkStmt begin
    match skind with
    | Instr _  | Return _ | Goto _ 
    | Break _  | Continue _  -> skind
    | If(e,b1,b2,l)  -> require 2 ; If(e,block 0,block 1,l)  
    | Switch(e,b,sl,l) -> require 1 ; Switch(e,block 0,sl,l) 
    | Loop(b,l,so1,so2) -> require 1 ; Loop(block 0,l,so1,so2) 
    | TryFinally(b1,b2,l) -> require 2 ; TryFinally(block 0,block 1,l) 
    | TryExcept(b1,(il,e),b2,l) -> 
      require 2; TryExcept(block 0,(il,e),block 1,l) 
    | Block _ -> Block(mkBlock (Array.to_list children)) 
  end 
  in
    stmt.labels <- labels ;
    stmt 

(** {b ast_to_fundec} node_info cil_fundec initial_stmt converts the cil
    function definition (cil_fundec) represented by the node initial_statement
    back into a complete Cil function definition. The Cil function definition
    represents the body of the fundec as a Cil Block, so this function assumes n
    corresponds to a block statement and swaps that into the otherwise untouched
    cil function definiton once it's been reconstructed using {b node_to_stmt}.
    Thus, if the node *doesn't* correspond to a block for any bizarre reason,
    this function will fail. *)
let ast_to_fundec (node_info : tree_node IntMap.t) (f:Cil.fundec) (n : tree_node) =
  let stmt = node_to_stmt node_info n in 
    match stmt.skind with 
    | Block(b) -> { f with sbody = b ; } 
    | _ -> 
      printf "fundec_to_ast: error: wanted child to be a block\n" ;
      failwith "fundec_to_ast" 

exception Necessary_line

(** {b apply_diff} node_info mapping ast1 ast2 edit_operation applies
    edit_operation to the file (presumably astt2).  This version is very fault
    tolerant because we're expecting our caller (= a delta-debugging script) to
    be throwing out parts of the diff script in an effort to minimize it.
    Assumes that mapping and node_info are properly populated.  Returns a
    potentially-modified node_info reflecting the changes produced by
    edit_operation *)
let apply_diff (node_info : tree_node IntMap.t) (m : NodeMap.t) (astt1 : node_id) 
    (astt2 : node_id) (s : edit_action) : tree_node IntMap.t = 
  let ast1 = node_of_nid node_info astt1 in
  let ast2 = node_of_nid node_info astt2 in
    try
      match s with
      (* delete sub-tree rooted at node x *)
      | Delete(nid) -> 
        let node = node_of_nid node_info nid in 
          delete node_info node 

      (* insert node x as pth child of node y *) 
      | Insert(xid,yopt,ypopt) ->
        let xnode = node_of_nid node_info xid in 
          
          (match yopt with
          | None -> printf "apply: error: insert to root?"  ; node_info
          | Some(yid) -> 
            let ynode = node_of_nid node_info yid in 
            (* let ynode = corresponding m ynode in  *)
            let ypos = match ypopt with
              | Some(x) -> x | None -> 0 
            in 

            (* Step 1: remove children of X *) 
            let node_info = 
              xnode.children <- [| |] ;
              IntMap.add xnode.nid xnode node_info
            in

            (* Step 2: remove X from its parent *)
            let node_info =
              let xparent1 = parent_of node_info ast1 xnode in
              let xparent2 = parent_of node_info ast2 xnode in 
                (match xparent1, xparent2 with
                | Some(parent), _ 
                | _, Some(parent) -> 
                  let plst = Array.to_list parent.children in
                  let plst = List.map (fun child ->
                    let child = node_of_nid node_info child in 
                      if child.nid = xid then
                        deleted_node.nid
                      else
                        child.nid
                  ) plst in
                    parent.children <- Array.of_list plst  ;
                    IntMap.add parent.nid parent node_info
                | _, _ -> node_info
                (* this case is fine, and typically comes up when we are
                   Inserting the children of a node that itself was Inserted over *)
                ) 
            in

            (* Step 3: put X as p-th child of Y *) 
            let len = Array.length ynode.children in 
            let before = Array.sub ynode.children 0 ypos in
            let after  = Array.sub ynode.children ypos (len - ypos) in 
            let result = Array.concat [ before ; [| xnode.nid |] ; after ] in 
              ynode.children <- result ;
              IntMap.add ynode.nid ynode node_info
          ) 

      (* move subtree rooted at node x to as p-th child of node y *) 
      | Move(xid,yopt,ypopt) -> 
        let xnode = node_of_nid node_info xid in 
          (match yopt with
          | None -> 
            printf "apply: error: %s: move to root?\n"  (edit_action_to_str s) ; node_info
          | Some(yid) -> 
            let ynode = node_of_nid node_info yid in 
            (* let ynode = corresponding m ynode in *)
            let ypos = match ypopt with
              | Some(x) -> x | None -> 0 
            in 
            (* Step 1: remove X from its parent *)
              
            let xparent1 = parent_of node_info ast1 xnode in    
            let xparent2 = parent_of node_info ast2 xnode in 
            let node_info = 
              match xparent1, xparent2 with
              | Some(parent), _ 
              | _, Some(parent) -> 
                let plst = Array.to_list parent.children in
                let plst = List.map (fun child ->
                  let child = node_of_nid node_info child in
                    if child.nid = xid then
                      deleted_node.nid
                    else
                      child.nid
                ) plst in
                  parent.children <- Array.of_list plst ; 
                  IntMap.add parent.nid parent node_info
              | None, None -> 
                printf "apply: error: %s: no x parent\n" 
                  (edit_action_to_str s) ; node_info
            in
            (* Step 2: put X as p-th child of Y *) 
            let len = Array.length ynode.children in 
            let before = Array.sub ynode.children 0 ypos in
            let after  = Array.sub ynode.children ypos (len - ypos) in 
            let result = Array.concat [ before ; [| xnode.nid |] ; after ] in 
              ynode.children <- result ;
              IntMap.add ynode.nid ynode node_info
          ) 
    with e -> raise Necessary_line

(** {b apply_diff_to_file} cil_file node_info patch_ht data_ht print_fun applies
    a diff script to the given Cil file.  The patch_ht contains the edits
    associated with a given global name.  Node_info maps node IDs to tree nodes.
    data_ht maps global names to mapping and trees for the two files we're
    diffing.  apply_diff_to_file also assumes that inv_typelabel_ht has been
    populated, because apprently I did not eliminate all state in this
    module. *)
let apply_diff_to_file f1 node_info patch_ht data_ht =
  foldGlobals f1 
    (fun (globals,node_info) g1 ->
      match g1 with
      | GFun(fd1,l) when Hashtbl.mem patch_ht fd1.svar.vname -> 
        let name = fd1.svar.vname in
        let patches = Hashtbl.find patch_ht name in
        let m, t1, t2 = Hashtbl.find data_ht name in 
        let node_info = 
          try
            List.fold_left 
              (fun node_info ->
                fun ea ->
                  apply_diff node_info m t1 t2 ea;
              ) node_info patches
          with Necessary_line -> node_info
        in
        let node_info = 
          cleanup_tree node_info (node_of_nid node_info t1) 
        in
        let output_fundec = 
          ast_to_fundec node_info fd1 (node_of_nid node_info t1) in 
          (GFun(output_fundec,l)) :: globals, node_info
      | _ -> g1 :: globals, node_info 
    ) ([], node_info)

(** usediff file node_info diff_script data_ht applies (usually the partial)
    diff_script to file; returns the file *)
let usediff (f1 : Cil.file) (node_info : tree_node IntMap.t) 
    (script : string list) data_ht =
  let patch_ht = Hashtbl.create 255 in 
  let add_patch fname ea = (* preserves order, fwiw *) 
    let sofar = try Hashtbl.find patch_ht fname with _ -> [] in
      Hashtbl.replace patch_ht fname (sofar @ [ea]) 
  in 
  let num_to_io x = if x < 0 then None else Some(x) in 
  let _ =
    List.iter
      (fun line ->
        Scanf.sscanf line "%s %s %s (%d,%d,%d)" (fun the_file fname ea a b c -> 
          let it = match String.lowercase ea with 
            | "insert" -> Insert(a, num_to_io b, num_to_io c) 
            | "move" ->   Move(a, num_to_io b, num_to_io c)
            | "delete" -> Delete(a) 
            | _ -> failwith ("invalid patch: " ^ line)
          in add_patch fname it 
        ) 
      ) script
  in
  let globals,_ = 
    apply_diff_to_file f1 node_info patch_ht data_ht in
    {f1 with globals = lrev globals }  
