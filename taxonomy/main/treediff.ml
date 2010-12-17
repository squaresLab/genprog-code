open Batteries
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
let dummyBlock = { blabels = []; battrs = [] ; bstmts = [] ; }  
let dummyLoc = {lineno = -1; 
				filename = "";
				byteno = -1;
				ident = -1}
let dummyExp = NOTHING
let dummyStmt = NOP(dummyLoc)
let dummyDt = JUSTBASE
let dummyName = ("",dummyDt,[],dummyLoc)
let dummyIng = ([],[])
let dummyNg = ([],[])
let dummyIE = NO_INIT

let typelabel_ht = Hashtbl.create 255 
(*let inv_typelabel_ht = Hashtbl.create 255 *)
let typelabel_counter = ref 0 

(*let cil_stmt_id_to_node_id = Hashtbl.create 255 
let node_id_to_cil_stmt = Hashtbl.create 255 
let node_id_to_node = Hashtbl.create 255 

let node_of_nid x = Hashtbl.find node_id_to_node x 
*)
(* determine the 'typelabel' of a CIL Stmt -- basically, turn 
 *  if (x<y) { foo(); }
 * into:
 *  if (x<y) { }
 * and then hash it. 
 *) 
(*let stmt_to_typelabel (s : Cil.stmt) = 
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

let rec stmt_to_node s = 
  let tl, (labels,skind) = stmt_to_typelabel s in
  let n = new_node tl in 
  (* now just fill in the children *) 
  let children = match s.skind with
    | Instr _  
    | Return _ 
    | Goto _ 
    | Break _   
    | Continue _  
    -> [| |]
    | If(e,b1,b2,l)  
    -> [| stmt_to_node (wrap_block b1) ;
          stmt_to_node (wrap_block b2) |] 
    | Switch(e,b,sl,l) -> 
       [| stmt_to_node (wrap_block b) |]
    | Loop(b,l,so1,so2) -> 
       [| stmt_to_node (wrap_block b) |] 
    | TryFinally(b1,b2,l) -> 
       [| stmt_to_node (wrap_block b1) ; stmt_to_node (wrap_block b2) |] 
    | TryExcept(b1,(il,e),b2,l) ->
       [| stmt_to_node (wrap_block b1) ; stmt_to_node (wrap_block b2) |] 
    | Block(block) -> 
       let children = List.map stmt_to_node block.bstmts in
       Array.of_list children 
  in
  n.children <- children ;
  Hashtbl.add cil_stmt_id_to_node_id s.sid n.nid ;
  Hashtbl.add node_id_to_cil_stmt n.nid s ;
  Hashtbl.add node_id_to_node n.nid n ;
  let s' = { s with skind = skind ; labels = labels } in 
  ignore (Pretty.printf "diff:  %3d = %3d = @[%a@]\n" n.nid tl 
    dn_stmt s') ;
  flush stdout ; 
  n 

let fundec_to_ast (f:Cil.fundec) =
  let b = wrap_block f.sbody in 
  stmt_to_node b 

(* convert a very abstract tree node into a CIL Stmt *) 
let rec node_to_stmt n = 
  let children = Array.map (fun child ->
    node_to_stmt child 
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

let ast_to_fundec (f:Cil.fundec) n =
  let stmt = node_to_stmt n in 
  match stmt.skind with 
  | Block(b) -> { f with sbody = b ; } 
  | _ -> 
    printf "fundec_to_ast: error: wanted child to be a block\n" ;
    failwith "fundec_to_ast" 

let corresponding m y =
  match find_node_that_maps_to m y with
  | Some(x) -> x
  | None -> y
*)
(* Apply a single edit operation to a file. This version if very fault
 * tolerant because we're expecting our caller (= a delta-debugging script)
 * to be throwing out parts of the diff script in an effort to minimize it.
 * So this is 'best effort'. *) 
(*let apply_diff m ast1 ast2 s =  
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
*)
(* Generate a set of difference between two Cil files. Write the textual
 * diff script to 'diff_out', write the data files and hash tables to
 * 'data_out'. *) 
(*let gendiff f1 f2 diff_out data_out = 
  let f1ht = Hashtbl.create 255 in 
  iterGlobals f1 (fun g1 ->
    match g1 with
    | GFun(fd,l) -> Hashtbl.add f1ht fd.svar.vname fd 
    | _ -> () 
  ) ; 
  let data_ht = Hashtbl.create 255 in 
  iterGlobals f2 (fun g2 ->
    match g2 with
    | GFun(fd2,l) -> begin
      let name = fd2.svar.vname in
      if Hashtbl.mem f1ht name then begin
        let fd1 = Hashtbl.find f1ht name in 
        printf "diff: processing f1 %s\n" name ; flush stdout ; 
        let t1 = fundec_to_ast fd1 in 
        printf "diff: processing f2 %s\n" name ; flush stdout ; 
        let t2 = fundec_to_ast fd2 in 
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
          (List.length s) ; flush stdout ; 
        List.iter (fun ea ->
          fprintf diff_out "%s %s\n" name (edit_action_to_str ea) ;
          printf "Script: %s %s\n" name (edit_action_to_str ea)
        ) s  ;
        Hashtbl.add data_ht name (m,t1,t2) ; 
      end else begin
        printf "diff: error: File 1 does not contain %s()\n" name 
      end 
    end 
    | _ -> () 
  ) ;
  Marshal.to_channel data_out data_ht [] ; 
  Marshal.to_channel data_out inv_typelabel_ht [] ; 
  Marshal.to_channel data_out f1 [] ; 
  (* Weimer: as of Mon Jun 28 15:51:11 EDT 2010, we don't need these  
  Marshal.to_channel data_out cil_stmt_id_to_node_id [] ; 
  Marshal.to_channel data_out node_id_to_cil_stmt [] ; 
  *)
  Marshal.to_channel data_out node_id_to_node [] ; 
  () 

(* Apply a (partial) diff script. *) 
let usediff diff_in data_in file_out = 
  let data_ht = Marshal.from_channel data_in in 
  let inv_typelabel_ht' = Marshal.from_channel data_in in 
  let copy_ht local global = 
    Hashtbl.iter (fun a b -> Hashtbl.add global a b) local
  in
  copy_ht inv_typelabel_ht' inv_typelabel_ht ; 
  let f1 = Marshal.from_channel data_in in 
  (* Weimer: as of Mon Jun 28 15:51:30 EDT 2010, we don't need these 
  let cil_stmt_id_to_node_id' = Marshal.from_channel data_in in 
  copy_ht cil_stmt_id_to_node_id' cil_stmt_id_to_node_id ; 
  let node_id_to_cil_stmt' = Marshal.from_channel data_in in 
  copy_ht node_id_to_cil_stmt' node_id_to_cil_stmt ; 
  *)
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

  let myprint glob =
    ignore (Pretty.fprintf file_out "%a\n" dn_global glob)
  in 

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
  ) ; 

  () 
*)
(*************************************************************************)
(*************************************************************************)


let counter = ref 1 
let get_next_count () = 
  let count = !counter in 
  incr counter ;
  count 

(*let stmt_id_to_stmt_ht = Hashtbl.create 255 *)

(* This visitor walks over the C program AST and builds the hashtable that
 * maps integers to statements. *) 
(*
let output = ref [] 
let label_prefix = ref "" 
*)
(*class numVisitor = object
  inherit nopCilVisitor
  method vstmt b = 
    let count = get_next_count () in 
    b.sid <- count ;
    (*
    let mylab = Label(Printf.sprintf "stmt_%s_%d" !label_prefix count, locUnknown, false) in 
    b.labels <- mylab :: b.labels ; 
    *)
    Hashtbl.add stmt_id_to_stmt_ht count b ; 
    DoChildren
end 
let my_num = new numVisitor
*)
let convert_tree (tree : Cabs.tree_node list) : diff_tree_node list = 
  let rec fc_dum = function
	| FC_EXP(_) -> FC_EXP(dummyExp)
	| FC_DECL(def) -> FC_DECL(def_dum def)
  and fc_tl s = 
	let dum = fc_dum s in
	  Pretty.sprint ~width:80 (d_fc () dum) 
  and fc_children = function
	  FC_EXP(exp) -> [| convert_exp exp |]
	| FC_DECL(def) -> [| convert_def def |]
  and asm_det_dum = function
	  Some(_) -> Some({aoutputs=[];ainputs=[];aclobbers=[]}) ->
	| None -> None
  and asm_det_tl a = 
	let dum = asm_det_dum a in
	  Pretty.sprint ~width:80 (d_asm_det () dum) 
  and asm_det_children = function
	  Some({aoutputs=aoutputs;ainputs=ainputs;aclobbers=aclobbers}) ->
		Array.of_list ((lmap (fun (sopt,s,exp) -> convert_exp exp) aoutputs) @
						 (lmap (fun (sopt,s,exp) -> convert_exp exp) ainputs))
	| None -> [| |]
  and attr_dum a = (fst a,(lmap (fun _ -> dummyExp) (snd a)))
  and attr_tl a = 
	let dum = attr_dum a in
	  Pretty.sprint ~width:80 (d_attr () dum) 
  and attr_children (s,elist) = Attr.of_list (lmap convert_exp elist)
	(* FIXME: does this make sense, or should I just return JUSTBASE for
	   all decltypes? *)
  and dt_dum = function
	| JUSTBASE -> JUSTBASE
	| PARENTYPE(alist1,declt,alist2) ->
		PARENTYPE(lmap attr_dum alist1, dt_dum declt, lmap attr_dum alist2)
	| ARRAY(declt,alist,exp) -> AARAY(dt_dum declt, lmap attr_dum alist, dummyExp)
	| PTR(alist,declt) -> PTR(lmap attr_dum alist, dt_dum declt)
	| PROTO(decl,sns,b) -> PROTO(dt_dum decl, lmap sn_dum sns, b)
  and dt_tl dt =
	let dum = dt_dum dt in
	  Pretty.sprint ~width:80 (d_decl_type () dum) 
  and dt_children = function
	| JUSTBASE -> [| |]
	| PARENTYPE(alist1,decl,alist2) ->
		Array.append (Array.of_list (lmap convert_attr alist1)) 
		  (Array.append (convert_dt decl) (Array.of_list (lmap convert_attr alist2)))
	| ARRAY(decl,alist,exp) ->
		Array.append (convert_dt decl) (Array.append (Array.of_list (lmap convert_attr alist)) @ (convert_exp exp))
	| PTR(alist,decl) ->
		Array.append (Array.of_list (lmap convert_attr alist)) (convert_dt decl)
	| PROTO(decl,sns,b) -> 
		 Array.append (convert_dt decl) (Array.of_list (lmap convert_sn sns))
  and ing_dum (spec,ns) = (spec_dum spec, lmap init_name_dum ns)
  and ing_tl ing = 
	let dum = ing_dum ing in
	  Pretty.sprint ~width:80 (d_init_name_group () dum) 
  and ing_children (spec,ns) = 
	Array.of_list ((convert_spec spec) :: (lmap convert_init_name ns))
  and ng_dum (spec,ns) = (spec_dum spec, lmap name_dum ns)
  and ng_tl ng = 
	let dum = ng_dum ng in
	  Pretty.sprint ~width:80 (d_name_group () dum) 
  and ng_children (spec,ns) = 
	 Array.of_list (convert_name spec) :: (lmap convert_name ns)
  and sn_dum (spec,name) = (spec_dum spec, name_dum name)
  and sn_tl sn = 
	let dum = sn_dum sn in
	  Pretty.sprint ~width:80 (d_single_name () dum) 
  and sn_children (spec,name) = [| convert_spec spec; convert_name name |]
  and spec_dum specelems = lmap spec_elem_dum specelems
  and spec_tl s = 
	let dum = spec_dum s in
	  Pretty.sprint ~width:80 (d_specifiers () dum) 
  and spec_children specelems = 
	Array.of_list (lmap convert_spec_elem specelems)
  and ie_dum = function
	| NO_INIT -> NO_INIT
	| SINGLE_INIT(exp) -> SINGLE_INIT(dummyExp)
	| COMPOUND_INIT(lst) -> failwith "Not implemented"
  and ie_tl ie = 
	let dum = ie_dum ie in
	  Pretty.sprint ~width:80 (d_init_expression () dum) 
  and ie_children = function
	| NO_INIT -> [| |]
	| SINGLE_INIT(exp) -> [| convert_exp exp |]
	| COMPOUND_INIT(lst) -> 
		Array.of_list 
		  (lfold (fun accum -> 
					(fun(iw,ie) -> [convert_iw ie; convert_ie ie] @ accum))
			 [] lst)
  and stmt_tl s = 
	let dum = stmt_dum s in
	  Pretty.sprint ~width:80 (d_stmt () dum) 
  and stmt_dum = function
	  NOP(_) -> NOP(dummyLoc)
	| COMPUTATION(_) -> COMPUTATION(dummyExp,dummyLoc)
	| BLOCK(_) -> BLOCK(dummyBlock,dummyLoc)
	| SEQUENCE(_) -> SEQUENCE(dummyStmt,dummyStmt,dummyLoc)
	| IF(_) -> IF(dummyExp,dummyStmt,dummyStmt,dummyLoc)
	| WHILE(_) -> WHILE(dummyExp,dummyStmt,dummyLoc)
	| DOWHILE(_) -> DOWHILE(dummyExp,dummyStmt,dummyLoc)
	| FOR(fc,_,_,_,_) -> FOR(fc_dum fc,dummyExp,dummyExp,dummyStmt,dummyLoc)
	| BREAK(_) -> BREAK(dummyLoc)
	| CONTINUE(_) -> CONTINUE(dummyLoc)
	| RETURN(_) -> RETURN(dummyExp,dummyLoc)
	| SWITCH(_) -> SWITCH(dummyExp,dummyStmt,dummyLoc)
	| CASE(_) -> CASE(dummyExp,dummyStmt,dummyLoc)
	| CASERANGE(_) -> CASERANGE(dummyExp,dummyExp,dummyStmt,dummyLoc)
	| DEFAULT(_) -> DEFAULT(dummyStmt,dummyLoc)
	| LABEL(str,_,_) -> LABEL(str,dummyStmt,dummyLoc)
	| GOTO(str,_) -> GOTO(str,dummyLoc)
	| COMPGOTO(_) -> COMPGOTO(dummyExp,dummyLoc)
	| DEFINITION(d) -> DEFINITION(def_dum d)
		(* FIXME: how to deal with ASM? *)
	| ASM(_) -> ASM([],[],None,dummyLoc)
	| TRY_EXCEPT(_) -> TRY_EXCEPT(dummyBlock,dummyExp,dummyBlock,dummyLoc)
	| TRY_FINALLY(_) -> TRY_FINALLY(dummyBlock,dummyBlock,dummyLoc)
  and exp_tl e =
	let dum = exp_dum e in
	  Pretty.sprint ~width:80 (d_exp () dum) 
  and exp_dum = function
	  NOTHING -> NOTHING
	| UNARY(uop,_) -> UNARY(uop,dummyExp)
	| LABELADDR(str) -> LABELADDR(str)
	| BINARY(bop,_,_) -> BINARY(bop,dummyExp,dummyExp)
	| QUESTION(_) -> QUESTION(dummyExp,dummyExp,dummyExp)
	| CAST((spec,dtype),ie) -> CAST(([],dummyDt),dummyIE)
	| CALL(_) -> CALL(dummyExp,[])
	| COMMA(_) -> COMMA([])
	| CONSTANT(c) -> CONSTANT(c) (* Maybe? *)
	| PAREN(_) -> PAREN(dummyExp)
	| VARIABLE(str) -> VARIABLE(str)
	| EXPR_SIZEOF(_) -> EXPR_SIZEOF(dummyExp)
	| TYPE_SIZEOF(spec,dtype) -> TYPE_SIZEOF([],dummyDt)
	| EXPR_ALIGNOF(_) -> EXPR_ALIGNOF(dummyExp)
	| TYPE_ALIGNOF(spec,decl_type) -> TYPE_ALIGNOF([],dummyDt)
	| INDEX(_) -> INDEX(dummyExp,dummyExp)
	| MEMBEROF(_,str) -> MEMBEROF(dummyExp,str)
	| MEMBEROFPTR(_,str) -> MEMBEROFPTR(dummyExp,str)
	| GNU_BODY(_) -> GNU_BODY(dummyBlock)
	| EXPR_PATTERN(str) -> EXPR_PATTERN(str)
		(* I have no idea if the following is right *)
  and block_tl block = 
	let dum = block_dum block in
	  Pretty.sprint ~width:80 (d_block () dum) 
  and block_dum block = {block with battrs=[];bstmts=[]} 
  and def_tl def =
	let dum = def_dum def in
	  Pretty.sprint ~width:80 (d_def () dum) 
  and def_dum = function
	  FUNDEF(_) -> FUNDEF(([],dummyName),dummyBlock,dummyLoc,dummyLoc)
	| DECDEF(_) -> DECDEF(dummyIng,dummyLoc)
	| TYPEDEF(_) -> TYPEDEF(dummyNg,dummyLoc)
	| ONLYTYPEDEF(_) -> ONLYTYPEDEF([],dummyLoc)
	| GLOBASM(str,_) -> GLOBASM(str,dummyLoc)
	| PRAGMA(_) -> PRAGMA(dummyExp,dummyLoc)
	| LINKAGE(str,_,_) -> LINKAGE(str,dummyLoc,[])
  and tree_node_tl def =
	let dum = tree_node_dum def in
	  Pretty.sprint ~width:80 (d_tree_node () dum) 
  and tree_node_dum = function
	| Globals(_) -> Globals([])
	| Stmts(_) -> Stmts([]) 
	| Exps(_) -> Exps([]) 
	| PartialStmt(_) -> PartialStmt(dummyStmt) 
	| PartialExp(_) -> PartialExp(dummyExp)
	| PartialGlobal(g) -> PartialGlobal(def_dum g)
	| Syntax(str) -> Syntax(str)
  and def_children = function
	  FUNDEF(sn,b,_,_) -> [| convert_sn sn; convert_block b |]
	| DECDEF(ing,_) -> [| convert_ing ing |]
	| TYPEDEF(ng,_) -> [| convert_ng ng |]
	| ONLYTYPEDEF(spec,_) -> [| convert_spec spec |]
	| GLOBASM(str,_) -> [| |]
	| PRAGMA(exp,_) -> [| convert_exp exp |]
	| LINKAGE(str,_,defs) -> Array.of_list (lmap convert_def defs)
  and exp_children = function
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
  and stmt_children = function
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
  and tree_node_children = function
	| Globals(defs) -> Array.of_list (lmap convert_def defs)
	| Stmts(ss) -> Array.of_list (lmap convert_stmt ss)
	| Exps(exps) -> Array.of_list (lmap convert_exp exps)
	| PartialStmt(s) -> [| convert_stmt s |]
	| PartialExp(exp) -> [| convert_exp exp |]
	| PartialGlobal(def) -> [| convert_def def |]
	| Syntax(str) -> [| |]
  and block_children block =
	let {blabels=blabels;battrs=battrs;bstmts=bstmts} = block in
	  Array.of_list ((lmap convert_attr battrs) @ (lmap convert_stmt bstmts))
  and convert_node (tlabel : string) (children: diff_tree_node array) : diff_tree_node =
	(* FIXME: this sprint won't work; pass the doc func as param to convert_node? *)
	let str2 = "" in (* Pretty.sprint ~width:80 (doc () tlabel) in*)
	let str2' = "" (*str2^str1 in*) in
	let tl = ht_find typelabel_ht str2' (fun x -> incr typelabel_counter; !typelabel_counter) in
	let n = new_node tl in
	  (* FIXME: I really need to number all the nodes in the cabs tree; how to manage? *)
	  (* FIXME: add to lots of hashtables *)
	  n.children <- children; 
	  n
  and convert_asm_det node = convert_node (asm_det_tl node) (asm_det_children node) 
  and convert_attr node = convert_node (attr_tl node) (attr_children node)
  and convert_fc node = convert_node (fc_tl node) (fc_children node)
  and convert_dt node = convert_node (dt_tl node) (dt_children node)
  and convert_def node = convert_node (def_tl node) (def_children node)
  and convert_ing node = convert_node (ing_tl node) (ing_children node)
  and convert_ng node = convert_node (ng_tl node) (ng_children node)
  and convert_sn node = convert_node (sn_tl node) (sn_children node)
  and convert_stmt node = convert_node (stmt_tl node) (stmt_children node)
  and convert_tree_node node = convert_node (tree_node_tl node) (tree_node_children node)
  and convert_exp node = convert_node (exp_tl node) (exp_children node)
  and convert_block node = convert_node (block_tl node) (block_children node)
  and convert_spec node = convert_node (spec_tl node) (spec_children node)
  and convert_ie node = convert_node (ie_tl node) (ie_children node)
  in
	lmap convert_tree_node tree

(*let generate tree1 tree2 = 
  let diffname = !generate ^ ".diff" in 
  let diff_out = open_out diffname in 
  let data_out = open_out_bin !generate in 
  let converted1,converted2 = convert_to_tree tree1, convert_to_tree tree2 in
    gendiff f1 f2 diff_out data_out ;
    close_out diff_out ; 
    close_out data_out

let apply =
  let data_in = open_in_bin (!use) in 
  let diff_in = open_in diff in 

  let file_out = stdout in 

	usediff diff_in data_in file_out 
*)

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
