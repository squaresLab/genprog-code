open Batteries
open Utils
open Map
open Cabs
open Cabsvisit
open Difftypes
open Cabswalker
open Convert

let standardize_diff patch =
  (* doing diffs at the expression level means that adding an
   * expression to a conditional, for example, actually involves
   * inserting about 5 different nodes, one for each component of the
   * expression.  This is particular to the "insert" operation
   * because it inserts nodes, not subtrees (the other two operations
   * actually operate on subtrees). "consolidate" consolidates,
   * whenever possible, insertions of several expression nodes that
   * actually compose into one expression into the insertion of just
   * that subtree, sort of increasing the granularity of the patch,
   * if you will.
   * 
   * This is totally "best effort."  *)
  let inserted = hcreate 10 in
  let deleted = hcreate 10 in
  let insertions = 
	lfilt (fun x -> match x with Insert(n,p,_) -> hadd inserted n x; true | Delete(n) -> hadd deleted n x; false | _ -> false) patch in 
  let collected = (* collected is a map *)
	lfoldl
	  (fun accum ->
		fun insertion ->
		  match insertion with
			 (* we only consolidate under nodes that are actually
				being inserted. *)
			Insert(nid,Some(parent),position) -> 
			  if hmem inserted parent then begin
				let children_list = 
				  if IntMap.mem parent accum then
					IntMap.find parent accum 
				  else [] in
				let inode = node_of_nid nid in
				let position = match position with Some(p) -> p | None -> -1 in
				  IntMap.add parent ((inode,position,insertion)::children_list) accum
			  end else 
				accum
		  | _ -> accum
	  ) IntMap.empty insertions 
  in 
  let removed_ops = hcreate 10 in
  let is_really_a_replace parentid position rest_of_patch = 
	let parentid = match parentid with Some(p) -> p | None -> -1 in
	let position = match position with Some(p) -> p | None -> -1 in 
	let node = node_of_nid parentid in
	  Array.fold_lefti 
		(fun accum -> 
		  fun index -> 
			fun ele -> 
			  if hmem deleted ele.nid && 
				position <> -1 && 
				(index - 1 == position || index + 1 == position || index == position) then
				begin
				  let op = hfind deleted ele.nid in 
					hadd removed_ops op (); (true,ele.nid)
				end
			  else accum) (false,-1) node.children
  in
  let subtree_cache : (int, bool) Hashtbl.t = hcreate 10 in (* this probably isn't necessary *)

  let is_really_a_subtree_insert nodeid = 
	let children_eq clist (carray : diff_tree_node array) =
	  ((llen clist) == (Array.length carray)) && 
		lfoldl
		(fun accum ->
		  fun (node,pos,op) ->
			(Array.exists (fun ele -> ele.nid == node.nid) carray &&
			   (Array.findi (fun x -> x.nid == node.nid) carray) == pos)
			&& accum) true clist
	in
	let rec st_helper (nodeid : int) : bool = 
	  if hmem subtree_cache nodeid then 
		hfind subtree_cache nodeid 
	  else begin
		if IntMap.mem nodeid collected then begin
		  let node = node_of_nid nodeid in 
		  let children = lrev (IntMap.find nodeid collected) in
			(children_eq children node.children) &&
			  (lfoldl
				 (fun truth ->
				   fun (cnode,io,ea) -> 
					 let ans : bool = st_helper cnode.nid in 
					   hadd subtree_cache nodeid ans; 
					   truth && ans) true children)
		end else hmem inserted nodeid
	  end
	in
	let rec remove_all_ops (nodeid : int) : unit =
	  let op = hfind inserted nodeid in 
		hadd removed_ops op ();
		try
		  let children = lrev (IntMap.find nodeid collected) in
			liter 
			  (fun (child,pos,op) ->
				hadd removed_ops op ();
				remove_all_ops child.nid) children
		with _ -> ()
	in
	  if st_helper nodeid then (remove_all_ops nodeid; true)
	  else false 
  in
  let easy_convert = function
	| Insert(x,y,p) -> SInsert((x,(insert_node_of_nid x).original_node),
										   (match y with
											  None -> None 
											| Some(y) -> Some(y,(node_of_nid y).original_node)),
										   p)
	| Move(x,y,p) -> SMove((x,(node_of_nid x).original_node), 
						   (match y with
							  None -> None 
							| Some(y) -> Some(y,(node_of_nid y).original_node)),
						   p)
	| Delete(x) -> SDelete(x,(node_of_nid x).original_node)
  in
	if (llen patch) > 1 then begin
	lrev 
	  (fst 
		 (lfoldl
			(fun (new_patch,rest_of_old_patch) ->
			  fun operation ->
				let rest = match rest_of_old_patch with [] -> [] | r::rs -> rs in
				  if hmem removed_ops operation then new_patch,rest else
					begin
					  let new_op = 
						match operation with
						  Insert(x,y,p) ->
							let is_replace,replacing =  is_really_a_replace y p rest_of_old_patch in
							  if is_replace then
								SReplace((x,(node_of_nid x).original_node), (replacing,(node_of_nid replacing).original_node))
							  else if is_really_a_subtree_insert x then
								SInsertTree((x,(node_of_nid x).original_node), 
											(match y with
											   None -> None 
											 | Some(y) -> Some(y,(node_of_nid y).original_node)),
											p)
							  else SInsert((x,(insert_node_of_nid x).original_node),
										   (match y with
											  None -> None 
											| Some(y) -> Some(y,(node_of_nid y).original_node)),
										   p)
						| Move(x,y,p) -> SMove((x,(node_of_nid x).original_node),
											   (match y with
												  None -> None 
												| Some(y) -> Some(y,(node_of_nid y).original_node)),
											   p)
						| Delete(x) -> SDelete(x,(node_of_nid x).original_node)
					  in
						new_op::new_patch,rest
					end
			) ([],List.tl patch) patch))
	  end
	else
	  lmap easy_convert patch

(*************************************************************************)
(* Alpha-renaming of a diff tree *)
(* NOTE: THIS IS NOT DONE, I don't think *)

(* CSpec, section 6.2.1: Scopes of identifiers:
 * 
 * there are 4 types of scopes in C: function, file, block, function
 * prototype.
 * 
 * 1) function only matters for label names.
 * 
 * 2) In general, things have file scope.
 * 
 * 3) Blocks have their own scopes.
 * 
 * 4) function prototypes have their own scope, which terminates at the
 * end of the declarator structure. *)


let a_cntr = ref 0
let new_alpha () = incr a_cntr; "a" ^ (String.of_int !a_cntr)
let alpha_tbl : (string, string) Hashtbl.t = hcreate 10
let alpha_context : string list list ref = ref []
let push_context _ = alpha_context := [] :: !alpha_context
let pop_context _ =
  match !alpha_context with
    [] -> ()
  | con::sub ->
		(alpha_context := sub;
		liter (fun name -> hrem alpha_tbl name) con)

(* note from cabsvisit.ml: "All visit methods are called in preorder!
 * (but you can use ChangeDoChildrenPost to change the order)"; I don't
 * know if this is different from the norm, so pay attention to see if
 * everything goes all foobar. *)

class alphaRename = object(self)
  inherit nopCabsVisitor

  method vblock b =
	self#vEnterScope(); ChangeDoChildrenPost(b, (fun b -> self#vExitScope(); b))

  method vvar name = 
	ht_find alpha_tbl name (fun x -> new_alpha())

  (*  method vdef def =
	  match def with
	  FUNDEF(sn,body,start,endloc) ->
	  let get_proto dtype =
	  match dtype with
	  | _ -> DoChildren*)

  method vtypespec ts = 
	match ts with
	| Tnamed(namestr) -> 
		let namestr' = ht_find alpha_tbl namestr (fun x -> new_alpha()) in
		  ChangeTo(Tnamed(namestr'))
	| _ -> DoChildren

  method vname nk spec (realname,dtype1,alist,loc) =
	let realname' = ht_find alpha_tbl realname (fun x -> new_alpha()) in
	let name' = (realname',dtype1,alist,loc) in
	  (*	  match dtype1 with
			  PROTO(dtype2,snames,ell) ->
			  ChangeDoChildrenPost(self#vEnterScope(); name'),
			  (fun name -> self#vExitScope(); name)
			  | _ -> *) ChangeDoChildrenPost(name', (fun name -> name))

  method vEnterScope () = push_context()
  method vExitScope () = pop_context()
	
end



let a_cntr = ref 0
let new_alpha () = incr a_cntr; "a" ^ (String.of_int !a_cntr)
let alpha_tbl : (string, string) Hashtbl.t = hcreate 10
let alpha_context : string list list ref = ref []
let push_context _ = alpha_context := [] :: !alpha_context
let pop_context _ =
  match !alpha_context with
    [] -> ()
  | con::sub ->
		(alpha_context := sub;
		liter (fun name -> hrem alpha_tbl name) con)

(* note from cabsvisit.ml: "All visit methods are called in preorder!
 * (but you can use ChangeDoChildrenPost to change the order)"; I don't
 * know if this is different from the norm, so pay attention to see if
 * everything goes all foobar. *)

(*class alphaRenameWalker  = object(self)
  inherit [(string,string) Map.t] singleCabsWalker
  (* todo: should I rename the labels? *)

  method combine one two = Map.union one two
  method default_res() = Map.empty

  method wExpression exp = 
	match exp.node with 
	| CONSTANT(CONST_STRING(str)) 
	| VARIABLE(str) -> 
	  if hmem alpha_tbl str then 
		Result(Map.singleton str (hfind alpha_tbl str))
	  else begin
		hadd alpha_tbl str (new_alpha());
		Result(Map.singleton str (hfind alpha_tbl str))
	  end
	| _ -> Children
	  
  method wName (str,_,_,_) = 
	CombineChildren(
	  if hmem alpha_tbl str then 
		Map.singleton str (hfind alpha_tbl str)
	  else begin
		hadd alpha_tbl str (new_alpha());
		Map.singleton str (hfind alpha_tbl str)
	  end)

  method enterScope () = push_context ()
  method exitScope () = pop_context ()

  method walkChanges (changes : changes) : (string,string) Map.t = failwith "Not implemented walkchanges in alpharenamewalker"
end*)

let renameVisit = new alphaRename 
let has_been_renamed = hcreate 100

let alpha_rename diff = 
  (* alpha rename should actually copy everything it alpha renames so that we have
	 both *)
  let diff' = copy diff in
  let rec rename_diff_tree_node dt = 
	let dumNode' : dummyNode = 
	  ht_find has_been_renamed dt.tl_node (fun dum -> rename_dummy_node dt.tl_node) in
	let children' = Array.map rename_diff_tree_node dt.children in
	let tl_str,tl_node = typelabel dumNode' in
	  node tl_str children' tl_node dt.original_node
  and rename_dummy_node = function (* fix the numbers here, somehow, in the node ids *)
	| DELETED -> DELETED
	| TREE(tree) -> TREE(visitTree renameVisit tree)
	| STMT(stmtn) -> 
	  (match (visitCabsStatement renameVisit stmtn) with
		[stmt] -> STMT(stmt)
	  | _ -> failwith "getting more than one statement when visiting a STMT in alpha renaming\n")
	| EXP(expn) -> EXP(visitCabsExpression renameVisit expn) 
	| DEF(defn) -> 
	  (match (visitCabsDefinition renameVisit defn) with
		[def] -> DEF(def)
	  | _ -> failwith "getting more than one definition when visiting a DEF in alpha renaming\n")
	| CHANGE(seas) -> CHANGE(rename_edit_action seas)
	| CHANGE_LIST(seasns) -> CHANGE_LIST(lmap rename_edit_action seasns)
  and rename_edit_action = function
    | SInsert((a,dt1),Some(b,dt2),io) -> 
	  let dt1' = rename_dummy_node dt1 in
	  let dt2' = rename_dummy_node dt2 in
		SInsert((a,dt1'),Some(b,dt2'),io) 
    | SInsert((a,dt1),None,io) -> 
	  let dt1' = rename_dummy_node dt1 in
		SInsert((a,dt1'),None,io) 
	| SInsertTree((a,dt1),Some(b,dt2),io) -> 
	  let dt1' = rename_dummy_node dt1 in
	  let dt2' = rename_dummy_node dt2 in
		SInsertTree((a,dt1'),Some(b,dt2'),io) 
	| SInsertTree((a,dt1),None,io) -> 
	  let dt1' = rename_dummy_node dt1 in
		SInsertTree((a,dt1'),None,io) 
	| SMove((a,dt1),Some(b,dt2),io) -> 
	  let dt1' = rename_dummy_node dt1 in
	  let dt2' = rename_dummy_node dt2 in
		SMove((a,dt1'),Some(b,dt2'),io) 
	| SMove((a,dt1),None,io) -> 
	  let dt1' = rename_dummy_node dt1 in
		SMove((a,dt1'),None,io) 
	| SDelete(a,dt1) -> 
	  let dt1' = rename_dummy_node dt1 in
		SDelete(a,dt1') 
	| SReplace((a,dt1),(b,dt2)) -> 
	  let dt1' = rename_dummy_node dt1 in
	  let dt2' = rename_dummy_node dt2 in
		SReplace((a,dt1'),(b,dt2'))
  in
	lmap rename_edit_action diff'
