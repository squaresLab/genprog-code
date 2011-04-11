open Batteries
open Utils
open Map
open Cabs
open Cabsvisit
open Difftypes
open Cabswalker
open Convert

let standardize_diff children1 patch =
(* TODO: make sure this is still working! *)
  let deleted = hcreate 10 in
	liter
	  (fun (_,x) -> 
		match x with 
		| DeleteExp(n,_) -> hadd deleted n.id x
		| DeleteStmt(n,_) -> hadd deleted n.id x
		| DeleteDef(n,_) -> hadd deleted n.id x
		| DeleteTN(n,_) -> hadd deleted n.id x
		| _ -> ()) patch;
  let removed_ops = hcreate 10 in
  let is_really_a_replace parent position =
	try
	let children = Map.find parent children1 in
	let index = ref (-1) in 
	  lfoldl
		(fun is_rep ->
		  fun ele -> 
			incr index;
			if hmem deleted ele && 
			  position <> -1 && 
			  (!index - 1 == position || !index + 1 == position || !index == position) then begin
				let op = hfind deleted ele in 
				  hadd removed_ops op ele; (true,op)
			  end else is_rep
		) (false,(DeleteTN(nd(Stmts([])),-1))) children
	with Not_found -> false,DeleteTN(nd(Stmts([])),-1)
  in
  let make_replace operation replacing = 
	match operation,replacing with
	| InsertTreeNode(tn1,pos),DeleteTN(tn2,_)
	| ReorderTreeNode(tn1,pos,_),DeleteTN(tn2,_) ->
	  ReplaceTreeNode(tn1,tn2,pos)
	| InsertDefinition(def1,parent,position,t1),DeleteDef(def2,_)
	| MoveDefinition(def1,parent,position,t1,_),DeleteDef(def2,_) ->
	  ReplaceDefinition(def1,def2,parent,position,t1)
	| InsertStatement(stmt1,parent,position,t1), DeleteStmt(stmt2,_)
	| MoveStatement(stmt1,parent,position,t1,_), DeleteStmt(stmt2,_) ->
	  ReplaceStatement(stmt1,stmt2,parent,position,t1)
	| InsertExpression(exp1,parent,position,t1), DeleteExp(exp2,_)
	| MoveExpression(exp1,parent,position,t1,_), DeleteExp(exp2,_) -> 
	  ReplaceExpression(exp1,exp2,parent,position,t1)
	| _ -> failwith "Unexpected operation in make_replace" 
  in
  let patch = 
	lmap (fun (id,operation) -> 
	  let op' = 
		match operation with
		| InsertTreeNode(_,pos) ->
		  let is_replace,replacing = is_really_a_replace (-1) pos in
			if is_replace then make_replace operation replacing else operation
		| InsertDefinition(_,parent,position,_)
		| MoveDefinition(_,parent,position,_,_)
		| InsertStatement(_,parent,position,_)
		| MoveStatement(_,parent,position,_,_)
		| InsertExpression(_,parent,position,_)
		| MoveExpression(_,parent,position,_,_) -> 
		  let is_replace,replacing = is_really_a_replace parent position in
			if is_replace then make_replace operation replacing else operation
		| _ -> operation
	  in id,op') patch in
	  lfilt (fun (_,x) -> not (hmem removed_ops x)) patch
	  
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
	match (dn exp) with 
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

let alpha_rename diff = 
  (* alpha rename should actually copy everything it alpha renames so that we have
	 both *)
  let diff' = copy diff in
  let rename_edit_action = function
	InsertTreeNode(tn,i) -> InsertTreeNode(visitTreeNode renameVisit tn, i)
  | ReorderTreeNode(tn,i1,i2) -> ReorderTreeNode(visitTreeNode renameVisit tn,i1,i2)
  | ReplaceTreeNode(tn1,tn2,i) -> ReplaceTreeNode(visitTreeNode renameVisit tn1, visitTreeNode renameVisit tn2,i)
  | InsertDefinition(def,i1,i2,ptyp) -> 
	let [def] = visitCabsDefinition renameVisit def in 
	  InsertDefinition(def, i1,i2,ptyp)
  | ReplaceDefinition(def1,def2,i1,i2,ptyp) ->
	let [def1] = visitCabsDefinition renameVisit def1 in
	let [def2] = visitCabsDefinition renameVisit def2 in
	  ReplaceDefinition(def1,def2, i1,i2,ptyp)
  | MoveDefinition(def,i1,i2,p1,p2) ->
	let [def] = visitCabsDefinition renameVisit def in 
	  MoveDefinition(def, i1,i2,p1,p2)
  | ReorderDefinition(def,i1,i2,i3,p) ->
	let [def] = visitCabsDefinition renameVisit def in 
	  ReorderDefinition(def, i1,i2,i3,p)
  | InsertStatement(stmt,i1,i2,p) ->
	let [stmt] = visitCabsStatement renameVisit stmt in
	  InsertStatement(stmt,i1,i2,p)
  | ReplaceStatement(stmt1,stmt2,i1,i2,p) ->
	let [stmt1] = visitCabsStatement renameVisit stmt1 in
	let [stmt2] = visitCabsStatement renameVisit stmt2 in
	  ReplaceStatement(stmt1,stmt2, i1,i2,p)
  | MoveStatement(stmt,i1,i2,p1,p2) ->
	let [stmt] = visitCabsStatement renameVisit stmt in
	  MoveStatement(stmt, i1,i2,p1,p2)
  | ReorderStatement(stmt,i1,i2,i3,p) ->
	let [stmt] = visitCabsStatement renameVisit stmt in
	  ReorderStatement(stmt, i1,i2,i3,p)
  | InsertExpression(exp,i1,i2,p) ->
	InsertExpression(visitCabsExpression renameVisit exp, i1,i2,p)
  | ReplaceExpression(exp1,exp2,i1,i2,p) ->
	ReplaceExpression(visitCabsExpression renameVisit exp1,visitCabsExpression renameVisit exp2, i1,i2,p)
  | MoveExpression(exp,i1,i2,p1,p2) ->
	MoveExpression(visitCabsExpression renameVisit exp,i1,i2,p1,p2)
  | ReorderExpression(exp,i1,i2,i3,p) ->
	ReorderExpression(visitCabsExpression renameVisit exp,i1,i2,i3,p)
  | DeleteTN(tn,par) -> DeleteTN(visitTreeNode renameVisit tn,par)
  | DeleteDef(def,par) -> 
	let [def] = visitCabsDefinition renameVisit def in 
	  DeleteDef(def,par)
  | DeleteStmt(stmt,par) -> 
	let [stmt] = visitCabsStatement renameVisit stmt in 
	  DeleteStmt(stmt,par)
  | DeleteExp(exp,par) -> DeleteExp(visitCabsExpression renameVisit exp,par)
  in
	lmap (fun (id,diff) -> id,rename_edit_action diff) diff'
