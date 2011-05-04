open Batteries
open Ref
open Map
open Set
open Utils
open Cabs
open Cabshelper
open Cprint
open Pretty
open Cabsvisit
open Cabswalker


let tree_str tn = Pretty.sprint ~width:80 (d_tree () tn)
let tn_str tn = Pretty.sprint ~width:80 (d_tree_node () tn)
let def_str def = Pretty.sprint ~width:80 (d_def () def)
let stmt_str stmt = Pretty.sprint ~width:80 (d_stmt () stmt)
let exp_str exp = Pretty.sprint ~width:80 (d_exp () exp)

type cnode = | BASIC_BLOCK of statement node list 
	     | CONTROL_FLOW of statement node * expression node 
	     | START 
	     | STOP
	     | ENTRY
	     | REGION_NODE of (cfg_node * label) list

and label = TRUE | FALSE | NONE | DATA | SW

and cfg_node = 
	{ cid : int ;
	  cnode : cnode ;
	  mutable preds : (int * label) list ;
	  mutable succs : (int * label) list  ;
	  all_ast : IntSet.t ;
	}

let labelstr = function
  | NONE -> "NONE"
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | DATA -> "DATA"
  | SW -> "SW"

let rec print_node node = 
  pprintf "((AST_nums: ";
  IntSet.iter (fun num -> pprintf "%d, " num) node.all_ast;
  pprintf "\n"; 
  (match node.cnode with
	BASIC_BLOCK(slist) ->
	  pprintf "BASIC BLOCK %d: [ \n" node.cid;
	  liter (fun stmt -> pprintf "%s\n" (Pretty.sprint ~width:80 (d_stmt () stmt))) slist;
	  pprintf "]\n"
  | CONTROL_FLOW(s1,e1) -> 
	pprintf "CONTROL FLOW %d: [ \n" node.cid;
	pprintf "%s\n" (Pretty.sprint ~width:80 (d_exp () e1));
	pprintf "]\n"
  | START -> pprintf "START %d\n" node.cid
  | STOP -> pprintf "STOP %d\n" node.cid
  | ENTRY -> pprintf "ENTRY %d\n" node.cid
  | REGION_NODE (cls) -> pprintf "REGION_NODE %d [" node.cid; 
	liter (fun (cnode,lab) -> print_node cnode; pprintf " label: %s\n" (labelstr lab)) cls);
  pprintf "PREDS: ["; liter (fun (pred,lab) -> pprintf "(%d,%s) " pred (labelstr lab)) node.preds; pprintf "]\n";
  pprintf "SUCCS: ["; liter (fun (succ,lab) -> pprintf "(%d,%s) " succ (labelstr lab)) node.succs; pprintf "]\n";
  pprintf "))\n"

let cfg_num = ref 0 
let new_cfg () = post_incr cfg_num

type cfg_info = 
	{ mutable ast_ht : (int, IntSet.t) Hashtbl.t ;
	  stmt_succs : (int * label) list IntMap.t ;
	  stmt_preds : (int * label) list IntMap.t;
	  bb_map : cfg_node IntMap.t ; (* maps statements to basic blocks *)
	  node_map : statement node IntMap.t;
	  nodes : cfg_node IntMap.t ;
	  current_node : statement node list }
(* that last one is for bookkeeping while building and
   should be empty eventually! *)

type nexts = 
	{ next : statement node option ;
	  cont : statement node option ;
	  break : statement node option ;
	  stop : statement node }

let new_cfg_info () =
  { ast_ht = hcreate 10 ;
    stmt_succs = IntMap.empty;
    stmt_preds = IntMap.empty;
    bb_map = IntMap.empty; 
    node_map = IntMap.empty;
    nodes = IntMap.empty;
    current_node = [] }

let def_nexts stop = { next = Some(stop); cont = None; break = None; stop = stop }

let add_node info stmt = 
  { info with node_map = IntMap.add stmt.id stmt info.node_map }

let entryn () = 
  let id = new_cfg () in
	{ cid = id; cnode = (ENTRY) ; preds = [] ; succs = [] ; all_ast = IntSet.empty } 

let extras label info = 
  let bb = 
    { cid = new_cfg (); cnode = label ; preds = [] ; succs = [] ; all_ast = IntSet.empty} 
  in
  let stmt = nd(NOP(cabslu)) in
    stmt,{ info with bb_map = IntMap.add stmt.id bb info.bb_map;
	     node_map = IntMap.add stmt.id stmt info.node_map ;
	     nodes = IntMap.add bb.cid bb info.nodes}

let adds info stmt =
  { info with current_node = info.current_node@[stmt] }

let newbb cfg_info =
  if not (List.is_empty cfg_info.current_node) then begin
    let asts = 
      lfoldl 
		(fun numset ->
		  fun stmt -> 
			IntSet.union (ht_find cfg_info.ast_ht stmt.id (fun _ -> pprintf "can't find %d\n" stmt.id; failwith "Not found")) numset)
		IntSet.empty cfg_info.current_node
    in
    let bb = 
      { cid = new_cfg(); 
		cnode = BASIC_BLOCK(cfg_info.current_node) ; 
		preds = []; 
		succs = [] ; 
		all_ast = asts} 
    in
    let cfg_info = 
      lfoldl
	(fun info -> 
	   fun stmt -> { info with bb_map = IntMap.add stmt.id bb info.bb_map }) 
	cfg_info cfg_info.current_node
    in
      {cfg_info with nodes = IntMap.add bb.cid bb cfg_info.nodes ; current_node=[] ; }
  end else cfg_info
    
let newcf cfg_info stmt exp = 
  let id = new_cfg () in
  let bb = { cid = id; cnode = CONTROL_FLOW(stmt,exp) ; preds = []; succs = [] ; all_ast = IntSet.add stmt.id (hfind cfg_info.ast_ht exp.id)} in
    {cfg_info with nodes=IntMap.add bb.cid bb cfg_info.nodes; bb_map = IntMap.add stmt.id bb cfg_info.bb_map}

let get_start nodes = 
  List.find
	(fun cfg_node ->
	  match cfg_node.cnode with
		START -> true
	  | _ -> false) nodes
let get_entry nodes = 
  List.find
	(fun cfg_node ->
	  match cfg_node.cnode with
		ENTRY -> true
	  | _ -> false) nodes
let get_end nodes = 
  List.find
	(fun cfg_node ->
	  match cfg_node.cnode with
		STOP -> true
	  | _ -> false) nodes

let link_up_basic_blocks (info : cfg_info) =
  let rec blockfind id = 
    if IntMap.mem id info.bb_map then IntMap.find id info.bb_map
    else begin
      let node = IntMap.find id info.node_map in
	match dn node with
	  BLOCK(b,_) -> 
	    begin
	      match b.bstmts with
		hd :: tl -> blockfind hd.id
	      | [] -> failwith (Printf.sprintf "something weird in blockfind 1, id: %d, cid: %d" id node.id)
	    end
	| _ -> failwith (Printf.sprintf "something weird in blockfind 2, id: %d, str: %s" id (stmt_str node))
    end 
  in
  let bbs = 
    IntMap.fold
      (fun stmt_id ->
	 fun bb ->
	   fun bbs ->
	     let preds = try IntMap.find stmt_id info.stmt_preds with Not_found -> [] in
	     let succs = try IntMap.find stmt_id info.stmt_succs with Not_found -> [] in
	       bb.preds <- bb.preds@ (lmap (fun (pred,label) -> (blockfind pred).cid,label) preds);
	       bb.succs <- bb.succs @ (lmap (fun (succ,label) -> (blockfind succ).cid,label) succs);
	       IntMap.add bb.cid bb bbs
      ) info.bb_map IntMap.empty
  in
  let lst = IntMap.fold (fun id -> fun bb -> fun lst -> lst @[bb]) bbs [] in
  let stop = get_end lst in
  let info = {info with nodes = bbs } in
	{info with nodes =
		IntMap.map
		  (fun bb ->
			if List.is_empty bb.succs && bb.cid <> stop.cid then begin
			  bb.succs <- [(stop.cid,NONE)];
			  stop.preds <- (bb.cid,NONE) :: stop.preds
			end; bb
		  ) info.nodes }


class getASTNums ht = object(self)
  inherit [IntSet.t] singleCabsWalker

  val ast_info = ht

  method default_res() = IntSet.empty
  method combine set1 set2 = IntSet.union set1 set2

  method wDefinition def = 
    CombineChildrenPost(IntSet.singleton def.id,
			(fun children -> 
(*			   pprintf "getASTNums: def: %d, %s\n" def.id (def_str def);*)
			   let old = ht_find ast_info def.id (fun _ -> IntSet.singleton def.id) in
			   let union = IntSet.union old children in
(*			     pprintf "children: [";
			     IntSet.iter (fun num -> pprintf "%d, " num) union;
			     pprintf "]\n";*)
			     hrep ast_info def.id union; children))

  method wStatement stmt = 
    (match dn stmt with
       BLOCK(b,_) when not (List.is_empty b.bstmts) ->
	 begin
	   let hd = List.hd b.bstmts in
	   let old = ht_find ast_info hd.id (fun _ -> IntSet.singleton hd.id) in
	   let union = IntSet.union old (IntSet.singleton stmt.id) in
	     hrep ast_info hd.id union
	 end
     | _ -> ());
    CombineChildrenPost(IntSet.singleton stmt.id,
			(fun children -> 
(*			   pprintf "getASTNums: stmt: %d, %s\n" stmt.id (stmt_str stmt);*)
			   let old = ht_find ast_info stmt.id (fun _ -> IntSet.singleton stmt.id) in
			   let union = IntSet.union old children in
(*			     pprintf "children: [";
			     IntSet.iter (fun num -> pprintf "%d, " num) union;
			     pprintf "]\n";*)
			     hrep ast_info stmt.id union; children))
	  
  method wExpression exp = 
    CombineChildrenPost(IntSet.singleton exp.id,
			(fun children -> 
(*			   pprintf "getASTNums: exp: %d, %s\n" exp.id (exp_str exp);*)
			   let old = ht_find ast_info exp.id (fun _ -> IntSet.singleton exp.id) in
			   let union = IntSet.union old children in
(*			     pprintf "children: [";
			     IntSet.iter (fun num -> pprintf "%d, " num) union;
			     pprintf "]\n";*)
			     hadd ast_info exp.id union; children))


end

class killSequence = object(self)
  inherit nopCabsVisitor 

(*
  method vdef def = 
	pprintf "def: %d --> %s\n" def.id (def_str def); flush stdout;DoChildren

  method vtreenode tn = 
	pprintf "def: %d --> %s\n" tn.id (tn_str tn); flush stdout;DoChildren

  method vexpr exp = 
	pprintf "exp: %d --> %s\n" exp.id (exp_str exp); flush stdout; DoChildren
*)
  method vstmt stmt = 
(*	pprintf "stmt: %d --> %s\n" stmt.id (stmt_str stmt); flush stdout;*)
	match dn stmt with
	  SEQUENCE(s1,s2,_) -> ChangeDoChildrenPost([s1;s2],fun stmts->stmts)
	| _ -> DoChildren
end

class endsBlockWalker = object(self)
  inherit [bool] singleCabsWalker

  method default_res () = false
  method combine one two = one || two

  method wExpression exp =
	match dn exp with
	  CALL(_,_)
	| EXPR_SIZEOF(_)
	| TYPE_SIZEOF(_)
	| EXPR_ALIGNOF(_)
	| TYPE_ALIGNOF(_)
	  -> Result(true)
	| _ -> Children
	
  method wStatement stmt = 
	match dn stmt with
	| GOTO(_,loc1) 
	| BREAK(loc1)
	| CONTINUE(loc1)
	| RETURN(_,loc1) 
	| COMPGOTO(_,loc1)
	  -> Result(true)
	| _ -> Children

end

class startsBlockWalker = object(self)
  inherit [bool] singleCabsWalker

  method default_res () = false
  method combine one two = one || two
	
  method wStatement stmt =
	match dn stmt with
	| LABEL(_,_,loc1) 
	| CASE(_,_,loc1)
	| CASERANGE(_,_,_,loc1) 
	| DEFAULT(_,loc1) -> Result(true)
	| _ -> Children
end

class findCaseLabels = object(self)
  inherit [statement node list] singleCabsWalker
  method default_res () = []
  method combine one two = one @ two

  method wStatement stmt = 
	match dn stmt with 
	| CASE(_,_,_)
	| CASERANGE(_,_,_,_) 
	| DEFAULT(_,_)
	| LABEL(_,_,_) -> CombineChildren([stmt])
	| _ -> Children
end

class withNumPrinter = object(self)
  inherit defaultCabsPrinterClass as super

  method pStatement () stmt =
	match dn stmt with
	  BLOCK(_) ->
		text (Printf.sprintf "Block num %d: ( " stmt.id) ++ 
		  (super#pStatement () stmt) ++ text ")\n"
	| _ -> super#pStatement () stmt

end

let myfind = new findCaseLabels

let findCaseLabeledStmts stmt = myfind#walkStatement stmt

let myends = new endsBlockWalker
let mystarts = new startsBlockWalker
let ends_exp = myends#walkExpression
let starts_exp = mystarts#walkExpression
let ends_def = myends#walkDefinition
let falls_through_stmt stmt = not ((myends#walkStatement stmt) || (mystarts#walkStatement stmt))


let addSucc info (pred : statement node) (succ: statement node) label =
  let sblock node = 
	match dn node with
	  BLOCK(b,_) ->  
		begin
		  match b.bstmts with
			hd :: tl -> hd
		  | [] -> node
		end
	| _ -> node
  in
  let pblock node = 
	match dn node with
	  BLOCK(b,_) ->  
		begin
		  match (lrev b.bstmts) with
			hd :: tl -> hd
		  | [] -> node
		end
	| _ -> node
  in
  let pred = pblock pred in
  let succ = sblock succ in
  let succlst = try IntMap.find pred.id info.stmt_succs with Not_found -> [] in
  let predlst = try IntMap.find succ.id info.stmt_preds with Not_found -> [] in
  let info =
    if not (List.mem (succ.id,label) succlst) then
	  { info with stmt_succs = IntMap.add pred.id ((succ.id,label) :: succlst) info.stmt_succs }
	else info in
	if not (List.mem (pred.id,label) predlst) then
	  { info with stmt_preds = IntMap.add succ.id ((pred.id,label) :: predlst) info.stmt_preds }
	else info 

let addOptionSucc info pred succ label =
  match succ with
    None -> info
  | Some n' -> addSucc info pred n' label

let rec cfgStmts slist cfg_info nexts = 
  match slist with
	[] -> cfg_info
  | [s] -> cfgStmt s cfg_info nexts
  | hd :: tl -> 
	let info = cfgStmt hd cfg_info { nexts with next = (Some(List.hd tl)) } in
	  cfgStmts tl info nexts
and cfgStmt stmt cfg_info nexts =
  (* FIXME: case exps! *)
  let cfg_info = add_node cfg_info stmt in
  let expFallsThrough exp = not ((ends_exp exp) || (starts_exp exp)) in
    match dn stmt with
      COMPUTATION(il,_) when not (expFallsThrough il) -> 
		newbb (addOptionSucc (adds cfg_info stmt) stmt nexts.next NONE)
    | RETURN _  -> newbb (addSucc (adds cfg_info stmt) stmt nexts.stop NONE)
    | COMPGOTO(_,_)
    | GOTO (_,_) -> newbb (adds cfg_info stmt)
    | BREAK _ -> 
	  newbb (addOptionSucc (adds cfg_info stmt) stmt nexts.break NONE)
    | CONTINUE _ -> 
	  newbb (addOptionSucc (adds cfg_info stmt) stmt nexts.cont NONE)
    | IF (exp, blk1, blk2, _) ->
	  let info = if (List.is_empty cfg_info.current_node) then cfg_info
		else
		  let last_stmt = List.hd (lrev cfg_info.current_node) in
			addSucc cfg_info last_stmt stmt NONE
	  in
	  let info = 
		lfoldl
	      (fun info ->
			fun (one,two,lab) ->
			  addOptionSucc info one two lab) (newbb info) 
	      [(stmt,Some(blk1),TRUE);(stmt,Some(blk2),FALSE);
	       (blk1,nexts.next,NONE);(blk2,nexts.next,NONE)] 
	  in
	(* after newbb, current_node is always empty, so we can compose these *)
	  let info = cfgStmt blk1 (newbb (newcf info stmt exp)) nexts in
	  (* after newbb, current_node is always empty *)
		newbb (cfgStmt blk2 (newbb info) nexts)
    | BLOCK (b,l) -> 
	  let empty = { stmt with node = NODE(BLOCK({b with bstmts=[] },l))} in
	  let info = adds cfg_info empty in
		cfgBlock b info nexts
	(*	  addBlockSucc b next;*)
    | SWITCH(exp,stmts,l) ->
	  let info = if (List.is_empty cfg_info.current_node) then cfg_info
		else
		  let last_stmt = List.hd (lrev cfg_info.current_node) in
			addSucc cfg_info last_stmt stmt NONE
	  in
	  let bl = findCaseLabeledStmts stmts in
	  let info = 
		lfoldl
	      (fun info ->
			fun succ ->
			  addSucc info stmt succ SW)
	      info bl in
	  let info = 
		if not (List.exists 
                  (fun stmt -> 
					match dn stmt with
					  DEFAULT(_) -> true | _ -> false)
                  bl) 
		then 
          addOptionSucc info stmt nexts.next NONE
		else info
	  in
		cfgStmt stmts (newcf  (newbb info) stmt exp) { nexts with break=nexts.next }
    | WHILE(exp,s1,loc) 
    | DOWHILE(exp,s1,loc) 
    | FOR(_,exp,_,s1,loc) ->
	(* OK: everything preceding the loop needs to be a new block, since the
	   loop condition is a jump target *)
	let info = 
	  if not (List.is_empty cfg_info.current_node) then begin
	    let last = List.hd (lrev cfg_info.current_node) in
	    let info = addSucc cfg_info last stmt NONE in
	    let bb = newbb info in 
	      bb
	  end else cfg_info
	  in
	(* we need a cf statement for the loop itself and a set of basic blocks
	   for everything in the loop *)
	  let info = 
		cfgStmt s1 (newcf info stmt exp) { nexts with cont=nexts.next; next = (Some(stmt))} 
	  in
	  let info = addSucc info s1 stmt NONE in 
	  let info = addSucc info stmt s1 TRUE in
	  let info = addOptionSucc info stmt nexts.next FALSE in
	  let info = newbb info in
		info
    | CASE(exp,s1,loc) ->
	  let info = 
		{ (newbb cfg_info) with current_node = [{ stmt with node = NODE(COMPUTATION(exp,loc)) }]} 
	  in
		cfgStmt s1 info nexts
    | CASERANGE(e1,e2,s1,loc) ->
	  let info = 
		{ (newbb cfg_info) with current_node = [{ stmt with node = NODE(COMPUTATION(e1,loc)) }]} 
	  in
		cfgStmt s1 info nexts
	(* FIXME: this is broken *)
    | DEFAULT(s1,loc) -> 
	  let info = addOptionSucc cfg_info s1 nexts.next NONE in
	  let info = newbb info in
		cfgStmt s1 { info with current_node = [stmt] } nexts 
    | LABEL(str,s1,_) ->
	  let info = 
		if (List.is_empty cfg_info.current_node) then cfg_info 
		else
		  let last = List.hd (lrev cfg_info.current_node) in
			addSucc cfg_info last stmt NONE 
	  in
		cfgStmt s1 ({ (newbb info) with current_node = [stmt] }) nexts
    | DEFINITION(def) when ends_def def -> 
	  newbb (adds (addOptionSucc cfg_info stmt nexts.next NONE) stmt)
    | TRY_EXCEPT(b1,e1,b2,loc) ->
	  let info = newbb (cfgBlock b1 (adds cfg_info stmt) nexts) in
	  let info = newbb (cfgBlock b2 info nexts) in
	  (* FIXME: this cf is ALSO BROKEN *)
		newbb (newcf info stmt e1)
    | TRY_FINALLY(b1,b2,loc) ->
	  let info = cfgBlock b1 (adds cfg_info stmt) {nexts with next= (Some(nd(BLOCK(b1,loc)))) } in
		newbb (cfgBlock b2 (newbb info) nexts)
    | _ -> adds cfg_info stmt 
and cfgBlock blk cfg_info nexts = cfgStmts blk.bstmts cfg_info nexts
  
and cfgDef (def : definition node) cfg_info stop startopt = 
  match dn def with
	FUNDEF(_,block,_,_) -> 
	  let info = 
		match block.bstmts,startopt with
		  hd :: tl,Some(start) -> addSucc cfg_info start hd NONE
		| _,_ -> cfg_info
	  in
		cfgBlock block info (def_nexts stop)
  | _ -> cfg_info

class fixForLoop = object(self)
  inherit nopCabsVisitor

  method vblock b = 
    if List.is_empty b.bstmts then
      ChangeTo {b with  bstmts = [nd(NOP(cabslu))]}
    else DoChildren

  method vstmt stmt = 
    match dn stmt with
    | FOR _ ->
	ChangeDoChildrenPost
	  ([stmt],
	   (fun stmts ->
	      lflat
		    (lmap 
		       (fun stmt ->
			  match dn stmt with
			    FOR(fc,e1,e2,s1,loc) ->
			      let def = 
				match fc with
				  FC_DECL(def) -> nd (DEFINITION(def))
				| FC_EXP(exp) -> nd (COMPUTATION(exp,loc))
			      in (* OK: to fix this, we're going to need to allow control_flow statements to generate and kill definitions *)
			      let modifier = COMPUTATION(e2,loc) in
			      let s1' = 
				match dn s1 with
				  BLOCK(b,loc) -> { s1 with node = NODE(BLOCK({b with bstmts = b.bstmts @ [nd(modifier)]},loc)) }
				| NOP _ -> {s1 with node = NODE(modifier) }
				| _ -> {s1 with node = NODE(BLOCK({blabels=[];battrs=[];bstmts=[s1;nd(modifier)]},cabslu)) }
			      in
			      let for' = {stmt with node = NODE(FOR(fc,e1,e2,s1',loc)) } in
				def :: [for']
			  | _ -> [stmt]) stmts)))
	| _ -> DoChildren

end

let ast2cfg def =
  let ast_ht = hcreate 10 in 
  let [def'] = visitCabsDefinition (new fixForLoop) def in
  let [def''] = visitCabsDefinition (new killSequence) def' in
  let ast_walk = new getASTNums ast_ht in 
	ignore(ast_walk#walkDefinition def'');
(*	hiter
	  (fun id ->
		fun children ->
		  pprintf "id: %d ---> [" id;
		  IntSet.iter (fun child -> pprintf "%d, " child) children;
		  pprintf "]\n") ast_ht;*)
  let info = new_cfg_info () in
	info.ast_ht <- ast_ht;
  let start_stmt,info = extras START info in
  let stop_stmt,info = extras STOP info in
  let info =  (cfgDef def'' info stop_stmt (Some(start_stmt))) in
  let info = newbb info in
(*	pprintf "print CFG:\n"; flush stdout;*)
	let info = link_up_basic_blocks info in
(*	  IntMap.iter
		(fun id ->
		  fun bb -> print_node bb) info.nodes;*)
	  info,def''

