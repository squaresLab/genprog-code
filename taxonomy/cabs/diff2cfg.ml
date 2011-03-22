open Batteries
open Ref
open Utils
open Cabs
open Cabshelper
open Cprint
open Pretty
open Cabsvisit
open Cabswalker

type cnode = BASIC_BLOCK of statement node list | CONTROL_FLOW of statement node * expression node

type cfg_node = 
	{ cid : int ;
	  cnode : cnode ;
	  mutable preds : int list ;
	  mutable succs : int list }

let cfg_num = ref 0 
let new_cfg () = post_incr cfg_num

let succs = hcreate 10
let preds = hcreate 10 
let bb_map = hcreate 10
let node_map = hcreate 10

let newbb (current : statement node list) (bbs : cfg_node list) = 
  let id = new_cfg () in
  let bb = { cid = id; cnode = BASIC_BLOCK(current) ; preds = []; succs = [] } in
	liter (fun stmt -> hadd bb_map stmt.id bb) current;
	bbs@[bb]

let newcf stmt exp bbs = 
  let id = new_cfg () in
  let bb = { cid = id; cnode = CONTROL_FLOW(stmt,exp) ; preds = []; succs = [] } in
	hadd bb_map stmt.id bb;
	bbs@[bb]

let link_up_basic_blocks bbs =
  let rec blockfind id = 
	if hmem bb_map id then hfind bb_map id
	else begin
	  let node = hfind node_map id in
		match node.node with
		  BLOCK(b,_) -> 
			begin
			  match b.bstmts with
				hd :: tl -> hfind bb_map hd.id
			  | [] -> failwith "something weird in blockfind"
			end
		| _ -> failwith "something weird in blockfind"
	end 
  in
  hiter
	(fun node ->
	  fun succlist ->
		let node_bb = blockfind node in
		let succs = 
		  lmap
			(fun succ ->
			  (blockfind succ).cid) succlist in
		  node_bb.succs <- succs;
		  hrep bb_map node node_bb;
	) succs;
  hiter
	(fun node ->
	  fun predlist ->
		let node_bb = blockfind node in
		let preds = 
		  lmap
			(fun pred ->
			  (blockfind pred).cid) predlist in
		  node_bb.preds <- preds;
		  hrep bb_map node node_bb;
	) preds

class killSequence = object(self)
  inherit nopCabsVisitor 
  method vstmt stmt = 
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

class makeBBs = object(self)
  inherit nopCabsVisitor

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

let rec cfgStmts slist next break cont bbs current_bb = 
  match slist with
	[] -> bbs,current_bb
  | [s] -> cfgStmt s next break cont bbs current_bb
  | hd :: tl -> 
	let bbs',current_bb' = cfgStmt hd (Some(List.hd tl)) break cont bbs current_bb in
	  cfgStmts tl next break cont bbs' current_bb'

and cfgStmt stmt next break cont (bbs : cfg_node list) (current_bb: statement node list) =
  hadd node_map stmt.id stmt;
  let addSucc (n: statement node) =
	let succlst = ht_find succs stmt.id (fun _ -> []) in
	let predlst = ht_find preds n.id (fun _ -> []) in
      if not (List.mem n.id succlst) then
		hrep succs stmt.id (n.id :: succlst);
	  if not (List.mem stmt.id predlst) then
		hrep preds n.id (stmt.id :: predlst)
  in
  let addOptionSucc (n: statement node option) =
    match n with
      None -> ()
    | Some n' -> addSucc n'
  in
  let addStmtSucc (stmt : statement node) (n : statement node option) =
    match n with
      None -> ()
    | Some n ->
	let succlst = ht_find succs stmt.id (fun _ -> []) in
	let predlst = ht_find preds n.id (fun _ -> []) in
      if not (List.mem n.id succlst) then
		hrep succs stmt.id (n.id :: succlst);
	  if not (List.mem stmt.id predlst) then
		hrep preds n.id (stmt.id :: predlst)
  in
  let addBlockSucc (b: block) (n: statement node option) =
    match b.bstmts with
      [] -> addOptionSucc n
    | hd::_ -> addSucc hd
  in
  let expFallsThrough exp = not ((ends_exp exp) || (starts_exp exp)) in
	match dn stmt with
      COMPUTATION(il,_) when not (expFallsThrough il) -> 
        addOptionSucc next;
		newbb (current_bb@[stmt]) bbs, []
	| RETURN _ 
	| COMPGOTO(_,_)
	| GOTO (_,_) -> newbb (current_bb@[stmt]) bbs, []
	| BREAK _ -> 
	  addOptionSucc break; 
	  newbb (current_bb@[stmt]) bbs,[] 
	| CONTINUE _ -> 
	  addOptionSucc cont; 
	  newbb (current_bb@[stmt]) bbs,[] 
	| IF (exp, blk1, blk2, _) ->
      addStmtSucc blk2 next;
      addStmtSucc blk1 next;
	  let bbs',current_bb' = 
		newcf stmt exp (newbb current_bb bbs),[] in
	  let bbs1,current_bb1 = cfgStmt blk1 next break cont [] [] in
      let bbs2,current_bb2 = cfgStmt blk2 next break cont [] [] in
		bbs'@
		  newbb current_bb1 bbs1 @
		  newbb current_bb2 bbs2, current_bb
	| BLOCK (b,_) -> 
	  addBlockSucc b next;
      cfgBlock b next break cont bbs current_bb
	| SWITCH(exp,stmts,l) ->
      let bl = findCaseLabeledStmts stmts in
		List.iter addSucc bl;
		if not (List.exists 
                  (fun stmt -> 
					match dn stmt with
					  DEFAULT(_) -> true | _ -> false)
                  bl) 
		then 
          addOptionSucc next;
		cfgStmt stmts next next cont (newcf stmt exp (newbb current_bb bbs)) []
	| WHILE(exp,s1,_) -> 
	  addStmtSucc s1 (Some stmt);
	  cfgStmt s1 (Some(stmt)) next (Some(stmt)) (newcf stmt exp (newbb current_bb bbs)) [] 
	| DOWHILE(exp,s1,_) ->
	  addStmtSucc s1 (Some stmt);
	  cfgStmt s1 (Some(stmt)) next (Some(stmt)) (newcf stmt exp (newbb current_bb bbs)) []
	| FOR(fc,exp1,exp2,s1,_) ->
	  addStmtSucc s1 (Some stmt);
	  cfgStmt s1 (Some(stmt)) next (Some(stmt)) (newcf stmt exp2 (newbb current_bb bbs)) []
	| CASE(exp,s1,_) ->
	  let bbs',current_bb' = cfgStmt s1 next break cont bbs [] in
		newcf stmt exp (newbb current_bb' bbs'),current_bb
	| CASERANGE(e1,e2,s1,_) ->
	  let bbs',current_bb' = cfgStmt s1 next break cont bbs [] in
		newcf stmt e1 (newbb current_bb' bbs'), current_bb
	| DEFAULT(s1,_) -> 
	let bbs',current_bb' = cfgStmt s1 next break cont bbs [] in
	  newcf stmt (nd(NOTHING)) (newbb current_bb' bbs'), current_bb
  | LABEL(str,s1,_) ->
	cfgStmt s1 next break cont (newbb current_bb bbs) [stmt]
  | DEFINITION(def) when ends_def def -> newbb (current_bb@[stmt]) bbs,[]
  | TRY_EXCEPT(b1,e1,b2,_) ->
	let bbs',current_bb' = cfgBlock b1 next break cont bbs (current_bb@[stmt]) in
	let bbs'',current_bb'' = cfgBlock b2 next break cont bbs' [] in
	  newbb current_bb' (newcf stmt e1 (newbb current_bb'' bbs')), []
  | TRY_FINALLY(b1,b2,loc) ->
	let bbs',current_bb' = cfgBlock b1 (Some(nd(BLOCK(b1,loc)))) break cont bbs (current_bb@[stmt]) in
	let bbs'',current_bb'' = cfgBlock b2 next break cont bbs' current_bb' in
	  newbb current_bb' (newbb current_bb' bbs''), []
  | _ -> bbs,current_bb@[stmt]
and cfgBlock  blk next break cont bbs current_bb = 
  cfgStmts blk.bstmts next break cont bbs current_bb
	
and cfgDef (def : definition node) = 
  match dn def with
	FUNDEF(_,block,_,_) -> cfgBlock block None None None [] []
  | _ -> [],[]

let ast2cfg tree =
  let fname,tns = tree in
  let rec conv_exps_stmts (tns : tree_node node list) : tree_node node list = 
	match tns with
	  hd :: rest ->
		begin
		  match dn hd with
		  | Exps([elist]) ->
			{hd with node = Stmts([nd(COMPUTATION(elist,cabslu))])} :: conv_exps_stmts rest 
		  | Exps(elist) ->
			{ hd with node = Stmts([nd(COMPUTATION(nd(COMMA(elist)),cabslu))])} :: conv_exps_stmts rest 
		  | _ -> hd :: conv_exps_stmts rest 
		end
	| [] -> []
  in
  let rec comb_stmts tns = 
	match tns with
	  node1 :: node2 :: rest ->
		begin
		  match dn node1, dn node2 with
		  | Stmts(slist1), Stmts(slist2) ->
			{node1 with node = Stmts(slist1 @ slist2)} :: comb_stmts rest 
		  | _ -> node1 :: (comb_stmts (node2 :: rest))
		end
	| fst :: rest -> fst :: (comb_stmts rest)
	| [] -> []
  in
  let tns' = conv_exps_stmts tns in
  let tns'' = comb_stmts tns' in
	Printf.printf "In diff2cfg, input AST:\n"; flush stdout;
	dumpTree defaultCabsPrinter Pervasives.stdout tree;
  let seqvisitor = new killSequence in
  let _,tns''' = visitTree seqvisitor (fname,tns'') in
	Printf.printf "After processing1, AST:\n"; flush stdout;
	  dumpTree defaultCabsPrinter Pervasives.stdout (fname,tns''') ;
  let process_tn tn = 
	let bb = 
	  match dn tn with
	  | Globals(dlist) -> 
		lflat
		  (lmap
			 (fun def ->
			   let bb,current_bb = cfgDef def in
				 if not (List.is_empty current_bb) then 
				   newbb current_bb bb
				 else bb) dlist)
	  | Stmts(slist) -> 
		let bb,current_bb = cfgStmts slist None None None [] [] in
		  if not (List.is_empty current_bb) then 
			newbb current_bb bb
		  else bb
	  | _ -> failwith "Unexpected tree node in diff2cfg process_tn"
	in
	  lfilt
		(fun cfg ->
		  match cfg.cnode with
			BASIC_BLOCK(lst) -> not (List.is_empty lst)
		  | _ -> true) bb
  in
	pprintf "Before process\n"; Pervasives.flush Pervasives.stdout;
	let printer = new withNumPrinter in
	  printer#dTree Pervasives.stdout tree ;

  let basic_blocks = lflat (lmap process_tn tns''') in
	pprintf "Before link up\n"; Pervasives.flush Pervasives.stdout;
	link_up_basic_blocks basic_blocks;
	pprintf "BASIC BLOCKS:\n"; 
	liter
	  (fun bb ->
		(match bb.cnode with
		  BASIC_BLOCK(slist) ->
			pprintf "BASIC BLOCK %d: [ \n" bb.cid;
			liter (fun stmt -> pprintf "%s\n" (Pretty.sprint ~width:80 (d_stmt () stmt))) slist;
			pprintf "]\n"
		| CONTROL_FLOW(s1,e1) -> 
			pprintf "CONTROL FLOW %d: [ \n" bb.cid;
			 pprintf "%s\n" (Pretty.sprint ~width:80 (d_exp () e1));
			 pprintf "]\n");
		pprintf "Preds: "; liter (fun pred -> pprintf "%d, " pred) bb.preds;
		pprintf "\nSuccs: "; liter (fun pred -> pprintf "%d, " pred) bb.succs;
		  pprintf "\n\n";
	  ) basic_blocks
