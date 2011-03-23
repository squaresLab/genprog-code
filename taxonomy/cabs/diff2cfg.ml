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
	(fun node_id ->
	  fun bb ->
		let preds = ht_find preds node_id (fun _ -> []) in
		let succs = ht_find succs node_id (fun _ -> []) in
		  bb.preds <- bb.preds @ (lmap (fun pred -> (blockfind pred).cid) preds);
		  bb.succs <- bb.succs @ (lmap (fun succ -> (blockfind succ).cid) succs);
		  ) bb_map 

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
  let addSucc (pred : statement node) (succ: statement node) =
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
	let succlst = ht_find succs pred.id (fun _ -> []) in
	let predlst = ht_find preds succ.id (fun _ -> []) in
      if not (List.mem succ.id succlst) then
		hrep succs pred.id (succ.id :: succlst);
	  if not (List.mem pred.id predlst) then
		hrep preds succ.id (pred.id :: predlst)
  in
  let addOptionSucc pred succ =
    match succ with
      None -> ()
    | Some n' -> addSucc pred n'
  in
  let expFallsThrough exp = not ((ends_exp exp) || (starts_exp exp)) in
	match dn stmt with
      COMPUTATION(il,_) when not (expFallsThrough il) -> 
        addOptionSucc stmt next;
		newbb (current_bb@[stmt]) bbs, []
	| RETURN _ 
	| COMPGOTO(_,_)
	| GOTO (_,_) -> newbb (current_bb@[stmt]) bbs, []
	| BREAK _ -> 
	  addOptionSucc stmt break; 
	  newbb (current_bb@[stmt]) bbs,[] 
	| CONTINUE _ -> 
	  addOptionSucc stmt cont; 
	  newbb (current_bb@[stmt]) bbs,[] 
	| IF (exp, blk1, blk2, _) ->
	  addSucc stmt blk1;
	  addSucc stmt blk2;
      addOptionSucc blk2 next;
      addOptionSucc blk1 next;
	  let bbs',current_bb' = 
		newcf stmt exp (newbb current_bb bbs),[] in
	  let bbs1,current_bb1 = cfgStmt blk1 None break cont [] [] in
		(* we already added the next *)
      let bbs2,current_bb2 = cfgStmt blk2 None break cont [] [] in
		bbs'@
		  newbb current_bb1 bbs1 @
		  newbb current_bb2 bbs2, current_bb
	| BLOCK (b,_) -> 
(*	  addBlockSucc b next;*)
      cfgBlock b next break cont bbs current_bb
	| SWITCH(exp,stmts,l) ->
      let bl = findCaseLabeledStmts stmts in
		List.iter (addSucc stmt) bl;
		if not (List.exists 
                  (fun stmt -> 
					match dn stmt with
					  DEFAULT(_) -> true | _ -> false)
                  bl) 
		then 
          addOptionSucc stmt next;
		cfgStmt stmts next next cont (newcf stmt exp (newbb current_bb bbs)) []
	| WHILE(exp,s1,_) -> 
(*	  addSucc s1 stmt;*)
	  addOptionSucc stmt next;
	  cfgStmt s1 (Some(stmt)) next (Some(stmt)) (newcf stmt exp (newbb current_bb bbs)) [] 
	| DOWHILE(exp,s1,_) ->
	  addSucc s1 stmt;
	  addOptionSucc stmt next;
	  cfgStmt s1 (Some(stmt)) next (Some(stmt)) (newcf stmt exp (newbb current_bb bbs)) []
	| FOR(fc,exp1,exp2,s1,_) ->
	  addSucc s1 stmt;
	  addOptionSucc stmt next;
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
	| DEFINITION(def) when ends_def def -> 
	  addOptionSucc stmt next;
	  newbb (current_bb@[stmt]) bbs,[]
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
	let printer = new withNumPrinter in
	  printer#dTree Pervasives.stdout tree ;

  let basic_blocks = lflat (lmap process_tn tns''') in
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
