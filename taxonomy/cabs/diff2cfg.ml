open Batteries
open Ref
open Utils
open Cabs
open Cabshelper
open Cprint
open Pretty
open Cabsvisit
open Cabswalker

type cfg_node = BASIC_BLOCK of int * statement node list | CONTROL_FLOW of int * statement node * expression node
let cfg_num = ref 0 
let new_cfg () = post_incr cfg_num

let succs = hcreate 10
let preds = hcreate 10 
let bb_map = hcreate 10

let newbb (current : statement node list) (bbs : cfg_node list) = 
  let id = new_cfg () in
  let bb = BASIC_BLOCK(id,current) in
	liter (fun stmt -> hadd bb_map stmt.id bb) current;
	bbs@[bb]
let newcf stmt exp bbs = 
  let id = new_cfg () in
  let bb = CONTROL_FLOW(id, stmt,exp)  in
	hadd bb_map stmt.id bb;
	bbs@[bb]

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

let as_block s = 
  match dn s with
	BLOCK(_) -> s 
  | s' -> {s with node = BLOCK({blabels=[]; battrs=[]; bstmts = [s]}, cabslu) }
(* FIXME: handle loc better *)

(*let ast2bbs tns = 
  let rec stmts2bbs stmts = 
	let bbs,curr = 
	  lfoldl
		(fun (bbs,current_bbs) ->
		  fun stmt ->
			let bbs,current_bbs =
			  match dn stmt with 
			| IF(e1,s1,s2,loc) ->
			  let bbs = bbs@[BASIC_BLOCK(current_bbs);CONTROL_FLOW(stmt,e1)] in
			  let s1' = stmt2bbs s1 in 
			  let s2' = stmt2bbs s2 in
				
			  let s1' = as_block s1 in
			  let s2' = as_block s2 in 
				[{s with node = IF(e1,s1',s2',loc) }]
			| WHILE(e1,s1,loc) ->
			  let s1' = as_block s1 in 
				[{ s with node = WHILE(e1,s1',loc) }]
			| DOWHILE(e1,s1,loc) -> 
			  let s1' = as_block s1 in 
				[{ s with node = DOWHILE(e1,s1',loc) }]
			| FOR(fc,e1,e2,s1,loc) ->
			  let s1' = as_block s1 in 
				[{ s with node = FOR(fc,e1,e2,s1',loc) }]
			| SWITCH(e1,s1,loc) ->
			  let s1' = as_block s1 in 
				[{ s with node = SWITCH(e1,s1',loc) }]
			| CASE(e1,s1,loc) ->
			  let s1' = as_block s1 in 
				[{ s with node = CASE(e1,s1',loc) }]
			| CASERANGE(e1,e2,s1,loc) ->
			  let s1' = as_block s1 in 
				[{ s with node = CASERANGE(e1,e2,s1',loc) }]
			| DEFAULT(s1,loc) ->
			  let s1' = as_block s1 in 
				[{ s with node = DEFAULT(s1',loc) }]
			| LABEL(str,s1,loc) ->
			  let s1' = as_block s1 in 
				[{ s with node = LABEL(str,s1',loc) }]
			| _ -> [s]
		  ) stmts)))

  method vblock block = 
	ChangeDoChildrenPost(block,
						 fun block ->
						   match block.bstmts with
							 [bstmt] -> block
						   | bstmts ->
							 let blocks,current =
							   List.fold_left
								 (fun (blocks,current_block) ->
								   fun stmt ->
									 match dn stmt with
									 | COMPGOTO(_,_)
									 | GOTO(_,_)
									 | CONTINUE(_)
									 | RETURN(_,_)
									 | BREAK(_) ->
										 blocks@ [nd(BLOCK({blabels=[];battrs=[];bstmts=current_block@[stmt]}))], []
									 | COMPUTATION(exp,loc) when contains_call_exp exp -> 
										 blocks@ [nd(BLOCK({blabels=[];battrs=[];bstmts=current_block@stmt}))], []
									 | CASERANGE(_,_,_,_)
									 | DEFAULT(_,_)
									 | CASE(_,_,_) 
									 | LABEL(_,_,_) -> blocks @ current_block, [stmt]
									 | WHILE(_,_,_)
									 | DOWHILE(_,_,_)
									 | FOR(_,_,_,_,_) -> blocks@current_block@[stmt],[]
									 | DEFINITION(def) ->
									   begin
										 match dn def with 
										 | DECDEF(ing,loc) when contains_call_def def -> 
										   blocks@[nd(BLOCK({blabels=[];battrs=[];bstmts=current_block@stmt}))], []
										 | _ ->
										   blocks, current_block@[stmt]
									   end
									 | _ -> blocks,current_block @ [stmt] 

	  ) ([],[]) stmts 
	in
	  bbs @ [curr]
  and block2bbs block = () 
  and tn2bbs tn = 
	match dn tn with
	| Globals(dlist) ->
	| Stmts(slist) -> stmts2bbs slist
	| _ -> failwith "Unexpected treenode in ast2bbs"
  in
	lflat (lmap tn2bbs tns)
*)


let rec cfgStmts slist next break cont bbs current_bb = 
  match slist with
	[] -> bbs,current_bb
  | [s] -> cfgStmt s next break cont bbs current_bb
  | hd :: tl -> 
	let bbs',current_bb' = cfgStmt hd (Some(List.hd tl)) break cont bbs current_bb in
	  cfgStmts tl next break cont bbs' current_bb'

and cfgStmt stmt next break cont (bbs : cfg_node list) (current_bb: statement node list) =
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
			new_bb current_bb bb
		  else bb
	  | _ -> failwith "Unexpected tree node in diff2cfg process_tn"
	in
	  lfilt
		(fun cfg ->
		  match cfg with
			BASIC_BLOCK(_,lst) -> not (List.is_empty lst)
		  | _ -> true) bb
  in
  let basic_blocks = lflat (lmap process_tn tns''') in
	link_up_basic_blocks basic_blocks;
	pprintf "BASIC BLOCKS:\n"; 
	liter
	  (fun bb ->
		match bb with
		  BASIC_BLOCK(num,slist) ->
			pprintf "BASIC BLOCK %d: [ \n" num;
			liter (fun stmt -> pprintf "%s\n" (Pretty.sprint ~width:80 (d_stmt () stmt))) slist;
			pprintf "]\n"
		| CONTROL_FLOW(num,s1,e1) -> 
			pprintf "CONTROL FLOW %d: [ \n" num;
			 pprintf "%s\n" (Pretty.sprint ~width:80 (d_exp () e1));
			 pprintf "]\n"
	  ) basic_blocks;

	let printer = new withNumPrinter in
	  printer#dTree Pervasives.stdout tree ;
	Printf.printf "Done computing, printing.  Preds: ";
	hiter (fun node -> fun preds -> liter (fun pred -> pprintf "%d --> %d\n" pred node) preds) preds;
	Printf.printf "Succs: ";
	hiter (fun node -> fun succs -> liter ( fun succ -> pprintf "%d --> %d\n" node succ) succs) succs
