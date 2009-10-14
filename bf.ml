open Cil
open Printf
open Utils

(*   (\*todo:  1) rehash contents of !gl_cbank so if multiple "return" statements will just be hashed to 1 place *)
  
(*   (\* let sid2 = 13 in  *\) *)
(*   (\* let stmt2 = H.find !gl_cbank sid2 in  *\) *)
(*   (\* printf "stmt2: %d\n%s\n" sid2 (get_stmtkind stmt2); *\) *)
(*   (\* m_ins chrome stmt2 ;   *\) *)


class manual_delVisitor (file : C.file) sid   = object
  inherit nopCilVisitor
  method vstmt s =   ChangeDoChildrenPost(
	s, fun s ->
	  if s.sid = sid then (
		bf_prog_stmt := get_stmt s;
		let block = {	battrs = [] ;bstmts = [] ; } in
		{ s with skind = Block(block) } 
	  ) else s
  ) 
end 
  
class manual_appVisitor (file : C.file) sid stmt = object
  inherit nopCilVisitor
  method vstmt s = ChangeDoChildrenPost(
	s, fun s ->
	  if s.sid = sid then (
		bf_prog_stmt := get_stmt s;
		let copy = copy_obj stmt in
		let block = { 
		  battrs = [] ;bstmts = [ s ; { s with skind = copy ; } ];
		} in
		{ s with skind = Block(block) }
	  ) else s
  )
end

(*	| Instr([Cil.Call(None, Cil.Lval (Var v, NoOffset), [Const zero], loc)])  -> 
		if v.name ="exit" then (debug "exit"; 1./. 2.2) else (1. /. 2.5)*)

let get_type_score mstmt = (
  let sk = mstmt.C.skind in 
  match sk with
	| Return _  -> if !dbl >= 5 then debug "Ret" ; 1. /. 1.
	| Break _  |  Continue _ 
		-> if !dbl >= 5 then debug "breakcont\n"; 1. /. 2.
	| Goto _ -> if !dbl >= 5 then debug "goto\n"; 1. /. 3.
	| Instr _  -> if !dbl >= 5 then debug "instr"; 1./. 3.5
	| Block _  -> if !dbl >= 5 then debug "block"; 1. /. 4.
	| If _  -> if !dbl >= 5 then debug "if\n"; 1. /. 5.
	| Loop _  | Switch _ | TryFinally _ | TryExcept _ ->
		if !dbl >= 5 then debug "long block\n"; 1./. 7.
)



let bf_start chrome = (
  let get_type_score (sk:C.stmtkind)= (
	let stmt:C.stmt = C.mkStmt sk in
	if !dbl >= 5 then debug "%s\n is a " (get_stmt stmt);
	let score = get_type_score stmt in
	if !dbl >= 5 then debug "\n";
	score
  ) in

  let cbank_score_list:(float*int) list ref =  ref [] in
  H.iter(
	fun cbank_id sk -> (
	  let score = get_type_score sk in
	  cbank_score_list := (score,cbank_id):: !cbank_score_list
	)
  )!gl_cbank;

  let cbank_sorted = L.rev (
	L.sort (fun (a,_) (b,_) -> compare a b) !cbank_score_list
  )
  and wpath_sorted = L.rev (
	L.sort (fun (a,_) (b,_) -> compare a b) !gl_wpath
  ) in
  
  if !dbl >= 5 then (
	print_path_fi cbank_sorted; debug "\n";
	print_path_fi wpath_sorted; debug "\n"
  );


  let orig_chrome = copy_obj chrome in

  if !use_alg = alg_BF_w_del then (
	if !dbl >= 3 then debug "Brute-Force DELETE\n";

	L.iter (  (*iter through stmts in programs*)
	  fun (_,sid)-> (
		bf_prog_stmtid := sid;
		printf "WPATH stmt %d\n" sid ;
		
		let new_chrome = copy_obj orig_chrome in
		let v = new manual_delVisitor new_chrome.cfile sid in
		visitCilFileSameGlobals v new_chrome.cfile;
		gp_fitness new_chrome;
	  )
	) wpath_sorted;
	Gc.full_major(); Gc.print_stat stdout
  );

  if !dbl >= 3 then debug "Brute-Force INSERT\n";

  (*iter through cbank*)
  let m_ins2 sid = L.iter (  
  	fun (_,cbank_id) -> (
	  bf_prog_stmtid := sid ;
	  bf_cb_stmtid := cbank_id;

	  let stmt = H.find !gl_cbank cbank_id in
  	  debug "  CBANK: cbank_id %d\n" cbank_id ;
  	  if !dbl >= 5 then debug "%s\n" (get_stmtkind stmt);
	  let new_chrome = copy_obj orig_chrome in
	  let v = new manual_appVisitor new_chrome.cfile sid stmt in
	  visitCilFileSameGlobals v new_chrome.cfile;
	  gp_fitness new_chrome
  	)
  ) cbank_sorted
  in

  (*iter through stmts in programs*)
  L.iter (
	fun (_,sid)-> (
	  printf "WPATH stmt %d\n" sid ;
	  m_ins2 sid ;
  	  Gc.full_major(); Gc.print_stat stdout
	)
  ) wpath_sorted;
)



let manual_ins chrome sid1 sid2 = (
  (* manual_ins chrome_orig 41 35;  (\*printtoken*\) *)
  (* manual_ins chrome_orig 5 13; (\*gcd*\) *)

  gp_fitness chrome;
  let stmt2 = H.find !gl_cbank sid2 in
  printf "stmt2: %d\n%s\n" sid2 (get_stmtkind stmt2);

  let v = new manual_appVisitor chrome.cfile sid1 stmt2 in
  visitCilFileSameGlobals v chrome.cfile ;
  
  gp_fitness chrome;
  write_file "test.c" chrome.cfile;
  failwith "MANUAL INSERT"
)


