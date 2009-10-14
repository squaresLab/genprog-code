open Printf
open Cil

module L = List 
module D = Digest
module H = Hashtbl 
module R = Random 
module S = String 
module A = Array 
module P = Printf
module U = Unix
module C = Cil


let soi = string_of_int 
and sof = string_of_float 
and sob = string_of_bool 
and foi =float_of_int

let dg_out:out_channel ref = ref stdout 
let write_dg fmt = let k res = (output_string !dg_out res) in kprintf k fmt

let debug_out:out_channel ref = ref stdout 
let debug fmt = (
  let k res = (
	output_string !debug_out res; output_string stdout res; flush stdout
  ) in
  kprintf k fmt 
)

let delay secs = (
  let end_t = U.gettimeofday() +. secs in 
  while (U.gettimeofday() < end_t) do () done
)

type time_stat = {tname:string; mutable calls: int; mutable total_t:float}

let func_ht:(string,time_stat)H.t = H.create 255
let time name f a = (
  let start_t = U.gettimeofday() in 
  let finish() = (
	let run_t = U.gettimeofday() -. start_t in
	if not (H.mem func_ht name) then (
	  let trecord:time_stat = {tname = name ; calls = 1; total_t = run_t;} in
	  H.replace func_ht name trecord ;
	) else (
	  let trecord:time_stat = H.find func_ht name in
	  trecord.calls <- trecord.calls + 1;
	  trecord.total_t <- trecord.total_t +. run_t;
	) 
  )in 
  try let res = f a in finish(); res with e -> finish(); raise e
)

let print_time_stat() = (
  let print_func_stat (ts:time_stat) = 
	debug "Name %-15s calls %8d time %7.2f\n" ("\""^ts.tname^"\"") ts.calls ts.total_t 
  in 
  H.iter(fun name ts -> print_func_stat ts) func_ht 
)

(* tracker stats *)
let del_idx=0 and ins_idx = 1 and swap_idx = 2 and xover_idx = 3
type chrome_tracker = int array
let new_chrome_tracker():chrome_tracker = A.make 5 0
let print_chrome_tracker (ct:chrome_tracker) = 
  debug "d %d, i %d, s %d, x %d" ct.(del_idx) ct.(ins_idx) ct.(swap_idx) ct.(xover_idx)

let no_idx = 0 and yes_idx = 1 and tot_idx = 2
type tracker = {
  mutable fitevals_cached_count:int array;
  (*design decision:  fit_avg_gen: total fit gen / # of chromes of that gen
	Even though many chromes are the same (e.g., never went through mutation),
	nonetheless, count them different because they make up the population*)
  mutable fit_total:float; (*fit avg = fit_total / fit_eval_count*)

  (*# of chromes, total fit, # of non-neg chromes, fit of non-neg chromes*)
  mutable popfit_per_gen:(int*float*int*float) array; (*size,fit_of_size*)

  (*fitness of *new* population, after zeros filter etc*)
  (*# of chromes, total fit, best fit*)
  mutable popfit_per_gen_after_filtered:(int*float*float) array;

  mutable compiles_count:int array;  
  mutable mut_count:int array;   
  mutable xover_count:int array;

  mutable curr_gen:int;  
  mutable curr_id: int;

  c_tkr: chrome_tracker;
}

let tkr:tracker = {
  fitevals_cached_count=[|0;0;0|];
  fit_total=0.;  
  popfit_per_gen=[||];
  popfit_per_gen_after_filtered=[||];

  compiles_count=[|0;0|]; 
  mut_count=[|0;0|];
  xover_count=[|0;0|];

  curr_gen = 0;  
  curr_id = 0;

  c_tkr=new_chrome_tracker()
}

let fitness_count = ref 0  (*total eval called (only when not cached)*)

let print_tracker() :unit = (
  debug "\tTracker\n";
  debug "cgen: %d, cid: %d\n" tkr.curr_gen tkr.curr_id ;
  debug "Fitness: eval %d, cached %d, nocached %d (compile %d, nocompile %d)\n"
	tkr.fitevals_cached_count.(tot_idx)
	tkr.fitevals_cached_count.(yes_idx) tkr.fitevals_cached_count.(no_idx)
	tkr.compiles_count.(yes_idx) tkr.compiles_count.(no_idx);
  
  debug "Num Fit Eval (compare) %d\n" !fitness_count ;
  debug "Fitness: total %g, avg=%g/%d= %g\n"
	tkr.fit_total tkr.fit_total tkr.fitevals_cached_count.(tot_idx) 
	(tkr.fit_total /. foi 	tkr.fitevals_cached_count.(tot_idx));
  debug "Mut: total %d succ %d, nosucc %d\n" 
	(tkr.mut_count.(yes_idx) + tkr.mut_count.(no_idx))
  	tkr.mut_count.(yes_idx) tkr.mut_count.(no_idx) ;
  debug "Xover: total %d succ %d, nosucc %d\n" 
	(tkr.xover_count.(yes_idx) + tkr.xover_count.(no_idx))
	tkr.xover_count.(yes_idx) tkr.xover_count.(no_idx) ;

  print_chrome_tracker tkr.c_tkr
)


(* objects *)
type stmt_map = (int, C.stmtkind) H.t 

type tChrome  = { 
  mutable id: int ;
  mutable digest_hex:  string;
  mutable cfile:   C.file ;
  
  (*mutable count:   int ;*)
  (*mutable path:    (float * int) list ;*)
  
  mutable parent1: tChrome option;
  (*mutable parent2 : tChrome option;*)
  
  mutable fitness: float ;
  
  mutable b_gen:   int ; 
  mutable b_t:     string;
  mutable b_t2:    string;
  
  mutable v_mut:   int; 
  mutable v_xover: int;
  
  mutable w_mut:   int ; (*to be consistent to wes*)
  mutable w_xover: int ; (*to be consistent to wes*)

  ct:chrome_tracker;

  (*info if this chrome is a sol candidate*)
  mutable eval_time: float; 
  mutable eval_num: int; 
  mutable eval_gen: int; 
}	



(*** global vars ****)

let port = ref 808
let prog_desc = "SREC: Software Repair using Evolution Computing\n"

let alg_SIMPL, alg_GP, alg_BF, alg_BF_w_del = 1, 2, 3, 4
let use_alg = ref alg_GP

let dbl= ref 3  (*Debug level, 0 is None/Clean output*)
and db_dir = ref "/tmp/jjj"
let seed = ref (-1)
let start_time = ref 0.


let continue = ref false 
and analysis = ref false 
and dcache = ref false
and use_segfault = ref false

let gcc_cmd, ldflags = ref "/home/thanhvu/LOCALDIR/gcc-4.1.2/bin/gcc", ref ""
and gcc_cmd_alt = ref "/nfs/adaptive/tnguyen/LOCALDIR/gcc-4.1.2/bin/gcc"
let good_cmd, bad_cmd = ref "./test-good.sh", ref  "./test-bad.sh" 


(* let total_micro_mut_rate:float ref= ref 0.  *)
(* and delins_rate:float ref =ref 0. *)
(* and delinsswap_rate:float ref =ref 0. *)
let good_path_factor, bad_path_factor, special_factor = ref 0.01, ref 1., ref 1.5
and good_testcase_factor, bad_testcase_factor = ref 1., ref 10. 

let gens = ref 10 and pop = ref 40
let min_fitness, max_fitness = ref 5. , ref 15. (*depends on progs*)

let select_ROULETTE,select_TOURN = 1,2
let use_select = ref select_ROULETTE
let tourn_n = ref 3 and tourn_k = ref 1 

let force_mut = ref true
let mut_WS, mutt_VN = 1, 2
let use_mut = ref mut_WS
let mut_rate = ref 0.06 
let ins_rate,del_rate,swap_rate = ref 1., ref 1., ref 1.

let xover_WS , xover_VN = 1,2 
let use_xover = ref xover_WS

let uniqifier = ref ""

let filename, baseline_file = ref "", ref ""

let count_xover = ref 0
let count_mut = ref 0

let count_fitness_evals = ref 0 (*total fitness evals called (including cached)*)
let compile_fail = ref 0

let v_avg_fit_l:float list ref = ref []  (*avg_fit list*)
let v_bc_fit_l:float list ref = ref []   (*best chrome fit list*)     

let gl_cbank:stmt_map ref = ref (H.create 1) 
and gl_cbank_size = ref 0

let gl_wpath:(float*int) list ref = ref [(0.,0)]
and gl_wpath_length = ref 0

(*let gl_wpath_ht:(int,float) H.t = H.create 255*)

let sol_ht:(D.t,tChrome)H.t = H.create 255
let fitness_ht : (D.t, float) H.t = H.create 255  

let my_settings_str = ref ""

let bf_cb_stmtid = ref (-1)
let	bf_prog_stmt = ref ""
let bf_prog_stmtid = ref (-1)


(***sect: Common Utils***)
let clean_files (s:string) (a:int)(b:int) :unit =  (
  if !dbl >= 4 then debug "cleaning up ... ";
  for i = a to b do 
	let fname = s ^ "-" ^ (soi i) ^"-"^ !filename in 
	let f1 = fname^"-file.c" 
	and f2 = fname^"-prog"
	and f3 = fname^"-good.result"
	and f4 = fname^"-bad.result" in 

	(try U.unlink (f1) with _ -> ());
	(try U.unlink (f2) with _ -> ());	   
	(try U.unlink (f3) with _ -> ());
	(try U.unlink (f4) with _ -> ())
  done;
  (try U.rmdir !db_dir with _ -> ())
)

let check_file_exist fname :bool = (
  if (Sys.file_exists fname) then true 
  else (debug "VERR: File %s does not exist!\n" fname;	false)
)

let get_etime():float = (U.gettimeofday() -. !start_time)

let print_list_int l = L.iter (fun e -> debug "%d " e) l 
and print_list_float l = L.iter (fun e -> debug "%g " e) l

let print_path_fi l = L.iter (fun (a,b) -> debug "(%g %d), " a b) l 
and print_path_i l = L.iter (fun (_,b) -> debug "%d, "  b) l 

 
let print_rand_num seed s =  debug "%s(seed %d [" s seed; print_list_float [R.float 1.0; R.float 1.0; R.float 1.0];  debug "])\n" 


(* return a copy of 'lst' where each element occurs once *) 
let uniq lst = (
  let ht = H.create 255 in 
  let lst = L.filter (
	fun elt -> if H.mem ht elt then false else (H.add ht elt ();true )
  ) lst in  lst 
)
  
let count_lines fname :float = (
  try
	let f_in = open_in fname and nLines = ref 0 in
	(try
	   while true do let line = input_line f_in in ignore line;incr nLines done;
	   0. (*return*)
	 with _ -> (close_in f_in; foi !nLines (*return*))
	)
  with _ -> 0. (*return*)
)

(* return the size of the given file on the disk *) 
let file_size fname = (try let stats = U.stat fname in stats.U.st_size with _ -> 0)
  
let my_int_of_string str:int =(
  try 
	let res = ref 0 in 
	Scanf.sscanf str " %i" (fun i -> res := i) ;
	!res
  with _ -> (
	if S.lowercase str = "true" then 1
	else if S.lowercase str = "false" then 0 
	else failwith ("cannot convert to an integer: " ^ str)
  )
)

let my_float_of_string str:float =
  try 
	let res = ref 0.0 in 
	Scanf.sscanf str " %g" (fun i -> res := i) ;
	!res
  with _ -> (failwith ("cannot convert to an float: " ^ str))
	
let probability p = if p <= 0.0 then false else if p >= 1.0 then true else R.float 1.0 <= p

let rec range i j = if i > j then [] else i :: (range (i+1) j)



let incr_vn2 (a:int ref):int = incr a ; !a   (*icr and return the incremented value*)
let  incr_array_val a idx = a.(idx)<- a.(idx)+1
let  incr_array_val2 a idx amount = a.(idx)<- a.(idx)+amount

(* Returns the elements of 'lst' in a random order. *) 
let random_order lst = (
  let a = L.map (fun x -> (R.float 1.0), x) lst in
  let b = L.sort (fun (a,_) (b,_) -> compare a b) a in 
  L.map (fun (_,a) -> a) b 
)


(*sect: utils with CIL*)
let get_stmt (b:stmt):string= let ds = C.d_stmt () b in let ss = Pretty.sprint ~width:80 ds in  ss
let get_stmtkind (sk:C.stmtkind):string = ( let m_stmt = C.mkStmt sk in get_stmt m_stmt)

let copy_file fname = (
  time "copy_file" (
	fun () -> { (*vn: what's the diff between { and ) *)
	  fname with C.globals = L.map (
		fun g -> match g with 
		  | C.GFun (fd,loc) ->
			  let newfd = 
				C.copyFunction fd fd.C.svar.C.vname in 
			  C.GFun(newfd,loc)
		  |x -> x
	  ) fname.C.globals ; 
	}
  )()
)
  
(* This makes a deep copy of an arbitrary Ocaml data structure *) 
let copy_obj (x : 'a) = let str = Marshal.to_string x [] in (Marshal.from_string str 0 : 'a) 

let print_cbank (a)(b) =
  H.iter(fun i s -> if (a <= i  && i<=b) then (debug "%d \n%s\n\n" i (get_stmtkind s))) !gl_cbank

let write_file fname (f_ast:C.file) = 
  let f_out = open_out fname in C.dumpFile C.defaultCilPrinter f_out fname f_ast; close_out f_out

let can_trace sk = match sk with
  | Instr _ | Return _ | If _ | Loop _ 	-> true
  | Goto _  | Break _ | Continue _ | Switch _ | Block _ | TryFinally _ | TryExcept _ -> false 




(* This visitor walks over the C program AST and builds the hashtable that
 * maps integers to statements. *) 
let vn_numVisitor_count = ref 1 
class vn_numVisitor (h:stmt_map) = object
  inherit nopCilVisitor
  method vblock b = ChangeDoChildrenPost(
	b,(fun b -> L.iter (
		 fun b -> if can_trace b.skind then (
		   let count = !vn_numVisitor_count in 
		   (*b.sid <- count ;*)  (*vn bug: it modifies the input file f (ast)*)
		   incr vn_numVisitor_count;
		   
		   (*printf "/*%d ->\n%s*/\n\n" count
			 (Pretty.sprint ~width:80 (C.d_stmt () b));	*)
		   H.add h count b.skind
		 );
	   ) b.bstmts ; b
	  ))
end 
  

(*utils*)

let printHT ht opt = (  (*print HT contents*)
  if H.length ht = 0 then (printf "ht empty!\n")
  else (
	if opt = 0 then (H.iter (fun k v -> printf "%d -> %d\n" k v) ht)
	else (
	  if opt = 1 then (
		let list_key = ref [] in 
		H.iter (fun k e -> list_key := k :: !list_key) ht;
		list_key := uniq !list_key;
		L.iter (
		  fun e -> printf "%d ->" e; print_list_int (H.find_all ht e); printf "\n" 
		)!list_key;
	  )
	)
  )
)

let printHT1 (h:stmt_map) =  if H.length h = 0 then printf "ht empty!\n"  else (H.iter(	  fun k sk -> printf "/*%d ->\n" k; printf "%s*/\n\n" (get_stmtkind sk) )h  )


let setHT (f:C.file)(h:stmt_map):unit  = 
  if H.length h > 0 then H.clear h;
  vn_numVisitor_count := 1; (*reset the counter*)
  let v = new vn_numVisitor h in 
  visitCilFileSameGlobals v f 
	(*printf "rebuilt ht (size %d)\n" (H.length h)*)

	
(*class def*)


(*sect: type def utilities*)
class vnVisitor (file:C.file) = object
  inherit nopCilVisitor
  method vstmt s =  
	ChangeDoChildrenPost(
	  s, fun s -> debug "sid %d :\n%s\n" s.sid (get_stmt s); s  
	)
end


class sanityVisitor (file : C.file) (visited)  = object
  (* If (x,y) is in the to_swap mapping, we replace statement x 
   * with statement y. Presumably (y,x) is also in the mapping. *) 
  inherit nopCilVisitor
  method vstmt s = ChangeDoChildrenPost( s, fun s -> H.add visited s.sid true ; s) 
end 



class swapVisitor (file : C.file) (to_swap : stmt_map)  = object
  (* If (x,y) is in the to_swap mapping, we replace statement x 
   * with statement y. Presumably (y,x) is also in the mapping. *) 
  inherit nopCilVisitor
  method vstmt s =  ChangeDoChildrenPost(
	s, fun s -> if H.mem to_swap s.sid then (
	  let swap_with:C.stmtkind = H.find to_swap s.sid in 
	  let copy = copy_obj swap_with in
	  (* tk.current.swap <- tk.current.swap + 1 ; 
		 tk.birth_type2 <- SWAP;*)
	  { s with skind = copy } 
	) else s 
  ) 
end 

class delVisitor (file : C.file) (to_del : stmt_map)   = object
  inherit nopCilVisitor
	(* If (x,_) is in the to_del mapping, we replace x with { } -- an
	 * empty statement. *) 
  method vstmt s =   ChangeDoChildrenPost(
	s, fun s ->
	  if H.mem to_del s.sid then (
		let block = {	battrs = [] ;bstmts = [] ; } in
		(*        tk.current.del <- tk.current.del + 1 ; 
				  tk.birth_type2 <- DEL;*)
		{ s with skind = Block(block) } 
	  ) else s
  ) 
end 

class appVisitor (file : C.file) (to_append : stmt_map) = object
  (* If (x,y) is in the to_append mapping, we replace x with
   * the block { x; y; } -- that is, we append y after x. *) 
  inherit nopCilVisitor
  method vstmt s = ChangeDoChildrenPost(
	s, fun s ->
	  if H.mem to_append s.sid then (
		let swap_with = H.find to_append s.sid in 
		let copy = copy_obj swap_with in
		let block = { battrs = [] ;bstmts = [ s ; { s with skind = copy ; } ];
					} in
		(*        tk.current.ins <- tk.current.ins + 1 ; 
				  tk.birth_type2 <- APP;*)
		{ s with skind = Block(block) } 
	  ) else s 
  ) 
end 
  
  
let get_new_chrome id1 file1 :tChrome = {
  id = id1;
  digest_hex = "" ;
  cfile = file1;
  parent1 = None ; (*parent2 = None; *)
  fitness = (-1.0);
  b_gen = 0; 
  b_t = "ORIG"; b_t2 = "UNDEF";
  v_mut = 0 ; v_xover = 0 ;
  w_mut = 0 ; w_xover = 0 ;
  ct=new_chrome_tracker();
  eval_time = (0.) ;
  eval_num = (-1);
  eval_gen = (-1);
} 


let update_chrome_info1 c id p1 (*p2*) fit bg bt bt2 wm wx vm vx:unit = (
  c.id <- id ;

  c.parent1 <- p1;
  (*c.parent2 <- p2;*)

  c.fitness <- fit;

  c.b_gen <- bg;
  c.b_t <- bt;
  c.b_t2 <- bt2;

  c.w_mut <- wm;  
  c.w_xover <- wx;

  c.v_mut <- vm;
  c.v_xover <- vx
)

let get_sum_w_path wp =(
  let t_list:float list = L.map(fun (a,b) -> a *. 1.0) wp in
  let t_sum = L.fold_left ( +. ) 0.0 t_list in
  t_sum
)


let print_chrome ?(details=false) c = (
  debug "%4d %s " c.id c.digest_hex; 
  print_chrome_tracker c.ct;   debug " ";

  if details then (
	debug "\nfit %g, b gen %d, type %s, type2 %s, xover %d, wxover %d, mut %d, wmut %d\n" 
	  c.fitness c.b_gen c.b_t c.b_t2 c.v_xover c.w_xover c.v_mut c.w_mut
  )
)

(* let print_spy_chrome (c)(action)(ht:(int,int)H.t):unit=( *)
(*   debug"\n"; *)
(*   H.iter( *)
(* 	fun s r->  *)
(* 	  let rs = H.find !gl_cbank r in  *)
(* 	  debug "%d, action: %d\n%d%s\n" s action r (get_stmtkind rs) *)
(*   )ht; *)
(*   debug "\n" *)
(* ) *)




let print_chrome_list (l:tChrome list) =(
  let i = ref 0  in
  L.iter (fun cc -> debug "%d: " (incr_vn2 i); print_chrome cc) l 
)

let rec get_parents_chain (c:tChrome)(cl:tChrome list ref) = (
  cl:= c:: !cl; 
  let c_p = c.parent1 in
  match c_p with | None -> ()|	Some (myparent1) -> ( get_parents_chain myparent1 cl ; ())
)
  
let print_chrome_list_item (s:string)(l:tChrome list):unit =
  let scount:int ref = ref 0  in
  
  match s with 
	|   "pcode" -> L.iter (fun cc -> write_file ((soi (incr_vn2 scount))^"-c"^(soi (cc.id))^"-"^cc.digest_hex^".p.c") cc.cfile) l
	|	"pname" ->  L.iter (fun cc -> debug "'c%d %s' " cc.id cc.digest_hex) l 
	|	"pfit" ->   L.iter (fun cc -> debug "%g " cc.fitness) l 
	|	"pwxover" -> L.iter (fun cc -> debug "%d " cc.w_xover) l 
	|	"pwmut" ->   L.iter (fun cc -> debug "%d " cc.w_mut) l 
	|	"pvxover" -> L.iter (fun cc -> debug "%d " cc.v_xover) l 
	|	"pvmut" ->   L.iter (fun cc -> debug "%d " cc.v_mut) l 
	|	"b_gen" ->   L.iter (fun cc -> debug "%d " cc.b_gen) l 
	|	"b_type" ->   L.iter (fun cc -> debug "'%s' " cc.b_t) l 
	|	"b_type2" ->   L.iter (fun cc -> debug "'%s' " cc.b_t2) l 
	| _ -> debug "err: unrecognized print chromme list item\n"

		
let compat = ref false (*if true then will try behave like Wes's orig version*)
let my_sol = ref None
let my_best = ref None
  (*let special_stmts_ht:(int) H.t =  H.create 255*)
  
let rt_intv, rt_avg_limit = 20., 10.0 and rt_avg_lst,rt_start = ref [], ref 0.0 

let get_avg_run_time i = (
  if (i mod (int_of_float rt_intv) = 0) then (
	let now = get_etime() in 
	let rt_avg = (now -. !rt_start) /. rt_intv in 
	if rt_avg > rt_avg_limit then 
	  debug "error: too much time (%.2f), something's wrong!\n" rt_avg;
	rt_avg_lst := rt_avg :: !rt_avg_lst;
	rt_start := now  ;

	if !dbl >= 3 then (
	  debug "avg run time (%d evals, %d sols so far): [ " i (H.length sol_ht);
	  L.iter(
		fun i -> try debug "%.2f " (L.nth !rt_avg_lst i) with _ -> ()
	  )(range 0 5);
	  debug "]\n"
	)
  );
)

let print_gen_stats gen_id = (
  let (tpop,tfit,tpop_noneg,tfit_noneg) = tkr.popfit_per_gen.(gen_id) 
  and (pop_af,fit_af,best_fit) = tkr.popfit_per_gen_after_filtered.(gen_id) in
  debug "--- Stats: Gen %d pop %d, fit %g (avg %g), pop_nonneg %d, fit_nonneg %g. "
	tkr.curr_gen tpop tfit (tfit/. (foi tpop)) tpop_noneg tfit_noneg;
  debug "After filtered: pop %d fit %g avg %g bestfit %g ---\n" 
	pop_af fit_af (fit_af /. (foi pop_af)) best_fit;

)

let print_trace chrome =(
  debug "af{i}=["; L.iter(fun e -> debug "%g " e)(L.rev !v_avg_fit_l);  debug "]; ";
  debug "bf{i}=[";  L.iter(fun e -> debug "%g " e)(L.rev !v_bc_fit_l);  debug "]; ";
  
  let p_chain = ref [] in get_parents_chain chrome p_chain; 
  
  debug "pf{i}= [ ";  print_chrome_list_item "pfit" !p_chain; debug "]; ";
  debug "pwxover{i}= [";  print_chrome_list_item "pwxover" !p_chain; debug "]; ";
  debug "pvxover{i}= [";  print_chrome_list_item "pvxover" !p_chain; debug "]; ";
  debug "pwmut{i}= ["; print_chrome_list_item "pwmut" !p_chain; debug "]; ";
  debug "pvmut{i}= ["; print_chrome_list_item "pvmut" !p_chain; debug "]; ";
  debug "b_gen{i}= ["; print_chrome_list_item "b_gen" !p_chain; debug "]; ";
  debug "pn{i}= ["; print_chrome_list_item "pname" !p_chain; debug "]; ";
  debug "b_type{i}= ["; print_chrome_list_item "b_type" !p_chain; debug "]; ";
  debug "\n"; debug "parents' code written out to *.p.c files\n" ;
  print_chrome_list_item "pcode" !p_chain
)
  
let print_results() =(
  let create_result_string c s = (
	let res = (
	  " " ^ s ^ "_eval_time: " ^ (sprintf "%.2f" c.eval_time) ^
		" " ^ s ^ "_eval_num: " ^ (sprintf "%d" c.eval_num) ^
		" " ^ s ^ "_eval_gen: " ^ (sprintf "%d" c.eval_gen) ^
		" id: "       ^ (soi c.id) ^
		" digest: "   ^ c.digest_hex ^
		" fit: "      ^ (sof c.fitness) 
	)	in
	res
  )in

  let num_sols = H.length sol_ht in
  let my_results = ref ("SOLS: " ^ (soi num_sols)) in  
  let lst_fitness = ref (-1.) in 

  (match !my_sol with
  	 | None -> (
		 (match !my_best with 
			|None -> ()
			|Some(best)->( 
				if !dbl >= 4 then (
				  let bst_file = "highestfit" ^ !uniqifier ^ "-id" ^ 
					soi best.id ^"-" ^ best.digest_hex ^ "-" ^ !filename in
  				  write_file bst_file best.cfile ;
				  
				  let best_res = create_result_string best "bst" in 
				  my_results :=  !my_results ^ " file: " ^ bst_file ^ best_res 
				)
			  )
		 )
	   )
  	 | Some(first_sol,last_sol) -> (
		 let fs,ls = H.find sol_ht first_sol,H.find sol_ht last_sol in 
		 let lst_file = "best" ^ !uniqifier ^ "-id" ^ 
		   soi ls.id ^"-" ^ ls.digest_hex ^ "-" ^ !filename in
  		 write_file lst_file ls.cfile ;
		 lst_fitness := ls.fitness;

		 my_results := !my_results ^ 
		   " cgen: "     ^ (soi tkr.curr_gen) ^
		   " file: "     ^ lst_file ^
		   (create_result_string ls "lst") ^ 
		   (create_result_string fs "fst" ) ^
		   " xover: "    ^ (soi !count_xover) ^
		   " muts: "     ^ (soi !count_mut)^
		   (sprintf " SOLS: %d %d %d" num_sols ls.eval_gen ls.eval_num);

		 
		 if !analysis then print_trace ls;
	   )
  );
  my_results:=!my_results ^ " run_time: " ^ (sprintf "%.2f" (get_etime())) ;
  
  if !dbl >= 3 then debug "%s\n" !my_results;

  if not !continue then (
	v_avg_fit_l:= 14.0 :: !v_avg_fit_l ;
	v_bc_fit_l:=  !lst_fitness :: !v_bc_fit_l ;
	
	if !dbl >= 3 then debug "-----------------\nRESULTS\n";

	debug "%s\n"!my_settings_str ;
	debug "F%s" !my_results ;
	print_rand_num !seed " done: ";

	if !dbl >= 3 then (
	  if num_sols > 0 then (
		debug "-----------------\n";
		
		let  ct_accum = new_chrome_tracker() in
		H.iter ( 
		  fun _ c -> 
			(*print_chrome ~details:true c ; debug "\n";*)
			incr_array_val2 ct_accum del_idx c.ct.(del_idx) ;
			incr_array_val2 ct_accum ins_idx c.ct.(ins_idx) ;
			incr_array_val2 ct_accum swap_idx c.ct.(swap_idx) ;
			incr_array_val2 ct_accum xover_idx c.ct.(xover_idx)
		) sol_ht ;
		
		debug "Tracker Sum: " ;
		print_chrome_tracker ct_accum;
		debug "\n";

	  );
	  
	  debug "-----------------\nTRACKERS\n";
	  print_time_stat();  print_tracker(); debug "\n";
	  
	  if !bf_prog_stmtid >= 0 then (
		if !bf_cb_stmtid >= 0 then (
		  debug "Insert statement (%d) from cbank\n%s\n"
			!bf_cb_stmtid 
			(get_stmtkind (H.find !gl_cbank !bf_cb_stmtid));
		  debug "to statement (%d) from program\n%s\n"
  			!bf_prog_stmtid !bf_prog_stmt
		)
		else(
		  debug "Delete statement (%d) from program\n%s\n"
  			!bf_prog_stmtid !bf_prog_stmt;
		)
	  )
	);
	exit 0
  )
)

let gp_chrome_analysis (chrome:tChrome)  = 

  let update_chrome chrome = (
	chrome.eval_time <- get_etime();
	chrome.eval_gen <- tkr.curr_gen;
	chrome.eval_num <- tkr.fitevals_cached_count.(tot_idx)
  ) in 

  (match !my_best with 
	 |None -> (update_chrome chrome; my_best := Some(chrome))
	 |Some(best_so_far)-> (
		 if chrome.fitness > best_so_far.fitness then (
		   update_chrome chrome; my_best := Some(chrome)
		 )
	   )
  );

  if chrome.fitness >=  !max_fitness then (
  	if not (H.mem sol_ht chrome.digest_hex) then (
	  update_chrome chrome;
  	  H.replace sol_ht chrome.digest_hex chrome;
  	  (match !my_sol with
  		 |None -> my_sol:= Some(chrome.digest_hex,chrome.digest_hex)
  		 |Some(first_sol,_) -> my_sol:=Some(first_sol,chrome.digest_hex)
  	  );
  	  print_results()
  	)
  )


let gp_analysis_mini (c:tChrome)= (

  (*if I was created prev generation and I was not updated*)
  if c.b_gen = tkr.curr_gen - 1  then (
 	(match c.parent1 with  
	   |None -> () 
	   |Some(p)-> (

		   (*todo: is it necessary to reupdate the digest here ? probably no*)
		   (*update_chrome_info p ""; (*updates p if needed*) *)

 		   if not (compare c.digest_hex p.digest_hex = 0) then (

			 (*(\*assert b_type is just SELF_MUT or SELF_XOVER ??*\) *)
 			 match c.b_t with 

 			   | "SELF_MUT"  -> (  (*mut applied to me *)
				   incr count_mut; (*incr count of mutation*)

				   c.v_mut <- 1;
				   c.b_t <- "MUT"; (*mut was applied to me*)

				   if c.b_gen = p.b_gen then (
					 match p.b_t with
					   | "XOVER"
					   | "SELF_XOVER" -> c.b_t <- "XM" 
						   (*result of both x and m*)
					   |_ -> debug "err1: \n";

 				   )
				 )

			   (*xover applied to me*)
			   |"SELF_XOVER" -> (
				   incr count_xover;
				   c.v_xover <- 1;
				   c.b_t <- "XOVER" ;(*birth type is xover*)
				 )
				  
			   |_ -> ()
 		   )
 		 )
 	)
  )
)

let count_compile = ref 0
let gp_fitness (chrome:tChrome) =  (
  time "gpfitness"(
	fun() ->
	  let fitness = ref 0. in 
	  let fname =  !db_dir^"/fittest-" ^ 
		(sprintf "%d" (incr_vn2 count_compile)) ^ "-" ^
		(sprintf "%d" chrome.id) ^ "-" ^ !filename in 

	  let ext_str = ref "" in 

	  (try 
		 (*should be equiv to Wes' compile_counter*)
		 if !compat then incr count_fitness_evals; 
		 
		 let source_out = fname^"-file.c" in
		 write_file source_out chrome.cfile; 
		 chrome.digest_hex <- D.to_hex (D.file source_out);

		 (*gp_analysis_mini chrome;*)
		 
		 if !dbl >= 3 then (debug "E" ; print_chrome chrome); 

		 if H.mem fitness_ht chrome.digest_hex then(
		   incr_array_val tkr.fitevals_cached_count yes_idx ;

		   fitness :=  H.find fitness_ht chrome.digest_hex ;
		   if !dbl >= 3 then ext_str :=  "cached ";
		 )
		 else (
		   incr_array_val tkr.fitevals_cached_count no_idx;

		   (*compile it*)
		   let exe_name = fname ^ "-prog" in 
		   let cmd_compile = sprintf "%s -o %s %s %s >& /dev/null" 
			 !gcc_cmd exe_name source_out !ldflags in 

		   (match time "compile" U.system cmd_compile with
			  | U.WEXITED(0) -> 
				  tkr.compiles_count.(yes_idx) <- tkr.compiles_count.(yes_idx) + 1;
			  | _ -> 
				  incr compile_fail;
				  tkr.compiles_count.(no_idx) <- tkr.compiles_count.(no_idx) + 1;
				  if !dbl >= 5 then debug "FAILED: %s\n" cmd_compile;
				  failwith "compile failed"
		   ); 
		   
		   
		   let g_res,b_res = fname ^ "-good.result", fname ^ "-bad.result" in 
		   
		   (try U.unlink g_res with _ -> () ) ; 
		   (try U.unlink b_res with _ -> () ) ; 
		   
		   (*opt: see if this can be moved, also see if there's much 
			 diff btw soi vs sprintf*)
		   let port_arg = sprintf "%d" !port in incr port ;

		   let cmd_exec_good = sprintf "%s %s %s %s >& /dev/null" 
			 !good_cmd exe_name g_res port_arg in  
		   (match time "execgood" U.system cmd_exec_good with
			  | U.WEXITED(0) -> ()
			  | _ ->  if !dbl >= 5 then debug "FAILED: %s\n" cmd_exec_good ; 
				  failwith "test good failed"
		   ) ; 

		   let cmd_exec_bad = sprintf "%s %s %s %s >& /dev/null" 
			 !bad_cmd exe_name b_res port_arg in 
		   (match time "execbad" U.system cmd_exec_bad with
			  | U.WEXITED(0) -> ()
			  | _ -> if !dbl >= 5 then debug "FAILED: %s\n" cmd_exec_bad ; 
				  failwith "bad failed"
		   ) ; 
		   
		   incr fitness_count ; (* total number of programs tested *) 
		   
		   (*Read in the testcase script results. *)
		   let good,bad = count_lines g_res , count_lines b_res  in 
		   fitness := good +. (!bad_testcase_factor *. bad) ;  

		   H.replace fitness_ht chrome.digest_hex !fitness ; (*cache it*)
		   
		   if !dbl >= 3 then ext_str := "caching";

		   if !dcache then (
			 if !dbl >= 3 then  debug " +dg " ;
			 write_dg "%s %g\n" chrome.digest_hex !fitness;  
			 flush !debug_out
		   );

		   (*clean up files*)
		   (try U.unlink exe_name with _ -> ());
		   (try U.unlink g_res with _ -> ());
		   (try U.unlink b_res with _ -> ());

		 );(*not in cache*)
		 (try U.unlink source_out with _ -> ());

	   with _ -> (
		 fitness := (-2.0) ; 
		 if not !compat then ((*for similar behavior as Wes's then don't exec*)
		   incr fitness_count ; (* total number of programs tested *)
		   H.replace fitness_ht chrome.digest_hex !fitness ;
		   if !dbl >= 3 then  ext_str := "caching no-compile" ;
		 );
		 
		 if !dcache then (
		   if !dbl >= 3 then debug " +dg " ;
		   write_dg "%s %g\n" chrome.digest_hex !fitness; 
		   flush !debug_out 
		 );
	   );

	  );(*try*)

	  chrome.fitness <- !fitness;
	  if !dbl >= 3 then debug "fitness %g %s\n" !fitness !ext_str;


	  tkr.fitevals_cached_count.(tot_idx) <- tkr.fitevals_cached_count.(tot_idx) + 1;
	  tkr.fit_total <- tkr.fit_total +. chrome.fitness;
	  let (tpop,tfit,tpop_noneg,tfit_noneg) = tkr.popfit_per_gen.(tkr.curr_gen) in
	  let (tpop_noneg_t,tfit_noneg_t) = 
		if chrome.fitness >= 0. then (tpop_noneg + 1, tfit_noneg +. chrome.fitness)
		else (tpop_noneg,tfit_noneg)
	  in
	  
	  tkr.popfit_per_gen.(tkr.curr_gen) <- (tpop + 1, tfit +. chrome.fitness, 
											tpop_noneg_t,tfit_noneg_t);


	  gp_chrome_analysis chrome;	  (*have we obtain a sol candidate ?*)
  )()
)(*gp_fitness*)


let get_settings()  = (
  let cmd_s = ref "" in 
  let argc = A.length Sys.argv - 1 in
  for i = 0 to argc do 
	cmd_s:=!cmd_s ^
	  (let f = if i = argc then (sprintf "%s") else (sprintf "%s ") 
	   in f Sys.argv.(i))  
  done;

  (*contains only stmts on bad path but not on pos bath,
	iow: statements having weights 1*)
  let wpath_uniq = L.filter (fun(a,b)-> a = 1.) !gl_wpath in
  let wpath_uniq_length = L.length wpath_uniq in 
  let wpath_weight = (L.fold_left (fun accu (p,_) -> accu +. p) 0.0 !gl_wpath) in
  

  my_settings_str :=  
	"SETTINGS: " ^
	  "name: " ^ !filename ^ 
	  " db_dir: " ^ !db_dir ^
	  " negpath: " ^ (soi !gl_wpath_length) ^ 
	  " negpath_uniq: " ^ (soi wpath_uniq_length) ^ 
	  " negpath_weight: " ^ (sof wpath_weight) ^
	  " bad_path_factor: " ^ (sof !bad_path_factor) ^
	  " good_path_factor: " ^ (sof !good_path_factor) ^
	  " cbank: " ^ (soi !gl_cbank_size) ^
	  " alg: " ^ (soi !use_alg) ^
	  " gens: " ^ (soi !gens) ^ 
	  " pop: " ^ (soi !pop) ^ 
	  " fit_min: " ^ (sof !min_fitness) ^
	  " fit_max: " ^  (sof !max_fitness) ^
	  " bad_testcase_factor: " ^ (sof !bad_testcase_factor) ^
	  " good_testcase_factor: " ^ (sof !good_testcase_factor) ^
	  " use_select: " ^ (soi !use_select) ^
	  " mut_rate: " ^ (sof !mut_rate) ^
	  " del: " ^ (sof !del_rate) ^
	  " ins: " ^ (sof !ins_rate) ^
	  " swap: " ^ (sof !swap_rate) ^
	  " gcc: " ^ !gcc_cmd ^
	  " ldflags: " ^ !ldflags ^
	  " dcache: "  ^ (soi (H.length fitness_ht))^
	  " force_mut: " ^ (sob !force_mut) ^
	  " continue: " ^ (sob !continue) ^
	  " use_alg: " ^ (soi !use_alg) ^
	  " use_xover: " ^ (soi !use_xover) ^
	  " dbl: " ^ (soi !dbl) ^ 
	  " seed: " ^ (soi !seed)  ^
	  " cmd: \"" ^ !cmd_s ^ "\"";

  if !dbl >= 3 then debug "%s\n" !my_settings_str;

  if !dbl >= 5 then (
	debug "Cbank: %d usable statements\n" !gl_cbank_size;
	print_cbank 1 !gl_cbank_size ;
  );


  if !dbl >= 5 then  (
	debug "Neg path: (length %d, uniq %d, weight %g)\n" 
	  !gl_wpath_length wpath_uniq_length wpath_weight;
	L.iter(fun (a,b) -> debug "(%g %d) " a b) !gl_wpath;

	debug "\n"
  );
)

let init_args()= (
  let argDescr = [
	"--dbl", Arg.Set_int dbl, sprintf "X debug output level (def: %d)" !dbl;
	"--seed", Arg.Set_int seed, sprintf "X rand seed (def: %d)" !seed;
	"--use_alg" , Arg.Set_int use_alg, sprintf "X use algorithms:(def: %d)" !use_alg;
	"--uniqifier", Arg.Set_string uniqifier, sprintf "S string to uniqify output (def: %s)" !uniqifier;

	"--dcache", Arg.Set dcache, sprintf "X use digest cache (def: %b)" !dcache;
	"--analysis" , Arg.Set analysis, sprintf "B use analysis: T/F (def: %b)" !analysis;
	"--compat" , Arg.Set compat, sprintf "B compat: T/F (def: %b)" !compat;
	"--continue", Arg.Set continue, sprintf "B continue after a repair is found (def: %b)" !continue; 
	"--use_segfault", Arg.Set use_segfault, sprintf "B bias toward segfault stmts (def: %b)" !use_segfault; 

	(*setup params*)
	"--gcc", Arg.Set_string gcc_cmd, sprintf "S compiler (def: %s)" !gcc_cmd;
	"--ldflags", Arg.Set_string ldflags, sprintf "S LDFLAGS when compiling (def: %s)" !ldflags;
	"--good", Arg.Set_string good_cmd, sprintf "S good-test command (def: %s)" !good_cmd; 
	"--bad", Arg.Set_string bad_cmd, sprintf "S bad-test command (def: %s)"  !bad_cmd; 
	"--gen", Arg.Set_int gens,	sprintf "X GP gens (def: %d)" !gens;
	"--pop", Arg.Set_int pop, sprintf "X Population size (def: %d)" !pop;
	"--min", Arg.Set_float min_fitness, sprintf "F Min fitness (def: %g)" !min_fitness; 
	"--max", Arg.Set_float max_fitness, sprintf "F Max fitness possible (def: %g)" !max_fitness; 

	(*GP methods*)
	"--use_select", Arg.Set_int use_select, sprintf "X use select type ? (def: %d)" !use_select;
	"--force_mut", Arg.Set force_mut, sprintf "B force mutation on initial population: (def: %b)" !force_mut;
	"--use_mut", Arg.Set_int use_mut, sprintf "X use mutation ? (def: %d)" !use_mut;
	"--use_xover", Arg.Set_int use_xover, 
	sprintf "X use xover type X (0: none, 1: xback, 2 xover) (def: %d)" !use_xover;


	"--mut", Arg.Set_float mut_rate, sprintf "F mutation rate (def: %g)" !mut_rate; 
	"--ins", Arg.Set_float ins_rate, sprintf "F rate of mutation insertion (def: %g)" !ins_rate; 
	"--del", Arg.Set_float del_rate, sprintf "F rate of mutation deletion (def: %g)" !del_rate;
	"--swap", Arg.Set_float swap_rate, sprintf "F rate of mutation swap (def: %g)" !swap_rate; 
	"--good_path_factor", Arg.Set_float good_path_factor, 
	sprintf "F multiply F prob to stmts in good path (def: %g)" !good_path_factor;

	"--bad_testcase_factor", Arg.Set_float bad_testcase_factor, 
	sprintf "F multiply F with 'bad' testcases (def: %g)"  !bad_testcase_factor;
  ] in 
  let handleArg str = filename := str in 
  Arg.parse (Arg.align argDescr) handleArg prog_desc;

  C.initCIL();
  
  if !seed = (-1) then (R.self_init();seed := R.bits());
  R.init !seed; (*init seed*)

  (*setup some values now that we have all the pass in arguments info*)
  tkr.popfit_per_gen <- A.create !gens (0,0.,0,0.);
  tkr.popfit_per_gen_after_filtered <- A.create !gens (0,0.,0.);


  (*just so I won't forget*)
  if (compare !filename "atris_comb.c" = 0 ) then (min_fitness := 2. ; max_fitness := 12.);
  if (compare !filename "httpd_comb.c" = 0 ) then (min_fitness := 6. ; max_fitness := 16.);
  if (compare !filename "zunebug.c" = 0 ) then (max_fitness := 25.);
  
  let uniq_t = !uniqifier in 
  uniqifier := 
	"-s"    ^ (soi !seed) ^
	  "-ua" ^ (soi !use_alg) ^
	  "-um" ^ (soi !use_mut) ^
	  "-ux" ^ (soi !use_xover) ^
	  "-g"  ^ (sof !good_path_factor) ^
	  "-m"  ^ (sof !mut_rate);
  if not (compare uniq_t "" = 0) then uniqifier := uniq_t ^ "-" ^ !uniqifier  
)	

let init_files() :tChrome = (
  (*essential file or program - will not work if any missing*)
  if not (check_file_exist !gcc_cmd) then (
	if not (check_file_exist !gcc_cmd_alt) then exit 1 
	else (
	  gcc_cmd := !gcc_cmd_alt ;
	  debug "trying alternative gcc %s\n" !gcc_cmd
	)
  );

  if not (check_file_exist !filename)
	|| not (check_file_exist !gcc_cmd)  
	|| not (check_file_exist !good_cmd)  
	|| not (check_file_exist !bad_cmd)  
  then exit 1 ;

  

  (*read in external files*)

  (*read ast file*)
  let tfile:in_channel = open_in_bin (!filename^".ast") in
  let ast_file:C.file = Marshal.from_channel tfile in 
  close_in tfile;

  (*read stmts hashtable*)
  let tfile:in_channel = open_in_bin (!filename^".ht") in
  let (stmt_size:int),(stmt_ht:stmt_map) = Marshal.from_channel tfile in 
  close_in tfile;

  (*read good path file*)
  let tfile:in_channel = open_in (!filename^".goodpath") in
  let goodpath_ht = H.create 255 in 
  (try 
	 while true do 
	   let stmt_i = my_int_of_string (input_line tfile) in 
	   if not (H.mem goodpath_ht stmt_i) then 
		 H.replace goodpath_ht stmt_i ()
	 done;
   with _ -> close_in tfile);


  (*read badpath file*)
  let tfile:in_channel = open_in (!filename^".path") in
  let path = ref [] in
  (try
  	 while true do
  	   let stmt_i = my_int_of_string (input_line tfile) in
  	   let stmt_w =
  		 if H.mem goodpath_ht stmt_i then  !good_path_factor
		 else !bad_path_factor
	   in
  	   path := (stmt_w, stmt_i) :: !path
  	 done;
   with _ -> close_in tfile);
  path := uniq(L.rev !path);

  
  if !use_segfault then (
	L.iter(fun (a,b) -> debug "(%g %d) " a b) !path;debug "\n";
	let path1:(float*int) list ref = ref [] in 
	let n = ref 2 in  (*these last n is heavily modified*)
	L.iter(
	  fun (w,i) -> 
		let w_t = ref w in 
		if !n > 0 then (
		  n:=!n -1; 
		  w_t := !special_factor;
		  let special_stmt = H.find stmt_ht i in 
		  debug "Special stmt %d w %g \n%s\n" i !w_t (get_stmtkind special_stmt)
		) ;
		path1 := (!w_t,i):: !path1;
	) (L.rev !path);
	path := !path1;
	L.iter(fun (a,b) -> debug "(%g %d) " a b) !path;debug "\n";
  );



  (*read .dg file*)
  let split = Str.split (Str.regexp_string " ") in
  let dg_str = !filename  ^".dg" in 
  if !dcache  then (
	dg_out := open_out_gen  
	  [Open_creat; Open_append; Open_text] 0o666 dg_str ; 
	at_exit (fun () -> close_out !dg_out) ; 
	
	let dg_fin = open_in dg_str in
	(try while true do
	   let pair_str = split (input_line dg_fin) in
	   let dg_id = L.hd pair_str in 
	   let dg_fitness = my_float_of_string (L.hd (L.tl pair_str)) in 
	   (*cache if doesn't exist OR exist with lower score*)
	   if (H.mem fitness_ht dg_id) then (
		 let existing_fitness_val = H.find fitness_ht dg_id in
		 debug "VERR: dg %s already has %s (%g), reading same item with value %g\n" 
		   dg_str dg_id existing_fitness_val dg_fitness;
		 if existing_fitness_val < dg_fitness then (
		   H.replace fitness_ht dg_id dg_fitness ;
		   debug "replaced: %s (%g)\n" dg_id dg_fitness;
		 )
	   )
	   else H.replace fitness_ht dg_id dg_fitness (*not exist*)
	 done ;
	 with _ -> close_in dg_fin); 
  );
  
  
  (
	let sanity_ht = H.create 255 in
	let sanity = new sanityVisitor ast_file sanity_ht in 
	visitCilFileSameGlobals sanity ast_file ; 
	let any = ref false in 
	L.iter (
	  fun (_,sid) ->
		if not (H.mem sanity_ht sid) then (
		  any := true ; debug "\tStatment %d in path but not in AST\n" sid 
		)
	) !path ;
	if !any then exit 1
  );
  

  (*read ldflag file if exists any*)
  if !ldflags = "" then (
	try
	  let tfile:in_channel = open_in "ldflags" in
	  ldflags := input_line tfile ;
	  close_in tfile    
	with _ -> ()
  );

  (*read min/max fitness files*)
  (try
	 let tfile:in_channel = open_in "minfit" in
	 min_fitness := my_float_of_string (input_line tfile) ;
	 close_in tfile;
   with _ -> ()
  );
  (try
	 let tfile:in_channel = open_in "maxfit" in
	 max_fitness := my_float_of_string (input_line tfile) ;
	 close_in tfile;
   with _ -> ()
  );


  baseline_file :=  !filename ^ "-baseline.c" ;
  write_file !baseline_file ast_file ;
  
  gl_cbank := stmt_ht;
  gl_cbank_size := H.length !gl_cbank;

  gl_wpath := !path ;
  gl_wpath_length := L.length !gl_wpath;

  let chrome = get_new_chrome tkr.curr_id ast_file in 

  chrome
)

let super_init() :tChrome = (
  start_time := U.gettimeofday();
  init_args();
  
  (try   (*debug stuff*)
	 let debug_file = !filename ^ !uniqifier ^ ".debug" in
	 debug_out := open_out debug_file ;

	 (let ctr = ref 1 in 
	  db_dir := !db_dir ^ (sof !start_time) ^ "_" ^ !filename ;
	  while Sys.file_exists !db_dir do (
		incr ctr; db_dir := !db_dir^(sof !start_time)^"_"^ (soi !ctr);
	  )done;
	  U.mkdir !db_dir 0o700 );

	 at_exit (fun ()-> close_out !debug_out);
	 at_exit (
	   fun () -> clean_files (!db_dir^"/fittest")
	 	 0 tkr.fitevals_cached_count.(tot_idx) )
   with _ -> (exit 1)
  );

  let chrome = init_files() in
  get_settings();

  
  if !dbl >= 5 then (
    debug "Orig chrome (buggy src) contents:\n";
    let v = new vnVisitor chrome.cfile in 
    visitCilFileSameGlobals v chrome.cfile ;
  );

  (*orig program must able to pass all pos testcases*)
  if !dbl >= 3 then debug "\tInitial test on input \"%s\" =>\n" !filename;
  gp_fitness chrome;
  (*temp hacks, reset tkr values since this initial part is just for testing*)
  tkr.popfit_per_gen.(tkr.curr_gen)<- (0,0.,0,0.);
  if not (chrome.fitness = !min_fitness) || chrome.fitness = !max_fitness then (
	debug "error: orig prog not working (fit %g, min %g max %g)! Time %g\n"
	  chrome.fitness !min_fitness !max_fitness (get_etime());
	print_chrome ~details:true chrome;
	exit 1;
  );
  
  chrome  (*return value*)
)
