open Cil
open Printf
open Utils


let gp_get_best_chrome chromes :tChrome=(
  let best_chrome = ref (L.hd chromes) and best_chrome_fitness = ref !min_fitness in
  L.iter (
	fun c -> 
	  let fit = c.fitness in
	  (*design : >= because to avoid the same chrome again (e.g., if there's some other chrome achieving same fit, use it*)
	  if fit >= !best_chrome_fitness then (best_chrome_fitness := fit;	best_chrome := c )
  ) chromes;
  !best_chrome
)


let create_new_chrome chrome visitor ht action :tChrome= (
  let new_cfile = copy_obj chrome.cfile in 
  let my_visitor = visitor new_cfile ht in 
  visitCilFileSameGlobals my_visitor new_cfile ;

  let new_b_t, new_bt2, new_w_mut, new_w_xover=
	if action = xover_idx then (
	  "SELF_XOVER",
	  "UNDEF2",
	  chrome.w_mut,
	  chrome.w_xover+1
	)else(
	  "SELF_MUT",
	  "UNDEF",
	  chrome.w_mut+1,
	  chrome.w_xover
	)
  in

  (*stmt map, only for compat w/ v's xover*)
  let new_ct = copy_obj chrome.ct in 
  incr_array_val new_ct action ;
  tkr.c_tkr.(action) <- tkr.c_tkr.(action) + new_ct.(action);
  
  tkr.curr_id <- tkr.curr_id +1;

  {
	id = tkr.curr_id ;
	digest_hex = "Something";
	cfile = new_cfile;
	parent1 = (Some chrome);
	fitness = (-1.0);
	b_gen = tkr.curr_gen;
	b_t = new_b_t;
	b_t2 = new_bt2;
	v_mut = 0; 
	v_xover = 0;
	w_mut = new_w_mut;
	w_xover = new_w_xover;
	ct = new_ct;
	eval_time = (0.);
	eval_num = (-1);
	eval_gen = (-1);
  }
)



let get_macro_mut_op() :int = (
  let r = R.float (!ins_rate +. !del_rate +. !swap_rate) in 
  if r  < !swap_rate then swap_idx
  else if r < !swap_rate +. !del_rate then del_idx
  else  ins_idx
)


let gp_mutate ?(force=false) chrome prob  :tChrome = (
  time "mutate_w"(
	fun()->
	  let must_modify_step = ref None in 
	  if force then (
		match random_order !gl_wpath with
		  | [] -> ()
		  | (step_prob,path_step) :: tl -> must_modify_step := Some(path_step )
	  ); 

	  let ht = H.create 255 
	  and  any = ref false in (* any mutations made? *) 
	  L.iter (
		fun (step_prob,path_step) ->  (*path step is global (not indiv)*)
		  let forced = Some(path_step) = !must_modify_step in 
		  
		  if (probability step_prob && probability prob) || forced then (
			let rand_id = 1 + (R.int !gl_cbank_size) in
			(* ~((a|b)&~c) = (~(a|b)|c) = ((~a & ~b)|c) *)
			if (not (H.mem ht rand_id) && not (H.mem ht path_step)) || forced then (
			  try 				
				let ss = H.find !gl_cbank path_step 
				and rs = H.find !gl_cbank rand_id in 
				H.add ht path_step rs ;
				H.add ht rand_id ss ;
				any := true ; 
			  with _ -> ()
			) ;
		  )
	  ) !gl_wpath;  (*only mutation with the global bad path*)
	  
	  if !any then (
		incr_array_val tkr.mut_count yes_idx;

		let action = get_macro_mut_op() in 
		let v = 
		  if action = swap_idx then (new swapVisitor)
		  else if action = del_idx then (new delVisitor)
		  else (new appVisitor)
		in 
		
		let new_chrome = create_new_chrome chrome v ht action in
		
		new_chrome
	  )
	  else (
		incr_array_val tkr.mut_count no_idx;
		
		if force then ( (* similar to Wes*)
		  (match !must_modify_step with
			 | None -> () 
			 | Some(x) -> debug "must modify %d, did not\n" x
		  ) ; 
		  assert false;
		) ;
		chrome  (*no mut made, returns orig*)
	  )
  )()
)(*gp_mutate*)




(*** Crossover ***)
let gp_xover_vn_helper_compat chrome1 chrome2 to_swap1 to_swap2  =(

  let ht1,ht2 = H.create 1, H.create 2 in setHT chrome1.cfile ht1;  setHT chrome2.cfile ht2;

  (*let l1,l2 = (H.length ht1), (H.length ht2) in
	if not (l1 = l2) then debug "l1: %d, l2: %d\n" l1 l2 ;*)

  let get_rand_stmt ht=	let rid = 1 + R.int (H.length ht) in let rs = H.find ht rid in rid,rs in

  let rid1,rs1 = get_rand_stmt ht1 and rid2,rs2 = get_rand_stmt ht2 in 
  H.add to_swap1 rid1 rs2;  H.add to_swap2 rid2 rs1
)
  

let gp_xover_vn	chrome1 chrome2 num_swap :(tChrome * tChrome) =(
  let child1,child2 = (copy_obj chrome1), (copy_obj chrome2) in
  
  tkr.curr_id <- tkr.curr_id +1;
  if !analysis then (
	update_chrome_info1 child1
	  tkr.curr_id
	  (Some chrome1) (*(Some chrome2)*)
	  (-1.0)
	  tkr.curr_gen
	  "SELF_XOVER" "UNDEF2"
	  (child1.w_mut)
	  (child1.w_xover+1)
	  0 (*v_mut*)
	  0 (*v_xover*) (*is updated later*)
  )
  else(
	update_chrome_info1 child1
	  tkr.curr_id
	  None (*(Some chrome2)*)
	  (-1.0)
	  tkr.curr_gen
	  "SELF_XOVER" "UNDEF2"
	  (child1.w_mut)
	  (child1.w_xover+1)
	  0 (*v_mut*)
	  0 (*v_xover*) (*is updated later*)
  )
  ;


  tkr.curr_id <- tkr.curr_id +1;
  if !analysis then (
	update_chrome_info1 child2
	  tkr.curr_id
	  (Some chrome2) (*(Some chrome1)*)
	  (-1.0)
	  tkr.curr_gen
	  "SELF_XOVER" "UNDEF2"
	  (child2.w_mut)
	  (child2.w_xover+1)
	  0 (*v_mut*)
	  0 (*v_xover*) (*is updated later*)
  )
  else(
	update_chrome_info1 child2
	  tkr.curr_id
	  None
	  (-1.0)
	  tkr.curr_gen
	  "SELF_XOVER" "UNDEF2"
	  (child2.w_mut)
	  (child2.w_xover+1)
	  0 (*v_mut*)
	  0 (*v_xover*) (*is updated later*)
  );

  let to_swap1,to_swap2 = (H.create 255), (H.create 255) in

  gp_xover_vn_helper_compat child1 child2 to_swap1 to_swap2 ;
  
  
  (let my_visitor1 = new swapVisitor child1.cfile to_swap1 in
   visitCilFileSameGlobals my_visitor1 child1.cfile);
  
  (let my_visitor2 = new swapVisitor child2.cfile to_swap2 in
   visitCilFileSameGlobals my_visitor2 child2.cfile);
  
  child1, child2
)

(*let gp_xover_vn chrome1 chrome2 num_swaps :(tChrome*tChrome)= (chrome1, chrome2)*)

let test visitor chrome ht= (
  let new_cfile = copy_obj chrome.cfile in 
  let my_visitor = visitor new_cfile ht in 
  visitCilFileSameGlobals my_visitor new_cfile ;  
)
  
let gp_xover_w chrome1 chrome2 :(tChrome*tChrome)= (
  time "gp_xover_w"(
	fun()->
	  let cutoff = 1 + (R.int (pred !gl_wpath_length))
	  and ht1,ht2 = (H.create 255), (H.create 255)
	  and any = ref false in (* any cross-over made? *) 
	  
	  let where = ref 0 in  (* where are we in the path? *)
	  L.iter(
		fun (pr,ps)	->  (
		  if !where >= cutoff then (
			try 
			  if probability pr then (
				let s = H.find !gl_cbank ps in 
				H.add ht1 ps s; H.add ht2 ps s;
				any := true ; 
			  )
			with _ -> ()
		  )
		) ;
		  incr where (* good catch, Vu *) 
	  ) !gl_wpath;
	  
	  
	  if !any then (
		incr_array_val tkr.xover_count yes_idx;

		let action = xover_idx in 
		let child1 = create_new_chrome chrome1 (new swapVisitor) ht1 action 
		and child2 = create_new_chrome chrome1 (new swapVisitor) ht2 action 
		in
		child1, child2	(*return*)
	  )
	  else(
		incr_array_val tkr.xover_count no_idx;
		chrome1, chrome2
	  )
  )()
)


let gp_pop_fitness chrome_pops desired_number :tChrome list * float= (

  let pop_with_fitness:(tChrome list) = L.map (fun chrome -> gp_fitness chrome; chrome) chrome_pops in

  (*if @ last gen, then quit*)
  if (tkr.curr_gen = !gens - 1) then ( 	continue := false;  print_results () );

  (*drop out 0 fitness indiv*)
  let filtered_pops = ref (	
	L.filter (
	  fun chrome ->	if !use_select = select_ROULETTE then chrome.fitness > 0. else chrome.fitness >= 0. 
	) pop_with_fitness ) in

  assert(L.length !filtered_pops > 0) ;

  assert(desired_number mod 2 = 0) ;
  while L.length !filtered_pops < desired_number do
	if !dbl >= 3 then (
	  debug "\tViable Size %d; doubling\n" (L.length !filtered_pops )
	);
	filtered_pops := !filtered_pops @ !filtered_pops;
  done ;

  let total = L.fold_left (fun acc chrome ->  ( acc +. chrome.fitness)  ) 0. !filtered_pops in 

  let best_chrome_fit = let bc = gp_get_best_chrome !filtered_pops in bc.fitness in
  let pop_len = L.length !filtered_pops in

  tkr.popfit_per_gen_after_filtered.(tkr.curr_gen) <- (pop_len,total,best_chrome_fit);
  
  v_bc_fit_l :=  best_chrome_fit::!v_bc_fit_l ;
  v_avg_fit_l := (total /. foi pop_len):: 
	!v_avg_fit_l ;
  (*debug "total %g, pop %d, avg %g\n" total pop_len (total /. foi pop_len);*)
  
  if total <= 0. then failwith "selection: total <= 0"; 

  if !dbl >= 3 then print_gen_stats tkr.curr_gen ;

  !filtered_pops,total (*return*)
)
  



let gp_select_helper(chromes:tChrome list) n k :tChrome list= (
  (*get n from lists*)

  let chromes_pop_size = L.length chromes and selected_chromes_ht = H.create n in

  assert (k < n && n < chromes_pop_size);
  
  while H.length selected_chromes_ht < n do
	let chrome_i = R.int chromes_pop_size in
	H.replace selected_chromes_ht chrome_i (L.nth chromes chrome_i)
  done;

  (*select best k*)
  let selected_chromes_list  = ref [] in
  while L.length !selected_chromes_list < k do
	(*find largest one*)
	let largest_fitness:float ref = ref (neg_infinity) and
		largest_fitness_id:int ref = ref (-1) in
	H.iter ( fun i c ->
			   if c.fitness > !largest_fitness then (
				 largest_fitness := c.fitness;
				 largest_fitness_id := i)
		   ) selected_chromes_ht ;

	assert (not (!largest_fitness_id = -1));
	
	(*added to return list*)
	let sc = H.find selected_chromes_ht !largest_fitness_id in
	selected_chromes_list := sc::!selected_chromes_list ;

	(*and remove it from the ht*)
	H.remove selected_chromes_ht !largest_fitness_id;
  done;

  !selected_chromes_list
)


let gp_select_tourn (chromes:tChrome list)(breeding_pop_size:int) :tChrome list= (
  time "tourn_sel" (
	fun() ->
	  let breeding_pop = ref [] in
	  
	  (*always save the best*)
	  breeding_pop := gp_get_best_chrome(chromes) :: !breeding_pop;
	  
	  (*get the rest*)
	  let k = ref !tourn_k in
	  while (L.length !breeding_pop) < breeding_pop_size do
		breeding_pop := (gp_select_helper chromes !tourn_n !k) @ !breeding_pop;
		let curr_breeding_pop_size:int = L.length !breeding_pop in
		if curr_breeding_pop_size + !k > breeding_pop_size then
		  k := breeding_pop_size - curr_breeding_pop_size
	  done;
	  
  	  !breeding_pop
  )()
)


let gp_select_roulette population total desired :tChrome list = (
  time "roulette_sel" (
	fun() ->
	  let normalized = L.map (fun a -> a, a.fitness /. total) population in 
	  let sofar = ref 0.0 in 
	  let accumulated = L.map (
		fun (a,normalized) ->
		  let res = normalized +. !sofar in sofar := !sofar +. normalized ;
		  (a,res)
	  ) normalized in 
	  let distance_between_pointers = 1.0 /. (float_of_int desired) in 
	  let offset = R.float distance_between_pointers in 
	  let result = ref [] in 
	  for i = 0 to pred desired do
		let marker = offset +. ((float_of_int i) *. distance_between_pointers) in 
		let rec walk lst = match lst with
		  | [] -> (* error! should never happen! *) 
			  debug "desired = %d\n" desired ; 
			  debug "distance_between_pointers = %g\n" distance_between_pointers ;
			  debug "offset = %g\n" offset ;
			  debug "i = %d\n" i ; 
			  debug "marker = %g\n" marker ;
			  failwith "selection problem" 
		  | (elt, acc) :: rest -> 
			  if acc > marker then result := elt :: !result 
			  else walk rest 
		in
		walk accumulated
	  done ;
	  !result 
  )()
)







let gp_step original chrome_pops desired_number: (tChrome list) = (

  (*filter / create new pop w/ pos fitness only*)
  let filtered_pops,total = gp_pop_fitness chrome_pops desired_number in

  (*sampling down to best x/2*)
  let breeding_population = 
	if !use_select = select_ROULETTE then gp_select_roulette filtered_pops total (desired_number/2) 
	else gp_select_tourn filtered_pops (desired_number/2) 
  in

  assert(L.length breeding_population = desired_number / 2) ;

  let order = random_order breeding_population in

  (*top half x/2 gets to reproduce*)
  let rec walk lst = match lst with
	| mom :: dad :: rest ->
		let kid1, kid2 = 
		  if !use_xover=0 then 
			mom, dad
		  else if !use_xover=1 then gp_xover_w mom dad (*default is Wes's*)
		  else if !use_xover=2 then gp_xover_vn mom dad 1 (*use Vu's*)
		  else( debug "err1: undef xover method %d" !use_xover; assert false)
		in
		[ mom; dad; kid1; kid2] :: (walk rest)
	| [] -> []
	| singleton -> [ singleton ; singleton ]
  in

  let res = L.flatten (walk order) in

  (*For every chrome, consider it and a mutant of it *)
  let res = L.flatten (
	L.map (fun chrome ->[chrome; if !use_mut = 1 then gp_mutate chrome !mut_rate else chrome]) res) 
  in

  assert(L.length res = desired_number * 2);
  res (*return*)
)


  
let gp_init_pop chrome mut_fun :tChrome list = (
  let res = ref [chrome] in 
  for i = 2 to !pop do
	let new_chrome = mut_fun chrome (!mut_rate *. 2.0) in 
	
	if !use_alg = alg_SIMPL then gp_fitness new_chrome	(*eval fitness right away*)
	else res := new_chrome :: !res ;
  done ;

  !res
)


let gp_start chrome = (
  if !dbl >= 3 then debug "\tGenetic Programming starts ==>\n" ;

  let population:(tChrome list) ref = ref (gp_init_pop chrome (gp_mutate ~force:!force_mut)) in  

  while tkr.curr_gen < !gens do
	if !dbl >= 3 then debug "*** Gen %d (size %d)\n" tkr.curr_gen (L.length !population);

	if !dbl >= 5 then print_rand_num !seed (sprintf "gen %d " tkr.curr_gen);
	population := gp_step chrome !population !pop;
	tkr.curr_gen <- tkr.curr_gen + 1;
  done;
)

let simpl_start	chrome = (
  if !dbl >= 3 then debug "\tSimple Algorithm starts ==>\n" ;
  ignore(gp_init_pop chrome (gp_mutate ~force:!force_mut))
)


(*testing, debug*)

(*xover debug*)
let gp_xover_db (chrome_orig:tChrome)= (
  printf "xover_db\n" ;
  printf "orig: " ; print_chrome chrome_orig;
  (* 	  let chrome1 = gp_mutate chrome_orig ~force:true !mut_rate in*)
  (* 	  let chrome2 = gp_mutate chrome_orig ~force:true !mut_rate in *)

  let chrome1 = chrome_orig in 
  let chrome2 = chrome_orig in 

  printf "p1: " ; print_chrome chrome1;
  printf "p2: " ; print_chrome chrome2;
  
  write_file "orig.cc" chrome_orig.cfile;	  
  write_file "chrome1.cc" chrome1.cfile;
  write_file "chrome2.cc" chrome2.cfile;
  
  (*printHT1 chrome1.ht;*)
  
  printf "\ncrossover\n\n";
  
  let kid1, kid2 = gp_xover_vn chrome1 chrome2 2 in
  
  printf "k1:\n" ; print_chrome kid1;
  printf "k2:\n" ; print_chrome kid2;

  write_file "kid1.cc" kid1.cfile;
  write_file "kid2.cc" kid2.cfile;

  failwith ("xover debug")
)


(* segp --seed 841 zunebug.c  --max 25 --dbl 3    453 Best Solution*)
