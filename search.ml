(* 
 * Program Repair Prototype (v2) 
 *
 * Search Strategies include: 
 *  -> Brute Force (e.g., all distance-one edits)
 *  -> Genetic Programming (e.g., ICSE'09)
 *     => delete, append and swap based on fault localization
 *     => crossover: none, one point, two point, uniform, ...
 *)
open Printf
open Global
open Fitness
open Rep

let weight_compare (stmt,prob) (stmt',prob') =
    if prob = prob' then compare stmt stmt' 
    else compare prob' prob 

(*************************************************************************
 *************************************************************************
                     Brute Force: Try All Single Edits
 *************************************************************************
 *************************************************************************)

let brute_force_1 (original : 'a Rep.representation) incoming_pop = 
  debug "search: brute_force_1 begins\n" ; 
  if incoming_pop <> [] then begin
    debug "search: incoming population IGNORED\n" ; 
  end ; 
  let fault_localization = original#get_fault_localization () in 
  let fault_localization = List.sort weight_compare fault_localization in 
  let fix_localization = original#get_fix_localization () in 
  let fix_localization = List.sort weight_compare fix_localization in 

  let worklist = ref [] in 

  (* first, try all single deletions *) 
  List.iter (fun (atom,weight) ->
    (* As an optimization, rather than explicitly generating the
     * entire variant in advance, we generate a "thunk" (or "future",
     * or "promise") to create it later. This is handy because there
     * might be over 100,000 possible variants, and we want to sort
     * them by weight before we actually instantiate them. *) 
    let thunk () = 
      let rep = original#copy () in 
      rep#delete atom; 
      rep
    in 
    worklist := (thunk,weight) :: !worklist ; 
  ) fault_localization ; 
  debug "search: brute: %d deletes\n" 
    (List.length fault_localization) ; 

  (* second, try all single appends *) 
  let append_counter = ref 0 in 
  List.iter (fun (dest,w1) ->
    let allowed = original#append_sources dest in 

    List.iter (fun (src,w2) -> 
      if IntSet.mem src allowed then begin 
        let thunk () = 
          let rep = original#copy () in 
          rep#append dest src; 
          rep 
        in 
        incr append_counter ; 
        worklist := (thunk, w1 *. w2 *. 0.9) :: !worklist ; 
      end 
    ) fix_localization 
  ) fault_localization ;  
  debug "search: brute: %d appends (out of %d)\n" 
    !append_counter
    ((List.length fault_localization) * (List.length fix_localization)) ; 

  (* third, try all single swaps *) 
  let swap_counter = ref 0 in 
  List.iter (fun (dest,w1) ->
    let allowed = original#swap_sources dest in 
    List.iter (fun (src,w2) -> 
      if IntSet.mem src allowed && dest <> src then begin (* swap X with X = no-op *) 
        let thunk () = 
          let rep = original#copy () in 
          rep#swap dest src;
          rep
        in 
        incr swap_counter ; 
        worklist := (thunk, w1 *. w2 *. 0.8) :: !worklist ; 
      end 
    ) fault_localization 
  ) fault_localization ;  
  debug "search: brute: %d swaps (out of %d)\n" 
    !swap_counter
    ((List.length fault_localization) * (List.length fault_localization)) ; 

  (* fourth, try subatom mutations *) 
  let sub_counter = ref 0 in 
  if original#subatoms && !use_subatoms then begin
    List.iter (fun (dest,w1) ->
      let subs = original#get_subatoms dest in 
      for sub_idx = 0 to pred (List.length subs) do
        let thunk () = 
          let rep = original#copy () in 
          rep#replace_subatom_with_constant dest sub_idx ;
          rep
        in 
        incr sub_counter ; 
        worklist := (thunk, w1 *. 0.9) :: !worklist ; 
      done 
    ) fault_localization ; 
  end ; 
  debug "search: brute: %d subatoms\n" 
    !sub_counter;

  (* fifth, try subatom swaps *) 
  let sub_counter = ref 0 in 
  if original#subatoms && !use_subatoms then begin
    List.iter (fun (dest,w1) ->
      let dests = original#get_subatoms dest in 
      let num_dest_subatoms = List.length dests in 
      List.iter (fun (src,w2) -> 
        let subs = original#get_subatoms src in 
        List.iter (fun subatom ->
          for sub_idx = 0 to pred num_dest_subatoms do 
            let thunk () = 
              let rep = original#copy () in 
              rep#replace_subatom dest sub_idx subatom ;
              rep
            in 
            incr sub_counter ; 
            worklist := (thunk, w1 *. 0.9) :: !worklist ; 
          done 
        ) subs 
      ) fix_localization ; 
    ) fault_localization ; 
  end ; 
  debug "search: brute: %d subatom swaps\n" 
    !sub_counter;

  if !worklist = [] then begin
    debug "WARNING: no variants to consider (no fault localization?)" ; 
  end ; 

  let worklist = List.sort 
    (fun (m,w) (m',w') -> compare w' w) !worklist in 
  let howmany = List.length worklist in 
  let sofar = ref 1 in 
  List.iter (fun (thunk,w) ->
    debug "\tvariant %d/%d (weight %g)\n" !sofar howmany w ;
    let rep = thunk () in 
    incr sofar ;
    test_to_first_failure rep 
  ) worklist ; 

  debug "search: brute_force_1 ends\n" ; 
  [] 

(*************************************************************************
 *************************************************************************
                          Basic Genetic Algorithm
 *************************************************************************
 *************************************************************************)

let generations = ref 10
let popsize = ref 40 
let mutp = ref 0.05
let subatom_mutp = ref 0.5
let subatom_constp = ref 0.5
let crossp = ref 0.5
let promut = ref 0 
let unit_test = ref false
let pred_ff = ref false
let rb_levels = ref 0
let last_mut = ref "none" 
let incoming_pop = ref "" 
 
let _ = 
  options := !options @ [
  "--generations", Arg.Set_int generations, "X use X genetic algorithm generations";
  "--popsize", Arg.Set_int popsize, "X variant population size";
  "--mutp", Arg.Set_float mutp, "X use X as mutation rate";	
  "--promut", Arg.Set_int promut, "X make X mutations per 'mutate' call";	
  "--subatom-mutp", Arg.Set_float subatom_mutp, "X use X as subatom mutation rate";	
  "--subatom-constp", Arg.Set_float subatom_constp, "X use X as subatom constant rate";	
  "--crossp", Arg.Set_float crossp, "X use X as crossover rate";
  "--unit_test", Arg.Set unit_test, "X Do a test?";
	"--pred-fitness", Arg.Set pred_ff, "X Use preds as fitness function";
	"--robustness", Arg.Set_int rb_levels, "X Number of robustness trials";
] 

(* Just get fault localization ids *)
let just_id inp = 
  List.map (fun (sid, prob) -> sid) (inp#get_fault_localization ())
let rec choose_from_weighted_list chosen_index lst = match lst with
  | [] -> failwith "localization error"  
  | (sid,prob) :: tl -> if chosen_index <= prob then sid else choose_from_weighted_list (chosen_index -. prob) tl

(* tell whether we should mutate an individual *)
let maybe_mutate prob =
  if (Random.float 1.0) <= (!mutp *. prob) then true else false 

let choose_one_weighted lst = 
  assert(lst <> []); 
  let total_weight = List.fold_left (fun acc (sid,prob) ->
    acc +. prob) 0.0 lst in
  assert(total_weight > 0.0) ; 
  let wanted = Random.float total_weight in
  let rec walk lst sofar = 
    match lst with
    | [] -> failwith "choose_one_weighted" 
    | (sid,prob) :: rest -> 
      let here = sofar +. prob in 
      if here >= wanted then (sid,prob)
      else walk rest here 
  in
  walk lst 0.0 

(***********************************************************************
 * Weighted Micro-Mutation
 *
 * Here we pick delete, append or swap, and apply that atomic operator
 * with some probability to each element of the fault localization path.
 ***********************************************************************)
let mutate ?(test = false) (variant : 'a Rep.representation) random = 
  let subatoms = variant#subatoms in 
  let result = variant#copy () in  
  let mut_ids = variant#get_fault_localization () in 
  let mut_ids = 
    if !promut <= 0 then mut_ids
    else uniq mut_ids
  in 
  let promut_list = 
    if !promut <= 0 then 
      []
    else begin
      let res = ref [] in
      for i = 1 to !promut do
        let sid, prob = choose_one_weighted mut_ids in 
        res := (sid) :: !res
      done ;
      !res
    end 
  in 
  List.iter (fun (x,prob) ->
    if (test || maybe_mutate prob || (List.mem x promut_list )) then 
      let atom_mutate () = (* stmt-level mutation *) 
        match Random.int 3 with 
        | 0 -> result#delete x
        | 1 -> 
          let allowed = variant#append_sources x in 
          result#append x (random allowed)
        | _ -> 
          let allowed = variant#swap_sources x in 
          result#swap x (random allowed)
      in 
      if subatoms && (Random.float 1.0 < !subatom_mutp) then begin
        (* sub-atom mutation *) 
        let x_subs = variant#get_subatoms x in 
        if x_subs = [] then atom_mutate ()
        else if ((Random.float 1.0) < !subatom_constp) then begin 
          let x_sub_idx = Random.int (List.length x_subs) in 
          result#replace_subatom_with_constant x x_sub_idx 
        end else begin 
          let allowed = variant#append_sources x in 
          let allowed = IntSet.elements allowed in 
          let allowed = random_order allowed in 
          let rec walk lst = match lst with
          | [] -> atom_mutate () 
          | src :: tl -> 
            let src_subs = variant#get_subatoms src in 
            if src_subs = [] then
              walk tl
            else begin
              let x_sub_idx = Random.int (List.length x_subs) in 
              let src_subs = random_order src_subs in 
              let src_sub = List.hd src_subs in 
              result#replace_subatom x x_sub_idx src_sub 
            end 
          in 
          walk allowed
        end 
      end else atom_mutate () 
  ) mut_ids ;
result

let force_mutate_full (variant : 'a Rep.representation) =
	let result = variant#copy() in
	let to_mutate = Random.int (variant#max_atom ()) + 1 in
	let alt = Random.int (variant#max_atom ()) + 1 in
	(match Random.int 3 with
		| 0 -> result#delete to_mutate ; last_mut := "delete"
		| 1 -> result#append to_mutate alt ; last_mut := "append"
		| 2 -> result#swap to_mutate alt ; last_mut := "swap" ); 
	result

let force_mutate (variant : 'a Rep.representation) random 
	: ('a representation) =
	let result = variant#copy () in
	let mut_ids = just_id result in
	let choice = List.nth mut_ids (Random.int (List.length mut_ids)) in
	List.iter (fun x -> 
							if choice = x then
								(match Random.int 3 with
									| 0 -> result#delete x ; last_mut := "delete"
									| 1 -> result#append x (random ()) ; last_mut := "append" 
									| 2 -> result#swap x (random ()) ; last_mut := "swap" 
							)) mut_ids ;
  result 
		
  (*(match Random.int 3 with
  | 0 -> result#delete (fault_location ())  
  | 1 -> result#append (fault_location ()) (fix_location ()) 
  | _ -> result#swap (fault_location ()) (fix_location ()) 
  ) ;*)

(* Helper function for generating ranges *)
let (--) i j = 
    let rec aux n acc =
      if n < i then acc else aux (n-1) (n :: acc)
    in aux j []


(* One point crossover *)
let do_cross ?(test = 0) 
        (variant1 : 'a Rep.representation) 
        (variant2 : 'a Rep.representation)
	: ('a representation) list =
	let c_one = variant1#copy () in
	let c_two = variant2#copy () in
	let mat_1 = just_id variant1 in
	let mat_2 = just_id variant2 in
	let _ = debug "Len: %d - %d\n", (List.length mat_1), (List.length mat_2) in
	let point = if test=0 then Random.int (List.length mat_1) else test in
	List.iter (fun p -> begin
				c_one#put (List.nth mat_1 p) (variant2#get (List.nth mat_2 p));
				c_two#put (List.nth mat_2 p) (variant1#get (List.nth mat_1 p));
				end ) 
			  (0--point) ;
    c_one#add_name_note (sprintf "x(:%d)" point) ;
    c_two#add_name_note (sprintf "x(%d:)" point) ;
	[c_one;c_two]

let proc (v1 : 'a Rep.representation) (v2 : 'a Rep.representation) : 'a Rep.representation =
	let newc = v1#copy () in
	let newm = just_id newc in
	let otherm = just_id v2 in
	List.iter (fun p -> if (p mod 2) = 0 then newc#put (List.nth newm p) (v2#get (List.nth otherm p))) (0--((List.length newm)-1));
	newc

let fit_scape = Hashtbl.create 500
let mut_scape = Hashtbl.create 500

let fit_stats (newf : float)  (oldf : float) (name : string) =
	let fdiff = abs_float (newf -. oldf) in
	Hashtbl.add fit_scape fdiff name ;
	if fdiff = 0.0 then Hashtbl.add mut_scape !last_mut 1 ;
	fdiff 

let pfd d = 
	let num = (List.length (Hashtbl.find_all fit_scape (float_of_int d))) in
	debug "distance of %d %d\n" d num ;
	num

let pfd_print () = 
	let how_many = Hashtbl.fold (fun k v a -> a + 1) fit_scape 0 in
	let neutral = pfd 0 in
	debug "how many : %d\n" how_many ;
	let percent_neutral = (float_of_int neutral) /. (float_of_int how_many) in
	debug "percent neutral : %g\n" percent_neutral ;
	let appends = List.length (Hashtbl.find_all mut_scape "append") in
	let deletes = List.length (Hashtbl.find_all mut_scape "delete") in
	let swaps = List.length (Hashtbl.find_all mut_scape "swap") in
	let percent_append = (float_of_int appends) /. (float_of_int neutral) in
	let percent_delete = (float_of_int deletes) /. (float_of_int neutral) in
	let percent_swap = (float_of_int swaps) /. (float_of_int neutral) in
	debug "percent deletes : %g\n" percent_delete ;
	debug "percent appends : %g\n" percent_append ;
	debug "percent swaps : %g\n" percent_swap 

let swap_chains ((v1 : 'a Rep.representation),(v2 : 'a Rep.representation)) =
	let newc = v1#copy () in
	let new2  = v2#copy () in
	let newm = just_id newc in
	let otherm = just_id v2 in
	let percent = (float_of_int (List.length newm)) *. 0.1 in
	let times = Random.int (int_of_float percent) in
	for i =  0 to 0 do
		let point = Random.int (List.length newm) in
		let first = newc#get (List.nth newm point) in
		newc#put (List.nth newm point) (v2#get (List.nth otherm point)) ;
		new2#put (List.nth otherm point) first ;
	done ;	
	(newc,new2)
	

let do_weird_cross (v1 : 'a Rep.representation * 'a Rep.representation) (v2 : 'a Rep.representation * 'a Rep.representation)
	: ('a Rep.representation * 'a Rep.representation) list =
	let (a1,_),(b1,_) = v1,v2 in
	let (_,a2),(_,b2) = v1,v2 in
	let [r11;r12] = do_cross a1 b1 in
	let [r21;r22] = do_cross a2 b2 in
	match Random.int 2 with
	| 0 -> [(r11,r21);(r12,r22)]
	| 1 -> [(r11,r22);(r12,r21)] 
	
  
(***********************************************************************
 * Tournament Selection
 ***********************************************************************)
let tournament_k = ref 2 
let tournament_p = ref 1.00 

let tournament_selection (population : ('a representation * float) list) 
           (desired : int) 
           (* returns *) : 'a representation list = 
  let p = !tournament_p in 
  assert ( desired >= 0 ) ; 
  assert ( !tournament_k >= 1 ) ; 
  assert ( p >= 0.0 ) ; 
  assert ( p <= 1.0 ) ; 
  assert ( List.length population > 0 ) ; 
  let rec select_one () = 
    (* choose k individuals at random *) 
    let lst = random_order population in 
    (* sort them *) 
    let pool = first_nth lst !tournament_k in 
    let sorted = List.sort (fun (_,f) (_,f') -> compare f' f) pool in 
    let rec walk lst step = match lst with
    | [] -> select_one () 
    | (indiv,fit) :: rest -> 
        let taken = 
          if p >= 1.0 then true
          else begin 
            let required_prob = p *. ((1.0 -. p)**(step)) in 
            Random.float 1.0 <= required_prob 
          end 
        in
        if taken then (indiv) else walk rest (step +. 1.0)
    in
    walk sorted 0.0
  in 
  let answer = ref [] in 
  for i = 1 to desired do
    answer := (select_one ()) :: !answer
  done ;
  !answer

(* Selection -- currently we have only tournament selection implemented,
 * but if/when we add others, we choose between them here. *)  
let selection (population : ('a representation * float) list) 
           (desired : int) 
           (* returns *) : 'a representation list = 
  tournament_selection population desired 

let weird_selection (population : ('a representation * float * 'a representation) list) 
			(desired : int) : ('a representation * 'a representation) list =
  let p = !tournament_p in 
  assert ( desired >= 0 ) ; 
  assert ( !tournament_k >= 1 ) ; 
  assert ( p >= 0.0 ) ; 
  assert ( p <= 1.0 ) ; 
  assert ( List.length population > 0 ) ; 
  let rec select_one () = 
    (* choose k individuals at random *) 
    let lst = random_order population in 
    (* sort them *) 
    let pool = first_nth lst !tournament_k in 
    let sorted = List.sort (fun (_,f,_) (_,f',_) -> compare f' f) pool in 
    let rec walk lst step = match lst with
    | [] -> select_one () 
    | (indiv,fit,alt) :: rest -> 
        let taken = 
          if p >= 1.0 then true
          else begin 
            let required_prob = p *. ((1.0 -. p)**(step)) in 
            Random.float 1.0 <= required_prob 
          end 
        in
        if taken then (indiv,alt) else walk rest (step +. 1.0)
    in
    walk sorted 0.0
  in 
  let answer = ref [] in 
  for i = 1 to desired do
    answer := (select_one ()) :: !answer
  done ;
  !answer
	

(***********************************************************************
 * Basic Genetic Algorithm Search Strategy
 *
 * This is parametric with respect to a number of choices (e.g.,
 * population size, selection method, fitness function, fault
 * localization, ...). 
 ***********************************************************************)
let genetic_algorithm (original : 'a Rep.representation) incoming_pop = 
  debug "search: genetic algorithm begins\n" ;

  (* choose a stmt uniformly at random *) 
  let random atom_set = 
    let elts = IntSet.elements atom_set in 
    let size = List.length elts in 
    List.nth elts (Random.int size) 
  in 
    
  
  (* transform a list of variants into a listed of fitness-evaluated
   * variants *) 
  let calculate_fitness pop =  
    List.map (fun variant -> (variant, test_all_fitness variant)) pop
  in 

  let pop = ref [] in (* our GP population *) 
  for i = 1 to pred !popsize do
    (* initialize the population to a bunch of random mutants *) 
    pop := (mutate original random) :: !pop 
  done ;

  if !unit_test then begin
	debug "printing out original\n";
	original#output_source "original.c" ;
	let mone = List.nth !pop 1 in
	let mtwo = List.nth !pop 2 in
	debug "outputing original mutants mut_one and mut_two\n" ;
	mone#output_source "mut_one.c" ;
	mtwo#output_source "mut_two.c" ;
	debug "crossing them over\n" ;
	let mylist = do_cross mone mtwo ~test:5 in
	let cone = List.hd mylist in 
	let ctwo = List.hd (List.tl mylist) in
	debug "printing out children c_one c_two with crosspoint 5\n" ;
	cone#output_source "c_one.c" ;
	ctwo#output_source "c_two.c" ;
	debug "exiting...\n" ;
	assert(false) ;
  end ;

  (* include the original in the starting population *)
  pop := (original#copy ()) :: !pop ;

	let crossover (population : 'a Rep.representation list) = 
  	let mating_list = random_order population in
  	(* should we cross an indi(Printf.sprintf (v#id ())dual? *)
  	let maybe_cross () = if (Random.float 1.0) <= !crossp then true else false in
  	let output = ref [] in
  	let half = (List.length mating_list) / 2 in
  	for it = 0 to (half - 1) do
			if maybe_cross () then
				output := (do_cross (List.nth mating_list it) (List.nth mating_list (half + it))) @ !output
			else
				output := (mutate original random) :: (mutate original random) :: !output
		done ;
		!output
	in

  (* Main GP Loop: *) 
  for gen = 1 to !generations do
    debug "search: generation %d\n" gen ; 
    (* Step 1. Calculate fitness. *) 
    let incoming_population = calculate_fitness !pop in 
    (* Step 2: selection *) 
	let selected = selection incoming_population !popsize in
	(* Step 3: crossover *)
	let crossed = crossover selected in
    (* Step 4: mutation *)
    let mutated = List.map (fun one -> (mutate one random)) crossed in
    pop := mutated ;
  done ;
  debug "search: genetic algorithm ends\n" ;
  !pop 


let robust (original : 'a Rep.representation) incoming =
	debug "on mutation path %d" (List.length (just_id original)) ;
	debug "on path total %d" (original#max_atom ()) ;
  let random () = 
    1 + (Random.int (original#max_atom ()) ) in
  
	let calculate_fitness pop = 
    List.map (fun variant -> (variant, test_all_fitness_nonstop variant)) pop
  in 
	let robust_fitness pop = 
    List.map (fun variant -> (variant, test_all_fitness_nonstop variant)) pop
  in 
  let orig_fit = test_all_fitness_nonstop original in
	let pop = ref [] in (* make a bunch of mutations on the input program *) 
  let rob_diff = ref 0.0 in
	let next = ref [] in
	let count_r = ref 0 in
	for i = 1 to pred !popsize do
    let v = force_mutate original random in (*switch here for full vs. paht *)
		let f = test_all_fitness_nonstop v in
		debug "looking %g %g \n" f orig_fit; 
		fit_stats f orig_fit "placeholdr";
		if f = orig_fit then begin count_r := !count_r + 1 ; v#output_source (Printf.sprintf "robust%d.c" !count_r) end 
		else ();
		pop := v :: !pop 
  done ;
	let incoming = calculate_fitness !pop in
	(*
	for i = 1 to !rb_levels do
		for i = 1 to pred !popsize do
			next := (force_mutate (List.nth !pop (i-1)) random) :: !next
		done ;
		let tmp_in = robust_fitness !next in
		let rec walk lst1 lst2 step =
			match lst1,lst2 with
			| (i1,f1) :: r1, (i2,f2) :: r2 -> walk r1 r2 (step +. (fit_stats f1 f2 "placehldt"))
			| _ -> step in
		rob_diff := !rob_diff +. (walk incoming tmp_in 0.0) ;
	done ;
	rob_diff := !rob_diff /. (float_of_int (!rb_levels * !popsize)) ;

	debug "robustness average is %g\n" !rob_diff ;
	*)

	pfd_print ();
	(*
	let crossover (population : Rep.representation list) = 
  	let mating_list = random_order population in
  	(* should we cross an individual? *)
  	let maybe_cross () = if (Random.float 1.0) <= !crossp then true else false in
  	let output = ref [] in
  	let half = (List.length mating_list) / 2 in
  	for it = 0 to (half - 1) do
			if maybe_cross () then
				output := (do_cross (List.nth mating_list it) (List.nth mating_list (half + it))) @ !output
			else
				output := (mutate original random) :: (mutate original random) :: !output
		done ;
		!output
	in
	append_file "robustness_track.csv" (Printf.sprintf "%d,%d,%g" !random_seed (Rep.num_test_evals_ignore_cache ()) !rob_diff)  ;
	(*for gen = 1 to !generations do
    debug "search: generation %d\n" gen ; 
    (* Step 1. Calculate fitness. *) 
    let incoming_population = calculate_fitness !pop in 
    (* Step 2: selection *) 
	let selected = selection incoming_population !popsize in
	(* Step 3: crossover *)
	let crossed = crossover selected in
    (* Step 4: mutation *)
    let mutated = List.map (fun one -> (mutate one random)) crossed in
    pop := mutated ;
  done ;*) *)
  debug "search: genetic algorithm ends\n" ;
	!pop
(*
let collect_predicates (original : Rep.representation) incoming = 
  let cmd = "rm -rf predicates & rm *.cache 00*" in
	(match Unix.system cmd with
	| Unix.WEXITED(0) -> ()
	| _ -> ()) ;
	let random () = 
    1 + (Random.int (original#max_atom ()) ) in
	let originalb = collect_pred_fitness original in
	let calculate_fitness pop = 
    List.map (fun variant -> (variant, collect_pred_fitness variant)) pop
  in 
	let pop = ref [] in (* make a bunch of mutations on the input program *) 
  for i = 1 to pred !popsize do
    pop := (mutate original random) :: !pop 
  done ;
	
	let crossover (population : Rep.representation list) = 
  	let mating_list = random_order population in
  	(* should we cross an individual? *)
  	let maybe_cross () = if (Random.float 1.0) <= !crossp then true else false in
  	let output = ref [] in
  	let half = (List.length mating_list) / 2 in
  	for it = 0 to (half - 1) do
			if maybe_cross () then
				output := (do_cross (List.nth mating_list it) (List.nth mating_list (half + it))) @ !output
			else
				output := (mutate original random) :: (mutate original random) :: !output
		done ;
		!output
	in
	
  (* Main GP Loop: *) 
  for gen = 1 to !generations do
    debug "search (pred collect): generation %d\n" gen ; 
    let incoming_population = calculate_fitness !pop in 
		let selected = selection incoming_population !popsize in
		let crossed = crossover selected in
  	let mutated = List.map (fun one -> (mutate one random)) crossed in
  	pop := mutated ;
  done ;
  debug "search: predicate collection ends\n" ;
	!pop

let more_neutral (original : Rep.representation) incoming =

  let random () = 
    1 + (Random.int (original#max_atom ()) ) in
  
	let calculate_fitness pop = 
    List.map (fun (v1,v2) -> (v1, (test_all_fitness (proc v1 v2)),v2)) pop in 
	
	let new_mutate (v1 : Rep.representation * Rep.representation) func = 
		let (a,b) = v1 in
		let m = func a random in
		let z = func b random in
 		(m,z) in
 
	let pop = ref [] in (* make a bunch of mutations on the input program *) 
  let rob_diff = ref 0.0 in
	let next = ref [] in
	for i = 1 to pred !popsize do
    pop := ((mutate original random),(mutate original random)) :: !pop 
  done ;
	
	let crossover (population : (representation * representation) list) = 
  	let mating_list = random_order population in
  	(* should we cross an individual? *)
  	let maybe_cross () = if (Random.float 1.0) <= !crossp then true else false in
  	let output = ref [] in
  	let half = (List.length mating_list) / 2 in
  	for it = 0 to (half - 1) do
			if maybe_cross () then
				output := (do_weird_cross (List.nth mating_list it) (List.nth mating_list (half + it))) @ !output
			else
				output := ((mutate original random),(mutate original random)) :: ((mutate original random),(mutate original random)) :: !output
		done ;
		!output
	in

	for gen = 1 to !generations do
    debug "search: generation %d\n" gen ; 
    (* Step 1. Calculate fitness. *) 
    let incoming_population = calculate_fitness !pop in 
    (* Step 2: selection *) 
		let selected = weird_selection incoming_population !popsize in
		(* Step 3: crossover *)
		let crossed = crossover selected in
		let extra_m = List.map (fun v -> if (Random.int 1) = 0 then swap_chains v else v) crossed in
    (* Step 4: mutation *)
    let mutated = List.map (fun v -> new_mutate v mutate) extra_m in
    pop := mutated ;
  done ;
  debug "search: genetic algorithm ends\n" ;
	List.map (fun (x,y) -> x) !pop
	


let robust_neutral (original : Rep.representation) incoming =

  let random () = 
    1 + (Random.int (original#max_atom ()) ) in
  
	let robust_fitness ( pop : (Rep.representation * Rep.representation) list ) = 
    List.map (fun (v1,v2) -> (v1, (test_all_fitness_nonstop (proc v1 v2)),v2)) pop in 
	let calculate_fitness pop = 
    List.map (fun (v1,v2) -> (v1, (test_all_fitness_nonstop (proc v1 v2)),v2)) pop in 

  let orig_fit = test_all_fitness_nonstop original in

	let pop = ref [] in (* make a bunch of mutations on the input program *) 
  let rob_diff = ref 0.0 in
	let next = ref [] in
	for i = 1 to pred !popsize do
    let v1 = force_mutate original random in
    let (va,vb) = swap_chains (v1,original) in
		let f = test_all_fitness_nonstop (proc va vb) in
		fit_stats f orig_fit "placeholdr";
    pop := (va,vb) :: !pop 
  done ;
	
	pfd_print () ;
	(*
	let incoming = calculate_fitness !pop in
			
	let new_mutate (v1 : Rep.representation * Rep.representation) func = 
		let (a,b) = v1 in
		let m = func a random in
		let z = func b random in
 		(m,z) in
	
	let robust_mutate (v1 : Rep.representation * Rep.representation) func = 
		let (a,b) = v1 in
		let m = func a random in
		let z = func b random in
 		if (Random.int 2) = 0 then (a,z) else (m,b) in
	
	for i = 1 to !rb_levels do
		for i = 1 to pred !popsize do
			let curr = List.nth !pop (i-1) in
			next := robust_mutate curr force_mutate :: !next
		done ;
		let extra_m = List.map (fun v -> if (Random.int 2) = 0 then swap_chains v else v) !next in
		let tmp_in = robust_fitness extra_m in
		let rec walk lst1 lst2 step =
			match lst1,lst2 with
			| (i1,f1,i11) :: r1, (i2,f2,i22) :: r2 -> walk r1 r2 (step +. (fit_stats f1 f2 "placehldr"))
			| _ -> step in
		rob_diff := !rob_diff +. (walk incoming tmp_in 0.0) ;
	done ;
	rob_diff := !rob_diff /. (float_of_int (!rb_levels * !popsize)) ;
	
	debug "robustness average is %g\n" !rob_diff ;
	pfd_print () ;

	append_file "robustness_track.csv" (Printf.sprintf "%d,%d,%g" !random_seed (Rep.num_test_evals_ignore_cache ()) !rob_diff)  ;
	
	let crossover (population : (representation * representation) list) = 
  	let mating_list = random_order population in
  	(* should we cross an individual? *)
  	let maybe_cross () = if (Random.float 1.0) <= !crossp then true else false in
  	let output = ref [] in
  	let half = (List.length mating_list) / 2 in
  	for it = 0 to (half - 1) do
			if maybe_cross () then
				output := (do_weird_cross (List.nth mating_list it) (List.nth mating_list (half + it))) @ !output
			else
				output := ((mutate original random),(mutate original random)) :: ((mutate original random),(mutate original random)) :: !output
		done ;
		!output
	in
(*
	for gen = 1 to !generations do
    debug "search: generation %d\n" gen ; 
    (* Step 1. Calculate fitness. *) 
    let incoming_population = calculate_fitness !pop in 
    (* Step 2: selection *) 
		let selected = weird_selection incoming_population !popsize in
		(* Step 3: crossover *)
		let crossed = crossover selected in
		let extra_m = List.map (fun v -> if (Random.int 2) = 0 then swap_chains v else v) crossed in
    (* Step 4: mutation *)
    let mutated = List.map (fun v -> new_mutate v mutate) extra_m in
    pop := mutated ;
  done ;*)*)
  debug "search: genetic algorithm ends\n" ;
	let re = List.map (fun (x,y) -> x) !pop in
	re


let neutral2 (original : Rep.representation) incoming =

  let random () = 
    1 + (Random.int (original#max_atom ()) ) in
  
	let calculate_fitness pop = 
    List.map (fun (v1,v2) -> 
			let v1f = test_all_fitness v1 in
			let v2f = test_all_fitness v2 in
			if v1f > v2f then (v1,v1f,v2) else (v1,v2f,v2)) pop in 
	
	let new_mutate (v1 : Rep.representation * Rep.representation) func = 
		let (a,b) = v1 in
		let m = func a random in
		let z = func b random in
 		(m,z) in
 
	let pop = ref [] in (* make a bunch of mutations on the input program *) 
  let rob_diff = ref 0.0 in
	let next = ref [] in
	for i = 1 to pred !popsize do
    pop := ((mutate original random),(mutate original random)) :: !pop 
  done ;
	
	let crossover (population : (representation * representation) list) = 
  	let mating_list = random_order population in
  	(* should we cross an individual? *)
  	let maybe_cross () = if (Random.float 1.0) <= !crossp then true else false in
  	let output = ref [] in
  	let half = (List.length mating_list) / 2 in
  	for it = 0 to (half - 1) do
			if maybe_cross () then
				output := (do_weird_cross (List.nth mating_list it) (List.nth mating_list (half + it))) @ !output
			else
				output := ((mutate original random),(mutate original random)) :: ((mutate original random),(mutate original random)) :: !output
		done ;
		!output
	in

	for gen = 1 to !generations do
    debug "search: generation %d\n" gen ; 
    (* Step 1. Calculate fitness. *) 
    let incoming_population = calculate_fitness !pop in 
    (* Step 2: selection *) 
		let selected = weird_selection incoming_population !popsize in
		(* Step 3: crossover *)
		let crossed = crossover selected in
    (* Step 4: mutation *)
    let mutated = List.map (fun v -> new_mutate v mutate) crossed in
    pop := mutated ;
  done ;
  debug "search: genetic algorithm ends\n" ;
	List.map (fun (x,y) -> x) !pop
	
*)
