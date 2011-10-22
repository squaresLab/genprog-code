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


let generations = ref 10
let popsize = ref 40
let mutp = ref 0.05
let subatom_mutp = ref 0.5
let subatom_constp = ref 0.5
let crossp = ref 0.5
let promut = ref 0
let incoming_pop = ref ""
let tournament_k = ref 2
let crossover = ref "one"
let continue = ref false
let gens_run = ref 0
let oracle_edit_history = ref ""
 (* split_search and num_comps are for distributed; sad to have it here, but so it goes *)
let split_search = ref false
let num_comps = ref 2
let mutrb_runs = ref 1000
let neutral_fitness = ref 5.0
let robustness_ops = ref "ads"
let neutral_walk_pop_size = ref 100
let neutral_walk_steps = ref 100
let neutral_walk_max_size = ref 0

let _ =
  options := !options @ [
  "--generations", Arg.Set_int generations, "X use X genetic algorithm generations";
  "--popsize", Arg.Set_int popsize, "X variant population size";
  "--mutp", Arg.Set_float mutp, "X use X as mutation rate";
  "--promut", Arg.Set_int promut, "X make X mutations per 'mutate' call";
  "--subatom-mutp", Arg.Set_float subatom_mutp, "X use X as subatom mutation rate";
  "--subatom-constp", Arg.Set_float subatom_constp, "X use X as subatom constant rate";
  "--tournament-size", Arg.Set_int tournament_k, "X use x as tournament size";
  "--crossover", Arg.Set_string crossover, "X use X as crossover [one,back,subset,flat]";
  "--crossp", Arg.Set_float crossp, "X use X as crossover rate";
  "--continue", Arg.Set continue, " Continue search after repair has been found.  Default: false";
  "--split-search", Arg.Set split_search, " Distributed: Split up the search space" ;
  "--oracle-edit-history", Arg.Set_string oracle_edit_history, "X use X as edit history for oracle search" ;
  "--robustness-ops", Arg.Set_string robustness_ops, "X only test robustness of operations in X, e.g., 'ad' for 'append' and 'delete'" ;
]


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
  let fault_localization =
	lsort weight_compare (original#get_fault_localization ())
  in
  let fix_localization =
	lsort weight_compare (original#get_fix_localization ())
  in

  (* first, try all single deletions *)
  let deletes =
	lmap (fun (atom,weight) ->
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
    thunk,weight
  ) fault_localization
  in
  debug "search: brute: %d deletes\n"
    (List.length fault_localization) ;

  (* second, try all single appends *)
	let appends =
  lflatmap (fun (dest,w1) ->
    let allowed = WeightSet.elements (original#append_sources dest) in
    lmap (fun (src,w2) ->
        let thunk () =
          let rep = original#copy () in
          rep#append dest src;
          rep
        in
        thunk, w1 *. w2 *. 0.9
    ) allowed
  ) fault_localization in
  debug "search: brute: %d appends (out of %d)\n"
    (llen appends)
    ((List.length fault_localization) * (List.length fix_localization)) ;

  (* third, try all single swaps *)
  let swaps =
  lflatmap (fun (dest,w1) ->
    let allowed = WeightSet.elements (original#swap_sources dest) in
    lmap (fun (src,w2) ->
        let thunk () =
          let rep = original#copy () in
          rep#swap dest src;
          rep
        in
        thunk, w1 *. w2 *. 0.8
    ) allowed
  ) fault_localization in
  debug "search: brute: %d swaps (out of %d)\n"
    (llen swaps)
    ((List.length fault_localization) * (List.length fix_localization)) ;

	let subatoms =
  if original#subatoms && !use_subatoms then begin
	let sub_dests =
	  lmap (fun (dest,w1) ->
		dest, llen (original#get_subatoms dest), w1)
		fault_localization
	in
  (* fourth, try subatom mutations *)
	let sub_muts =
    lflatmap (fun (dest,subs,w1) ->
	  lmap (fun sub_idx ->
        let thunk () =
          let rep = original#copy () in
          rep#replace_subatom_with_constant dest sub_idx ;
          rep
        in
          thunk, w1 *. 0.9) (0 -- subs)
    ) sub_dests in
  debug "search: brute: %d subatoms\n" (llen sub_muts);

  (* fifth, try subatom swaps *)
	  let sub_swaps =
		let fix_srcs =
		  lmap (fun (src,w1) -> original#get_subatoms src, w1)
			fix_localization
		in
		  lflatmap (fun (dest,dests, w1) ->
			lflatmap (fun (subs,w2) ->
			  lflatmap (fun subatom ->
				lmap (fun sub_idx ->
				  let thunk () =
					let rep = original#copy () in
					  rep#replace_subatom dest sub_idx subatom ;
					  rep
				  in
					thunk, w1 *. w2 *. 0.8
				) (0 -- dests)
			  ) subs
			) fix_srcs
		  ) sub_dests
	  in
		debug "search: brute: %d subatom swaps\n" (llen sub_swaps);
		sub_muts @ sub_swaps
	  end else []
	in
	let worklist = deletes @ appends @ swaps @ subatoms in
  if worklist = [] then begin
    debug "WARNING: no variants to consider (no fault localization?)\n" ;
  end ;

  let worklist = List.sort
    (fun (m,w) (m',w') -> compare w' w) worklist in
  let howmany = List.length worklist in
  let sofar = ref 1 in
  List.iter (fun (thunk,w) ->
    debug "\tvariant %d/%d (weight %g)\n" !sofar howmany w ;
    let rep = thunk () in
    incr sofar ;
	  try
		test_to_first_failure rep original
	  with Fitness.Found_repair(_) -> exit 1
  ) worklist ;

  debug "search: brute_force_1 ends\n" ;
  []

(*************************************************************************
 *************************************************************************
                          Basic Genetic Algorithm
 *************************************************************************
 *************************************************************************)

(* Just get fault localization ids *)
let just_id inp =
  List.map (fun (sid, prob) -> sid) (inp#get_fault_localization ())

let rec choose_from_weighted_list chosen_index lst = match lst with
  | [] -> failwith "localization error"
  | (sid,prob) :: tl -> if chosen_index <= prob then sid
                  else choose_from_weighted_list (chosen_index -. prob) tl

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

let mutate ?comp:(comp = 1) ?(test = false)  (variant : 'a Rep.representation) random =
  let subatoms = variant#subatoms && !use_subatoms in
  let result = variant#copy () in
  let mut_ids = ref (variant#get_fault_localization ()) in

  (* Splits search space for distributed algorithms *)
	if !split_search then
      mut_ids := (List.filter (fun (x , prob) -> (x mod !num_comps) == comp) !mut_ids);

  let mut_ids =
    if !promut <= 0 then !mut_ids
    else uniq !mut_ids
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
	let rec atom_mutate max_op = (* stmt-level mutation *)
          match Random.int max_op with
          | 0 -> result#delete x
          | 1 ->
	    let allowed = variant#append_sources x in
	      if WeightSet.cardinal allowed > 0 then
		let after = random allowed in
		  result#append x after
	      else atom_mutate 1
          | _ ->
	    let allowed = variant#swap_sources x in
	      if WeightSet.cardinal allowed > 0 then
		let swapwith = random allowed in
		  result#swap x swapwith
	      else atom_mutate 2
	in
      if subatoms && (Random.float 1.0 < !subatom_mutp) then begin
        (* sub-atom mutation *)
        let x_subs = variant#get_subatoms x in
        if x_subs = [] then atom_mutate 3
        else if ((Random.float 1.0) < !subatom_constp) then begin
          let x_sub_idx = Random.int (List.length x_subs) in
          result#replace_subatom_with_constant x x_sub_idx
        end else begin
          let allowed = variant#append_sources x in
          let allowed = List.map fst (WeightSet.elements allowed) in
          let allowed = random_order allowed in
          let rec walk lst = match lst with
          | [] -> atom_mutate 3
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
      end else atom_mutate 3
  ) mut_ids ;
  result

(***********************************************************************
 * Crossover
 *
 * We currently have three approaches to crossover: a standard "one-point"
 * crossover, "patch subset" crossover and "flat" crossover.
 ***********************************************************************)

(* Flat crossover: preserves flattened length or doesn't crossover *)
let flat_crossover
    (original : 'a Rep.representation)
    (variant1 : 'a Rep.representation)
    (variant2 : 'a Rep.representation)
    : ('a representation) list =
  let borders lsts =
    List.rev
      (List.tl
         (List.fold_left
            (fun acc el -> ((variant1#atom_length el) + (List.hd acc)) :: acc)
            [0] lsts)) in
  let intersection a b =
    let index = ref 0 in
      List.fold_left
        (fun acc i ->
           while ((List.nth a !index)<i) && (!index<((List.length a) - 1)) do
             index := !index + 1
           done ;
           if ((List.nth a !index) = i) then
             i :: acc
           else
             acc ) [] b in
  let place el lst =
    let out = ref (-1) in
      Array.iteri
        (fun i it ->
           if (!out < 0) && (el = it) then out := i) (Array.of_list lst);
      !out in
  (* let flat_len genome = *)
  (*   List.fold_left (+) 0 *)
  (*     (List.map variant1#atom_length genome) in *)
  let c_one = variant1#copy () in       (* copies *)
  let c_two = variant2#copy () in
  let g_one = c_one#get_genome () in    (* raw genomes *)
  let g_two = c_two#get_genome () in
  let b_one = borders g_one in          (* lengths at atom borders *)
  let b_two = borders g_two in
  let point = List.hd (random_order (intersection b_one b_two)) in
  let point_one = place point b_one in  (* crossover points *)
  let point_two = place point b_two in
  let new_one = ref [] in               (* to hold raw genomes *)
  let new_two = ref [] in
    (* (\* print debug information *\) *)
    (* debug "c (%d:%d)->" (flat_len g_one) (flat_len g_two) ; *)
    for i = 0 to point_one do
      new_one := (List.nth g_one i) :: !new_one
    done ;
    for i = (point_two + 1) to ((List.length g_two) - 1) do
      new_one := (List.nth g_two i) :: !new_one
    done ;
    for i = 0 to point_two do
      new_two := (List.nth g_two i) :: !new_two
    done ;
    for i = (point_one + 1) to ((List.length g_one) - 1) do
      new_two := (List.nth g_one i) :: !new_two
    done ;
    (* print debug information *)
    (* debug "(%d:%d)\n" (flat_len g_one) (flat_len g_two) ; *)
    c_one#set_genome (List.rev !new_one) ;
    c_two#set_genome (List.rev !new_two) ;
    c_one#add_history (Crossover((Some point_one),(Some point_two))) ;
    c_two#add_history (Crossover((Some point_one),(Some point_two))) ;
    [c_one;c_two]

(* Patch Subset Crossover *)
let crossover_patch_subset
        (original : 'a Rep.representation)
        (variant1 : 'a Rep.representation)
        (variant2 : 'a Rep.representation)
	: ('a representation) list =
  let h1 = variant1#get_history () in
  let h2 = variant1#get_history () in
  let new_h1 = List.fold_left (fun acc elt ->
      if probability !crossp then acc @ [elt] else acc
    ) [] (h1 @ h2) in
  let new_h2 = List.fold_left (fun acc elt ->
      if probability !crossp then acc @ [elt] else acc
    ) [] (h2 @ h1) in
	let c_one = original#copy () in
	let c_two = original#copy () in
  c_one#set_history new_h1 ;
  c_two#set_history new_h2 ;
  [ c_one ; c_two ; variant1 ; variant2 ]

(* One point crossover *)
let crossover_one_point ?(test = 0)
        (original : 'a Rep.representation)
        (variant1 : 'a Rep.representation)
        (variant2 : 'a Rep.representation)
	: ('a representation) list =
	let c_one = variant1#copy () in
	let c_two = variant2#copy () in
	let mat_1 = just_id variant1 in
	let mat_2 = just_id variant2 in
	let point = if test=0 then Random.int (List.length mat_1) else test in
	List.iter (fun p -> begin
				c_one#put (List.nth mat_1 p) (variant2#get (List.nth mat_2 p));
				c_two#put (List.nth mat_2 p) (variant1#get (List.nth mat_1 p));
				end )
			  (0--point) ;
    c_one#add_history (Crossover((Some point),None)) ;
    c_two#add_history (Crossover(None,(Some point))) ;
	[c_one;c_two]

let do_cross ?(test = 0)
        (original : 'a Rep.representation)
        (variant1 : 'a Rep.representation)
        (variant2 : 'a Rep.representation)
	: ('a representation) list =
  match !crossover with
  | "one" -> crossover_one_point ~test original variant1 variant2

  | "back" -> crossover_one_point ~test original variant1 original

  | "patch"
  | "subset" -> crossover_patch_subset original variant1 variant2

  | "flat"
  | "flatten" -> flat_crossover original variant1 variant2

  | x -> abort "unknown --crossover %s\n" x


(***********************************************************************
 * Tournament Selection
 ***********************************************************************)
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

(***********************************************************************
 * Basic Genetic Algorithm Search Strategy
 *
 * This is parametric with respect to a number of choices (e.g.,
 * population size, selection method, fitness function, fault
 * localization, ...).
 ***********************************************************************)

type info = { generation : int ; test_case_evals : int }
let success_info = ref []

let calculate_fitness generation pop orig =
  let record_success () =
	let info = { generation = generation;
				 test_case_evals = Rep.num_test_evals_ignore_cache() }
	in
	  success_info := info :: !success_info;
  in
  let max_fitness =
	let fac = (float !pos_tests) *. !negative_test_weight /.
	  (float !neg_tests) in
	  (float !pos_tests) +. ( (float !neg_tests) *. fac)
  in
  lmap (fun variant ->
	  try
		variant, test_all_fitness variant orig
	  with Found_repair(rep) -> begin
	    record_success();
		if not !continue then raise (Fitness.Found_repair(rep))
		else variant, max_fitness
	  end) pop


  (* choose a stmt at random based on the fix localization strategy *)
let random atom_set =
  if (*!uniform*) false then begin
    let elts = List.map fst (WeightSet.elements atom_set) in
    let size = List.length elts in
	  List.nth elts (Random.int size)
  end
  else (* Roulette selection! *)
    fst (choose_one_weighted (WeightSet.elements atom_set))

let crossover original (population : 'a Rep.representation list) =
  let mating_list = random_order population in
    (* should we cross an individual? *)
  let maybe_cross () = if (Random.float 1.0) <= !crossp then true else false in
  let output = ref [] in
  let half = (List.length mating_list) / 2 in
	for it = 0 to (half - 1) do
	  let parent1 = List.nth mating_list it in
	  let parent2 = List.nth mating_list (half + it) in
	    if maybe_cross () then
		  output := (do_cross original parent1 parent2) @ !output
	    else
		  output := parent1 :: parent2 :: !output
	done ;
	!output

(* generate the initial population *)

let initialize_ga ?comp:(comp=1) (original : 'a Rep.representation) incoming_pop =
  let pop = ref incoming_pop in (* our GP population *)
    assert((llen incoming_pop) <= !popsize);
    let remainder = !popsize - (llen incoming_pop) in
      if remainder > 0 then
	(* include the original in the starting population *)
		pop := (original#copy ()) :: !pop ;
      for i = 2 to remainder do
	(* initialize the population to a bunch of random mutants *)
		pop := (mutate ~comp:comp original random) :: !pop
      done ;
	  debug "search: initial population (sizeof one variant = %g MB)\n"
      (debug_size_in_mb (List.hd !pop));
	  calculate_fitness 0 !pop original

(* run the genetic algorithm for a certain number of generations, given the last generation as input *)

let run_ga ?comp:(comp=1) ?start_gen:(start_gen=1) ?num_gens:(num_gens = (!generations))
	(incoming_population : ('a Rep.representation * float) list) (original : 'a Rep.representation) :
	('a Rep.representation * float) list =
  let rec iterate_generations gen incoming_population =
	if gen < (start_gen + num_gens) then begin
	  debug "search: generation %d (sizeof one variant = %g MB)\n" gen
      (debug_size_in_mb (List.hd incoming_population));
	  incr gens_run;
      (* debug "search: %d live bytes; %d bytes in !pop (start of gen %d)\n"
        (live_bytes ()) (debug_size_in_bytes !pop) gen ;  *)
	  (* Step 1: selection *)
	  let selected = selection incoming_population !popsize in
	  (* Step 2: crossover *)
	  let crossed = crossover original selected in
	  (* Step 3: mutation *)
	  let mutated = List.map (fun one -> (mutate ~comp:comp one random)) crossed in
	  let gen' = gen + 1 in
		  (* Step 4. Calculate fitness. *)
		iterate_generations gen' (calculate_fitness gen mutated original)
      (*
		debug "search: %d live bytes; %d bytes in !pop (end of gen %d)\n"
		(live_bytes ()) (debug_size_in_bytes !pop) gen ;
	  *)
	end else incoming_population
  in
	iterate_generations start_gen incoming_population

(* basic genetic_algorithm, as called from main, for example *)
let genetic_algorithm (original : 'a Rep.representation) incoming_pop =
(* transform a list of variants into a listed of fitness-evaluated
 * variants *)
  debug "search: genetic algorithm begins (|original| = %g MB)\n"
    (debug_size_in_mb original);
  assert(!generations > 0);
  let initial_population = initialize_ga original incoming_pop in
	incr gens_run;
  (* Main GP Loop: *)
  let retval = run_ga initial_population original in
	debug "search: genetic algorithm ends\n" ;
	retval

(***********************************************************************
 * Mutational Robustness
 *
 * Evaluate the mutational robustness across the three mutational
 * operators.
 *
 * **********************************************************************)
let _ =
  options := !options @ [
    "--mutrb-runs", Arg.Set_int mutrb_runs, "X evaluate neutrality of X runs of each mutation operation";
    "--neutral", Arg.Set_float neutral_fitness, "X Neutral fitness";
  ]

let neutral_variants (rep : 'a Rep.representation) = begin
  debug "search: mutational robustness testing begins\n" ;
  debug "search: mutational robustness of %s\n" !robustness_ops ;
  promut := 1 ;                 (* exactly one mutation per variant *)
  mutp := 0.0 ;                 (* no really, exactly one mutation per variant *)
  subatom_mutp := 0.0 ;         (* no subatom mutation *)
  let do_op_p op = try ignore (String.index !robustness_ops op); true with Not_found -> false in
  let pick elts =
    let size = List.length elts in
      List.nth elts (Random.int size) in
  let mut_ids = ref (rep#get_fault_localization ()) in
  let appends = ref [] in
  let deletes = ref [] in
  let swaps   = ref [] in
    for i = 1 to !mutrb_runs do
      let variant_app = rep#copy () in
      let variant_swp = rep#copy () in
      let variant_del = rep#copy () in
      let (x_app,_) = (pick !mut_ids) in
      let (x_swp,_) = (pick !mut_ids) in
      let (x_del,_) = (pick !mut_ids) in
      let app_allowed = rep#append_sources x_app in
      let swp_allowed = rep#swap_sources x_swp in
        if do_op_p 'a' then begin
          if WeightSet.cardinal app_allowed <= 0 then
            failwith "no append sources" ;
          variant_app#append x_app (random app_allowed) ;
          appends := variant_app :: !appends
        end ;
        if do_op_p 's' then begin
          if WeightSet.cardinal swp_allowed <= 0 then
            failwith "no swap sources";
          variant_swp#swap x_swp (random swp_allowed) ;
          swaps := variant_swp :: !swaps
        end ;
        if do_op_p 'd' then begin
          deletes := variant_del :: !deletes ;
          variant_del#delete x_del ;
        end ;
    done ;
    let fitness variants =
      List.map (fun variant ->
	          try (variant, test_all_fitness variant rep)
	          with Found_repair(rep) -> (variant, -1.0))
        variants in
    let num_neutral variants_w_fit =
      List.length
        (List.filter (fun (_,fitness) ->
                        (fitness >= !neutral_fitness) || (fitness < 0.0))
           variants_w_fit) in
    let appends_fit = fitness !appends in
    let deletes_fit = fitness !deletes in
    let swaps_fit = fitness !swaps in
      (* print summary robustness information to STDOUT *)
      debug "%d append are neutral\n" (num_neutral appends_fit) ;
      debug "%d delete are neutral\n" (num_neutral deletes_fit) ;
      debug "%d swap   are neutral\n" (num_neutral swaps_fit) ;
      debug "search: mutational robustness testing ends\n" ;
      List.flatten [appends_fit; deletes_fit; swaps_fit]
end

(***********************************************************************
 * Oracle Search
 *
 * Instantly find a repair based on a given history. Use in conjunction
 * with oracle-edit-history command line arg to create one variant with
 * an edit history of a known repair.
 *
 * **********************************************************************)
let oracle_search (orig : 'a Rep.representation) = begin
  let the_repair = orig#copy () in
  (* Parse oracle-edit-history and build up the repair's edit history *)
  let split_repair_history = Str.split (Str.regexp " ") !oracle_edit_history in
  let repair_history =
    List.fold_left ( fun acc x ->
      let the_action = String.get x 0 in
      match the_action with
	'd' ->   Scanf.sscanf x "%c(%d)" (fun _ id -> (Delete(id)) :: acc)
	| a -> match a with
	   'a' -> Scanf.sscanf x "%c(%d,%d)" (fun _ id1 id2 -> (Append(id1,id2)) :: acc)
	 | 's' -> Scanf.sscanf x "%c(%d,%d)" (fun _ id1 id2 -> (Swap(id1,id2)) :: acc)
	 |  _ -> assert(false)
    ) [] split_repair_history
    in
 
    the_repair#set_history (List.rev repair_history);
    test_to_first_failure the_repair orig;
    []
end

(***********************************************************************
 * Neutral Walk
 *
 * Walk the neutral space of a program.
 *
 * **********************************************************************)
let _ =
  options := !options @ [
    "--neutral-walk-pop-size", Arg.Set_int neutral_walk_pop_size,
    "X Walk a population of size X through the neutral space.";
    "--neutral-walk-steps", Arg.Set_int neutral_walk_steps,
    "X Take X steps through the neutral space.";
    "--neutral-walk-max-size", Arg.Set_int neutral_walk_max_size,
    "X Maximum allowed size of variants in neutral walks, 0 to accept any size, -1 to maintain original size.";
  ]

let neutral_walk (original : 'a Rep.representation) incoming_pop = begin
  debug "search: neutral walking testing begins\n" ;
  promut := 1 ;                 (* exactly one mutation per variant *)
  mutp := 0.0 ;                 (* no really, exactly one mutation per variant *)
  subatom_mutp := 0.0 ;         (* no subatom mutation *)
  (* possibly update the --neutral-walk-max-size as appropriate *)
  if (!neutral_walk_max_size == -1) then
    neutral_walk_max_size := original#genome_length() ;
  let pop = ref incoming_pop in
    if ((List.length !pop) <= 0) then
      pop := original :: !pop ;
  let pick lst =
    let size = List.length lst in
      List.nth lst (Random.int size) in
  let random atom_set =
    pick (List.map fst (WeightSet.elements atom_set)) in
  let step = ref 0 in
    while !step <= !neutral_walk_steps do
      step := !step + 1;
      let new_pop = ref [] in
      let tries = ref 0 in
        (* take a step *)
        while (List.length !new_pop) < !neutral_walk_pop_size do
          tries := !tries + 1;
          let variant = mutate (pick !pop) random in
          let fitness =
            try test_all_fitness variant original
	    with Found_repair(original) -> -1.0 in
            if (((!neutral_walk_max_size == 0) ||
                   (variant#genome_length() <= !neutral_walk_max_size)) &&
                  ((fitness >= !neutral_fitness) || (fitness < 0.0))) then
              new_pop := variant :: !new_pop
        done ; 
        pop := random_order !new_pop;
        (* print the history (#name) of everyone in the population *)
        debug "pop[%d]:" !tries;
        List.iter (fun variant -> debug "%s " (variant#name())) !pop;
        debug "\n";
        (* print the genome lengths as recorded internally *)
        debug "sizes:";
        List.iter (fun variant -> debug "%d " (variant#genome_length())) !pop;
        debug "\n";
    done ;
    List.map (fun rep -> (rep,test_all_fitness rep original)) !pop
end
