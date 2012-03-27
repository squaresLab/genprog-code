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
open Population

let generations = ref 10
let max_evals = ref 0
let mutp = ref 0.05
let subatom_mutp = ref 0.5
let subatom_constp = ref 0.5
let promut = ref 0
let continue = ref false
let gens_run = ref 0
let oracle_edit_history = ref ""
let mutrb_runs = ref 1000
let neutral_fitness = ref 5.0
let robustness_ops = ref "ads"
let neutral_walk_pop_size = ref 100
let neutral_walk_steps = ref 100
let neutral_walk_max_size = ref 0
let neutral_walk_weight = ref ""

let app_prob = ref 0.33333
let del_prob = ref 0.33333
let swap_prob = ref 0.33333
let rep_prob = ref 0.0

let _ =
  options := !options @ [
	"--appp", Arg.Set_float app_prob, "probability of an append";
	"--delp", Arg.Set_float del_prob, "probability of an delete";
	"--swapp", Arg.Set_float swap_prob, "probability of an swap";
	"--repp", Arg.Set_float rep_prob, "probability of an replace";
  "--generations", Arg.Set_int generations, "X use X genetic algorithm generations";
  "--max-evals", Arg.Set_int max_evals, "X allow X maximum fitness evaluations in GA runs";
  "--mutp", Arg.Set_float mutp, "X use X as mutation rate";
  "--promut", Arg.Set_int promut, "X make X mutations per 'mutate' call";
  "--subatom-mutp", Arg.Set_float subatom_mutp, "X use X as subatom mutation rate";
  "--subatom-constp", Arg.Set_float subatom_constp, "X use X as subatom constant rate";
  "--continue", Arg.Set continue, " Continue search after repair has been found.  Default: false";
  "--oracle-edit-history", Arg.Set_string oracle_edit_history, "X use X as edit history for oracle search" ;
  "--robustness-ops", Arg.Set_string robustness_ops, "X only test robustness of operations in X, e.g., 'ad' for 'append' and 'delete'" ;
]

exception Maximum_evals of int

let weight_compare (stmt,prob) (stmt',prob') =
    if prob = prob' then compare stmt stmt'
    else compare prob' prob

(*************************************************************************
 *************************************************************************
                     Brute Force: Try All Single Edits
 *************************************************************************
 *************************************************************************)

let brute_force_1 (original : ('a,'b) Rep.representation) incoming_pop =
  debug "search: brute_force_1 begins\n" ;
  if incoming_pop <> [] then begin
    debug "search: incoming population IGNORED\n" ;
  end ;
  let fault_localization = 
	lsort weight_compare (original#get_faulty_atoms ())
  in
  let fix_localization = []
(*	lsort weight_compare (original#get_fix_localization ())*)
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

let rec choose_from_weighted_list chosen_index lst = match lst with
  | [] -> failwith "localization error"
  | (sid,prob) :: tl -> if chosen_index <= prob then sid
                  else choose_from_weighted_list (chosen_index -. prob) tl

(* tell whether we should mutate an individual *)
let maybe_mutate prob =
  if (Random.float 1.0) <= (!mutp *. prob) then true else false


let choose_one_weighted_triple lst =
  assert(lst <> []);
  let total_weight = List.fold_left (fun acc (sid,prob,_) ->
    acc +. prob) 0.0 lst in
  assert(total_weight > 0.0) ;
  let wanted = Random.float total_weight in
  let rec walk lst sofar =
    match lst with
    | [] -> failwith "choose_one_weighted"
    | (sid,prob,t) :: rest ->
      let here = sofar +. prob in
      if here >= wanted then (sid,prob,t)
      else walk rest here
  in
  walk lst 0.0

(***********************************************************************
 * Weighted Micro-Mutation
 *
 * Here we pick delete, append or swap, and apply that atomic operator
 * with some probability to each element of the fault localization path.
 ***********************************************************************)

let mutate ?(test = false)  (variant : ('a,'b) Rep.representation) random =
  let result = variant#copy () in
  let mut_ids = variant#get_faulty_atoms () in
  let promut_list =
    if !promut <= 0 then
      []
    else begin
      let res = ref [] in
      for i = 1 to !promut do
		let sid = variant#choose_faulty_atom () in
          res := (sid) :: !res
      done ;
      !res
    end
  in
    List.iter (fun (x,prob) ->
      if (test || maybe_mutate prob || (List.mem x promut_list )) then begin
		let atom_mutate () = (* stmt-level mutation *)
		  let mutations = variant#available_mutations x in
			if (llen mutations) > 0 then begin
			  match fst (choose_one_weighted mutations) with 
			  | Delete_mut -> result#delete x
			  | Append_mut ->
				let allowed = variant#append_sources x in
				let after = random allowed in
				  result#append x after
			  | Swap_mut ->
				let allowed = variant#swap_sources x in
				let swapwith = random allowed in
				  result#swap x swapwith
			  | Replace_mut ->
				let allowed = variant#replace_sources x in
				let replacewith = random allowed in
				  result#replace x replacewith
			  | Template_mut(str) -> 
				let templates = variant#template_available_mutations x in
				let template, float, fillins = 
				  choose_one_weighted_triple templates in 
				  (* FIXME: this is kind of wrong; the template has been
					 selected so we don't need to generate all possible options. *)
				  result#mutate template fillins
			end
		in
		let subatoms = variant#subatoms && !use_subatoms in

		  if subatoms && (Random.float 1.0 < !subatom_mutp) then begin
			(* sub-atom mutation *)
			let x_subs = variant#get_subatoms x in
			  if x_subs = [] then atom_mutate ()
			  else if ((Random.float 1.0) < !subatom_constp) then begin
				let x_sub_idx = Random.int (List.length x_subs) in
				  result#replace_subatom_with_constant x x_sub_idx
			  end else begin
				let allowed = variant#append_sources x in
				let allowed = List.map fst (WeightSet.elements allowed) in
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
	  end
  ) mut_ids ;
  result



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
    lmap (fun variant ->
            (* possibly abort if too many fitness evaluations *)
            if (!max_evals > 0) then begin
              let evals = Rep.num_test_evals_ignore_cache() in
                if (evals > !max_evals) then
                  raise (Maximum_evals(evals))
            end;
	    try
	      ignore(test_all_fitness generation variant orig); variant
	    with Found_repair(rep) -> begin
	      record_success();
	      if not !continue then raise (Fitness.Found_repair(rep))
	      else variant
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

(* generate the initial population *)

let initialize_ga (original : ('a,'b) Rep.representation) incoming_pop =
  (* prepare the original/base representation for search by modifying the
	 search space and registering all available mutations. We used to load
	 templates in "load"; maybe move all this to "initialize"? Or have an
	 "initialize search" where we compute the localization, split the search
	 space, load the templates, and register the mutations?  That idea is
	 growing on me. *)
  (* also, want to convey the semantic check here instead of checking for that
	 flag separately elsewhere, which is annoying and wasteful *)
  original#reduce_search_space (fun _ -> true) (not (!promut <= 0));
  original#register_mutations 
	[(Delete_mut,!del_prob); (Append_mut,!app_prob); 
	 (Swap_mut,!swap_prob); (Replace_mut,!rep_prob)];
  if !Rep.templates <> "" then
	original#load_templates !Rep.templates;

  let pop = ref incoming_pop in (* our GP population *)
    assert((llen incoming_pop) <= !popsize);
    let remainder = !popsize - (llen incoming_pop) in
      if remainder > 0 then
	(* include the original in the starting population *)
		pop := (original#copy ()) :: !pop ;
      for i = 2 to remainder do
	(* initialize the population to a bunch of random mutants *)
		pop := (mutate original random) :: !pop
      done ;
	  debug ~force_gui:true "search: initial population (sizeof one variant = %g MB)\n"
      (debug_size_in_mb (List.hd !pop));
	  calculate_fitness 0 !pop original

(* run the genetic algorithm for a certain number of generations, given the last generation as input *)

let run_ga ?start_gen:(start_gen=1) ?num_gens:(num_gens = (!generations))
	(incoming_population : ('a,'b) GPPopulation.t) (original : ('a,'b) Rep.representation) :
	('a,'b) GPPopulation.t =
  let rec iterate_generations gen incoming_population =
    if ((!max_evals > 0) or (gen < (start_gen + num_gens))) then begin
	  debug ~force_gui:true "search: generation %d (sizeof one variant = %g MB)\n" gen
      (debug_size_in_mb (List.hd incoming_population));
	  incr gens_run;
      (* debug "search: %d live bytes; %d bytes in !pop (start of gen %d)\n"
        (live_bytes ()) (debug_size_in_bytes !pop) gen ;  *)
	  (* Step 1: selection *)
	  let selected = GPPopulation.selection incoming_population !popsize in
	  (* Step 2: crossover *)
	  let crossed = GPPopulation.crossover selected original in
	  (* Step 3: mutation *)
	  let mutated = List.map (fun one -> mutate one random) crossed in
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
let genetic_algorithm (original : ('a,'b) Rep.representation) incoming_pop =
(* transform a list of variants into a listed of fitness-evaluated
 * variants *)
  debug "search: genetic algorithm begins (|original| = %g MB)\n"
    (debug_size_in_mb original);
  assert(!generations > 0);
  try begin
    let initial_population = initialize_ga original incoming_pop in
      incr gens_run;
      try begin
        (* Main GP Loop: *)
        let retval = run_ga initial_population original in
          debug "search: genetic algorithm ends\n" ;
          retval
      end with Maximum_evals(evals) -> begin
        debug "reached maximum evals (%d)\n" evals; []
      end
  end with Maximum_evals(evals) -> begin
    debug "reached maximum evals (%d) during population initialization\n" evals; []
  end

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

let neutral_variants (rep : ('a,'b) Rep.representation) = begin
  debug "search: mutational robustness testing begins\n" ;
  debug "search: mutational robustness of %s\n" !robustness_ops ;
  promut := 1 ;                 (* exactly one mutation per variant *)
  mutp := 0.0 ;                 (* no really, exactly one mutation per variant *)
  subatom_mutp := 0.0 ;         (* no subatom mutation *)
  let do_op_p op = try ignore (String.index !robustness_ops op); true with Not_found -> false in
  let pick elts =
    let size = List.length elts in
      List.nth elts (Random.int size) in
  let mut_ids = ref (rep#get_faulty_atoms ()) in
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
	          try variant,test_all_fitness (-1) variant rep
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
      lmap fst (List.flatten [appends_fit; deletes_fit; swaps_fit])
end

(***********************************************************************
 * Oracle Search
 *
 * Instantly find a repair based on a given history. Use in conjunction
 * with oracle-edit-history command line arg to create one variant with
 * an edit history of a known repair.
 *
 * **********************************************************************)
let oracle_search (orig : ('a,'b) Rep.representation) = begin
  let the_repair = orig#copy () in
  (* Parse oracle-edit-history and build up the repair's edit history *)
  let split_repair_history = Str.split (Str.regexp " ") !oracle_edit_history in
  let repair_history =
    List.fold_left ( fun acc x ->
      let the_action = String.get x 0 in
      match the_action with
		'd' -> Scanf.sscanf x "%c(%d)" (fun _ id -> (Delete(id)) :: acc)
	  | 'a' -> Scanf.sscanf x "%c(%d,%d)" (fun _ id1 id2 -> (Append(id1,id2)) :: acc)
	  | 's' -> Scanf.sscanf x "%c(%d,%d)" (fun _ id1 id2 -> (Swap(id1,id2)) :: acc)
	  | 'r' -> Scanf.sscanf x "%c(%d,%d)" (fun _ id1 id2 -> (Replace(id1,id2)) :: acc)
	  |  _ -> assert(false)
    ) [] split_repair_history
    in
 
    the_repair#set_history (List.rev repair_history);
    test_to_first_failure the_repair orig;
	exit 1
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
    "--neutral-walk-weight", Arg.Set_string neutral_walk_weight,
    "X Weight selection to favor X individuals. (e.g., small)";
  ]

let neutral_walk (original : ('a,'b) Rep.representation) (incoming_pop : ('a,'b) GPPopulation.t) = begin
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
  let pick lst = List.nth lst (Random.int (List.length lst)) in 
  let weighted_pick lst = 
    if (!neutral_walk_weight <> "") then begin
      let compare a b =
        match !neutral_walk_weight with
          | "small" -> a#genome_length() - b#genome_length()
          | _ -> failwith (Printf.sprintf "search: bad neutral_walk_weight: %s\n" !neutral_walk_weight)
      in
      let pre_pool = random_order !pop in
      let pool = first_nth pre_pool !tournament_k in
      let sorted_pool = List.sort compare pool in
        List.hd sorted_pool
    end else (pick lst) in
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
          let variant = mutate (weighted_pick !pop) random in
          let fitness =
            try test_all_fitness !step variant original
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
    List.map (fun rep -> ignore(test_all_fitness (!step + 1) rep original); rep) !pop
end
