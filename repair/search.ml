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

(*Global(ish) variables necessary for splitting up the search space, recording
  the total number of generations and variants evaluated before exit (respectively)*)
let compnumber = ref 1
let totgen = ref (-1)

let weight_compare (stmt,prob) (stmt',prob') =
    if prob = prob' then compare stmt stmt' 
    else compare prob' prob 

(* transform a list of variants into a listed of fitness-evaluated
 * variants *) 
let calculate_fitness pop =  
  lmap (fun variant -> variant, test_all_fitness variant) pop

let generations = ref 10
let popsize = ref 40 
let mutp = ref 0.05
let subatom_mutp = ref 0.5
let subatom_constp = ref 0.5
let crossp = ref 0.5
let promut = ref 0 
let incoming_pop = ref "" 
let crossover = ref "one" 
 
let _ = 
  options := !options @ [
  "--generations", Arg.Set_int generations, "X use X genetic algorithm generations";
  "--popsize", Arg.Set_int popsize, "X variant population size";
  "--mutp", Arg.Set_float mutp, "X use X as mutation rate";	
  "--promut", Arg.Set_int promut, "X make X mutations per 'mutate' call";	
  "--subatom-mutp", Arg.Set_float subatom_mutp, "X use X as subatom mutation rate";	
  "--subatom-constp", Arg.Set_float subatom_constp, "X use X as subatom constant rate";	
  "--crossover", Arg.Set_string crossover, "X use X as crossover [one,subset,flat]";
  "--crossp", Arg.Set_float crossp, "X use X as crossover rate";

] 
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
    test_to_first_failure rep 
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

let mutate ?(test = false)  (variant : 'a Rep.representation) random = 
  let subatoms = variant#subatoms && !use_subatoms in 
  let result = variant#copy () in  
  let mut_ids = ref (variant#get_fault_localization ()) in 

  (* Splits search space for distributed algorithms *)
(*  if (!distributed || !network_dist) && !split_search then
    mut_ids := (List.filter (fun (x , prob) -> (x mod !num_comps) == !compnumber) !mut_ids)
  else ();*)
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
    (variant1 : 'a Rep.representation) 
    (variant2 : 'a Rep.representation)
    : ('a representation) list =
  let borders lsts =
    List.rev
      (List.fold_left
         (fun acc el -> ((variant1#atom_length el) + (List.hd acc)) :: acc)
         [0] lsts) in
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
    let ind = ref 0 in
    let out = ref None in
      List.iter (fun it ->
                   match !out with
                     | None -> if (it = el) then out := Some ind ;
                     | _    -> () ;
                   ind := !ind + 1)
        (List.rev lst);
      match !out with
        | Some o -> !o
        | None -> 0 in
  let min x y = if x < y then x else y in
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
    for i = 0 to (min point_one ((List.length g_one) - 1)) do
      new_one := (List.nth g_one i) :: !new_one
    done ;
    for i = (point_two + 1) to ((List.length g_two) - 1) do
      new_one := (List.nth g_two i) :: !new_one
    done ;
    for i = 0 to (min point_two ((List.length g_two) - 1)) do
      new_two := (List.nth g_two i) :: !new_two
    done ;
    for i = (point_one + 1) to ((List.length g_one) - 1) do
      new_two := (List.nth g_one i) :: !new_two
    done ;
    c_one#set_genome (List.rev !new_one) ;
    c_two#set_genome (List.rev !new_two) ;
    c_one#add_history (Crossover((Some point_one),(Some point_two))) ; 
    c_two#add_history (Crossover((Some point_one),(Some point_two))) ; 
    [c_one;c_two]

(* Patch Subset Crossover *)
let crossover_patch_subset
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
	let c_one = variant1#copy () in
	let c_two = variant2#copy () in
  c_one#set_history new_h1 ;
  c_two#set_history new_h2 ;
  [ c_one ; c_two ; variant1 ; variant2 ] 

(* One point crossover *)
let crossover_one_point ?(test = 0) 
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
    c_one#add_history (Crossover((Some point),None)) ; 
    c_two#add_history (Crossover(None,(Some point))) ; 
	[c_one;c_two]
	
let do_cross ?(test = 0) 
        (variant1 : 'a Rep.representation) 
        (variant2 : 'a Rep.representation)
	: ('a representation) list =
  match !crossover with
  | "one" -> crossover_one_point ~test variant1 variant2 

  | "patch" 
  | "subset" -> crossover_patch_subset variant1 variant2

  | "flat"
  | "flatten" -> flat_crossover variant1 variant2

  | x -> debug "unknown --crossover %s\n" x ; exit 1 

  
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

(***********************************************************************
 * Basic Genetic Algorithm Search Strategy
 *
 * This is parametric with respect to a number of choices (e.g.,
 * population size, selection method, fitness function, fault
 * localization, ...). 
 ***********************************************************************)

exception FoundIt of int

let genetic_algorithm ?(generations = !generations) (original : 'a Rep.representation) incoming_pop = 
  debug "search: genetic algorithm begins\n" ;
  assert(generations > 0);

  (* Splitting up the search space for distributed algorithms *)
  (*  if (!distributed || !network_dist) && !split_search then
      compnumber := comp
      else ();
  *)

  (* choose a stmt at random based on the fix localization strategy *) 
  let random atom_set = 
    if (*!uniform*) false then begin
      let elts = List.map fst (WeightSet.elements atom_set) in
      let size = List.length elts in 
	List.nth elts (Random.int size) 
    end
    else (* Roulette selection! *)
      fst (choose_one_weighted (WeightSet.elements atom_set))
  in  

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
	
      let crossover (population : 'a Rep.representation list) = 
	let mating_list = random_order population in
      (* should we cross an individual? *)
	let maybe_cross () = if (Random.float 1.0) <= !crossp then true else false in
	let output = ref [] in
	let half = (List.length mating_list) / 2 in
	  for it = 0 to (half - 1) do
	    let parent1 = List.nth mating_list it in
	    let parent2 = List.nth mating_list (half + it) in 
	      if maybe_cross () then
		output := (do_cross parent1 parent2) @ !output 
	      else 
		output := parent1 :: parent2 :: !output 
	  done ;
	  !output
      in

	  (* Main GP Loop: *) 
	for gen = 1 to generations do 
	  debug "search: generation %d\n" gen ;
      (*
		debug "search: %d live bytes; %d bytes in !pop (start of gen %d)\n"
        (live_bytes ()) (debug_size_in_bytes !pop) gen ; 
        *) 
	  (* Step 1. Calculate fitness. *) 
	  let incoming_population = calculate_fitness !pop in 
	  (* Exits upon success, while allowing the rest of the simulated computers
	     to continue *)
	  (* Step 2: selection *)
	  (*if gen < !generations then begin *)
	  (* do not apply mutation, selection, or crossover if we're just
	   * going to exit anyway, since we already applied mutation to
	   * the incoming population [i.e., if we don't skip this now, 
	   * and someone specifies --generations 1,we'll actually do 2X 
	   * mutations where X is the popsize. *)  
	  let selected = selection incoming_population !popsize in
	  (* Step 3: crossover *)
	  let crossed = crossover selected in
	  (* Step 4: mutation *)
	  let mutated = List.map (fun one -> (mutate one random)) crossed in
	    pop := mutated ;
	  (*end ;*)
 
  (*
    debug "search: %d live bytes; %d bytes in !pop (end of gen %d)\n"
    (live_bytes ()) (debug_size_in_bytes !pop) gen ; 
  *) 
	  done ;
	  debug "search: genetic algorithm ends\n" ;
	  
  (* Returns a population, fitness pair*)
	  (calculate_fitness !pop)
 
