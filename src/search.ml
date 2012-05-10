(** The {b Search} module provides an interface to conduct various types of
    searches over populations of individuals.  Search strategies include:

    {ul
    {- Brute Force (e.g., all distance-one edits)}
    {- Genetic Programming (e.g., ICSE'09)}
    {- Distributed (see [Network]) }
    {- Neutral walks}}

 *)

open Printf
open Global
open Fitness
open Template
open Rep
open Population

(**/**)
let generations = ref 10
let max_evals = ref 0
let subatom_mutp = ref 0.0
let subatom_constp = ref 0.5
let promut = ref 1
let continue = ref false
let gens_run = ref 0
let neutral_walk_max_size = ref 0
let neutral_walk_weight = ref ""

let app_prob = ref 0.33333
let del_prob = ref 0.33333
let swap_prob = ref 0.33333
let rep_prob = ref 0.0

let templates = ref ""

let _ =
  options := !options @ [
      "--templates", Arg.Set_string templates, 
      " Use repair templates; read from file X.  Default: none";

    "--appp", Arg.Set_float app_prob, 
    "X relative append probability. Default: 0.3333.";

    "--delp", Arg.Set_float del_prob, 
    "X relative delete probability. Default: 0.3333.";

    "--swapp", Arg.Set_float swap_prob, 
    "X relative swap probability. Default: 0.3333";

    "--repp", Arg.Set_float rep_prob, 
    "X relative replace probability. Default: 0.0";

    "--generations", Arg.Set_int generations, 
    "X conduct X iterations of the given search strategy. Default: 10.";

    "--max-evals", Arg.Set_int max_evals, 
    "X allow X maximum fitness evaluations in GA runs";

    "--promut", Arg.Set_int promut, "X make X mutations per 'mutate' call";

    "--subatom-mutp", Arg.Set_float subatom_mutp, 
    "X use X as subatom mutation rate";

    "--subatom-constp", Arg.Set_float subatom_constp, 
    "X use X as subatom constant rate";

    "--continue", Arg.Set continue, 
    " Continue search after repair has been found.  Default: false";
  ]

(**/**)

(** thrown if the number of fitness evaluations conducted so far exceeds
    [max_evals], a command-line parameter.  This feature is off by default *)
exception Maximum_evals of int
(** thrown by some search strategies when a repair is found *)
exception Found_repair of string

(**/**)
let random atom_set = 
  let elts = List.map fst (WeightSet.elements atom_set) in 
  let size = List.length elts in 
    List.nth elts (Random.int size) 
(**/**)

(* What should we do if we encounter a true repair? *)

(** We track success info, particularly for those strategies that might find
    more than one repair *)
type info = { generation : int ; test_case_evals : int }
let success_info = ref []

(* CLG is not convinced that the responsibility for writing out the successful
   repair should lie in search, but she does think it's better to have it here
   than in fitness, where it was before *)
(** Different strategies and representation types can do different
    things when a repair is found.  This at least stores information about the
    successful variant and may write it to disk or otherwise dispatch to the
    successful variant itself (potentially leading to minimization, for example,
    depending on the representation and command-line arguments). 

    @param rep successful variant
    @param orig original variant
    @param generation generation in which the repair was found *)
let note_success (rep : ('a,'b) Rep.representation) 
    (orig : ('a,'b) Rep.representation) (generation : int) : unit = 
  let record_success () =
    let info = { generation = generation;
                 test_case_evals = Rep.num_test_evals_ignore_cache() }
    in
      success_info := info :: !success_info;
  in
    record_success();
    match !search_strategy with
    | "mutrb" | "neut" | "neutral" | "walk" | "neutral_walk" -> ()
    | _ -> begin
      let name = rep#name () in 
        debug "\nRepair Found: %s\n" name ;
        let subdir = add_subdir (Some("repair")) in
        let filename = "repair"^ !Global.extension in
        let filename = Filename.concat subdir filename in
          rep#output_source filename ;
          rep#note_success ();
          if not !continue then raise (Found_repair(name))
    end

(**** Brute Force: Try All Single Edits ****)

(** tries all single-atom delete, append, and swap edits on a given input
    representation (original).  The search is biased by the fault and fix
    weights in the original variant. Deletions are favored over appends and
    swaps, appends are favored over swaps.  is ignored.  Subatom mutations are
    included if the representation supports it and if the subatom mutation rate
    is greater than 0.0.

    @param original original variant
    @param incoming_pop ignored
*)
let brute_force_1 (original : ('a,'b) Rep.representation) incoming_pop =
  debug "search: brute_force_1 begins\n" ;
  if incoming_pop <> [] then debug "search: incoming population IGNORED\n" ;

  let fault_localization = 
    lsort (fun (stmt,prob) (stmt',prob') ->
      if prob = prob' then compare stmt stmt'
      else compare prob' prob)
      (original#get_faulty_atoms ())
  in
  let fix_localization = 
    lsort (fun (stmt,prob) (stmt',prob') ->
      if prob = prob' then compare stmt stmt'
      else compare prob' prob)
      (original#get_fix_source_atoms ())
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
  let _ = debug "search: brute: %d deletes\n" (llen fault_localization) in

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
    ) fault_localization 
  in
  let _ = debug "search: brute: %d appends\n" (llen appends) in

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
    ) fault_localization 
  in
  let _ = debug "search: brute: %d swaps (out of %d)\n" (llen swaps) in
    
  let subatoms =
    if original#subatoms && !subatom_mutp > 0.0 then begin
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
      let _ = debug "search: brute: %d subatoms\n" (llen sub_muts) in

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
      let _ = debug "search: brute: %d subatom swaps\n" (llen sub_swaps) in
        sub_muts @ sub_swaps
    end else []
  in
  let worklist = deletes @ appends @ swaps @ subatoms in
    if worklist = [] then 
      debug "WARNING: no variants to consider (no fault localization?)\n" ;
    
    let worklist = 
      List.sort (fun (m,w) (m',w') -> compare w' w) worklist in
    let howmany = List.length worklist in
    let sofar = ref 1 in
      try 
        List.iter (fun (thunk,w) ->
          debug "\tvariant %d/%d (weight %g)\n" !sofar howmany w ;
          let rep = thunk () in
            incr sofar ;
            if test_to_first_failure rep then begin
              note_success rep original (-1); raise (Found_repair(rep#name()))
            end;
        ) worklist ;
        debug "search: brute_force_1 ends\n" ; 
      with Found_repair(_) -> debug "search: brute_force_1 ends, repair found\n"

(*
  Basic Genetic Algorithm
*)

(** randomly chooses an atomic mutation operator,
    instantiates it as necessary (selecting an insertion source, for example),
    and applies it to some variant.  These choices are guided by certain
    probabilities, such as the node weights or the probabilities associated with
    each operator. If applicable for the given experiment/representation, may
    use subatom mutation. 
    @param test optional; force a mutation on every atom of the variant
    @param variant individual to mutate
    @return variant' modified/potentially mutated variant
*)
(* CLG changed this in March 2012.  Where before atom_mutate would pick a
   mutation and, if there existed no legal sources to fill in the rest of a
   mutation (such as append or swap), call itself recursively, removing that
   mutation from consideration, now it asks the representation what is legal at
   a given faulty atom and selects from there.  Thus, if it picks append, it
   assumes that there exist valid append sources in that representation and does
   not check that the returned set is non-empty.  If such a set *is* empty, in
   other words, atom_mutate will fail. *)
let mutate ?(test = false)  (variant : ('a,'b) Rep.representation) =
  (* tell whether we should mutate an individual *)
  let result = variant#copy () in
  let atoms = variant#get_faulty_atoms () in
  let promut_list =
      let res = ref [] in
        for i = 1 to !promut do
          let sid = fst (choose_one_weighted atoms) in
            res := (sid) :: !res
        done ;
        !res
  in
    List.iter (fun (x,prob) ->
      if test || (List.mem x promut_list ) then begin
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
                let templates =
                  variant#template_available_mutations str x 
                in
                let fillins,_ = 
                  choose_one_weighted (lmap (fun (a,b,c) -> c,b) templates) 
                in 
                  result#apply_template str fillins
            end
        in
        let subatoms = variant#subatoms && !subatom_mutp > 0.0 in
          if subatoms && (Random.float 1.0 < !subatom_mutp) then begin
            (* sub-atom mutation *)
            let x_subs = variant#get_subatoms x in
              if x_subs = [] then atom_mutate ()
              else if (Random.float 1.0) < !subatom_constp then
                let x_sub_idx = Random.int (List.length x_subs) in
                  result#replace_subatom_with_constant x x_sub_idx
              else begin
                let allowed = variant#append_sources x in
                let allowed = List.map fst (WeightSet.elements allowed) in
                let allowed = random_order allowed in
                let rec walk lst = match lst with
                  | [] -> atom_mutate ()
                  | src :: tl ->
                    let src_subs = variant#get_subatoms src in
                      if src_subs = [] then
                        walk tl
                      else
                        let x_sub_idx = Random.int (List.length x_subs) in
                        let src_subs = random_order src_subs in
                        let src_sub = List.hd src_subs in
                          result#replace_subatom x x_sub_idx src_sub
                in
                  walk allowed
              end
          end else atom_mutate ()           
      end
    ) atoms ;
    result

(** computes the fitness of a variant by dispatching to the {b Fitness}
    module. If the variant has maximal fitness, calls [note_success], which may
    terminate the search.

    @param generation current generation
    @param orig original variant
    @param variant individual being tested
    @return variant post-fitness-testing, which means it should know its fitness
    (assuming the [Fitness] module behaved as it should)
    @raise Maximum_evals if max_evals is less than infinity and is reached. *)
let calculate_fitness generation orig variant =
    let evals = Rep.num_test_evals_ignore_cache() in
      if !max_evals > 0 && evals > !max_evals then 
        raise (Maximum_evals(evals));
      if test_fitness generation variant then 
        note_success variant orig generation;
      variant

(** prepares for GA by registering available mutations (including templates if
    applicable) and reducing the search space, and then generates the initial
    population, using [incoming_pop] if non-empty, or by randomly mutating the
    [original]. The resulting population is evaluated for fitness before being
    returned.  This may terminate early if a repair is found in the initial
    population (by [calculate_fitness]).

    @param original original variant
    @param incoming_pop possibly empty, incoming population
    @return initial_population generated by mutating the original *)
let initialize_ga (original : ('a,'b) Rep.representation) 
    (incoming_pop: ('a,'b) GPPopulation.t) : ('a,'b) GPPopulation.t =

  (* prepare the original/base representation for search by modifying the
     search space and registering all available mutations.*)
  original#reduce_search_space (fun _ -> true) (not (!promut <= 0));
  original#register_mutations 
    [(Delete_mut,!del_prob); (Append_mut,!app_prob); 
     (Swap_mut,!swap_prob); (Replace_mut,!rep_prob)];
  if !templates <> "" then 
    original#load_templates !templates;
  let pop = ref incoming_pop in
    if (llen incoming_pop) > !popsize then
      pop := first_nth incoming_pop !popsize; 

    let remainder = !popsize - (llen incoming_pop) in
      (* include the original in the starting population *)
      if remainder > 0 then pop := (original#copy ()) :: !pop ;

      (* initialize the population to a bunch of random mutants *)
      pop :=
        GPPopulation.generate !pop  (fun () -> mutate original) !popsize;
      debug ~force_gui:true 
        "search: initial population (sizeof one variant = %g MB)\n"
        (debug_size_in_mb (List.hd !pop));
      (* compute the fitness of the initial population *)
      GPPopulation.map !pop (calculate_fitness 0 original)

(** runs the genetic algorithm for a certain number of iterations, given the
    most recent/previous generation as input.  Returns the last generation, unless it
    is killed early by the search strategy/fitness evaluation.  The optional
    parameters are set to the obvious defaults if omitted. 

    @param start_gen optional; generation to start on (defaults to 1) 
    @param num_gens optional; number of generations to run (defaults to
    [generations]) 
    @param incoming_population population produced by the previous iteration 
    @raise Found_Repair if a repair is found
    @raise Max_evals if the maximum fitness evaluation count is reached
    @return population produced by this iteration *)
let run_ga ?start_gen:(start_gen=1) ?num_gens:(num_gens = (!generations))
    (incoming_population : ('a,'b) GPPopulation.t)
    (original : ('a,'b) Rep.representation) : ('a,'b) GPPopulation.t =
  
  (* the bulk of run_ga is performed by the recursive inner helper
     function, which Claire modeled off the MatLab code sent to her by the
     UNM team *)
  let rec iterate_generations gen incoming_population =
    if gen < (start_gen + num_gens) then begin
      debug ~force_gui:true 
        "search: generation %d (sizeof one variant = %g MB)\n" 
        gen (debug_size_in_mb (List.hd incoming_population));
      incr gens_run;
      (* Step 1: selection *)
      let selected = GPPopulation.selection incoming_population !popsize in
      (* Step 2: crossover *)
      let crossed = GPPopulation.crossover selected original in
      (* Step 3: mutation *)
      let mutated = GPPopulation.map crossed (fun one -> mutate one) in
      (* Step 4. Calculate fitness. *)
      let pop' = 
        GPPopulation.map mutated (calculate_fitness gen original) 
      in
        (* iterate *)
        iterate_generations (gen + 1) pop'
    end else incoming_population
  in
    iterate_generations start_gen incoming_population

(** {b genetic_algorithm } is parametric with respect to a number of choices
    (e.g., population size, selection method, fitness function, fault localization,
    many of which are set at the command line or at the representation level.
    May exit early if exceptions are thrown in fitness evalution ([Max_Evals])
    or a repair is found [Found_Repair].

    @param original original variant
    @param incoming_pop incoming population, possibly empty
    @raise Found_Repair if a repair is found
    @raise Max_evals if the maximum fitness evaluation count is set and then reached *)
let genetic_algorithm (original : ('a,'b) Rep.representation) incoming_pop =
  debug "search: genetic algorithm begins (|original| = %g MB)\n"
    (debug_size_in_mb original);
  assert(!generations > 0);
  try begin
    let initial_population = initialize_ga original incoming_pop in
      incr gens_run;
      try 
        ignore(run_ga initial_population original);
        debug "search: genetic algorithm ends\n" ;
      with Maximum_evals(evals) -> 
        debug "reached maximum evals (%d)\n" evals
  end with Maximum_evals(evals) -> begin
    debug "reached maximum evals (%d) during population initialization\n" evals;
  end


(***********************************************************************)
(** constructs a representation out of the genome as specified at the command
    line and tests to first failure.  This assumes that the oracle genome
    corresponds to a maximally fit variant.

    @param original individual representation
    @param starting_genome string; either a filename (binary representation) or
    as a string representation of the genome (like the history; this is the more
    likely use-case)
*)
let oracle_search (orig : ('a,'b) Rep.representation) (starting_genome : string) = 
  let the_repair = orig#copy () in
    if Sys.file_exists starting_genome then
      the_repair#deserialize starting_genome
    else 
      the_repair#load_genome_from_string starting_genome;
    assert(test_to_first_failure the_repair);
    note_success the_repair orig (1)

(***********************************************************************)


(** {5 {L Mutational Robustness: Evaluate the mutational robustness across the
    three mutational operators. }}
*)

(** explores the mutational robustness using
    append, delete, and swap mutation operators applied to the original
    (input) representation.
    @param original individual representation
    @raise Fail("append or swap sources") if the search tries to explore a
    mutation for which there are no valid sources (e.g., append, swap) for the
    randomly-selected atom
*)
(* FIXME ERIC: I replaced mutrb_runs with generation, but I now almost wonder if
   popsize wouldn't be a better choice. Thoughts? *)
let neutral_variants (rep : ('a,'b) Rep.representation) = begin
  debug "search: mutational robustness testing begins\n" ;
  let neutral_fitness = float_of_int !pos_tests in
  let pick elts =
    let size = List.length elts in
      List.nth elts (Random.int size) in
  let random atom_set =
    pick (List.map fst (WeightSet.elements atom_set)) in
  let mut_ids = ref (rep#get_faulty_atoms ()) in
  let appends =
    if !app_prob > 0.0 then
      GPPopulation.generate []
        (fun () ->
          let variant_app = rep#copy() in
          let x_app,_ = pick !mut_ids in
          let app_allowed = rep#append_sources x_app in
            if WeightSet.cardinal app_allowed <= 0 then
              failwith "no append sources" ;
            variant_app#append x_app (random app_allowed) ;
            variant_app) !generations
    else []
  in
  let deletes =
    if !del_prob > 0.0 then 
      GPPopulation.generate []
        (fun () ->
          let variant_del = rep#copy () in
          let x_del,_ = pick !mut_ids in
            variant_del#delete x_del;
            variant_del) !generations
    else [] 
  in
  let swaps = 
    if !swap_prob > 0.0 then 
      GPPopulation.generate []
        (fun () ->
          let variant_swp = rep#copy () in
          let x_swp,_ = pick !mut_ids in
          let swp_allowed = rep#swap_sources x_swp in
            if WeightSet.cardinal swp_allowed <= 0 then
              failwith "no swap sources";
            variant_swp#swap x_swp (random swp_allowed) ;
            variant_swp)
        !generations
    else []
  in
  let fitness variants =
    List.map (fun variant ->
      if test_fitness (-1) variant then
        variant, -1.0
      else variant,get_opt (variant#fitness()))
      variants 
  in
  let num_neutral variants_w_fit =
    List.length
      (List.filter (fun (_,fitness) ->
        fitness >= neutral_fitness || fitness < 0.0)
         variants_w_fit) 
  in
  let appends_fit = fitness appends in
  let deletes_fit = fitness deletes in
  let swaps_fit = fitness swaps in
    (* print summary robustness information to STDOUT *)
    debug "%d append are neutral\n" (num_neutral appends_fit) ;
    debug "%d delete are neutral\n" (num_neutral deletes_fit) ;
    debug "%d swap   are neutral\n" (num_neutral swaps_fit) ;
    debug "search: mutational robustness testing ends\n" ;
end

let _ =
  options := !options @ [
    "--neutral-walk-max-size", Arg.Set_int neutral_walk_max_size,
    "X Maximum neutral variant size; 0 for any size, -1 to maintain original.";

    "--neutral-walk-weight", Arg.Set_string neutral_walk_weight,
    "X Weight selection to favor X individuals. (e.g., small)";
  ]

(** walks the neutral space of a program. 

    @param original individual variant
    @param incoming_pop possibly empty incoming population
    @raise Fail("invalid neutral walk weight") if the command-line specified
    neutral walk weight is invalid.
*)
let neutral_walk (original : ('a,'b) Rep.representation) 
    (incoming_pop : ('a,'b) GPPopulation.t) =
  debug "search: neutral walking testing begins\n" ;
  assert(not (!subatom_mutp > 0.0));
  (* possibly update the --neutral-walk-max-size as appropriate *)
  let neutral_fitness = float_of_int !pos_tests in
    if !neutral_walk_max_size == -1 then
      neutral_walk_max_size := original#genome_length() ;

    let pick lst = List.nth lst (Random.int (List.length lst)) in 
    let weighted_pick lst = 
      if !neutral_walk_weight <> "" then begin
        let compare a b =
          match !neutral_walk_weight with
          | "small" -> a#genome_length() - b#genome_length()
          | _ -> 
            abort "search: bad neutral_walk_weight: %s\n" 
                        !neutral_walk_weight
        in
        let pre_pool = random_order lst in
        let pool = first_nth pre_pool !tournament_k in
        let sorted_pool = List.sort compare pool in
          List.hd sorted_pool
      end else pick lst
    in
    let tries = ref 0 in

    let rec take_neutral_steps pop step =
      let rec generate_neutral_variant pop = 
        incr tries;
        let variant = mutate (weighted_pick pop) in
        let fitness =
          if test_fitness step variant then
            -1.0
          else get_opt (variant#fitness())
        in
          if ((!neutral_walk_max_size == 0) ||
                 (variant#genome_length() <= !neutral_walk_max_size)) &&
            ((fitness >= neutral_fitness) || (fitness < 0.0)) then
            variant
          else generate_neutral_variant pop
      in
        if step <= !generations then begin
          let new_pop =
            GPPopulation.generate [] (fun () -> generate_neutral_variant pop) !popsize
          in
          let pop = random_order new_pop in 
          (* print the history (#name) of everyone in the population *)
            debug "pop[%d]:" !tries;
            List.iter (fun variant -> debug "%s " (variant#name())) pop;
            debug "\n";
          (* print the genome lengths as recorded internally *)
            debug "sizes:";
            List.iter (fun variant -> debug "%d " (variant#genome_length())) pop;
            debug "\n";
            take_neutral_steps pop (step + 1)
        end else pop
    in
    let pop = 
      GPPopulation.generate incoming_pop (fun () -> original#copy()) 1
    in
      ignore(take_neutral_steps pop 0)
