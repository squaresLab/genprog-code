(*
 *
 * Copyright (c) 2012-2018,
 *  Wes Weimer          <weimerw@umich.edu>
 *  Stephanie Forrest   <steph@asu.edu>
 *  Claire Le Goues     <clegoues@cs.cmu.edu>
 *  Eric Schulte        <eschulte@cs.unm.edu>
 *  Jeremy Lacomis      <jlacomis@cmu.edu>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)
(** The {b Search} module provides an interface to conduct various types of
    searches over populations of individuals.  Search strategies include:

    {ul
    {- Brute Force (e.g., all distance-one edits)}
    {- Genetic Programming (e.g., ICSE'09)}
    {- Distributed (see [Network]) }
    {- Neutral walks}}

*)

open Global
open Fitness
open Rep
open Population

(**/**)
let generations = ref 10
let max_evals = ref 0
let subatom_mutp = ref 0.0
let subatom_constp = ref 0.5
let promut = ref 1
let geomp = ref 0.5
let continue = ref false
let gens_run = ref 0
let neutral_walk_max_size = ref 0
let neutral_walk_weight = ref ""

let app_prob = ref 0.33333
let del_prob = ref 0.33333
let swap_prob = ref 0.33333
let rep_prob = ref 0.0
let lase_prob = ref 0.0

let excluded_edits_str = ref ""
let excluded_edits = ref []

let pd_mutp = ref 0.25

let eviction_strategy = ref "random"
let fitness_log = ref ""

let disable_reduce_fix_space = ref false
let disable_reduce_search_space = ref false

(* The "--search adaptive" strategy interprets these strings as
 * mathematical expressions. They determine the order in which edits
 * and tests are considered, based on model variables.
 *
 * "1 * A ; 2 * B" means "first, sort by model variable A and take the
 * best. In case of a tie, break ties by taking the element that
 * maximizes 2 * model variable B." *)
let best_edit_rule = ref "1 * fault_loc_weight ; 1 * max_test_fail_prob ; -1 * num_tests"

let _ =
  options := !options @ [
      "--appp", Arg.Set_float app_prob,
      "X relative append probability. Default: 0.3333.";

      "--delp", Arg.Set_float del_prob,
      "X relative delete probability. Default: 0.3333.";

      "--swapp", Arg.Set_float swap_prob,
      "X relative swap probability. Default: 0.3333";

      "--repp", Arg.Set_float rep_prob,
      "X relative replace probability. Default: 0.0";

      "--lasep", Arg.Set_float lase_prob,
      "X relative probability of applying LASE templates. Default: 0.0";

      "--generations", Arg.Set_int generations,
      "X conduct X iterations of the given search strategy. Default: 10.";

      "--max-evals", Arg.Set_int max_evals,
      "X allow X maximum fitness evaluations in GA runs";

      "--promut", Arg.Set_int promut, "X make X mutations per 'mutate' call";

      "--geomp", Arg.Set_float geomp,
      "X probability of success for geometric distribution of mutations";

      "--subatom-mutp", Arg.Set_float subatom_mutp,
      "X use X as subatom mutation rate";

      "--subatom-constp", Arg.Set_float subatom_constp,
      "X use X as subatom constant rate";

      "--continue", Arg.Set continue,
      " Continue search after repair has been found.  Default: false";

      "--best-edit-rule", Arg.Set_string best_edit_rule,
      "X use X to rank possible edits in adaptive search" ;

      "--exclude-edits", Arg.Set_string excluded_edits_str,
      "X exclude all edits specified in X when running repair (space-seperated)" ;

      "--pd-mutp", Arg.Set_float pd_mutp,
      "X use X as the binomial mutation rate in exploration proactive diversity search";

      "--eviction-strategy", Arg.Set_string eviction_strategy,
      "X strategy for evicting from the steady-state popultion (random,tournament,worst)";

      "--fitness-log", Arg.Set_string fitness_log,
      "X log pop fitness to CSV file; used for steady-state where pop fitness is not clear from debug log";

      "--disable-reduce-fix-space", Arg.Set disable_reduce_fix_space,
      " Disable fix space reductions.  Default: false";

      "--disable-reduce-search-space", Arg.Set disable_reduce_search_space,
      " Disable search (fault) space reductions.  Default: false";
    ]

(**/**)

(** thrown if the number of fitness evaluations conducted so far exceeds
    [max_evals], a command-line parameter.  This feature is off by default *)
exception Maximum_evals of int
(** thrown by some search strategies when a repair is found *)
exception Found_repair of string

(**/**)
let random atom_set =
  let elts = List.rev (List.rev_map fst (WeightSet.elements atom_set)) in
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
      let h = rep#get_history () in
      debug "\nRepair Found:" ;
      List.iter (fun e ->
          debug " %s" (rep#history_element_to_str e)
        ) h ;
      let name = rep#name () in
      let skipped =
        lfilt (fun x -> x <> "") (Str.split comma_regexp !skipped_tests)
      in
      debug "\nRepair Name: %s\n" name ;
      debug "Test Cases Skipped: %S\n" (String.concat "," skipped) ;
      debug "Current Time: %f\n" (Unix.gettimeofday ()) ;
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
  if incoming_pop <> [] then debug "search: incoming population IGNORED\n" ;

  if not !disable_reduce_fix_space then begin
    debug "search: reduce_fix_space\n";
    original#reduce_fix_space () ;
  end;

  debug "search: brute_force_1 begins\n";
  original#register_mutations [
    (Delete_mut,!del_prob);
    (Append_mut,!app_prob);
    (Swap_mut,!swap_prob);
    (Replace_mut,!rep_prob);
    (Lase_Template_mut,!lase_prob);
  ];

  let rescale items =
    let scale = 1.0 /. (lfoldl (fun sum (_,w) -> sum +. w) 0.0 items) in
    lmap (fun (x,w) -> (x, w *. scale)) items
  in

  let fold_mutations rep f a =
    let lase, a =
      lfoldl (fun (lase, a) (fault, p1) ->
          lfoldl (fun (lase, a) (mut, p2) ->
              let w = p1 *. p2 in
              let fold2 apply a sources =
                lfoldl (fun a (fix, p3) ->
                    let rep' = rep#copy () in
                    apply rep' fault fix;
                    f a rep' (w *. p3)
                  ) a (rescale (WeightSet.elements sources))
              in
              match mut with
              | Delete_mut ->
                let rep' = rep#copy () in
                rep'#delete fault;
                lase, f a rep' w
              | Append_mut ->
                let sources = rep#append_sources fault in
                lase, fold2 (fun rep d s -> rep#append d s) a sources
              | Swap_mut ->
                let sources = rep#swap_sources fault in
                lase, fold2 (fun rep d s -> rep#swap d s) a sources
              | Replace_mut ->
                let sources = rep#replace_sources fault in
                lase, fold2 (fun rep d s -> rep#replace d s) a sources
              | Lase_Template_mut ->
                let p3 =
                  1.0 /. (float_of_int (map_cardinal Lasetemplates.templates))
                in
                let lase =
                  StringMap.fold (fun n _ lase ->
                      let ps = try StringMap.find n lase with Not_found -> [] in
                      StringMap.add n ((w *. p3) :: ps) lase
                    ) Lasetemplates.templates lase
                in
                lase, a
            ) (lase, a) (rescale (rep#available_mutations fault))
        ) (StringMap.empty, a) (rescale (rep#get_faulty_atoms()))
    in
    StringMap.fold (fun n ps a ->
        let rep' = rep#copy () in
        rep'#lase_template n;
        f a rep' (lfoldl ( +. ) 0.0 ps)
      ) lase a
  in

  let fold_k_mutations k f a =
    let rec helper k w a rep p =
      let w = w *. p in
      if k = 1 then
        f a rep w
      else
        fold_mutations rep (helper (k-1) w) a
    in
    fold_mutations original (helper k 1.0) a
  in

  (* FIXME: this should be !generations instead of hardcoding a particular
     value for k. Only problem is !generations defaults to 10, which is WAY too
     deep for a brute-force search. *)
  let k = 1 in
  debug "search: counting available mutants\n";
  let count = fold_k_mutations k (fun n _ _ -> n + 1) 0 in
  debug "search: %d mutants in search space\n" count;

  let exclude_edit =
    if !excluded_edits_str <> "" then
      let exclude_list = Str.split space_regexp !excluded_edits_str in
      let trim_regexp = Str.regexp "[ \t]+$" in
      fun rep ->
        let name = Str.replace_first trim_regexp "" (rep#name ()) in
        List.mem name exclude_list
    else
      fun _ -> false
  in

  let wins  = ref 0 in
  let sofar = ref 1 in
  fold_k_mutations k (fun _ rep w ->
      if not (exclude_edit rep) then begin
        if test_to_first_failure rep then begin
          note_success rep original (-1);
          incr wins;
          if not !continue then
            raise (Found_repair(rep#name ()))
        end;
        debug "\tvariant %d/%d/%d (w: %g) %s\n"
          !wins !sofar count w (rep#name ());
      end;
      incr sofar
    ) () ;

  debug "search: brute_force_1 ends\n"

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
  let mutate_one x result =
    let atom_mutate () = (* stmt-level mutation *)
      let mutations = result#available_mutations x in
      if (llen mutations) > 0 then begin
        let has_sources sources = not (WeightSet.is_empty (sources x)) in
        match fst (choose_one_weighted mutations) with
        | Delete_mut -> result#delete x
        | Append_mut when has_sources variant#append_sources ->
          variant#append_sources x |> random |> result#append x
        | Swap_mut when has_sources variant#swap_sources ->
          variant#swap_sources x |> random |> result#swap x
        | Replace_mut when has_sources variant#replace_sources ->
          variant#replace_sources x |> random |> result#replace x
        | Template_mut(str) ->
          let templates =
            variant#template_available_mutations str x
          in
          let fillins,_ = choose_one_weighted templates
          in
          result#apply_template str fillins
        | Lase_Template_mut ->
          let allowed =
            StringMap.fold (fun n _ ns -> n :: ns) Lasetemplates.templates []
          in
          let name = List.hd (random_order allowed) in
          result#lase_template name
        | _ -> failwith "No legal mutations"
      end
    in
    let subatoms = variant#subatoms && !subatom_mutp > 0.0 in
    if subatoms && (Random.float 1.0 < !subatom_mutp) then begin
      (* sub-atom mutation *)
      let x_subs = variant#get_subatoms ~fault_src:true x in
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
            let src_subs = variant#get_subatoms ~fault_src:false src in
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
  in

  (* In case of nested mutations, we need to recheck the list of faulty atoms
     before each additional mutation. *)
  let rec add_mutation remaining variant =
    let faulty = variant#get_faulty_atoms () in
    if List.length faulty = 0 then
      variant
    else
      let sid = fst (choose_one_weighted faulty) in
      mutate_one sid variant ;
      if remaining > 1 then
        add_mutation (remaining-1) variant
      else
        variant

  in

  (* tell whether we should mutate an individual *)
  if test then begin
    let result = variant#copy () in
    List.iter (fun (sid,_) -> mutate_one sid result)
      (variant#get_faulty_atoms()) ;
    result
  end else if !promut > 0 then
    add_mutation !promut (variant#copy ())
  else
    variant

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
let initialize_ga
    ?get_fitness
    (original : ('a,'b) Rep.representation)
    (incoming_pop: ('a,'b) GPPopulation.t) : ('a,'b) GPPopulation.t =

  let get_fitness =
    match get_fitness with
    | Some(f) -> f
    | None -> calculate_fitness 0 original
  in

  (* prepare the original/base representation for search by modifying the
     search space and registering all available mutations.*)

  if not !disable_reduce_search_space then
    original#reduce_search_space (fun _ -> true) (not (!promut <= 0));

  if not !disable_reduce_fix_space then
    original#reduce_fix_space ();

  original#register_mutations
    [(Delete_mut,!del_prob); (Append_mut,!app_prob);
     (Swap_mut,!swap_prob); (Replace_mut,!rep_prob);
     (Lase_Template_mut,!lase_prob)];
  let pop = ref incoming_pop in
  if (llen incoming_pop) > !popsize then
    pop := first_nth incoming_pop !popsize;

  let remainder = !popsize - (llen incoming_pop) in
  (* include the original in the starting population *)
  if remainder > 0 then pop := (original#copy ()) :: !pop ;

  debug ~force_gui:true
    "search: initial population (sizeof one variant = %g MB)\n"
    (debug_size_in_mb (List.hd !pop));

  (* compute the fitness of the initial population *)
  let _ = GPPopulation.map !pop get_fitness in

  (* initialize the population to a bunch of random mutants *)
  GPPopulation.generate !pop  (fun () ->
      let rep = mutate original in
      let _ = get_fitness rep in
      rep
    ) !popsize

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

(** {b genetic_algorithm_template } is a wrapper for initializing and running a
    genetic algorithm. This includes generating the initial population and
    handling [Maximum_evals] exceptions gracefully. The actual genetic algorithm
    is currently delegated to the [run_ga] callback which takes the initial
    population and returns the final population. *)
let genetic_algorithm_template
    (run_ga : ('a,'b) GPPopulation.t -> ('a,'b) GPPopulation.individual -> ('a,'b) GPPopulation.t)
    (original : ('a,'b) Rep.representation)
    incoming_pop =
  debug "search: genetic algorithm begins (|original| = %g MB)\n"
    (debug_size_in_mb original);
  if !popsize > 0 then begin
    try begin
      let initial_population = initialize_ga original incoming_pop in
      incr gens_run;
      try
        ignore(run_ga initial_population original);
        debug "search: genetic algorithm ends\n" ;
      with Maximum_evals(evals) ->
        debug "reached maximum evals (%d)\n" evals
    end with Maximum_evals(evals) ->
      debug "reached maximum evals (%d) during population initialization\n" evals
  end

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
  assert(!generations >= 0);
  genetic_algorithm_template run_ga original incoming_pop

(** {b steady_state_ga } is parametric with respect to a number of choices,
    similar to {!genetic_algorithm}. Unlike the algorithm implemented in
    [genetic_algorithm], the steady-state algorithm is non-generational,
    continually updating a single population. Since this algorithm does not
    count generations, it will only terminate if the maximum number of test
    case evaluations is reached or a repair is found.

    @param original     original variant
    @param incoming_pop incoming population, possibly empty
    @raise Found_Repair if a repair is found *)
let steady_state_ga (original : ('a,'b) Rep.representation) incoming_pop =
  let write_fitness_log, cleanup =
    if !fitness_log = "" then
      (fun _ _ -> ()), (fun _ -> ())
    else begin
      let best = ref 0.0 in
      let chan = open_out !fitness_log in
      Printf.fprintf chan "peak,best,average,nevals1,fitness1,nevals2,fitness2\n%!";
      let write_fitness_log (pop : ('a,'b) GPPopulation.t) newreps =
        let fitnesses =
          GPPopulation.map pop (fun one -> get_opt (one#fitness()))
        in
        let top, avg, _ =
          lfoldl (fun (top, avg, n) fit ->
              let m = n +. 1.0 in
              best := max !best fit ;
              (max top fit), ( (n *. avg +. fit) /. m ), m
            ) (0.0, 0.0, 0.0) fitnesses
        in
        Printf.fprintf chan "%g,%g,%g" !best top avg;
        liter (fun (nevals, (rep : ('a,'b) Rep.representation)) ->
            Printf.fprintf chan ",%d,%g" nevals (get_opt (rep#fitness()))
          ) newreps;
        Printf.fprintf chan "\n%!"
      in
      write_fitness_log, (fun _ -> close_out chan)
    end
  in
  let evict_two =
    match !eviction_strategy with
    | "random" -> fun pop -> snd (split_nth (random_order pop) 2)
    | "tournament" ->
      let remove_rep rep pop =
        let id = Oo.id rep in
        List.filter (fun r' -> (Oo.id r') <> id) pop
      in
      fun pop ->
        let compare_func a b = GPPopulation.compare_fitness b a in
        let loser = GPPopulation.one_tournament ~compare_func pop in
        let pop = remove_rep loser pop in
        let loser = GPPopulation.one_tournament ~compare_func pop in
        remove_rep loser pop
    | "worst" ->
      fun pop -> snd (split_nth (List.sort GPPopulation.compare_fitness pop) 2)
    | _ -> failwith ("unrecognized eviction strategy: " ^ !eviction_strategy)
  in
  let get_fitness one =
    (Rep.num_test_evals_ignore_cache ()), (calculate_fitness (-1) original one)
  in
  let rec run_ga (pop : ('a,'b) GPPopulation.t) original =
    let parents = GPPopulation.selection pop 2 in
    let children = first_nth (GPPopulation.crossover parents original) 2 in
    let mutated = GPPopulation.map children (fun one -> mutate one) in
    let inserts = GPPopulation.map mutated get_fitness in
    write_fitness_log pop inserts;
    run_ga ((lmap snd inserts) @ (evict_two pop)) original
  in
  genetic_algorithm_template run_ga original incoming_pop ;
  cleanup ()

(** {b gasga } is parametric with respect to a number of choices (e.g.,
    population size, selection method, fitness function, fault localization,
    many of which are set at the command line or at the representation level.
    May exit early if exceptions are thrown in fitness evalution ([Max_Evals])
    or a repair is found [Found_Repair].

    Implements a Greedy Average Sample GA (GASGA):

    Steven Prestwich, S. Armagan Tarim, Roberto Rossi, and Brahim Hnich. 2008.
    A Steady-State Genetic Algorithm with Resampling for Noisy Inventory
    Control. In Parallel Problem Solving from Nature.

    @param original original variant
    @param incoming_pop incoming population, possibly empty
    @raise Found_Repair if a repair is found
    @raise Max_evals if the maximum fitness evaluation count is set and then reached *)
let gasga (original : ('a,'b) Rep.representation) incoming_pop =
  let ejection_fold ((b : ('a,'b) Rep.representation),
                     (w : ('a,'b) Rep.representation),
                     pop)
      (rep : ('a,'b) Rep.representation) =
    let b' =
      if ((rep#num_evals ()) >= !num_fitness_samples)
      || (get_opt (b#fitness())) >= (get_opt (rep#fitness())) then b
      else rep
    in
    if (get_opt (w#fitness())) <= (get_opt (rep#fitness())) then
      b', w, rep::pop
    else
      b', rep, w::pop
  in
  let rec run_ga (pop : ('a,'b) GPPopulation.t) original =
    let parents = GPPopulation.selection pop 2 in
    let child = mutate (List.hd (GPPopulation.crossover parents original)) in
    let _ = calculate_fitness 0 original child in
    let best, worst, pop = lfoldl ejection_fold (child, child, []) pop in
    (* if (best == worst), then we are evicting it out of the population, so
       don't bother reevaluating it *)
    if best != worst then
      ignore (calculate_fitness 0 original best) ;
    run_ga pop original
  in
  genetic_algorithm_template run_ga original incoming_pop

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
(** constructs a representation out of the genome as specified at the command
    line and tests to determine both neutrality and the number of negative tests passed

    @param original individual representation
    @param starting_genome string; either a filename (binary representation) or
    as a string representation of the genome (like the history; this is the more
    likely use-case)
*)
let pd_oracle_search (orig : ('a,'b) Rep.representation) (starting_genome : string) =
  let the_repair = orig#copy () in
  if Sys.file_exists starting_genome then
    the_repair#deserialize starting_genome
  else
    the_repair#load_genome_from_string starting_genome;
  let allowed t = match t with | Positive _ -> true | Negative _ -> false in
  let fneutral = Fitness.test_to_first_failure ~allowed the_repair in
  if fneutral then
    let allowed t = match t with | Positive _ -> false | Negative _ -> true in
    let cpass = Fitness.count_tests_passed allowed the_repair in
    debug "%s was neutral and passed %d negative tests\n" (the_repair#name()) cpass
  else
    debug "%s was not neutral\n" (the_repair#name())

(***********************************************************************)
(** Takes an input file (overloading starting genome because I suck) and creates
    the specified variants in order *)

let sequence (orig : ('a,'b) Rep.representation) (starting_genome : string) =
  List.iter
    (fun genome ->
       debug "genome: %s\n" genome;
       let variant = orig#copy() in
       variant#load_genome_from_string genome;
       if test_to_first_failure variant then
         note_success variant orig (1)
    ) (get_lines starting_genome)



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

(** {5 {L Adaptive Equality: Quotient the space of edits with respect to an
    approximation to program equivalence, and then adaptively explore that
    space based on an on-line model. }}

    Command-line arguments relevant to "Genprog 3.0" adaptive equality:

    --best-edit-rule
    --best-test-rule

    --coverage-per-test

    --ignore-dead-code
    --ignore-standard-headers
    --ignore-equiv-appends
    --ignore-string-equiv-fixes
    --ignore-untyped-returns

    --skip-failed-sanity-tests
*)

(* The model state is used to determine both "which edit to consider next"
 * and "which test to run next (given an edit)". *)
type adaptive_model_1 = {
  mutable failed_repairs_at_this_fault_atom : float AtomMap.t ;
  mutable failed_repairs_at_this_fix_atom : float AtomMap.t ;
  mutable test_pass_count : float TestMap.t ;
  mutable test_fail_count : float TestMap.t ;
  mutable test_cost : float TestMap.t ; (* "test runtime in seconds" *)
}

(**
   Enumerates all one-distance edits, quotients them with respect to program
   equivalence (controlled by command line options), and then repeatedly
   picks the best edit (based on an adaptive model) until a repair is found.

   Only "Delete" and "Append" are considered. (Replace is Delete + Append,
   so we leave that for 2-distance edits.)

    @param original original variant
    @param incoming_pop ignored
*)
let ww_adaptive_1 (original : ('a,'b) Rep.representation) incoming_pop =
  let time = Unix.gettimeofday () in
  debug "search: ww_adaptive_1 begins (time = %f)\n" time ;
  if incoming_pop <> [] then debug "search: incoming population IGNORED\n" ;

  if !excluded_edits_str <> "" then begin
    excluded_edits := (Str.split (Str.regexp "[ \t]+") !excluded_edits_str)
  end ;

  (* Eagerly rule out equivalent edits. This shrinks the set
   * #append_sources will return, etc. *)
  if not !disable_reduce_fix_space then
    original#reduce_fix_space () ;

  let time2 = Unix.gettimeofday () in
  let delta = time2 -. time in
  debug "search: fix space reduced (time_taken = %g)\n" delta ;

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
  let deletes = ref [] in
  deletes :=
    lmap (fun (atom,weight) ->
        (* As an optimization, rather than explicitly generating the
         * entire variant in advance, we generate a "thunk" (or "future",
         * or "promise") to create it later. This is handy because there
         * might be over 10,000 possible variants, and we want to sort
         * them by weight before we actually instantiate them. Recall: not
         * every rep is as efficient as cilpatchrep. *)
        let thunk () =
          let rep = original#copy () in
          rep#delete atom;
          rep
        in
        ((Delete atom),thunk,weight)
      ) fault_localization ;
  let num_pre_deletes = llen !deletes in
  if(List.length !excluded_edits) > 0 then begin
    deletes := List.filter
        (fun (Delete(src),_,_) ->
           let app_str = Printf.sprintf "d(%d)" src in
           not (List.mem app_str !excluded_edits))
        !deletes
  end ;
  debug "excluded %d deletes (from list of %d total edits)\n" (num_pre_deletes-(llen !deletes)) (llen !excluded_edits) ;
  debug "search: ww_adaptive: %d deletes\n" (llen !deletes) ;
  let deletes = !deletes in

  (* Second, try all single appends.
     The size of this list is often sufficiently large (30k+) that we use
     hideous imperative construction rather than pretty functional
     construction.
  *)
  let appends = ref [] in
  List.iter (fun (dest,w1) ->
      let appsrc = Stats2.time "append_sources" original#append_sources dest in
      WeightSet.iter (fun (src,w2) ->
          let thunk () =
            let rep = original#copy () in
            rep#append dest src;
            rep
          in
          let this_append = (Append(dest,src)),thunk, w1 in
          appends := this_append :: !appends
        ) appsrc ;
    ) fault_localization ;
  let num_pre_appends = llen !appends in
  if(List.length !excluded_edits) > 0 then begin
    appends := List.filter
        (fun (Append(dest,src),_,_) ->
           let app_str = Printf.sprintf "a(%d,%d)" dest src in
           not (List.mem app_str !excluded_edits))
        !appends
  end ;
  debug "excluded %d appends (from list of %d total edits)\n" (num_pre_appends-(llen !appends)) (llen !excluded_edits) ;
  let appends = List.sort (fun (_,_,a) (_,_,b) ->
      int_of_float (b *. 100.0 -. a *. 100.0)) !appends in
  debug "search: ww_adaptive: %d appends\n" (llen appends) ;
  let all_edits = deletes @ appends in
  let num_all_edits = llen all_edits in
  debug "search: ww_adaptive: %d possible edits\n" (num_all_edits) ;
  assert(num_all_edits > 0);

  (* The model starts out empty. We may have a priori information, however
   * (e.g., the original program passes some test cases and fails some
   * others). *)
  let model = {
    failed_repairs_at_this_fault_atom = AtomMap.empty ;
    failed_repairs_at_this_fix_atom = AtomMap.empty ;
    test_pass_count = TestMap.empty ;
    test_fail_count = TestMap.empty ;
    test_cost = TestMap.empty ;
  }
  in
  (* The original variant passes the positives and fails the negative
   * (and we're 'sure' because of the sanity check), so that information
   * provides our initial model. *)
  for i = 1 to !pos_tests do
    model.test_pass_count <-
      TestMap.add (Positive i) 1.0 model.test_pass_count
  done ;
  for i = 1 to !neg_tests do
    model.test_fail_count <-
      TestMap.add (Negative i) 1.0 model.test_fail_count
  done ;

  (* Our adaptive search repeatedly calls "find best" (or "find max") to
   * pick the next edit or test to try. The strategy can be specified on
   * the command line and is interpreted as a simple mathematical formula.
   * Currently, the syntax is:
   * NUM * TERM ... NUM * TERM ; NUM * TERM ... ; ...
   *
   * "1 A 2 B" means sort by "compute 1*A + 2*B", taking the max.
   * ";" means "in case of a tie, break ties by the next term".
  *)
  let best_edit_rules = Str.split space_regexp !best_edit_rule in

  (* Utility functions for evaluating strategies. *)
  let fault_atom_of e = match e with
    | Replace(dst,_)
    | Append(dst,_)
    | Delete(dst) -> dst
    | _ -> failwith "ww_adaptive: cannot compute fault loc of atom"
  in
  let fix_atom_of e = match e with
    | Replace(_,src)
    | Append(_,src) -> src
    | _ -> fault_atom_of e
  in
  let tests_of_e e =
    let atom = fault_atom_of e in
    let atomset = AtomSet.singleton atom in
    let tests = original#tests_visiting_atoms atomset in
    tests
  in

  let get_edit_attr (e,t,w) attr =
    match attr with
    | "fault_loc_weight" -> w
    | "fix_loc_weight" ->
      let src = fix_atom_of e in
      let src_w =
        try snd (List.find (fun (a,b) -> a = src) fix_localization)
        with _ -> 0.0 in
      src_w

    | "failed_repairs_at_this_fault_atom" ->
      let dst = fault_atom_of e in
      (try AtomMap.find dst model.failed_repairs_at_this_fault_atom
       with _ -> 0.0)

    | "failed_repairs_at_this_fix_atom" ->
      let atom = fix_atom_of e in
      (try AtomMap.find atom model.failed_repairs_at_this_fix_atom
       with _ -> 0.0)

    | "num_tests" ->
      float_of_int (TestSet.cardinal (tests_of_e e))

    | "total_test_pass_count" ->
      let s1 = tests_of_e e in
      TestSet.fold (fun test acc ->
          acc +. (try TestMap.find test model.test_pass_count
                  with _ -> 0.0)
        ) s1 0.0

    | "max_test_fail_prob" ->
      let s1 = tests_of_e e in
      TestSet.fold (fun test acc ->
          max acc (try
                     let np = try TestMap.find test model.test_pass_count with _ -> 0.0 in
                     let nf = try TestMap.find test model.test_fail_count with _ -> 0.0 in
                     if np +. nf = 0. then 0.
                     else nf /. (np +. nf)
                   with _ -> 0.0)
        ) s1 0.0

    | "total_test_fail_count" ->
      let s1 = tests_of_e e in
      TestSet.fold (fun test acc ->
          acc +. (try TestMap.find test model.test_fail_count
                  with _ -> 0.0)
        ) s1 0.0

    | x ->
      debug "search: ERROR: unknown edit attribute %s" x ;
      failwith "get_edit_attr"
  in

  (* Given a way of obtaining attribute values from a model, a strategy
   * rule, and two possibilities (e.g., edits to consider), return true if
   * edit1 is better than edit2 according to the strategy rule (which is
   * mathematical in terms of the model variables). *)
  let is_better get_attr rules edit1 edit2 =
    let score1 = ref 0.0 in
    let score2 = ref 0.0 in
    let rec interpret rules = match rules with
      (* We 'should' be using a real parser here, but WRW was too cheap. *)
      | weight :: "*" :: attribute :: rest ->
        let weight = my_float_of_string weight in
        score1 := !score1 +. (weight *. (get_attr edit1 attribute)) ;
        score2 := !score2 +. (weight *. (get_attr edit2 attribute)) ;
        interpret rest
      | ";" :: rest -> (* 'rest' are only used to break ties! *)
        if !score1 = !score2 then begin
          score1 := 0.0 ;
          score2 := 0.0 ;
          interpret rest
        end else !score1 > !score2
      | [] -> !score1 > !score2
      | x :: rest ->
        debug "search: ERROR: unknown command %S\n" x ;
        failwith "is_better"
    in
    interpret rules
  in

  (* Given 'is_better' as above and a list of possibilities, we make a
   * linear scan down the list and return the best option. *)
  let find_best get_attr rules remaining =
    match remaining with
    | [] -> failwith "find_best called on empty list"
    | hd :: tl ->
      let best = ref hd in
      List.iter (fun new_edit ->
          if is_better get_attr rules new_edit !best then best := new_edit
        ) tl ;
      !best
  in
  let find_best_edit = find_best get_edit_attr best_edit_rules in

  let find_k_best_unsupered_edits k remaining =
    (* places worst element in first position with List.sort *)
    let my_compare a b =
      if is_better get_edit_attr best_edit_rules a b
      then 1 else if is_better get_edit_attr best_edit_rules b a then -1
      else 0 in
    let rec walk best_sofar remaining =
      match best_sofar, remaining with
      | [], _ -> failwith "find_k_best_unsupered_edits"
      | _, [] -> best_sofar
      | (worst_of_best :: rest_of_best) ,
        (first_of_next :: rest_of_next) ->
        if is_better get_edit_attr best_edit_rules
            first_of_next worst_of_best then begin
          let new_best = List.sort my_compare
              (first_of_next :: rest_of_best) in
          walk new_best rest_of_next
        end else begin
          walk best_sofar rest_of_next
        end
    in
    let first_k, rest = split_nth remaining k in
    let first_k = List.sort my_compare first_k in
    let best_k = walk first_k rest in
    best_k
  in


  let variants_explored_sofar = ref 0 in

  let rec search_edits remaining =
    if remaining = [] then begin
      debug "search: ww_adaptive: ends (no repair)\n" ;
      ()
    end else begin
      (* pick the best edit, based on the model *)
      debug "search: ww_adaptive: finding best\n" ;
      let t1 = Unix.gettimeofday () in
      let edit, thunk, weight = Stats2.time "find_best_edit"
          find_best_edit remaining in
      let t2 = Unix.gettimeofday () in
      debug "search: ww_adaptive: found best (time_taken = %g)\n" (t2 -. t1) ;
      let variant = thunk () in
      let test_set = tests_of_e edit in
      (* If we're using --coverage-per-test, our 'impact analysis' may
       * determine that only some tests are relevant to this edit.
       * Otherwise, all tests are relevant. *)
      incr variants_explored_sofar ;
      debug "\tvariant %5d/%5d = %-15s (%d tests)\n"
        !variants_explored_sofar num_all_edits (variant#name  ())
        (TestSet.cardinal test_set) ;
      assert(not (TestSet.is_empty test_set));
      let cf_before = !compile_failures in
      let success =
        test_to_first_failure ~allowed:(fun t -> TestSet.mem t test_set) variant
      in
      if success then
        begin
          debug "search: ww_adaptive: ends (yes repair)\n" ;
          note_success variant original !variants_explored_sofar ;
          raise (Found_repair(variant#name()))
        end;

      let cf_after = !compile_failures in
      let failed_to_compile = cf_after <> cf_before in
      variant#cleanup () ;

      (* update the model *)
      let fault_atom = fault_atom_of edit in
      let fix_atom = fix_atom_of edit in
      model.failed_repairs_at_this_fault_atom <- AtomMap.add fault_atom
          (1.0 +. try AtomMap.find fault_atom model.failed_repairs_at_this_fault_atom with _ -> 0.)
          model.failed_repairs_at_this_fault_atom ;
      model.failed_repairs_at_this_fix_atom <- AtomMap.add fix_atom
          (1.0 +. try AtomMap.find fix_atom model.failed_repairs_at_this_fix_atom with _ -> 0.)
          model.failed_repairs_at_this_fix_atom ;

      (* recursive call: try the next edit *)
      let remaining = List.filter (fun (e',t',w') -> edit <> e') remaining in
      search_edits remaining
    end
  in
  let time3 = Unix.gettimeofday () in
  let delta = time3 -. time2 in
  debug "search: ready to start (time_taken = %g)\n" delta ;
  search_edits all_edits

(**
*)
let geometric (original : ('a,'b) Rep.representation) incoming_pop =
  if not !disable_reduce_fix_space then begin
    debug "search: reduce_fix_space\n";
    original#reduce_fix_space () ;
  end;

  debug "search: geometric search begins\n";
  original#register_mutations [
    (Delete_mut,!del_prob);
    (Append_mut,!app_prob);
    (Swap_mut,!swap_prob);
    (Replace_mut,!rep_prob);
    (Lase_Template_mut,!lase_prob);
  ];

  let rec coin_flip rep =
    if probability !geomp then mutate rep else coin_flip (mutate rep)
  in
  let rec eval_variant pop =
    let rep = coin_flip (List.hd (random_order pop)) in
    let _ = calculate_fitness (-1) original rep in
    eval_variant pop
  in
  eval_variant (if incoming_pop = [] then [original] else incoming_pop)

(**
   Basic proactive diversity search. Generate a number of variants that pass
   all of the positive test cases. Report those that also pass any negative
   tests.

   Generates a few neutral variants and then attempts to combine them in
   many different ways. Hence "exploit".

    @param original original variant
    @param incoming_pop ignored
*)
let pd_exploit (original : ('a,'b) Rep.representation) incoming_pop =
  if not !disable_reduce_fix_space then begin
    debug "search: reduce_fix_space\n";
    original#reduce_fix_space () ;
  end;
  debug "search: proactive diversity search begins\n";
  original#register_mutations [
    (Delete_mut,!del_prob);
    (Append_mut,!app_prob);
    (Swap_mut,!swap_prob);
    (Replace_mut,!rep_prob);
    (Lase_Template_mut,!lase_prob);
  ];

  (* Maintain a mapping from variant names to variants -- but only for
   * neutral variants. At the start, the original is known to be neutral. *)
  let neutrals = ref (StringMap.add (original#name()) original StringMap.empty) in
  let random_neutral () =
    let all = lrev (StringMap.fold (fun k v lst -> (k,v)::lst) !neutrals []) in
    List.hd (random_order all)
  in

  (* Consider a variant to see if it is neutral. *)
  let determine_neutrality v =
    if StringMap.mem (v#name ()) !neutrals then
      () (* seen it before *)
    else begin
      (* try positive tests, in model order, until one fails *)
      let allowed t = match t with
        | Positive _ -> true
        | Negative _ -> false
      in
      let is_neutral = Fitness.test_to_first_failure ~allowed v in
      if is_neutral then begin
        debug "\t+ %s is neutral\n" (v#name ()) ;
        neutrals := StringMap.add (v#name ()) v !neutrals
      end else begin
        debug "\t- %s\n" (v#name ())
      end
    end
  in

  for i = 1 to !popsize do
    debug "pd_exploit: probe %d/%d\n" i !popsize ;
    let new_variants =
      if probability 0.5 then begin
        (* mutate *)
        let _, neutral_variant = random_neutral () in
        (* let neutral_variant = original in  *)
        let result = mutate neutral_variant in
        [ result ]
      end else begin
        (* crossover *)
        let _, neutral_variant_1 = random_neutral () in
        let _, neutral_variant_2 = random_neutral () in
        let children = GPPopulation.do_cross original
            neutral_variant_1 neutral_variant_2 in
        let combination = original#copy () in
        combination#set_genome (
          (neutral_variant_1#get_genome()) @
          (neutral_variant_2#get_genome()) ) ;
        combination :: children
      end
    in
    List.iter determine_neutrality new_variants
  done ;

  debug "pd_exploit: testing %d neutral variants on negative tests\n"
    (StringMap.fold (fun _ _ n -> n+1) !neutrals 0) ;

  let negative_fixed = Hashtbl.create 255 in
  StringMap.iter (fun name rep ->
      for j = 1 to !neg_tests do
        let res, real_value = rep#test_case (Negative j) in
        if res then begin
          debug "\tn%d passed by %s\n" j name ;
          Hashtbl.add negative_fixed name j
        end
      done
    ) !neutrals ;
  let bindings =
    lrev (StringMap.fold (fun k v lst -> (k,v)::lst) !neutrals [])
  in
  let sorted = List.sort (fun (n1,v1) (n2,v2) ->
      let l1 = List.length (v1#get_genome ()) in
      let l2 = List.length (v2#get_genome ()) in
      compare l1 l2
    ) bindings in
  debug "\n\nNeutral Variants\n\n" ;
  debug "negative_passed_count,negative_passed,genome_size,genome\n" ;
  List.iter (fun (name,rep) ->
      let negative_passed_list = Hashtbl.find_all negative_fixed name in
      let negative_passed_count = List.length negative_passed_list in
      let negative_passed_string = List.fold_left (fun acc elt ->
          Printf.sprintf "%d %s" elt acc
        ) "" negative_passed_list in
      let genome_count = List.length (rep#get_genome ()) in
      debug "%d,%S,%d,%S\n"
        negative_passed_count negative_passed_string
        genome_count (rep#name ()) ;
    ) sorted ;
  ()

(**
   Basic proactive diversity search. Generate a number of variants that pass
   all of the positive test cases. Report those that also pass any negative
   tests. Modified version of Wes' pd search that uses an algorithm more like
   what Martin and Jamie have been doing in the past

   This search is focused on generating new variants; it never does
   "crossover" in any form or takes advantage of any knowledge it
   has about existing neutral variants. Hence "explore".

    @param original original variant
    @param incoming_pop ignored
*)
let pd_explore (original : ('a,'b) Rep.representation) incoming_pop =
  if not !disable_reduce_fix_space then begin
    debug "search: reduce_fix_space\n";
    original#reduce_fix_space () ;
  end;
  debug "search: proactive diversity search begins\n";
  original#register_mutations [
    (Delete_mut,!del_prob);
    (Append_mut,!app_prob);
    (Swap_mut,!swap_prob);
    (Replace_mut,!rep_prob);
    (Lase_Template_mut,!lase_prob);
  ];

  let binomial_mutate original =
    if !promut <> 1 then failwith "promut should be 1 for pd";
    let promutOld = !promut in
    let fContinue = ref true in
    decr promut;
    while !fContinue do
      incr promut;
      fContinue := if (Random.float 1.0) < !pd_mutp then true else false
    done;
    let var = mutate original in
    promut := promutOld;
    var
  in

  let cneutral = ref 0 in
  let cneg = ref 0 in

  (* Consider a variant to see if it is neutral. *)
  let determine_neutrality var cneutral cneg =
    begin
      (* run only the positive tests to first failure to determine neutrality *)
      let allowed t = match t with
        | Positive _ -> true
        | Negative _ -> false
      in
      let fNeutral = Fitness.test_to_first_failure ~allowed var in
      if fNeutral then begin
        debug "\t+ %s is neutral\n" (var#name ()) ;
        incr cneutral;
        (* determine if it passes any negative tests here *)
        let neg_only t = match t with | Positive _ -> false | Negative _ -> true in
        let cpass = Fitness.count_tests_passed neg_only var in
        debug "\t %s passed %d negative tests\n" (var#name()) cpass ;
        if cpass > 0 then incr cneg
      end else begin
        debug "\t- %s\n" (var#name ())
      end
    end
  in

  for i = 1 to !popsize do
    debug "pd_explore: probe %d/%d\n" i !popsize ;
    let varBase = original#copy() in
    let var = binomial_mutate varBase in
    determine_neutrality var cneutral cneg
  done ;
  debug "pd_explore: There were %d neutral mutants\n" !cneutral ;
  debug "pd_explore: %d of those passed a negative test\n" !cneg
