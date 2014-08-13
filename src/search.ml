(*
 *
 * Copyright (c) 2012-2013, 
 *  Wes Weimer          <weimer@cs.virginia.edu>
 *  Stephanie Forrest   <forrest@cs.unm.edu>
 *  Claire Le Goues     <legoues@cs.virginia.edu>
 *  Eric Schulte        <eschulte@cs.unm.edu>
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

let excluded_edits_str = ref ""
let excluded_edits = ref []

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

    "--best-edit-rule", Arg.Set_string best_edit_rule,
    "X use X to rank possible edits in adaptive search" ; 

    "--exclude-edits", Arg.Set_string excluded_edits_str,
    "X exclude all edits specified in X when running repair (space-seperated)" ;
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
        let h = rep#get_history () in 
        let h = List.map (fun e -> match e with
          | Conditional(c,e) -> 
            if c = !test_condition then [e] else []
          | _ -> [e] 
        ) h in
        let h = List.flatten h in 
        debug "\nRepair Found:" ;
        List.iter (fun e -> 
          debug " %s" (rep#history_element_to_str e)
        ) h ; 
        let name = rep#name () in 
        debug "\nRepair Name: %s\n" name ;
        debug "Test Condition: %d\n" !test_condition ; 
        debug "Test Cases Skipped: %S\n" !skipped_tests ; 
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

  debug "search: reduce_fix_space\n";
  original#reduce_fix_space () ; 

  debug "search: brute_force_1 begins\n";
  original#register_mutations [
    (Delete_mut,!del_prob);
    (Append_mut,!app_prob);
    (Swap_mut,!swap_prob);
    (Replace_mut,!rep_prob);
  ];

  debug "search: counting available mutants\n";
  let count =
    List.fold_left (fun n (stmt,_) ->
      List.fold_left (fun n (mut,_) ->
        match mut with
        | Delete_mut -> n + 1
        | Append_mut -> WeightSet.fold (fun _ n -> n+1) (original#append_sources stmt) n
        | Swap_mut   -> WeightSet.fold (fun _ n -> n+1) (original#swap_sources stmt) n
        | Replace_mut ->
          WeightSet.fold (fun _ n -> n+1) (original#replace_sources stmt) n
      ) n (original#available_mutations stmt)
    ) 0 (original#get_faulty_atoms ())
  in
  debug "search: %d mutants in search space\n" count;

  let rescale items =
    let scale = 1.0 /. (List.fold_left (fun sum (_,w) -> sum +. w) 0.0 items) in
    List.map (fun (x,w) -> (x, w *. scale)) items
  in

  let wins  = ref 0 in
  let sofar = ref 1 in
  let do_work probs apply_mut =
    let rep = original#copy () in
    apply_mut rep;
    if test_to_first_failure rep then begin
      note_success rep original (-1);
      incr wins;
      if not !continue then
        raise (Found_repair(rep#name ()))
    end;
    debug "\tvariant %d/%d/%d (w: %s) %s\n" !wins !sofar count probs (rep#name ());
    incr sofar
  in

  let atoms = rescale (original#get_faulty_atoms ()) in
  List.iter (fun (stmt,prob1) ->
    let avail = rescale (original#available_mutations stmt) in
    List.iter (fun (mut,prob2) ->
      let s = Printf.sprintf "%g %g" prob1 prob2 in
      match mut with
      | Delete_mut -> do_work s (fun rep -> rep#delete stmt)
      | Append_mut ->
        List.iter (fun (src,prob3) ->
          let s = Printf.sprintf "%s %g" s prob3 in
          do_work s (fun rep -> rep#append stmt src)
        ) (rescale (WeightSet.elements (original#append_sources stmt)))
      | Swap_mut ->
        List.iter (fun (src,prob3) ->
          let s = Printf.sprintf "%s %g" s prob3 in
          do_work s (fun rep -> rep#swap stmt src)
        ) (rescale (WeightSet.elements (original#swap_sources stmt)))
      | Replace_mut ->
        List.iter (fun (src,prob3) ->
          let s = Printf.sprintf "%s %g" s prob3 in
          do_work s (fun rep -> rep#replace stmt src)
        ) (rescale (WeightSet.elements (original#replace_sources stmt)))
    ) avail
  ) atoms;

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
                let fillins,_ = choose_one_weighted templates
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
  assert(!generations >= 0);
  if !popsize > 0 then begin
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

    --super-mutant
    --super-mutant-size 
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

  if (!excluded_edits_str <> "") then begin
    excluded_edits := (Str.split (Str.regexp "[ \t]+") !excluded_edits_str) 
  end ;

  (* Eagerly rule out equivalent edits. This shrinks the set
   * #append_sources will return, etc. *)  
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
      	let app_str = (Printf.sprintf "d(%d)" src) in
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
      let this_append = ((Append(dest,src)),thunk, w1) in 
      appends := this_append :: !appends 
    ) appsrc ; 
  ) fault_localization ;
  let num_pre_appends = llen !appends in 
  if(List.length !excluded_edits) > 0 then begin
    appends := List.filter 
      (fun (Append(dest,src),_,_) -> 
      	let app_str = (Printf.sprintf "a(%d,%d)" dest src) in
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

  let super_mutants = Hashtbl.create 2047 in 
  let find_k_best_unsupered_edits k remaining = 
    let remaining = List.filter (fun (e,t,w) ->
      not (Hashtbl.mem super_mutants e)
    ) remaining in
    (*
    debug "find_k_best_unsupered: k=%d remainign=%d\n"
      k (List.length remaining) ; 
      *) 
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
    let best_k = (walk first_k rest) in
    best_k 
  in 


  let variants_explored_sofar = ref 0 in 

  let edits_in_supers = ref [] in 

  let create_super_mutant remaining = 
    assert(!super_mutant);
    assert(!super_mutant_size > 0); 
    let num = !super_mutant_size in 
    debug "search: ww_adaptive: finding %d best for super-mutant\n" num ;
    let t1 = Unix.gettimeofday () in  
    let batch = 
      Stats2.time "find_k_best_edits" 
      (fun () -> find_k_best_unsupered_edits num remaining) () in 
    let t2 = Unix.gettimeofday () in  
    debug "search: ww_adaptive: found %d best (time_taken = %g)\n" 
      (List.length batch) (t2 -. t1) ; 
    let cond = ref 0 in 
    let variant = original#copy () in 
    List.iter (fun ((e,_,_) as elt) -> 
      incr cond ;
      match e with 
      | Delete x -> 
        variant#conditional_delete !cond x ;
        edits_in_supers := elt :: !edits_in_supers ; 
        Hashtbl.replace super_mutants e (variant, !cond) 

      | Append(x,y) ->
        variant#conditional_append !cond x y ;
        edits_in_supers := elt :: !edits_in_supers ; 
        Hashtbl.replace super_mutants e (variant, !cond) 

      | _ -> failwith "search: super-mutant unsupported edit"
    ) batch ;  
    let undo_thunk () = 
      (* don't remove them from edits_in_supers (or we'll spend too
       * much time in find-best later), but do remove them from the
       * super_mutants hash table *) 
      (*
      edits_in_supers := List.filter (fun (e,_,_) ->
        not (List.exists (fun (e',_,_) -> e = e') batch))
        !edits_in_supers ; 
       *) 
      List.iter (fun (e,_,_) -> 
        let v = original#copy () in 
        match e with 
        | Delete x -> 
          v#delete x ;
          Hashtbl.replace super_mutants e (v,0) 
        | Append(x,y) -> 
          v#append x y ;
          Hashtbl.replace super_mutants e (v,0) 
        | _ -> failwith "search: super-mutant unsupported edit" 
      ) batch 
    in 
    batch, undo_thunk
  in 

  let variant_of remaining (edit, thunk, weight) = 
    if !super_mutant then begin
      if Hashtbl.mem super_mutants edit then begin
        let a, b = Hashtbl.find super_mutants edit in
        a,b, (fun () -> ())
      end else begin 
        debug "search: WARNING: variant_of_remaining: unexpected\n" ; 
        (thunk ()), 0, (fun () -> ()) 
      end 
    end else (thunk ()), 0, (fun () -> ()) 
  in 

  let rec search_edits remaining = 
    if remaining = [] then begin 
      debug "search: ww_adaptive: ends (no repair)\n" ;
      ()
    end else begin 
      (* pick the best edit, based on the model *) 
      debug "search: ww_adaptive: finding best\n" ;
      let choose_from, undo_thunk = 
        if !edits_in_supers <> [] then begin 
          debug "\tfrom existing super-mutants\n" ; 
          !edits_in_supers, (fun () -> ())
        end else if !super_mutant then begin
          debug "\tfrom new super-mutant\n" ; 
          create_super_mutant remaining 
        end else 
          remaining, (fun () -> ())
      in 
      let t1 = Unix.gettimeofday () in 
      let (edit, thunk, weight) = Stats2.time "find_best_edit" 
        find_best_edit choose_from in 
      let t2 = Unix.gettimeofday () in 
      debug "search: ww_adaptive: found best (time_taken = %g)\n" (t2 -. t1) ; 
      let variant, condition, undo_thunk = 
        variant_of remaining (edit, thunk, weight) in 
      let test_set = tests_of_e edit in 
      (* If we're using --coverage-per-test, our 'impact analysis' may
       * determine that only some tests are relevant to this edit.
       * Otherwise, all tests are relevant. *) 
      incr variants_explored_sofar ; 
      debug "\tvariant %5d/%5d = %-15s (%d tests, cond %d)\n" 
        !variants_explored_sofar num_all_edits (variant#name  ())
        (TestSet.cardinal test_set) 
        condition ;
      assert(not (TestSet.is_empty test_set)); 
      Rep.set_condition condition ; 
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

      if failed_to_compile && condition <> 0 then begin
        (* special case: we failed to compile a super-mutant, so we have
         * to retry with the component edits separated *) 
        debug "\t\tWARNING: super-mutant fails to compile\n" ;
        undo_thunk () ; 
        search_edits remaining ; 

      end else begin 

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
        edits_in_supers := List.filter (fun (e',t',w') ->
          edit <> e') !edits_in_supers ; 
        search_edits remaining 
      end 
    end 
  in
  let time3 = Unix.gettimeofday () in 
  let delta = time3 -. time2 in 
  debug "search: ready to start (time_taken = %g)\n" delta ; 
  search_edits all_edits 

