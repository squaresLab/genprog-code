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
    }

 *)

open Printf
open Global
open Fitness
open Rep
open Population

(**/**)
let generations = ref 10
let promut = ref 1
let continue = ref false
let gens_run = ref 0

let app_prob = ref 0.33333
let del_prob = ref 0.33333
let swap_prob = ref 0.33333
let rep_prob = ref 0.0

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

    "--generations", Arg.Set_int generations, 
    "X conduct X iterations of the given search strategy. Default: 10.";

    "--promut", Arg.Set_int promut, "X make X mutations per 'mutate' call";

    "--continue", Arg.Set continue, 
    " Continue search after repair has been found.  Default: false";
  ]

(**/**)

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
    let name = rep#name () in 
      debug "\nRepair Found: %s\n" name ;
      let filename = "repair"^ !Global.extension in
        rep#output_source filename ;
        rep#note_success ();
        if not !continue then raise (Found_repair(name))

(**** Brute Force: Try All Single Edits ****)

(** tries all single-atom delete, append, and swap edits on a given input
    representation (original).  The search is biased by the fault and fix
    weights in the original variant. Deletions are favored over appends and
    swaps, appends are favored over swaps.  is ignored.  

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
    
  let worklist = deletes @ appends @ swaps in
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
            end
        in
          atom_mutate ()           
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
    (assuming the [Fitness] module behaved as it should) *)
let calculate_fitness generation orig variant =
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
  let pop = ref incoming_pop in
    if (llen incoming_pop) > !popsize then
      pop := first_nth incoming_pop !popsize; 

    let remainder = !popsize - (llen incoming_pop) in
      (* include the original in the starting population *)
      if remainder > 0 then pop := (original#copy ()) :: !pop ;

      (* initialize the population to a bunch of random mutants *)
      pop :=
        GPPopulation.generate !pop  (fun () -> mutate original) !popsize;
      debug
        "search: initial population (sizeof one variant = %g MB)\n"
        (debug_size_in_mb (List.hd !pop));
      (* compute the fitness of the initial population *)
      GPPopulation.map !pop (calculate_fitness 0 original)

(** {b genetic_algorithm } is parametric with respect to a number of choices
    (e.g., population size, selection method, fitness function, fault localization,
    many of which are set at the command line or at the representation level.
    May exit early if exceptions are thrown in fitness evalution or a repair is found [Found_Repair]. 

    @param original original variant
    @param incoming_pop incoming population, possibly empty
    @raise Found_Repair if a repair is found *)
let genetic_algorithm (original : ('a,'b) Rep.representation) incoming_pop =
  debug "search: genetic algorithm begins (|original| = %g MB)\n"
    (debug_size_in_mb original);
  assert(!generations >= 0);
  if !popsize > 0 then begin
    let initial_population = initialize_ga original incoming_pop in
      incr gens_run;
  (* the bulk of run_ga is performed by the recursive inner helper
     function, which Claire modeled off the MatLab code sent to her by the
     UNM team *)

  let rec iterate_generations gen incoming_population =
    if gen < !generations then begin
      debug "search: generation %d (sizeof one variant = %g MB)\n" 
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
    ignore(iterate_generations 1 initial_population);
      debug "search: genetic algorithm ends\n" 
end

(***********************************************************************)
(** constructs a representation out of the genome as specified at the command
    line and tests to first failure.  This assumes that the oracle genome
    corresponds to a maximally fit variant.

    @param original individual representation
    @param starting_genome string representation of the genome (like the
    history; this is the more likely use-case)
*)
let oracle_search (orig : ('a,'b) Rep.representation) (starting_genome : string) = 
  let the_repair = orig#copy () in
    the_repair#load_genome_from_string starting_genome;
    assert(test_to_first_failure the_repair);
    note_success the_repair orig (1)
