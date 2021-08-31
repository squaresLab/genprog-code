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
(** Population -- implements operations over sets of individuals in a
    search space.  Populations may be serialized, deserialized, selected,
    reduced, or added to, and individuals in a population can be crossed over.
    Right now, populations are implemented as lists of representations, but if
    we add a different kind of search with a different kind of population, this
    module will be very easy to abstract *)
open Global
open Rep

let crossp = ref 0.5
let popsize = ref 40
let incoming_pop = ref ""
let tournament_k = ref 2
let crossover = ref "one"
let output_format = ref "txt"
(* there doesn't appear to be a mechanism for specifying the probability of
   selection, but if there were to be such an option, this is the flag it would
   set *)
let tournament_p = ref 1.00

let _ =
  options := !options @ [
      "--popsize", Arg.Set_int popsize, "X variant population size";

      "--crossover", Arg.Set_string crossover,
      "X use X as crossover [one,back,subset,flat]";

      "--crossp", Arg.Set_float crossp, "X use X as crossover rate";

      "--format", Arg.Set_string output_format,
      "X format for serialized population.  Options: bin/binary, txt.  Default: txt";

      "--tournament-size", Arg.Set_int tournament_k,
      "X use x as tournament size";
    ]

let population_version = "1"

module GPPopulation =
struct

  type ('a,'b) individual = ('a,'b) Rep.representation
  type ('a,'b) t = ('a,'b) individual list

  let sanity variable_length =
    match !crossover with
    | "flat" | "flatten"
    | "one" | "patch-one-point" | "back" -> true
    | "patch" | "subset"
    | "uniform" | "patch-old" -> variable_length
    | x -> abort  "unknown --crossover %s\n" x

  (** {b generate} generates a population.  Generate_function generates a new
      variant.  incoming is the incoming population.  Size is the desired
      population size *)
  let rec generate incoming generate_function size =
    if (llen incoming) < size then begin
      let individual = generate_function () in
      generate (individual :: incoming) generate_function size
    end else incoming

  (** map population map_function applies map_function to every individual on
      the population and returns the result *)
  let map population map_function = lmap map_function population
  let iterate population iterate_function = liter iterate_function population

  (** {b serialize} serializes a population to disk.  The first variant is
      optionally instructed to print out the global information necessary for a
      collection of representations.  The remaining variants print out only
      their variant-specific local information *)
  let serialize ?out_channel (population : ('a,'b) t) (filename : string) =
    match !output_format with
      "bin" | "binary" ->
      let fout =
        match out_channel with
          Some(v) -> v
        | None -> open_out_bin filename
      in
      Marshal.to_channel fout (population_version) [] ;
      liter (fun variant -> variant#serialize ?out_channel:(Some(fout)) ?global_info:(Some(false)) filename) population;
      if out_channel = None then close_out fout
    | "txt" ->
      debug "serializing population to txt; ?out_channel ignored\n";
      let fout = open_out filename in
      liter (fun variant ->
          let name = variant#name () in
          output_string fout (name^"\n"))
        population;
      if out_channel = None then close_out fout
    | _ -> failwith ("unknown population output format: " ^ !output_format)

  (** {b deserialize} deserializes a population from disk, to be used as
      incoming_pop.  The incoming variant is assumed to have loaded the global
      state (which CLG doesn't love so she might change it).  Remaining variants
      are read in individually, using only their own local information *)
  (* deserialize can fail if the file does not conform to the expected format
     for Marshal or if there is a version mismatch between the population module
     that wrote the binary file and this one (that is loading it). *)
  let deserialize ?in_channel filename original =
    (* the original should have loaded the global state *)
    let fin =
      match in_channel with
        Some(v) -> v
      | None -> open_in_bin filename in
    let pop = ref [] in
    try
      if !output_format = "txt" then
        failwith "txt format, skipping binary attempt";
      let version = Marshal.from_channel fin in
      if version <> population_version then begin
        debug "population: %s has old version: %s\n" filename version;
        failwith "version mismatch"
      end ;
      let attempt = ref 1 in
      try
        while true do
          debug "attempt %d\n" !attempt; incr attempt;
          let rep' = original#copy () in
          rep'#deserialize ?in_channel:(Some(fin)) ?global_info:(None) filename;
          pop := rep'::!pop
        done; !pop
      with End_of_file -> !pop
    with _ -> begin
        close_in fin;
        pop := [];
        try
          let individuals = get_lines filename in
          liter
            (fun genome ->
               let copy = original#copy() in
               copy#load_genome_from_string genome;
               pop := copy :: !pop
            ) individuals; !pop
        with End_of_file -> !pop
      end

  (*** Tournament Selection ***)

  (** [compare_fitness x y] compares the fitness of the given individuals. It is
      used as the default comparison function for tournament selection. *)
  let compare_fitness (i : ('a,'b) individual) (i' : ('a,'b) individual) =
    compare (get_opt (i#fitness ())) (get_opt (i'#fitness ()))

  (** [one_tournament compare_func population] conducts a single tournament to
      select a variant from the population. The [compare_func] should take two
      inviduals and return a positive number if the first is preferred, a
      negative number if the second is preferred, and 0 if neither is preferred.
  *)
  let one_tournament ?(compare_func=compare_fitness) (population : ('a,'b) t) =
    assert ( !tournament_k >= 1 ) ;
    assert ( 0.0 <= !tournament_p ) ;
    assert ( !tournament_p <= 1.0 ) ;
    assert ( List.length population > 0 ) ;

    let rec select_one () =
      (* choose k individuals at random *)
      let pool = first_nth (random_order population) !tournament_k in
      (* sort them from most fit to least *)
      let sorted = lrev (List.sort compare_func pool) in
      (* select one with geometrically decreasing probability *)
      let rec walk p = function
        | [] -> select_one ()
        | indiv :: rest ->
          let taken = (1.0 = p) || (Random.float 1.0 <= p) in
          if taken then indiv else walk (p *. (1.0 -. !tournament_p)) rest
      in
      walk !tournament_p sorted
    in
    select_one ()

  (** {b tournament_selection} variant_comparison_function population
      desired_pop_size uses tournament selction to select desired_pop_size
      variants from population using variant_comparison_function to compare
      individuals, if specified, and variant fitness if not.  Returns a subset
      of the population.  *)
  let tournament_selection
      ?(compare_func=compare_fitness)
      (population : ('a,'b) t)
      desired =
    assert ( desired >= 0 ) ;
    lmap (fun _ -> one_tournament ~compare_func population) (1 -- desired)

  (** {b Selection} population desired_size dispatches to the appropriate
      selection function. Currently we have only tournament selection implemented,
      but if/we we add others we can choose between them here *)
  let selection ?(compare_func=compare_fitness) population desired =
    tournament_selection ~compare_func population desired

  (** Crossover is an operation on more than one variant, which is why it
      appears here.  We currently have one-point crossover implemented on
      variants of both stable and variable length, patch_subset_crossover, which
      is something like uniform crossover (but which works on all
      representations now, not just cilRep patch) and "ast_old_behavior", which
      Claire hasn't fixed yet.  The nitty-gritty of how to combine
      representation genomes to accomplish crossover has been mostly moved to
      the representation classes, so this implementation doesn't know much about
      particular genomes.  Crossback implements one-point between variants and
      the original. *)
  (* this implements the old AST/WP crossover behavior, typically intended to be
     used on the patch representation.  I don't like keeping it around, since
     the point of refactoring is to decouple the evolutionary behavior from the
     representation.  I'm still thinking about it *)
  (* this can fail if the edit histories contain unexpected elements, such as
     crossover, or if load_genome_from_string fails (which is likely, since it's
     not implemented across the board, which is why I'm mentioning it in this
     comment) *)
  let crossover_patch_old_behavior ?(test = 0)
      (original :('a,'b) Rep.representation)
      (variant1 :('a,'b) Rep.representation)
      (variant2 :('a,'b) Rep.representation)
    : (('a,'b) representation) list =
    let h1 = variant1#get_history () in
    let h2 = variant2#get_history () in
    let wp = lmap fst (variant1#get_faulty_atoms ()) in
    let point = if test=0 then Random.int (llen wp) else test in
    let first_half,second_half = split_nth wp point in
    let c_one = original#copy () in
    let c_two = original#copy () in
    let h11, h12 =
      List.partition
        (fun edit ->
           match edit with
           | Delete(num)
           | Append(num, _)
           | Swap(num,_)
           | Replace(num,_) -> List.mem num first_half
           | _ ->
             abort "unexpected edit in history in patch_old_behavior crossover")
        h1
    in
    let h21, h22 =
      List.partition
        (fun edit ->
           match edit with
           | Delete(num) | Append(num, _)
           | Swap(num,_) | Replace(num,_)  ->
             List.mem num first_half
           | _ ->
             abort "unexpected edit in history in patch_old_behavior crossover")
        h2
    in
    let new_h1 = lmap (c_one#history_element_to_str) (h11 @ h22) in
    let new_h2 = lmap (c_two#history_element_to_str) (h21 @ h12) in
    let new_h1 = lfoldl (fun acc str -> acc^str^" ") "" new_h1 in
    let new_h2 = lfoldl (fun acc str -> acc^str^" ") "" new_h2 in
    c_one#load_genome_from_string new_h1 ;
    c_two#load_genome_from_string new_h2 ;
    [ c_one ; c_two ]

  (* Patch Subset Crossover; works on all representations even though it was
     originally designed just for cilrep patch *)
  let crossover_patch_subset
      (original :('a,'b) Rep.representation)
      (variant1 :('a,'b) Rep.representation)
      (variant2 :('a,'b) Rep.representation)
    : (('a,'b) representation) list =
    let g1 = variant1#get_genome () in
    let g2 = variant2#get_genome () in
    let new_g1 = List.fold_left (fun acc elt ->
        if probability !crossp then acc @ [elt] else acc
      ) [] (g1 @ g2) in
    let new_g2 = List.fold_left (fun acc elt ->
        if probability !crossp then acc @ [elt] else acc
      ) [] (g2 @ g1) in
    let c_one = original#copy () in
    let c_two = original#copy () in
    c_one#set_genome new_g1 ;
    c_two#set_genome new_g2 ;
    [ c_one ; c_two ]

  (* One point crossover *)
  let crossover_one_point ?(test = 0)
      (original :('a,'b) Rep.representation)
      (variant1 :('a,'b) Rep.representation)
      (variant2 :('a,'b) Rep.representation)
    : (('a,'b) representation) list =
    let child1 = original#copy () in
    let child2 = original#copy () in
    let point1,point2 =
      if test <> 0 then test,test
      else
        (* this is a little squirrly.  available_crossover_points returns a
           set of legal crossover points along the genome list and a function
           to combine the variant's legal crossover points with another
           variant's legal crossover points *)
        let legal1,interfun1 = variant1#available_crossover_points () in
        let legal2,interfun2 = variant2#available_crossover_points () in
        let legal1' = interfun1 legal1 legal2 in
        let legal2' = interfun2 legal2 legal1 in
        (* if variants are of stable length, we only need to choose one
           point *)
        if not variant1#variable_length then
          let rand = List.hd (random_order legal1') in
          rand,rand
        else
          let rand1 = List.hd (random_order legal1') in
          let rand2 = List.hd (random_order legal2') in
          rand1,rand2
    in
    let g1a,g1b = split_nth (variant1#get_genome()) point1 in
    let g2a,g2b = split_nth (variant2#get_genome()) point2 in
    child1#set_genome (g1a@g2b);
    (* do we care that the history info is destroyed for patch representation
       here? *)
    child2#set_genome (g2a@g1b);
    [child1;child2]

  (** do_cross original variant1 variant2 performs crossover on variant1 and
      variant2, producing two children [child1;child2] as a result.  Dispatches
      to the appropriate crossover function based on command-line options *)
  (* do_cross can fail if given an unexpected crossover option from the command
     line *)
  let do_cross ?(test = 0)
      (original :('a,'b) Rep.representation)
      (variant1 :('a,'b) Rep.representation)
      (variant2 :('a,'b) Rep.representation)
    : (('a,'b) representation) list =
    match !crossover with
    (* CLG: flat crossover is now implemented by default on elfrep based on
       available_crossover_points *)
    | "flat" | "flatten"
    | "one" | "patch-one-point" ->
      crossover_one_point ~test original variant1 variant2
    | "back" -> crossover_one_point ~test original variant1 original
    | "patch" | "subset"
    | "uniform" -> crossover_patch_subset original variant1 variant2
    | "patch-old" ->
      crossover_patch_old_behavior ~test original variant1 variant2
    | x -> abort "unknown --crossover %s\n" x

  (** crossover population original_variant performs crossover over the entire
      population, returning a new population with both the old and the new
      variants *)
  let crossover population original =
    let mating_list = random_order population in
    (* should we cross an individual? *)
    let maybe_cross () = Random.float 1.0 <= !crossp in
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

end
