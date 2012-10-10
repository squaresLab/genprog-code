(*
 *
 * Copyright (c) 2012-2013, 
 *  Wes Weimer          <weimer@cs.virginia.edu>
 *  Stephanie Forrest   <forrest@cs.unm.edu>
 *  Claire Le Goues     <legoues@cs.virginia.edu>
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
(* there doesn't appear to be a mechanism for specifying the probability of
   selection, but if there were to be such an option, this is the flag it would
   set *)
let tournament_p = ref 1.00

let _ = 
  options := !options @ [
    "--popsize", Arg.Set_int popsize, "X variant population size";

    "--crossover", Arg.Set_string crossover, 
    "X use X as crossover [one,back,subset]";

    "--crossp", Arg.Set_float crossp, "X use X as crossover rate";

    "--tournament-size", Arg.Set_int tournament_k, 
    "X use x as tournament size";
  ]

let population_version = "1"

module GPPopulation =
struct

  type ('a,'b) individual = ('a,'b) Rep.representation
  type ('a,'b) t = ('a,'b) individual list

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
      debug "serializing population to txt; ?out_channel ignored\n";
      let fout = open_out filename in 
        liter (fun variant -> 
          let name = variant#name () in
            output_string fout (name^"\n"))
          population;
        if out_channel = None then close_out fout

  (** {b deserialize} deserializes a population from disk, to be used as
      incoming_pop.  The incoming variant is assumed to have loaded the global
      state (which CLG doesn't love so she might change it).  Remaining variants
      are read in individually, using only their own local information *)
  (* deserialize can fail if the file does not conform to the expected format
     for Marshal or if there is a version mismatch between the population module
     that wrote the binary file and this one (that is loading it). *)
  let deserialize ?in_channel filename original = 
    (* the original should have loaded the global state *)
    let pop = ref [original] in
      try
        let individuals = get_lines filename in 
          liter
            (fun genome ->
              let copy = original#copy() in
                copy#load_genome_from_string genome;
                pop := copy :: !pop
            ) individuals; !pop
      with End_of_file -> !pop

  (*** Tournament Selection ***)

  (** {b tournament_selection} variant_comparison_function population
      desired_pop_size uses tournament selction to select desired_pop_size
      variants from population using variant_comparison_function to compare
      individuals, if specified, and variant fitness if not.  Returns a subset
      of the population.  *)
  let tournament_selection ?compare_func (population : ('a,'b) t) desired =
    let my_compare = 
      match compare_func with 
        Some(func) -> func
      | None ->
        (fun (i : ('a,'b) individual) (i' : ('a,'b) individual)  -> 
          let f = get_opt (i#fitness ()) in
          let f' = get_opt (i'#fitness ()) in
            compare f' f)
    in
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
        let sorted = List.sort my_compare pool in
        let rec walk lst step = match lst with
          | [] -> select_one ()
          | indiv :: rest ->
            let taken =
              if p >= 1.0 then true
              else begin
                let required_prob = p *. ((1.0 -. p)**(step)) in
                  Random.float 1.0 <= required_prob
              end
            in
              if taken then indiv else walk rest (step +. 1.0)
        in
          walk sorted 0.0
      in
      let answer = ref [] in
        for i = 1 to desired do
          answer := (select_one ()) :: !answer
        done ;
        !answer

  (** {b Selection} population desired_size dispatches to the appropriate
      selection function. Currently we have only tournament selection implemented,
      but if/we we add others we can choose between them here *)
  let selection ?compare_func population desired = 
    tournament_selection ?compare_func:compare_func population desired

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
        let legal1' = 0 -- (llen (variant1#get_genome())) in
        let legal2' = 0 -- (llen (variant2#get_genome())) in
        (* FIXME CLAIRE: make sure that range is exclusive! *)
        let rand1 = List.hd (random_order legal1') in
        let rand2 = List.hd (random_order legal2') in
          rand1,rand2
    in
    let g1a,g1b = split_nth (variant1#get_genome()) point1 in
    let g2a,g2b = split_nth (variant2#get_genome()) point2 in
      child1#add_history (Crossover((Some point1),None)) ;
      child2#add_history (Crossover(None,(Some point2))) ;
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
            output := (do_cross original parent1 parent2) @ 
              [parent1;parent2] @ !output
          else
            output := parent1 :: parent2 :: !output
      done ;
      !output

end
