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
(** The "fitness" interface enables the computation of a variant.  Available
    fitness strategies include: 
    -> test until first failure 
    -> standard "test all"
    -> test a subset of all available
    -> test by calling an outside script and reading in a resulting file *)

open Printf
open Global
open Rep

let negative_test_weight = ref 2.0 
let single_fitness = ref false
let print_source_name = ref false
let print_incremental_evals = ref false
let sample = ref 1.0

(* sample_strategy is used to compare the effect of sampling once per variant as
   compared to once per generation.  When set to "all", the debug output includes
   the fitness as measured by all the test cases, a once-per-generation sample, and
   a once-per-variant sample (for aforementioned experiments) *)
let sample_strategy = ref "variant"

let _ = 
  options := !options @ [
    "--negative-test-weight", Arg.Set_float negative_test_weight, 
    "X negative tests fitness factor. Default: 2.0";

    "--single-fitness", Arg.Set single_fitness, " use a single fitness value";

    "--sample", Arg.Set_float sample, 
    "X sample size of positive test cases to use for fitness. Default: 1.0";

    "--samp-strat", Arg.Set_string sample_strategy, 
    "X Sample strategy: variant, generation, all. Default: variant";

    "--print-source-name", Arg.Set print_source_name, 
    " Print the source name(s) of variants with their fitness. Default: false";

    "--print-incremental-evals", Arg.Set print_incremental_evals, 
    " Print the number of evals to date along with variants/fitness. Default:false"
  ] 

exception Test_Failed

(* utilities to help test fitness *)

let get_rest_of_sample sample = 
  List.filter 
    (fun test -> not (List.mem test sample)) (1 -- !pos_tests)

let test_one_rep rep test_maker tests factor = 
  let results = rep#test_cases (lmap test_maker tests) in
    lfoldl 
      (fun fitness (res,_) -> if res then fitness +. factor else fitness)
      0.0 results

let one_sample_fitness rep sample fac =
  let neg_fitness = 
    test_one_rep rep (fun x -> Negative x) (1 -- !neg_tests) fac 
  in 
  let pos_fitness = 
    test_one_rep rep (fun x -> Positive x) sample 1.0 
  in
    neg_fitness +. pos_fitness

let test_sample (rep) (sample) : float * float = 
  let sample_size = llen sample in
  let fac = 
    (float !pos_tests) *. !negative_test_weight /. (float !neg_tests) in
  let max_sample_fitness = 
    (float sample_size) +. ((float !neg_tests) *. fac) 
  in
  let fitness = one_sample_fitness rep sample fac in
    if fitness < max_sample_fitness then fitness,fitness
    else
      let rest_sample = get_rest_of_sample sample in 
        fitness, fitness +. (test_one_rep rep (fun x -> Positive x) rest_sample 1.0)

let generate_random_sample sample_size = 
  let random_pos = random_order (1 -- !pos_tests) in
    List.sort compare (first_nth random_pos sample_size)


(* three different sampling strategies *)
let test_fitness_variant rep = 
  (* always sample at least one test case *) 
  let sample_size = 
    int_of_float (max ((float !pos_tests) *. !sample) 1.0) 
  in
    test_sample rep (generate_random_sample sample_size)

(* storage of info for generation-based sampling *)
let current_generation = ref (-1) 
let generation_sample = ref []

let test_fitness_generation rep generation =
  (* always sample at least one test case *) 
  let sample_size = 
    int_of_float (max ((float !pos_tests) *. !sample) 1.0) 
  in
  let generation_sample = 
    if generation <> !current_generation then begin
      current_generation := generation;
      generation_sample := generate_random_sample sample_size;
      !generation_sample
    end else !generation_sample
  in
    test_sample rep generation_sample

let test_fitness_all rep = test_sample rep (1 -- !pos_tests) 

(* get all three, for experiments that compare the amount of noise in different
   random sampling strategies *)
let test_fitness_all_three (rep) (generation) : (float * float) * ((float * float) * (float * float)) option = 
  let variant_fitness = test_fitness_variant rep in 
  let generation_fitness = test_fitness_generation rep generation in
  let all_fitness =  test_fitness_all rep in
    all_fitness, Some(variant_fitness,generation_fitness)

(** {b test_to_first_failure} variant returns true if the variant passes all
    test cases and false otherwise; unlike other search strategies and as an
    optimization for brute_force search, gives up on a variant as soon as it
    fails a test case.  This makes less sense for single_fitness, but
    single_fitness being true won't break it.  Does do sampling if specified. *)
let test_to_first_failure (rep :('a,'b) Rep.representation) : bool = 
  let count = ref 0.0 in 
    try
      if !single_fitness then begin
        let res, real_value = rep#test_case (Single_Fitness) in 
          count := real_value.(0) ;
          if not res then raise Test_Failed
          else (rep#cleanup(); true )
      end else begin 
        (* We start with the negative tests because small changes to the
         * original variant are likely to pass the positive tests but fail the
         * negative one. *)
        for i = 1 to !neg_tests do
          let res, v = rep#test_case (Negative i) in 
            if not res then raise Test_Failed
            else begin 
              assert(Array.length v > 0); 
              count := !count +. v.(0)
            end 
        done ;
        let sample_size = 
          int_of_float (max ((float !pos_tests) *. !sample) 1.0) in
        let actual_sample = 
          if !sample < 1.0 then
            generate_random_sample sample_size 
          else 1 -- !pos_tests 
        in 
          liter
            (fun i -> 
              let res, v = rep#test_case (Positive i) in 
                if not res then raise Test_Failed
                else begin 
                  assert(Array.length v > 0); 
                  count := !count +. v.(0)
                end ) actual_sample;
          let rest = 
            if !sample < 1.0 then get_rest_of_sample actual_sample else [] 
          in
            liter
              (fun i -> 
                let res, v = rep#test_case (Positive i) in 
                  if not res then raise Test_Failed
                  else begin 
                    assert(Array.length v > 0); 
                    count := !count +. v.(0)
                  end ) rest;
            rep#cleanup ();
            true
      end 
    with Test_Failed -> 
      rep#cleanup ();
      false 

(** {b test_fitness} generation variant returns true if the variant passes all
    test cases and false otherwise.  Only tests fitness if the rep has not
    cached it.  Postcondition: records fitness in rep, calls rep#cleanup(). May
    implement sampling strategies if specified by the command line.*)

let test_fitness generation (rep : ('a,'b) Rep.representation) = 
  (* Find the relative weight of positive and negative tests
   * If negative_test_weight is 2 (the default), then the negative tests are
   * worth twice as much, total, as the positive tests. This is the old
   * ICSE'09 behavior, where there were 5 positives tests (worth 1 each) and
   * 1 negative test (worth 10 points). 10:5 == 2:1. *)
  if !single_fitness then begin
    let res, real_value = rep#test_case (Single_Fitness) in
      debug ~force_gui:true "\t%3g %s\n" real_value.(0) (rep#name ());
      rep#set_fitness real_value.(0);
      rep#cleanup(); res
  end else begin
    let fac = 
      (float !pos_tests) *. !negative_test_weight /. (float !neg_tests) in
    let max_fitness = (float !pos_tests) +. ((float !neg_tests) *. fac) in
    let print_info fitness rest =
      (match !sample_strategy,rest with
        "all",Some((generation_fitness,_),(variant_fitness,_)) when !sample < 1.0 -> 
          debug ~force_gui:true "\t%3g\t%3g\t%3g %s" 
            fitness generation_fitness variant_fitness (rep#name ())
      | _,_ -> 
        debug ~force_gui:true "\t%3g %s" fitness (rep#name ()));
      if !print_source_name then
        List.iter (fun name -> debug " %s" name) rep#source_name;
      if !print_incremental_evals then
        debug " %g" ((float (Rep.num_test_evals_ignore_cache ())) /.
                        (float (!pos_tests + !neg_tests)));
      debug ~force_gui:true "\n"
    in

  (* rest here is the additional data provided by test_fitness_all_three, when
     applicable *)
    let (sample_fitness, fitness),rest = 
      match (rep#fitness()) with
      | Some(f) -> (f,f),None
      | None ->
        if !sample < 1.0 then 
          match !sample_strategy with
          | "generation" -> test_fitness_generation rep generation, None
          | "variant" -> test_fitness_variant rep, None
          | "all" -> test_fitness_all_three rep generation
        else 
          test_fitness_all rep, None
    in
      print_info fitness rest;
      (* debugging for --coverage-per-test
      if !Rep.coverage_per_test then begin
        let tests = rep#tests_visiting_edited_atoms () in  
        debug "coverage_per_test: %d for %s\n" (TestSet.cardinal tests) 
          (rep#name ()) 
      end;
      *) 
      rep#cleanup();
      rep#set_fitness sample_fitness;
      not (fitness < max_fitness)
  end
