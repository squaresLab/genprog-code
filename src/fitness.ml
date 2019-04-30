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

let best_test_rule = ref "1 * test_fail_prob ; 1 * test_fail_count ; -1 * test_pass_count"

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
      " Print the number of evals to date along with variants/fitness. Default:false";

      "--best-test-rule", Arg.Set_string best_test_rule,
      "X use X to rank possible tests in adaptive search";
    ]

exception Test_Failed

(* utilities to help test fitness *)

let get_rest_of_sample sample =
  List.filter
    (fun test -> not (List.mem test sample)) (1 -- !pos_tests)

let test_one_rep (rep : ('a, 'b) Rep.representation) test_maker tests factor =
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

type test_model_1 = {
  queue : PriorityQueue.t ;
  get_fitness : test_metrics -> float list
}

let test_model = ref {
    queue = PriorityQueue.empty ;
    get_fitness = fun _ -> []
  }

let count_tests_passed allowed (rep :('a,'b) Rep.representation) : int =
  let cpass = ref 0 in
  for i = 1 to !pos_tests do
    let t = Positive i in
    if allowed t then
      let res, _ = rep#test_case t in
      if res then incr cpass
  done;
  for i = 1 to !neg_tests do
    let t = Negative i in
    if allowed t then
      let res, _ = rep#test_case t in
      if res then incr cpass
  done;
  !cpass

(** {b test_to_first_failure} variant returns true if the variant passes all
    test cases and false otherwise; unlike other search strategies and as an
    optimization for brute_force search, gives up on a variant as soon as it
    fails a test case.  This makes less sense for single_fitness, but
    single_fitness being true won't break it.  Does not do sampling since that
    makes no sense. *)
let test_to_first_failure ?(allowed=fun _ -> true) (rep :('a,'b) Rep.representation) : bool =
  let int_to_test i =
    if i = 0
    then Single_Fitness
    else if i <= !neg_tests
    then Negative (i)
    else Positive (i - !neg_tests)
  in
  if PriorityQueue.is_empty !test_model.queue then begin
    (* First run of this function: initialize the model with the user-defined
       best_test_rule *)
    let get_test_attr attr =
      match attr with
      | "test_pass_count" -> fun m -> m.pass_count
      | "test_fail_count" -> fun m -> m.fail_count
      | "test_fail_prob"  ->
        fun m ->
          let total = m.pass_count +. m.fail_count in
          if total = 0. then 0. else m.fail_count /. total
      | "test_cost" -> fun m -> m.cost
      | _ ->
        debug "fitness: ERROR: unknown test attribute %s\n" attr;
        failwith "get_test_attr"
    in
    let rec interpret r rs = function
      | ";" :: rest ->
        interpret [] (r::rs) rest
      | weight :: "*" :: attribute :: rest ->
        let weight = my_float_of_string weight in
        let attr = get_test_attr attribute in
        interpret ((weight, attr) :: r) rs rest
      | x :: _ ->
        debug "fitness: ERROR: unknown command %S\n" x;
        failwith "interpret"
      | [] -> r::rs
    in
    let best_test_rules = Str.split space_regexp !best_test_rule in
    let rules = List.rev (interpret [] [] best_test_rules) in
    let apply_rules m =
      (* We negate the weight so that high priority weights will sort first
         according to compare. *)
      List.map
        (fun r -> List.fold_left (fun sum (w, a) -> sum -. w *. a m) 0.0 r)
        rules
    in
    let queue =
      let ids =
        if !single_fitness then [0] else 1 -- (!neg_tests + !pos_tests) in
      List.fold_left
        (fun queue i ->
           let m = rep#test_metrics (int_to_test i) in
           PriorityQueue.add ((apply_rules m), i) queue)
        PriorityQueue.empty ids
    in
    test_model := { queue = queue; get_fitness = apply_rules }
  end;
  let rec run_tests tried pending =
    if PriorityQueue.is_empty pending then
      (* no more tests to run -- they all passed! *)
      true, tried, pending
    else begin
      let w, i = PriorityQueue.min_elt pending in
      let pending = PriorityQueue.remove (w, i) pending in
      let t = int_to_test i in
      let passed, w' =
        if allowed t then begin
          let passed, _ = rep#test_case t in
          passed, !test_model.get_fitness (rep#test_metrics t)
        end else
          true, w
      in
      let tried = PriorityQueue.add (w', i) tried in
      if passed
      then run_tests tried pending
      else false, tried, pending
    end
  in
  let success, tried, pending =
    run_tests PriorityQueue.empty !test_model.queue
  in
  rep#cleanup () ;
  test_model := {!test_model with queue = PriorityQueue.union tried pending} ;

  (* Possible FIXME: should we be thorough and run the skipped tests before
     officially calling this a pass? *)
  success

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
    let values, stddevs = col_mean_stddev real_value in
    let n = sqrt (float_of_int (llen real_value)) in
    let b = Buffer.create 255 in
    Array.iteri (fun i v ->
        Printf.bprintf b "%g" v ;
        if (classify_float stddevs.(i)) != FP_nan then
          Printf.bprintf b " +/- %g" (1.96 *. stddevs.(i) /. n) ;
        Printf.bprintf b " "
      ) values ;
    debug ~force_gui:true "\t%s%s\n" (Buffer.contents b) (rep#name ()) ;
    rep#set_fitness values.(0);
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
