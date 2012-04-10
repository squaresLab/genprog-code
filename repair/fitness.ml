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

let test_sample rep sample = 
  let sample_size = llen sample in
  let fac = 
    (float !pos_tests) *. !negative_test_weight /. (float !neg_tests) in
  let max_fitness = (float !pos_tests) +. ((float !neg_tests) *. fac) in
  let max_sample_fitness = 
    ((float sample_size) *. max_fitness) +. ((float !neg_tests) *. fac) 
  in
  let fitness = one_sample_fitness rep sample fac in
    if fitness < max_sample_fitness then fitness
    else
      let rest_sample = get_rest_of_sample sample in 
        fitness +. (test_one_rep rep (fun x -> Positive x) rest_sample 1.0)

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
let test_fitness_all_three rep generation = 
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

(* Our default fitness evaluation involves testing a variant on
 * all available test cases. *) 
let current_generation = ref (-1)
let current_sample = ref []

exception Quit_early of unit

let test_fitness (generation:int) (rep : ('a,'b) representation ) = 
  let failed = ref false in
  let generation_fitness = ref 0.0 in
  let variant_fitness = ref 0.0 in
  let fitness = 
	if !single_fitness then 
      (* call just a single test that will return a real value *) 
      let res, real_value = rep#test_case (Single_Fitness) in 
		(failed := not res ; real_value.(0))
	else begin 
      assert(!sample <= 1.0);
	  let sample_size = if !sample < 1.0 then
	  (* always sample at least one test case *) 
		  int_of_float (max ((float !pos_tests) *. !sample) 1.0) 
		else !pos_tests 
	  in
	  let generate_random_sample () = 
		let random_pos = random_order (1 -- !pos_tests) in
		  List.sort compare (first_nth random_pos sample_size)
	  in
	  (* Find the relative weight of positive and negative tests *)
      (* If negative_test_weight is 2 (the default), then the negative tests
       * are worth twice as much, total, as the positive tests. This is the
       * old ICSE'09 behavior, where there were 5 positives tests (worth 1
       * each) and 1 negative test (worth 10 points). 10:5 == 2:1. *) 
	  let fac = (float !pos_tests) *. !negative_test_weight /. 
		(float !neg_tests) in
	  let max_fitness = (float !pos_tests) +. ( (float !neg_tests) *. fac) in

		match (rep#fitness()) with 
		  Some(f) -> (if f < max_fitness then failed := true); f
		| None -> 
		  let sorted_sample = 
			if !sample < 1.0 then 
			  match !sample_strategy with
				"all" ->
				  if generation <> !current_generation then begin
					(* regenerate the sample for the new generation *)
					current_generation := generation;
					current_sample := generate_random_sample()
				  end; 1 -- !pos_tests
			  | "generation" ->
				if generation <> !current_generation then begin
				  (* regenerate the sample for the new generation *)
				  current_generation := generation;
				  current_sample := generate_random_sample()
				end; !current_sample
			  | "variant" -> generate_random_sample()
			  (* Sometimes we choose a random sample of the positive test cases
			   * and evaluate only on those. This technique is described formally
			   * in GECCO'10. We choose N at random by randomly ordering all of
			   * them and taking the first N. *) 
			else 1 -- !pos_tests
		  in
		  let pos_results = 
			rep#test_cases (List.map (fun x -> Positive x) sorted_sample)
		  in
		  let pos_fitness = 
			lfoldl
			  (fun fitness ->
				fun (res,_) ->
				  if res then fitness +. 1.0 else (failed := true; fitness))
			  0.0 pos_results 
		  in
		  (* currently, we always run every negative test -- no sub-sampling *) 
		  let neg_results = 
			rep#test_cases (lmap (fun x -> Negative x) (1 -- !neg_tests))
		  in
		  let neg_fitness = 
			lfoldl
			  (fun fitness ->
				fun (res,_) ->
				  if res then fitness +. fac else (failed := true; fitness))
			  0.0 neg_results
		  in
		  let fitness = pos_fitness +. neg_fitness in
			if (not !failed) && ((sample_size < !pos_tests) && !sample_strategy <> "all") then begin
			  (* If we are sub-sampling and it looks like we have a candidate
			   * repair, we must run it on all of the rest of the tests to make
			   * sure! *)  
			  let rest_tests = List.filter (fun possible_test -> 
				not (List.mem possible_test sorted_sample)) (1 -- !pos_tests)
			  in 
				assert((llen rest_tests) + (llen sorted_sample) = !pos_tests);
				try
				  liter (fun pos_test ->
					let res, _ = rep#test_case (Positive pos_test) in
					  if not res then raise (Quit_early())) rest_tests
				with (Quit_early()) -> (failed := true)
			end;
			if !sample_strategy = "all" then begin
			  let generation_pos_fitness = 
				let gen_pos_results = rep#test_cases (List.map (fun x -> Positive x) !current_sample)
				in
				  lfoldl
					(fun fitness ->
					  fun (res,_) ->
						if res then fitness +. 1.0 else (failed := true; fitness))
					0.0 gen_pos_results 
			  in
			  let variant_pos_fitness = 
				let variant_sample = generate_random_sample() in
				let var_pos_results = rep#test_cases (List.map (fun x -> Positive x) variant_sample) in
				  lfoldl
					(fun fitness ->
					  fun (res,_) ->
						if res then fitness +. 1.0 else (failed := true; fitness))
					0.0 var_pos_results 		  
			  in 
				generation_fitness := generation_pos_fitness +. neg_fitness;
				variant_fitness := variant_pos_fitness +. neg_fitness
			end;
			rep#set_fitness fitness; fitness
	end
  in
	if !sample_strategy = "all" then begin
	  debug ~force_gui:true "\t%3g\t%3g\t%3g %s" fitness !generation_fitness !variant_fitness (rep#name ());
	end else begin
	  (* debugging information, etc. *) 
	  debug ~force_gui:true "\t%3g %s" fitness (rep#name ());
	end;
	if !print_source_name then
	  List.iter (fun name -> debug " %s" name) rep#source_name;
	if !print_incremental_evals then
	  debug " %g" ((float (Rep.num_test_evals_ignore_cache ())) /.
					  (float (!pos_tests + !neg_tests)));
	debug ~force_gui:true "\n";
	rep#cleanup();  
    not !failed
(*
let test_fitness generation rep = 
  (* Find the relative weight of positive and negative tests
   * If negative_test_weight is 2 (the default), then the negative tests are
   * worth twice as much, total, as the positive tests. This is the old
   * ICSE'09 behavior, where there were 5 positives tests (worth 1 each) and
   * 1 negative test (worth 10 points). 10:5 == 2:1. *)
  let fac = 
    (float !pos_tests) *. !negative_test_weight /. (float !neg_tests) in
  let max_fitness = (float !pos_tests) +. ((float !neg_tests) *. fac) in
  let print_info fitness rest =
    (match !sample_strategy,rest with
      "all",Some(generation_fitness,variant_fitness) when !sample < 1.0 -> 
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
  let fitness,rest = 
    match (rep#fitness()) with
    | Some(f) -> f,None
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
    rep#cleanup();
    rep#set_fitness fitness;
    not (fitness < max_fitness)
*)
