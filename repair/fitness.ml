(* 
 * Program Repair Prototype (v2) 
 *
 * Fitness Strategies include:
 *  -> test until first failure
 *  -> standard "test all"
 *  -> test a subset of all available
 *  -> test by calling an outside script and reading in a resulting file
 *)
open Printf
open Global
open Rep
open Pervasives
open Minimization
open Sourcereader

(* Global variable to store successful rep *)
let successes = ref 0
let negative_test_weight = ref 2.0 
let single_fitness = ref false
let minimization = ref false
let print_source_name = ref false
let print_incremental_evals = ref false
(* sample_strategy is used to compare the effect of sampling once per variant as
   compared to once per generation.  When set to "all", the debug output includes
   the fitness as measured by all the test cases, a once-per-generation sample, and
   a once-per-variant sample (for aforementioned experiments) *)
let sample_strategy = ref "variant"

let _ = 
  options := !options @ [
	"--negative_test_weight", Arg.Set_float negative_test_weight, "X negative tests fitness factor";
	"--single-fitness", Arg.Set single_fitness, " use a single fitness value";
	"--sample", Arg.Set_float sample, "X sample size of positive test cases to use for fitness. Default: 1.0";
	"--samp-strat", Arg.Set_string sample_strategy, " Sample strategy: variant, generation, all. Default: variant";
	"--minimization", Arg.Set minimization, " Attempt to minimize diff script using delta-debugging";
	"--print-source-name", Arg.Set print_source_name, " Print the source name(s) of variants with their fitness.";
	"--print-incremental-evals", Arg.Set print_incremental_evals, " Print the number of evals to date along with variants and their fitness."
  ] 

exception Found_repair of string

(* What should we do if we encounter a true repair? *)
let note_success (rep : 'a Rep.representation) (orig : 'a Rep.representation) =
  let name = rep#name () in 
    match !search_strategy with
      | "mutrb" | "neut" | "neutral" | "walk" | "neutral_walk" -> ()
      | _ -> begin
        debug "\nRepair Found: %s\n" name ;
	successes := !successes+1;
        let subdir = add_subdir (Some("repair")) in
		let filename = Filename.concat subdir ("repair."^ !Global.extension^ !Global.suffix_extension ) in
	      rep#output_source filename 
	  end;
		(* Diff script minimization *)
		if !minimization || (!orig_file <> "") then begin
		  let orig_sig = orig#structural_signature in
		  let rep_sig = rep#structural_signature in
		  let map_union (map1) (map2) : Cdiff.tree_node Cdiff.IntMap.t = 
			Cdiff.IntMap.fold
			  (fun k -> fun v -> fun new_map -> Cdiff.IntMap.add k v new_map)
			  map1 map2
		  in
		  let node_map : Cdiff.tree_node Cdiff.IntMap.t = map_union orig_sig.node_map rep_sig.node_map in 
		  let node_id_to_node = hcreate 10 in
			  (* CLG: HACK *)
			Cdiff.IntMap.iter (fun node_id -> fun node -> hadd node_id_to_node node_id node) node_map;
			let diff_script = Rep.structural_difference_to_string orig_sig rep_sig in
			  debug "\nDifference script:\n*****\n%s*****\n\n" diff_script;
			  let to_minimize = diff_script_from_repair diff_script in
			  let my_script =
				if !minimization then 
				  Minimization.delta_debugging orig to_minimize node_map
				else to_minimize
			  in
				if !orig_file <> "" then begin
					(* Automatic application of repairs. *)
				  ensure_directories_exist "Minimization_Files/original_diffscript";
				  write_script my_script "Minimization_Files/original_diffscript";
				  let files_to_repair = script_to_pair_list my_script in
			(* Create a minimized script for each file here? *)
					Diffprocessor.initialize_node_info Cdiff.verbose_node_info Cdiff.node_id_to_cil_stmt ;
					List.iter (fun (filename,file_script) ->
					  let the_name = 
						let filename_without_slashes =
						  if (String.contains filename '/') then
							List.hd (List.rev (Str.split (Str.regexp "/") filename))
						  else filename
						in
  					  Minimization.write_script file_script ("Minimization_Files/minimized.diffscript-"^(Filename.chop_extension filename_without_slashes));
						  if (!minimization) then ("Minimization_Files/minimized.diffscript-"^(Filename.chop_extension filename_without_slashes))
						  else "Minimization_Files/original_diffscript"
					  in
						Diffprocessor.build_action_list the_name node_id_to_node;
						Diffprocessor.generate_sourcereader_script ((!orig_file)^"/"^filename) ; 
					) files_to_repair;
					Sourcereader.repair_files !(Diffprocessor.repair_script_list)
		  end
 end;
 raise (Found_repair(rep#name()))

exception Test_Failed

(* As an optimization, brute force gives up on a variant as soon
 * as that variant fails a test case. *) 
let test_to_first_failure (rep : 'a Rep.representation) (orig : 'a Rep.representation) = 
  let count = ref 0.0 in 
  try
    if !single_fitness then begin
      (* If there's just one single test case, try that. *) 
      let res, real_value = rep#test_case (Single_Fitness) in 
      count := real_value.(0) ;
      if not res then raise Test_Failed
      else (rep#cleanup(); note_success rep orig)

    end else begin 
      (* Otherwise, if there are multiple test cases, try them all
       * until one fails. We start with the negative tests because
       * small changes to the original variant are likely to pass
       * the positive tests but fail the negative one. *)
      for i = 1 to !neg_tests do
        let res, v = rep#test_case (Negative i) in 
        if not res then raise Test_Failed
        else begin 
         assert(Array.length v > 0); 
         count := !count +. v.(0)
        end 
      done ;
      for i = 1 to !pos_tests do
        let res, v = rep#test_case (Positive i) in 
        if not res then raise Test_Failed
        else begin 
         assert(Array.length v > 0); 
         count := !count +. v.(0)
        end 
      done ;
      rep#cleanup ();
      note_success rep orig
    end 

  with Test_Failed -> 
    rep#cleanup ();
    debug "\t%3g %s\n" !count  (rep#name ()) 

(* Our default fitness evaluation involves testing a variant on
 * all available test cases. *) 
let current_generation = ref (-1)
let current_sample = ref []

exception Quit_early of unit

let test_all_fitness (generation:int) (rep : 'a representation ) (orig : 'a representation)= 
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

		match (rep#saved_fitness()) with 
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
	(if not !failed then note_success rep orig); fitness
