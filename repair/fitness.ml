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
let sample = ref 1.0
let minimization = ref false

let _ = 
  options := !options @ [
  "--negative_test_weight", Arg.Set_float negative_test_weight, "X negative tests fitness factor";
  "--single-fitness", Arg.Set single_fitness, " use a single fitness value";
  "--sample", Arg.Set_float sample, "X sample size of positive test cases to use for fitness. Default: 1.0";
  "--minimization", Arg.Set minimization, " Attempt to minimize diff script using delta-debugging";
] 

exception Found_repair of string

(* What should we do if we encounter a true repair? *)
let note_success (rep : 'a Rep.representation) (orig : 'a Rep.representation) =
  let name = rep#name () in 
    match !search_strategy with
      | "mutrb" | "neut" | "neutral" -> ()
      | _ -> begin
          debug "\nRepair Found: %s\n" name ;
          let subdir = add_subdir (Some("repair")) in
	  let filename = Filename.concat subdir ("repair."^ !Global.extension^ !Global.suffix_extension ) in
	    rep#output_source filename ;
	  
  (* Diff script minimization *)
  if (!minimization) then begin
  (* We're only producing the diff script if minimization is asked for
   * because structural signatures currently don't work on multiple files
   * (Tue Aug  2 20:48:29 EDT 2011) but we do need to find repairs
   * on multiple files. *) 
    let orig_struct = orig#structural_signature in
    let rep_struct = rep#structural_signature in
    let diff_script = Rep.structural_difference_to_string orig_struct rep_struct in

    Printf.printf "\nDifference script:\n*****\n%s" diff_script;
    Printf.printf "*****\n\n";

    diff_script_from_repair diff_script;
    
(*      
    Minimization.naive_delta_debugger rep orig (rep_struct) (orig_struct) ;
    Printf.printf "__________\n";
    Minimization.debug_diff_script (!(Minimization.my_min_script));)
*)
    Printf.printf "sanity...\n";
    Minimization.delta_debugging rep orig ;

  end
  end;

	 
  (* Automatic application of repairs. *)
  if (!orig_file)<>"" then begin
    (* Ensure that we have a diff script with which to work, even if minimization was not (for some reason) called. *)
    if not (!minimization) then begin
    
      let orig_struct = orig#structural_signature in
      let rep_struct = rep#structural_signature in
      let diff_script = Rep.structural_difference_to_string orig_struct rep_struct in

      Printf.printf "\nDifference script:\n*****\n%s" diff_script;
      Printf.printf "*****\n\n";

      diff_script_from_repair diff_script;
    end;
    (* Thoughts on multi-file: (bal2ag Thu Aug  4 15:23:39 EDT 2011)
     *
     * I assume we get a series of diffscripts, one for each file.
     * The order in which we process them is irrelevant. Diffprocessor's
     * node info stuff should only have to be updated once, assuming structural
     * differencing does what it should for multi-files (node IDs are still unique,
     * 1-100 for one file, 101-200 for another, etc. etc.). So all we should have to do
     * is iterate over each diffscript (perhaps the edit script returns a list for multi-file
     * reps, or we can just process them based on filenames if they're in one string buffer)
     * and call generate_sourcereader_script on each of the associated files. 
     *
     * More pressing is finding the exact paths for files, assuming they are not all in the same directory.
     * Either the entire path must be included, or we must search all subdirectories.
     * Additionally, look.c and look-original.c are different; the former is what we parse,
     * and the latter is what we want to pass to sourcereader/diffprocessor. So all of the
     * original files must subscribe to a specific format, or stored somewhere in repair. *)

     (* TODO (bal2ag Thu Aug  4 15:25:05 EDT 2011) :
     * Make the directory names for Minimization and Change-Original global variables,
     * so they can be changed in the future *)
    ensure_directories_exist "Minimization_Files/original_diffscript";
    write_script !my_script "Minimization_Files/original_diffscript";

    Diffprocessor.initialize_node_info Cdiff.verbose_node_info Cdiff.node_id_to_cil_stmt ;
    let base,_ = split_ext !program_to_repair in
    Diffprocessor.build_action_list 
      (if (!minimization) then ("Minimization_Files/"^base^".minimized.diffscript") else "Minimization_Files/original.diffscript") 
      Cdiff.node_id_to_node;
    Diffprocessor.generate_sourcereader_script (!orig_file) ; 
    Sourcereader.repair_files !(Diffprocessor.repair_script_list)
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
let test_all_fitness (rep : 'a representation ) (orig : 'a representation)= 
  let fitness = ref 0.0 in 
  let failed = ref false in

  if !single_fitness then begin
    (* call just a single test that will return a real value *) 
    let res, real_value = rep#test_case (Single_Fitness) in 
    fitness := real_value.(0) ;
    failed := not res ; 

  end else begin 
    assert(!sample <= 1.0);
	let cached = rep#saved_fitness () in
  let max_fitness = 
	let fac = (float !pos_tests) *. !negative_test_weight /. 
	  (float !neg_tests) in 
	  (float !pos_tests) +. ( (float !neg_tests) *. fac)
  in
	match cached with 
	  Some(f) -> fitness := f; if f < max_fitness then failed := true
	| None -> begin
    (* Find the relative weight of positive and negative tests *)
    let fac = (float !pos_tests) *. !negative_test_weight /. 
              (float !neg_tests) in 
    (* If negative_test_weight is 2 (the default), then the negative tests
     * are worth twice as much, total, as the positive tests. This is the
     * old ICSE'09 behavior, where there were 5 positives tests (worth 1
     * each) and 1 negative test (worth 10 points). 10:5 == 2:1. *) 

    let sorted_sample, sample_size = 
      if !sample < 1.0 then begin
        (* Sometimes we choose a random sample of the positive test cases
         * and evaluate only on those. This technique is described formally
         * in GECCO'10. We choose N at random by randomly ordering all of
         * them and taking the first N. *) 
        let sample_size = int_of_float (
          max ((float !pos_tests) *. !sample) 1.0) 
          (* always sample at least one test case *) 
        in
        let random_pos = random_order (1 -- !pos_tests) in
        List.sort compare (first_nth random_pos sample_size), sample_size
      end else 
        (1 -- !pos_tests), !pos_tests
    in 

    let pos_results = rep#test_cases 
      (List.map (fun x -> Positive x) sorted_sample) 
    in 

    liter (fun (res, _) -> 
      if res then fitness := !fitness +. 1.0 
      else failed := true
    ) pos_results;

    (* currently, we always run every negative test -- no sub-sampling *) 
    for i = 1 to !neg_tests do
      let res, _ = rep#test_case (Negative i) in 
      if res then begin
        fitness := !fitness +. fac
      end
      else 
        failed := true 
    done ;

    if (not !failed) && (sample_size < !pos_tests) then begin
      (* If we are sub-sampling and it looks like we have a candidate
       * repair, we must run it on all of the rest of the tests to make
       * sure! *)  
      let rest_tests = List.filter (fun possible_test -> 
        not (List.mem possible_test sorted_sample)) (1 -- !pos_tests)
      in 
	assert((llen rest_tests) + (llen sorted_sample) = !pos_tests);
	liter (fun pos_test ->
	  let res, _ = rep#test_case (Positive pos_test) in
	    if not res then failed := true) rest_tests
    end;
  end ;
	rep#set_fitness !fitness;
  end;
  (* debugging information, etc. *) 
  debug "\t%3g %s\n" !fitness (rep#name ()) ;
  rep#cleanup();  
  if not !failed then begin
    note_success rep orig
  end ; 
  !fitness 
