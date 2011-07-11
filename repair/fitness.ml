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

let negative_test_weight = ref 2.0 
let single_fitness = ref false
let sample = ref 1.0

let _ = 
  options := !options @ [
  "--negative_test_weight", Arg.Set_float negative_test_weight, "X negative tests fitness factor";
  "--single-fitness", Arg.Set single_fitness, " use a single fitness value";
  "--sample", Arg.Set_float sample, "X sample size of positive test cases to use for fitness. Default: 1.0"; 
] 


(* What should we do if we encounter a true repair? *)
let note_success (rep : 'a Rep.representation) =
  let name = rep#name () in 
	debug "\nRepair Found: %s\n" name ;
	let subdir = add_subdir (Some("repair")) in
	let filename = Filename.concat subdir ("repair."^ !Global.extension^ !Global.suffix_extension ) in
	  rep#output_source filename ;
  exit 1 

exception Test_Failed

(* As an optimization, brute force gives up on a variant as soon
 * as that variant fails a test case. *) 
let test_to_first_failure (rep : 'a Rep.representation) = 
  let count = ref 0.0 in 
  try
    if !single_fitness then begin
      (* If there's just one single test case, try that. *) 
      let res, real_value = rep#test_case (Single_Fitness) in 
      count := real_value.(0) ;
      if not res then raise Test_Failed
      else (rep#cleanup(); note_success rep )

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
      note_success rep 
    end 

  with Test_Failed -> 
    rep#cleanup ();
    debug "\t%3g %s\n" !count  (rep#name ()) 

(* Our default fitness evaluation involves testing a variant on
 * all available test cases. *) 
let test_all_fitness (rep : 'a representation ) = 
  let fitness = ref 0.0 in 
  let failed = ref false in

  if !single_fitness then begin
    (* call just a single test that will return a real value *) 
    let res, real_value = rep#test_case (Single_Fitness) in 
    fitness := real_value.(0) ;
    failed := not res ; 

  end else begin 
    assert(!sample <= 1.0);
    (* Find the relative weight of positive and negative tests *)
    let fac = (float !pos_tests) *. !negative_test_weight /. 
              (float !neg_tests) in 
    (* If negative_test_weight is 2 (the default), then the negative tests
     * are worth twice as much, total, as the positive tests. This is the
     * old ICSE'09 behavior, where there were 5 positives tests (worth 1
     * each) and 1 negative test (worth 10 points). 10:5 == 2:1. *) 
    let tests = ref IntSet.empty in
	  for i = 1 to !pos_tests do
      tests := IntSet.add i !tests
	  done;
    (* Sometimes we choose a random sample of the positive test cases and
     * evaluate only on those. This technique is described formally in
     * GECCO'10. We choose N at random by randomly ordering all of them and
     * taking the first N. *) 
    let sample_size = int_of_float ((float !pos_tests) *. !sample) in
	  let random_pos = random_order (1 -- !pos_tests) in
    let sample = first_nth random_pos sample_size in
    let sorted_sample = List.sort compare (first_nth sample sample_size) in

	  liter (fun pos_test ->
		  let res, _ = rep#test_case (Positive pos_test) in 
			if res then fitness := !fitness +. 1.0 
			else failed := true) sorted_sample;

    (* currently, we always run every negative test -- no sub-sampling *) 
    for i = 1 to !neg_tests do
      let res, _ = rep#test_case (Negative i) in 
      if res then 
        fitness := !fitness +. fac
      else 
        failed := true 
    done ;

    if (not !failed) && (sample_size < !pos_tests) then begin
      (* If we are sub-sampling and it looks like we have a candidate
       * repair, we must run it on all of the rest of the tests to make
       * sure! *)  
      let rest_tests = List.sort compare 
        (first_nth (lrev random_pos) (!pos_tests - sample_size)) 
      in
      assert((llen rest_tests) + (llen sorted_sample) = !pos_tests);
		  liter (fun pos_test ->
			  let res, _ = rep#test_case (Positive pos_test) in
				if not res then failed := true) rest_tests
    end;
  end ;
  (* debugging information, etc. *) 
  debug "\t%3g %s\n" !fitness (rep#name ()) ;
  rep#cleanup();
  if not !failed then begin
    note_success rep 
  end ; 
  !fitness 
