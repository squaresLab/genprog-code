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
let _ = 
  options := !options @ [
  "--negative_test_weight", Arg.Set_float negative_test_weight, "X negative tests fitness factor";
] 


(* What should we do if we encounter a true repair? *)
let note_success (rep : 'a Rep.representation) =
  let name = rep#name () in 
  debug "\nRepair Found: %s\n" name ;
  rep#output_source ("repair." ^ !Global.extension) ;
  exit 1 

exception Test_Failed

(* As an optimization, brute force gives up on a variant as soon
 * as that variant fails a test case. *) 
let test_to_first_failure (rep : 'a Rep.representation) = 
  let count = ref 0 in 
  try
    for i = 1 to !neg_tests do
      if not (rep#test_case (Negative i)) then raise Test_Failed
      else incr count 
    done ;
    for i = 1 to !pos_tests do
      if not (rep#test_case (Positive i)) then raise Test_Failed
      else incr count 
    done ;
    note_success rep 

  with Test_Failed -> 
    debug "\t%3d %s\n" !count  (rep#name ()) 

(* Our default fitness evaluation involves testing a variant on
 * all available test cases. *) 
let test_all_fitness (rep : 'a representation ) = 
  (* Find the relative weight of positive and negative tests *)
  let fac = (float !pos_tests) *. !negative_test_weight /. 
            (float !neg_tests) in 
  let fitness = ref 0.0 in 
  let failed = ref false in
  for i = 1 to !pos_tests do
    if (rep#test_case (Positive i)) then fitness := !fitness +. 1.0 
    else failed := true 
  done ;
  for i = 1 to !neg_tests do
    if (rep#test_case (Negative i)) then fitness := !fitness +. fac
    else failed := true 
  done ;
  debug "\t%3g %s\n" !fitness (rep#name ()) ;
  if not !failed then begin
    note_success rep 
  end ; 
  !fitness 

(* ZAK - added this *)
(* Call an outside script and read in the resulting textfile *) 
let test_fitness_coverity (rep : 'a Rep.representation) (script_name : string) =
  let fitness = ref 0.0 in 
  let failed = ref false in
  (* run all test cases *)
  for i = 1 to !pos_tests do
    if not (rep#test_case (Positive i)) then failed := true 
  done ;

  (* if any test cases failed, fitness=-1, otherwise fitness=coverity*)
  fitness := ( match !failed with
    |  true -> -1.0
    |  false -> rep#run_coverity () ) ; 
  !fitness
