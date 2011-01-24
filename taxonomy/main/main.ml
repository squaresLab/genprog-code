(* step 1: given a project, a URL, and a start and end revision,
 * collect all changes referencing bugs, bug numbers, or "fix."
 * 1a: diff option 1: tree-based diffs
 * 1b: diff option 2: syntactic (w/alpha-renaming)
 * step 2: process each change
 * step 3: cluster changes (distance metric=what is Ray doing/Hamming
 * distance from Gabel&Su, FSE 10?)
 *)

open Batteries
open List
open Utils
open Globals
open Diffs
open Datapoint
open Cluster
open Distance

let xy_data = ref ""
let test_distance = ref false 
let diff_files = ref []
let test_change_diff = ref false
let test_cabs_diff = ref false

(* config: per benchmark, we need a repository, potentially rstart and rend, a
   log file, an htfile, and whether we read it in or not *)

let configs = ref []
let fullsave = ref ""

let _ =
  options := !options @
[
  "--test-cluster", Arg.Set_string xy_data, "\t Test data of XY points to test the clustering";
  "--test-distance", Arg.Set test_distance, "\t Test distance metrics\n";
  "--test-cd", Arg.String (fun s -> test_change_diff := true; diff_files := s :: !diff_files), "\t Test change diffing.  Mutually  exclusive w/test-cabs-diff\n";
  "--test-cabs-diff", Arg.String (fun s -> test_cabs_diff := true;  diff_files := s :: !diff_files), "\t Test C snipped diffing\n";
  "--fullsave", Arg.Set_string fullsave, "\t file to save composed hashtable\n";
  "--configs", Arg.Rest (fun s -> configs := s :: !configs), 
  "\t input config files for each benchmark. Processed separately in the same way as regular command-line arguments.";
]

let main () = 
  begin
	Random.init (Random.bits ());
	let config_files = ref [] in
	let handleArg1 str = config_files := str :: !config_files in 
	let handleArg str = configs := str :: !configs in
	let aligned = Arg.align !options in
      Arg.parse aligned handleArg1 usageMsg ;
	  liter (parse_options_in_file ~handleArg:handleArg aligned usageMsg) !config_files;
		(* If we're testing stuff, test stuff *)
	  if !test_distance then
		(Distance.levenshtein "kitten" "sitting";
		 Distance.levenshtein "Saturday" "Sunday")
	  else if !xy_data <> "" then 
		let lines = File.lines_of !xy_data in
		let points = 
		  Set.of_enum 
			(Enum.map 
			   (fun line -> 
				 let split = Str.split comma_regexp line in
				 let x,y = int_of_string (hd split), int_of_string (hd (tl split)) in
				   XYPoint.create x y 
			   ) lines)
		in
		  ignore(TestCluster.kmedoid !k points)
	  else if !test_cabs_diff then 
		Treediff.test_diff_cabs (lrev !diff_files)
	  else if !test_change_diff then 
		Treediff.test_diff_change (lrev !diff_files)
	  else begin
		  (* if we're not testing stuff, do the normal thing *)
(*		let diffs =*)
		let diffs = Diffs.get_many_diffs !configs in
		  (* can we save halfway through clustering if necessary? *)
		  if !cluster then ignore(DiffCluster.kmedoid !k diffs)
	  end
 end ;;

main () ;;
