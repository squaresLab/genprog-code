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
open Diffs
open User

let xy_data = ref ""
let test_distance = ref false 
let diff_files = ref []
let test_change_diff = ref false
let test_cabs_diff = ref false

let fullload = ref ""
let user_feedback_file = ref ""

let ray = ref ""
let htf = ref ""

let _ =
  options := !options @
	[
	  "--test-cluster", Arg.Set_string xy_data, "\t Test data of XY points to test the clustering";
	  "--test-distance", Arg.Set test_distance, "\t Test distance metrics\n";
	  "--test-cd", Arg.String (fun s -> test_change_diff := true; diff_files := s :: !diff_files), "\t Test change diffing.  Mutually  exclusive w/test-cabs-diff\n";
	  "--test-cabs-diff", Arg.String (fun s -> test_cabs_diff := true;  diff_files := s :: !diff_files), "\t Test C snipped diffing\n";
	  "--user-distance", Arg.Set_string user_feedback_file, "\t Get user input on change distances, save to X.txt and X.bin";
	  "--fullload", Arg.Set_string fullload, "\t load big_diff_ht and big_change_ht from file, skip diff collecton.";
	  "--combine-hts", Arg.Set_string htf, "\t Combine diff files from many benchmarks, listed in X file\n"; 
	  "--ray", Arg.String (fun file -> interactive := true; ray := file), "\t Ray mode.  Files written X.txt and X.ht"
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
		let big_diff_ht = 
		  if !fullload <> "" || !ray <> "" then
			begin 
			  let file = if !ray <> "" then 
				  "/home/claire/taxonomy/main/test_data/"^(!ray)^"_full_ht.bin"
				else !fullload 
			  in
			  let succ,ht = full_load_from_file file in
				if succ || (!ray <> "") then ht else
				  failwith "Failed to load BigFile"
			end else begin
			  let hts_out = 
				if !fullsave <> "" then Some(Pervasives.open_out_bin !fullsave) else 
				  if !ray <> "" then Some(Pervasives.open_out_bin ("/home/claire/taxonomy/main/test_data/"^(!ray)^"_full_ht.bin")) else
				  None
			  in
			  let tmp = get_many_diffs !ray !configs !htf hts_out in
				(match hts_out with
				  Some(fout) -> Pervasives.close_out fout
				| None -> ()); tmp
			end
		  (* IMPORTANT NOTE: right now, we are returning a set of DIFF IDS, not
			 CHANGE IDS. DO NOT FORGET THIS FACT BECAUSE IT IS IMPORTANT *)
		  (* can we save halfway through clustering if necessary? *)
(*		  if !cluster then ignore(DiffCluster.kmedoid !k diffs);*)
		in
		  if !ray <> "" then get_user_feedback !ray big_diff_ht
		  else if !user_feedback_file <> "" then get_user_feedback !user_feedback_file big_diff_ht
	  end
  end ;;

main () ;;
