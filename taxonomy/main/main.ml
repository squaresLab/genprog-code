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
open Diffs
open Cluster
open Distance

let repos = ref ""
let rstart = ref 0
let rend = ref 0
let xy_data = ref ""
let k = ref 2
let save_prefix = ref ""
let saved_diffs = ref ""
let test_distance = ref false 
let usageMsg = "Fix taxonomy clustering.  Right now assumes svn repository.\n"
let diff_files = ref []
let test_change_diff = ref false
let test_cabs_diff = ref false

let options = [
  "--repos", Arg.Set_string repos, "\t URL of the repository.";
  "--rstart", Arg.Set_int rstart, "\t Start revision.  Default: 0.";
  "--rend", Arg.Set_int rend, "\t End revision.  Default: latest.";
  "--test-cluster", Arg.Set_string xy_data, "\t Test data of XY points to test the clustering";
  "--k", Arg.Set_int k, "\t k - number of clusters.  Default: 2.\n"; 
  "--save-diffs", Arg.Set_string save_prefix, "\t Prefix for files to save intermediate state to obviate need to call svn like a million times.\n";
  "--load-diffs", Arg.Set_string saved_diffs, "\t Load diff set from file.";
  "--test-distance", Arg.Set test_distance, "\t Test distance metrics\n";
  "--test-cd", Arg.Set test_change_diff, "\t Test change diffing\n";
  "--test-cabs-diff", Arg.Set test_cabs_diff, "\t Test C snipped diffing\n";
  "--logfile", Arg.Set_string svn_log_file_in, "\t file containing the svn log\n";
  "--writelog", Arg.Set_string svn_log_file_out, "\t file to which to write the svn log\n";
  "--diffht", Arg.Set_string diff_ht_file, "\t file from which and to which to read/write basic diff information\n";
]

let main () = 
  begin
	 Random.init (Random.bits ());
	let config_files = ref [] in
	let handleArg1 str = config_files := str :: !config_files in 
	let handleArg str = diff_files := str :: !diff_files in 
	let aligned = Arg.align options in
      Arg.parse aligned handleArg1 usageMsg ;
	  liter (parse_options_in_file ~handleArg:handleArg options usageMsg) !config_files;
	  Arg.parse aligned handleArg usageMsg;
	  (begin
		if !test_distance then
		  begin
			 Distance.levenshtein "kitten" "sitting";
			 Distance.levenshtein "Saturday" "Sunday";
		   end else 
			begin
			   if !xy_data <> "" then 
				 (begin
					let lines = File.lines_of !xy_data in
					let points = 
					  Set.of_enum 
						(Enum.map 
						   (fun line -> 
							  let split = Str.split comma_regexp line in
							  let x,y = (int_of_string (hd split)), (int_of_string (hd (tl split))) in
								XYPoint.create x y 
						   ) lines)

					in
					  ignore(TestCluster.kmedoid !k points)
				  end) else 
				   (begin
					  if !test_cabs_diff then 
						Treediff.test_diff_cabs (lrev !diff_files)
					  else
						if !test_change_diff then 
						  Treediff.test_diff_change (lrev !diff_files)
						else
						  begin
							 let diffs = 
							   if !saved_diffs <> "" then Diffs.load_from_saved !saved_diffs 
							   else Diffs.get_diffs !svn_log_file_in !svn_log_file_out !repos !rstart !rend in
							   if !save_prefix <> "" then 
								 Diffs.save diffs (!save_prefix^".diffinfo");
							   (* can we save halfway through clustering if necessary? *)
							   ignore(DiffCluster.kmedoid !k diffs)
						   end
					end)
			 end
	  end)
   end ;;

main () ;;
