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

let options = [
  "--repos", Arg.Set_string repos, "\t URL of the repository.";
  "--rstart", Arg.Set_int rstart, "\t Start revision.  Default: 0.";
  "--rend", Arg.Set_int rend, "\t End revision.  Default: latest.";
  "--test-cluster", Arg.Set_string xy_data, "\t Test data of XY points to test the clustering";
  "--k", Arg.Set_int k, "\t k - number of clusters.  Default: 2.\n"; 
  "--save", Arg.Set_string save_prefix, "\t Prefix for files to save intermediate state to obviate need to call svn like a million times.\n";
  "--load-diffs", Arg.Set_string saved_diffs, "\t Load diff set from file.";
  "--test-distance", Arg.Set test_distance, "\t Test distance metrics\n";
]

let main () = begin
  Random.init (Random.bits ());
  handle_options options usageMsg;
  if !test_distance then begin
	Distance.levenshtein "kitten" "sitting";
	Distance.levenshtein "Saturday" "Sunday";
  end else begin
	if !xy_data <> "" then begin
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
	end else begin
	  let diffs = 
		if !saved_diffs <> "" then Diffs.load_from_saved !saved_diffs 
		else Diffs.get_diffs !repos !rstart !rend in
		if !save_prefix <> "" then 
		  Diffs.save diffs (!save_prefix^".diffinfo");
		(* can we save halfway through clustering if necessary? *)
		ignore(DiffCluster.kmedoid !k diffs)
	end
  end
end ;;

main () ;;
