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

let repos = ref ""
let rstart = ref 0
let rend = ref 0
let xy_data = ref ""

let usageMsg = "Fix taxonomy clustering.  Right now assumes svn repository.\n"

let options = [
  "--repos", Arg.Set_string repos, "\t URL of the repository.";
  "--rstart", Arg.Set_int rstart, "\t Start revision.  Default: 0.";
  "--rend", Arg.Set_int rend, "\t End revision.  Default: latest.";
  "--test-cluster", Arg.Set_string xy_data, "\t Test data of XY points to test the clustering"
]


let main () = begin
  Random.init (Random.bits ());
  handle_options options usageMsg;
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
	  pprintf "made data set\n"; flush stdout;
	  ignore(TestCluster.kmedoid 2 points)
  end else ignore(Diffs.get_diffs !repos !rstart !rend)

end ;;

main () ;;
