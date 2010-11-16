(* step 1: given a project, a URL, and a start and end revision,
 * collect all changes referencing bugs, bug numbers, or "fix."
 * 1a: diff option 1: tree-based diffs
 * 1b: diff option 2: syntactic (w/alpha-renaming)
 * step 2: process each change
 * step 3: cluster changes (distance metric=what is Ray doing/Hamming
 * distance from Gabel&Su, FSE 10?)
 *)

open Batteries
open Utils
open Diffs

let repos = ref ""
let rstart = ref 0
let rend = ref 0

let usageMsg = "Fix taxonomy clustering.  Right now assumes svn repository.\n"

let options = [
  "--repos", Arg.Set_string repos, "\t URL of the repository.";
  "--rstart", Arg.Set_int rstart, "\t Start revision.  Default: 0.";
  "--rend", Arg.Set_int rend, "\t End revision.  Default: latest.";
]


let main () = begin
  handle_options options usageMsg;
  ignore(Diffs.get_revs !repos !rstart !rend)
(*	Diffs.get_diffs !repos fix_revs*)
	

end ;;

main () ;;
