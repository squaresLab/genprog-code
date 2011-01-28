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
open Unix
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
	  "--combine", Arg.Set_string htf, "\t Combine diff files from many benchmarks, listed in X file\n"; 
	  "--ray", Arg.String (fun file -> ray := file), "\t  Ray mode.  X is config file; if you're Ray you probably want \"default\""
	]

let ray_logfile = ref ""
let ray_htfile = ref ""
let ray_bigdiff = ref ("/home/claire/taxonomy/main/test_data/ray_full_ht.bin")
let ray_reload = ref true

let ray_options =
  [
	"--logfile", Arg.Set_string ray_logfile, "Write to X.txt.  If .ht file is unspecified, write to X.ht.";
	"--htfile", Arg.Set_string ray_htfile, "Write response ht to X.ht.";
	"--bigdiff", Arg.Set_string ray_bigdiff, "Get diff information from bigdiff; if bigdiff doesn't exist, compose existing default hts and write to X.";
	"--no-reload", Arg.Clear ray_reload, "Don't read in response ht if it already exists/add to it; default=false"
  ]

exception Reload

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
	  else begin (* all the real stuff *)
		if !ray <> "" then begin
		  pprintf "Hi, Ray!\n";
		  pprintf "%s" ("I'm going to parse the arguments in the specified config file, try to load a big hashtable of all the diffs I've collected so far, and then enter the user feedback loop.\n"^
				  "Type 'h' at the prompt when you get there if you want more help.\n");
		  let handleArg _ = 
			failwith "unexpected argument in RayMode config file\n"
		  in
		  let aligned = Arg.align ray_options in
		  let config_file  =
			if !ray = "default" 
			then "/home/claire/taxonomy/main/ray_default.config"
			else !ray
		  in
			parse_options_in_file ~handleArg:handleArg aligned "" config_file
		end;
		let big_diff_ht,big_diff_id,benches = 
			if (!ray_bigdiff <> "" && Sys.file_exists !ray_bigdiff) || !fullload <> "" then begin
			  let bigfile = if !ray_bigdiff <> "" then !ray_bigdiff else !fullload 
			  in
				full_load_from_file bigfile 
			end else hcreate 10, 0, []
		in
		let big_diff_ht,big_diff_id = 
		  if !htf <> "" || (llen !configs) > 0 then
			let fullsave = 
			  if !ray_bigdiff <> "" then Some(!ray_bigdiff) 
			  else if !fullsave <> "" then Some(!fullsave)
			  else None

			in
			  get_many_diffs !configs !htf fullsave big_diff_ht big_diff_id benches
		  else big_diff_ht,big_diff_id
		in
		let ht_file = 
		  if !ray <> "" then
			if !ray_htfile <> "" then !ray_htfile else !ray_logfile ^".ht"
		  else
			!user_feedback_file^".ht"
		in
		let logfile = 
		  if !ray <> "" then
			let localtime = Unix.localtime (Unix.time ()) in
			  Printf.sprintf "%s.h%d.m%d.d%d.y%d.txt" !ray_logfile localtime.tm_hour (localtime.tm_mon + 1) localtime.tm_mday (localtime.tm_year + 1900)
		  else
			!user_feedback_file^".txt"
		in
		let reload = if !ray <> "" then !ray_reload else false in
		  if !ray <> "" || !user_feedback_file <> "" then
			get_user_feedback logfile ht_file big_diff_ht reload
		  (* can we save halfway through clustering if necessary? *)
(*		  if !cluster then ignore(DiffCluster.kmedoid !k diffs);*)
	  end
  end ;;

main () ;;
