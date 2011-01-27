open Batteries
open Random
open Utils
open Globals
open Diffs

exception Quit
type mode = Interactive | Batch | Data_entry

let get_user_feedback (text_file : string) (ht_file : string) big_diff_ht reload =

  let fout_text = open_out text_file in 

  let max_diff = hlen big_diff_ht in
  let random_nums = enum_int max_diff in
  let double_print str =
    pprintf "%s" str; output_string fout_text str; flush stdout; flush fout_text
  in
  let help_text = 
    "Welcome to the diff inspector/feedback collector!  We are happy you are here.\n"^
      "There are three inspector modes: Interactive, Batch, and DataEntry.\n"^
	  "At mode select, enter i, b, or d to select mode.\n"^
	  "At pretty much any prompt, you can enter 'h' to see this message, 'hm' to see help for your current mode, 'cm' to change mode, or 'q' to quit."^
      (Printf.sprintf "A textual log of this session will be saved to %s\n" text_file)^
      (Printf.sprintf "Your saved responses in hashtable form will be serialized to %s\n" ht_file)
  in
  let interactive_help = 
	"This is the interactive mode help\n"^
      "The inspector will ask you how many diffs you want to inspect at once.  Enter an integer at the prompt.\n"^
      "\tIf the inspector is confused, it will default to 1\n"^
      "\tEnter 0 if you want to quit.\n"^
      "The inspector will pick that many diffs at random from the big diff ht that was loaded before you got here.\n"^
      "It will then print them out.  It will be lengthy.  Each diff corresponds to a checkin, so it may list multiple changes to multiple files.\n"^
      "(This is probably a mistake, and Claire will fix it when that fact becomes apparent.)\n"^
      "Type a textual description, hit enter.\n"^
      "Your feedback will be saved.\n"
  in
  let batch_help =
	"This is the batch mode help\n"^
	  "Batch mode allows you to print a bunch of changes to a log file so you can look at them non-interactively."^
	  "You can later use data entry mode to enter in descriptions for a bunch of changes at once.\n"^
	  "At the prompt, enter the number of diffs you'd like to look at.\n"^
	  "A random set of diffs will be printed to the log file.\n"
  in
  let data_entry_help =
	"This is the data entry mode help\n"^
	  "Data entry mode allows you to save a bunch of descriptions at once, probably after using batch mode to print a bunch of changes to inspect\n"^
      "Enter a list of diff ids you'd like to summarize, separated by spaces.\n"^
	  "If you want to summarize individual changes instead, enter diffid-changeid for each change.\n"^
	  "Hit enter, then enter the text you'd like to use to summarize them.\n"
  in
  let print_mode_help = function
  Interactive -> double_print interactive_help
    | Batch -> double_print batch_help
    | Data_entry -> double_print data_entry_help
  in
  let rec get_mode () = 
    double_print "Mode? I for interactive, B for batch, D, for data-entry.\n";
    let user_input = read_line() in
	  match List.hd (Str.split space_regexp (lowercase user_input)) with 
		"i" -> Interactive
	  | "b" -> Batch
	  | "d" -> Data_entry
	  | "q" -> raise (Quit)
	  | "h"  
	  | "hm" -> double_print help_text; get_mode ()
	  | "cm" -> double_print "You're already in get_mode, silly.  Try again\n"; get_mode ()
	  | _ -> double_print "I didn't understand that, try again.\n"; get_mode ()
  in
  let get_new_index () =
	match Enum.get random_nums with
	  Some(n) -> n
	| None -> failwith "You've specified a tragically large number of diffs to look at, and Claire's too lazy to handle this case.\n"
  in
  let print_changes =
    emap
	  (fun diff ->
		double_print (Printf.sprintf "Diff id: %d, benchmark: %s, revision_number: %d.\n" diff.fullid diff.dbench diff.rev_num);
		double_print (Printf.sprintf "LOG MSG: %s\n\n" diff.msg);
		double_print "CHANGES:\n";
		liter
		  (fun change ->
			double_print (Printf.sprintf "Changeid: %d, Filename: %s\n" change.changeid change.fname);
			double_print (Printf.sprintf "%s\n" change.syntactic)) diff.changes; 
		diff.fullid) 
  in
  let generic_user_input mode ?(ifcm=(fun _ -> ())) ifcon =
    let user_input = read_line () in
	  match List.hd (Str.split space_regexp user_input) with
	    "cm" -> ifcm (); double_print "Changing mode...\n"; get_mode ()
	  | "q" -> double_print "Quitting!\n"; raise (Quit)
	  | "hm" -> print_mode_help mode; get_mode ()
	  | "h" -> double_print help_text; get_mode ()
	  | _ -> ifcon user_input
  in
  let get_diffs user_input = 
    let num_diffs = 
	  try
		int_of_string user_input 
	  with _ -> (double_print "I didn't understand that, defaulting to 1\n"; 1)
    in
	  if num_diffs == 0 then 
		begin
		  double_print "You entered 0 for number of diffs, which is secret code for quitting, which is what I'm about to do.\n";
		  raise (Quit)
		end
	  else
		Enum.init num_diffs
		  (fun num ->
			let index = get_new_index () in
			  hfind big_diff_ht index)
  in
  let interactive () = 
	let response_ht = 
	  if reload then begin
		double_print (Printf.sprintf "Trying to load an old response hashtable from %s...\n" ht_file);
		if Sys.file_exists ht_file then 
		  try
			let fin = open_in_bin ht_file in
			let ht = Marshal.input fin in
			  close_in fin; ht
		  with _ ->
			(double_print (Printf.sprintf "WARNING: problem reading from ht_file %s; starting from scratch with a fresh hashtable\n" ht_file);
			 hcreate 10)
		else hcreate 10 
	  end
	  else hcreate 10
	in
	let save_hts () =
	  let fout = open_out_bin ht_file in
		Marshal.output fout response_ht;
		close_out fout
	in
	let rec real_work () = 
	  let rec get_response diffs = 
		double_print "Summarize these for me:\n"; flush stdout;
		let ids = List.of_enum (print_changes diffs) in 
		  generic_user_input (Interactive)
			~ifcm:save_hts
			(fun user_input ->
			  output_string fout_text user_input;
			  flush fout_text;
			  hadd response_ht ids user_input;
			  real_work ())
	  in
		double_print "Number of random diffs to inspect?\n";
		generic_user_input
		  (Interactive)
		  (fun user_input ->
			let diffs = get_diffs user_input in
			  get_response diffs)
	in real_work ()
  in
  let batch () = 
    double_print "Welcome to batch mode!  How many changes would you like me to print out?\n";
    generic_user_input 
	  (Batch)
	  (fun user_input -> 
		let diffs = get_diffs user_input in
	      Enum.force (print_changes diffs); 
	      double_print "Changes printed to log; Now what?\n";
	      get_mode ())
  in
  let data_entry () = failwith "Not implemented"
  in
  let rec input_iter = function
  Interactive -> double_print "Entering interactive mode...\n"; interactive ()
    | Batch -> batch ()
    | Data_entry -> data_entry ()
  in
	try
	  ignore(input_iter (get_mode ()))
	with Quit -> ();
	  double_print "quitting...\n"; flush stdout;
	  close_out fout_text
