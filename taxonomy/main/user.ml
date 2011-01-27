open Batteries
open Random
open Utils
open Diffs

	  
(* the way this works: working off the giant hashtable containing changes
   (changes, or diffs?  That is the question...) randomly pick two of them, wait
   for and then parse user input.  User input is a scale of 1-5, where 1 is
   "totally dissimilar", 5 is "basically the same thing", and 2 - 4 are in the
   middle.  Number comes first.  If you want to save the pair as particularly
   interesting, follow the number with a space and then a "S" or "s".  If you
   want to change the previous answer, follow all that with a "U" or "u". If you
   want to quit, do "Q" or "q".  Currently, we are ignoring the "alpha renaming"
   part of the show, because I'm too lazy to implement it at the moment.  But it
   is annoying and should be done at some point. *)

exception Quit
type mode = Interactive | Batch | Data_entry

let get_user_feedback (text_file : string) (ht_file : string) big_diff_ht reload =
  let response_ht = 
	if reload then begin
	  pprintf "Trying to load an old response hashtable from %s...\n" ht_file;
	  if Sys.file_exists ht_file then 
		try
		  let fin = open_in_bin ht_file in
		  let ht = Marshal.input fin in
			close_in fin; ht
		with _ ->
		  (begin
			pprintf "WARNING: problem reading from ht_file %s; starting from scratch with a fresh hashtable\n" ht_file; flush stdout;
			hcreate 10
		   end) 
	  else hcreate 10 
	end
	else hcreate 10
  in
  let fout_text = open_out text_file in 
  let save_hts () =
	let fout = open_out_bin ht_file in
	  Marshal.output fout response_ht;
	  close_out fout
  in
  let max_diff = hlen big_diff_ht in
  let random_nums = enum_int max_diff in
  let double_print str =
    pprintf "%s" str; output_string fout_text str; flush stdout; flush fout_text
  in
  let help_text = 
    "Welcome to the diff inspector/feedback collector!  We are happy you are here.\n"^
      "There are three inspector modes: Interactive, Batch, and DataEntry.\n"^
      "The inspector will ask you how many diffs you want to inspect at once.  Enter an integer at the prompt.\n"^
      "\tIf the inspector is confused, it will default to 1\n"^
      "\tEnter 0 if you want to quit.\n"^
      "The inspector will pick that many diffs at random from the big diff ht that was loaded before you got here.\n"^
      "It will then print them out.  It will be lengthy.  Each diff corresponds to a checkin, so it may list multiple changes to multiple files.\n"^
      "(This is probably a mistake, and Claire will fix it when that fact becomes apparent.)\n"^
      "Type a textual description, hit enter.\n"^
      "Your feedback will be saved.\n"^
      "Additional notes:\n"^
      "\t If you want more help, type h and hit enter and this message will reappear.\n"^
      "\t If you want to quit after having been presented with changes to inspect, type q and hit enter.\n"^
      (Printf.sprintf "A textual log of this session will be saved to %s\n" text_file)^
      (Printf.sprintf "Your saved responses in hashtable form will be serialized to %s\n" ht_file)
  in

    let get_new_index () =
	match Enum.get random_nums with
	  Some(n) -> n
	| None -> failwith "Impossibly empty random number Enum!"
    in
    let rec get_mode () = 
      double_print "Mode? I for interactive, B for batch, D, for data-entry.\n";
      let user_input = read_line() in
	match String.get 0 (lowercase user_input) with
	  'i' -> Interactive
	| 'b' -> Batch
	| 'd' -> Data_entry
	| 'q' -> raise (Quit)
	| 'h' -> double_print help; get_mode ()
	| _ -> double_print "I didn't understand that, try again.\n"; get_mode ()
    in
    let mode = get_mode () in
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
	   diff.fullid) in
    let print_mode_help = function
	Interactive -> double_print interactive_help
      | Batch -> double_print batch_help
      | Data_entry -> double_print data_entry_help
    in
    let generic_user_input mode ifcon =
      	let user_input = read_line () in
	  match List.hd (String.split space_regexp user_input) with
	    "cm" -> double_print "Changing mode...\n"; get_mode ()
	  | "q" -> double_print "Quitting!\n" raise (Quit)
	  | "hm" -> print_mode_help mode; get_mode ()
	  | "h" -> double_print help; get_mode ()
	  | _ -> ifcon user_input
    in
    let get_diffs user_input = 
      let num_diffs = 
      try
	int_of_string user_input 
      with _ -> (double_print "I didn't understand that, defaulting to 1\n"; 1)
      in
	if num_diffs == 0 then raise (Quit);
	Enum.init num_diffs
	  (fun num ->
	     let index = get_new_index () in
	       hfind big_diff_ht index) 
    in
    let rec interactive () = 
      double_print "Number of random diffs to inspect?\n";
      (* this double-printing is hideous but I can't get the double-output thing to work *)
      generic_user_input
	(fun user_input ->
	   let diffs = get_diffs user_input in
	   let rec get_response () = 
	     double_print "Summarize these for me:\n"; flush stdout;
	     let ids = List.of_enum print_changes in 
	       generic_user_input (Interactive)
		 (fun user_input ->
		    output_string fout_text user_input;
		    flush fout_text;
		    hadd response_ht ids user_input;
		    interactive ())
	   in
	     get_response ())
    in
    let batch () = 
      double_print "Welcome to batch mode!  How many changes would you like me to print out?\n";
      generic_user_input 
	(fun user_input -> 
	   let diffs = get_diffs user_input in
	     Enum.force (print_changes diffs); 
	     double_print "Changes printed to log; Now what?\n";
	     get_mode ())
    in
    let data_entry_help =
      "Enter a list of diff ids you'd like to summarize, separated by spaces.\n"^
	"If you want to summarize individual changes instead, enter diffid-changeid for each change.\n"^
	"Hit enter, then enter the text you'd like to use to summarize them.\n"
    in
	
    let data_entry () = 
      double_print 
      let user_input 
    let rec input_iter = function
	Interactive -> 
      | Batch ->
      | Data_entry ->
	with Quit -> ();
	  pprintf "quitting...\n"; flush stdout;
	  save_hts ();
	  pprintf "Saved hts, closing\n";
	  close_out fout_text
