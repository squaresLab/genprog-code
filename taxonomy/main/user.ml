open Batteries
open Random
open Utils
open Globals
open Difftypes

exception Quit
type mode = No_mode | Interactive | Batch | Data_entry

let get_user_feedback (text_file : string) (ht_file : string) big_diff_ht reload =
  let text_file = 
    if Sys.file_exists text_file then begin
      pprintf "File %s exists; if we use it, we will overwrite whatever's already there.  Is this OK?\n" text_file; flush stdout;
      let user_input = read_line () in
		match (List.hd (Str.split space_regexp (lowercase user_input))) with
		  "y" -> pprintf "Cool, using %s as the logfile...\n" text_file; flush stdout; text_file
		| "q" -> pprintf "If you really want to quit from here, try CTRL-C; otherwise, enter another file name\n"; flush stdout; read_line ()
		| _ -> pprintf "What would you prefer?\n"; flush stdout; read_line ()
    end else text_file
  in
  let fout_text = open_out text_file in 
    
  let max_diff = hlen big_diff_ht in
  let random_nums = enum_int max_diff in
  let double_print str =
    pprintf "%s" str; output_string fout_text str; flush stdout; flush fout_text
  in
  let help_text = 
    "\n\nWelcome to the diff inspector/feedback collector!  We are happy you are here.\n"^
      "There are three inspector modes: Interactive, Batch, and DataEntry.\n"^
      "At mode select, enter i, b, or d to select mode.\n"^
      "At pretty much any prompt, enter 'h' to see this message, 'hm' to see help for your current mode, 'cm' to change mode, 'p' to print the current response hashtable, or 'q' to quit.\n"^
      (Printf.sprintf "A textual log of this session will be saved to %s\n" text_file)^
      "NOTE: while the text log is continuously updated, be careful with the response_ht file, which is written when you quit or change modes."^
      "\t Arbitrary quitting with CTRL-C not recommended\n"
  in
  let interactive_help = 
	"\n\nThis is the interactive mode help\n"^
      "The inspector will ask you how many diffs you want to inspect at once.  Enter an integer at the prompt. \n"^
      "\tIf the inspector is confused, it will default to 1 \n"^
      "\tEnter 0 if you want to quit.\n"^
      "The inspector will pick that many diffs at random from the big diff ht that was loaded before you got here.\n"^
      "It will then print them out.  It will be lengthy.  Each diff corresponds to a checkin, so it may list multiple changes to multiple files.\n"^
      "(This is probably a mistake, and Claire will fix it when that fact becomes apparent.)\n"^
	  "(If you really want to limit your descriptions to particular changes, try batch/data entry modes.)\n"^
      "Type a textual description, hit enter. \n"^
      (Printf.sprintf "Your saved responses in hashtable form will be serialized to %s\n" ht_file)
  in
  let batch_help =
	"\n\nThis is the batch mode help\n"^
	  "Batch mode allows you to print a bunch of changes to a log file so you can look at them non-interactively."^
	  "You can later use data entry mode to enter in descriptions for a bunch of changes at once.\n"^
	  "At the prompt, enter the number of diffs you'd like to look at.\n"^
	  "A random set of diffs will be printed to the log file.\n"
  in
  let data_entry_help =
	"\n\nThis is the data entry mode help\n"^
	  "Data entry mode is currently not implemented, so if you ask for it it will choke.\n"^
	  "Data entry mode allows you to save a bunch of descriptions at once, probably after using batch mode to print a bunch of changes to inspect\n"^
      "Enter a list of diff ids you'd like to summarize, separated by spaces.\n"^
	  "If you want to summarize individual changes instead, enter diffid-changeid for each change.\n"^
	  "Hit enter, then enter the text you'd like to use to summarize them.\n"
  in
  let print_mode_help = function
  Interactive -> double_print interactive_help
    | Batch -> double_print batch_help
    | Data_entry -> double_print data_entry_help
    | No_mode -> double_print help_text
  in
  let rec get_line () =
    let user_input = read_line() in
      if (String.length user_input) > 0 then user_input 
      else get_line ()
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
			double_print (Printf.sprintf "%s\n" "FIXME CLAIREBEAR")) diff.changes; 
		diff.fullid,-1) 
  in
  let get_diffs user_input = 
    let num_diffs = 
	  try
		let num = int_of_string user_input in
		  if num < 1 then
			(double_print "Number of diffs < 1; defaulting to 1\n"; 1)
		  else num
	  with _ -> (double_print "I didn't understand that, defaulting to 1\n"; 1)
    in
	  Enum.init num_diffs
		(fun num ->
		  let index = get_new_index () in
			hfind big_diff_ht index)
  in
  let load_ht reload = 	  
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
  let save_hts response_ht () =
	let fout = open_out_bin ht_file in
	  Marshal.output fout response_ht;
	  close_out fout
  in
  let rec generic_user_input mode ?(ifcm=(fun _ -> ())) ifcon =
    let mode_str = 
      match mode with
	Interactive -> "Interactive"
      | Batch -> "Batch"
      | Data_entry -> "Data entry"
      | No_mode -> "Chose mode"
    in
    let where_were_you =
      Printf.sprintf "\n Now what? You are in %s mode, whatever you were doing before applies here...\n" mode_str 
    in
    let user_input = get_line () in
	  match List.hd (Str.split space_regexp user_input) with
	    "cm" -> ifcm (); double_print "Changing mode...\n";  input_iter (get_mode ())
	  | "q" ->  ifcm(); raise (Quit)
	  | "hm" -> print_mode_help mode; double_print where_were_you; generic_user_input mode ~ifcm:ifcm ifcon
	  | "h" -> double_print help_text; double_print where_were_you; generic_user_input mode ~ifcm:ifcm ifcon
	  | "p" -> 
	      ifcm();
	      let ht = load_ht true in
		hiter (fun k -> fun v -> 
			 double_print "Diff/change-id list:  ";
			 liter (fun (diff,change) ->
				  if change > -1 then
				    double_print (Printf.sprintf "%d-%d, " diff change)
				  else 
				    double_print (Printf.sprintf "%d, " diff)) k;
			 double_print (Printf.sprintf "\n Annotation: %s\n" v)) ht;
		double_print where_were_you;
		generic_user_input mode ~ifcm:ifcm ifcon
	  | _ -> ifcon user_input
  and get_mode () = 
    double_print "Mode? I for interactive, B for batch, D, for data-entry.\n";
    generic_user_input 
      (No_mode)
      (fun user_input ->
		match List.hd (Str.split space_regexp (lowercase user_input)) with 
		  "i" -> Interactive
		| "b" -> Batch
		| "d" -> Data_entry
		| _ -> double_print "I didn't understand that, try again.\n"; get_mode ())
  and annotate mode diffs ht ifcm con =
	double_print "Enter your summary/annotation:\n";
	generic_user_input mode
	  ~ifcm:ifcm 
	  (fun user_input ->
		output_string fout_text (Printf.sprintf "%s\n" user_input);
		flush fout_text;
		hadd ht diffs user_input;
		con ())
  and interactive () = 
	let response_ht = load_ht reload in
	let rec real_work () = 
	  double_print "Number of random diffs to inspect?\n";
	  generic_user_input
		(Interactive)
		~ifcm:(save_hts response_ht)
		(fun user_input ->
		  let diffs = get_diffs user_input in
		  let ids = List.of_enum (print_changes diffs) in
			annotate (Interactive) ids response_ht (save_hts response_ht) real_work)
	in real_work ()
  and batch () = 
    double_print "\nWelcome to batch mode!  How many changes would you like me to print out?\n";
    generic_user_input 
	  (Batch)
	  (fun user_input -> 
		let diffs = get_diffs user_input in
	      Enum.force (print_changes diffs); 
	      double_print "\nChanges printed to log; Now what? Generic options apply...\n";
	      generic_user_input (No_mode) (fun user_input -> double_print "I didn't understand that, try again\n"; (input_iter (get_mode ()))))
  and data_entry () =
	let response_ht = load_ht reload in
	  double_print "\nWelcome to data entry mode! Enter your diffids on one line, separated by spaces.\n";
	  double_print "If you want to specify a particular change for a particular diffid, enter diffid-changeid.\n";
	  double_print "Then hit enter, then enter your annotation, then hit enter.\n";
	  double_print "Rinse, repeat.\n"; 
	    
	  let rec real_work () =
	    double_print "Enter diffids or diffid-changeids, separated by spaces\n";
		generic_user_input
		  (Data_entry)
		  ~ifcm:(save_hts response_ht)
		  (fun user_input ->
			let split = Str.split space_regexp (lowercase user_input) in
			let dash_regexp = Str.regexp_string "-" in
			let diffs = lfoldl
			  (fun diffs ->
			  fun str -> 
			    try
				if any_match dash_regexp str then begin
				  let split = Str.split dash_regexp str in
					  (int_of_string (List.hd split),
					  int_of_string (List.hd (List.tl split))) :: diffs

				end
				else (int_of_string str, -1) :: diffs 
			    with _ -> 
			      begin
				double_print (Printf.sprintf "I didn't understand %s, skipping\n" str); diffs
			      end
			  ) [] split
			in
			  annotate (Data_entry) diffs response_ht (save_hts response_ht) real_work
		  )
	  in
		real_work ()
  and input_iter = function
  Interactive -> double_print "Entering interactive mode...\n"; interactive ()
    | Batch -> batch ()
    | Data_entry -> data_entry ()
    | No_mode -> failwith "Impossible; how did we get to input_iter and no mode?\n"
  in
    try
	  double_print help_text;
	  ignore(input_iter (get_mode ()))
	with Quit -> ();
	  double_print "quitting...\n"; flush stdout;
	  close_out fout_text
