open Batteries
open Random
open Utils
open Diffs

let response_ht = hcreate 10 
	  
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

exception QuitEarly

let get_user_feedback (logfile : string) big_diff_ht =
  let ht_file = logfile ^ ".ht" in
  let text_file = logfile ^ ".txt" in 
  let fout_text = open_out text_file in 
  let fout = IO.combine (fout_text, stdout) in
  let save_hts () =
	if logfile <> "" then begin
	  let fout = open_out_bin ht_file in
		Marshal.output fout response_ht;
		close_out fout
	end
  in
  let max_diff = hlen big_diff_ht in
  let random_nums = enum_int max_diff in
  let get_new_index () = 
	match Enum.get random_nums with
	  Some(n) -> n
	| None -> failwith "Impossibly empty random number Enum!"
  in
	try
	  Enum.force
		(Enum.from
		   (fun _ -> 
			 IO.nwrite fout "Number of random diffs to inspect?\n"; flush stdout;
			 let user_input = read_line () in
			 let num_diffs = 
			   try
				 int_of_string user_input 
			   with _ -> (IO.nwrite fout "I didn't understand that, defaulting to 1\n"; 1)
			 in
			 let diffs =
			   Enum.init num_diffs
				 (fun num ->
				   let index = get_new_index () in
					 hfind big_diff_ht index) 
			 in
			 let print_changes =
				Enum.map 
				  (fun diff ->
					IO.nwrite fout (Printf.sprintf "Diff id: %d, benchmark: %s, revision_number: %d.\n" diff.fullid diff.dbench diff.rev_num);
					IO.nwrite fout (Printf.sprintf "\t log msg: %s\n\n" diff.msg);
					IO.nwrite fout "\t changes:\n";
					liter
					  (fun change ->
						IO.nwrite fout (Printf.sprintf "\t\tChangeid: %d, Filename: %s\n" change.changeid change.fname);
						IO.nwrite fout (Printf.sprintf "\t\t%s\n" change.syntactic)) diff.changes; 
					diff.fullid) diffs in
			   IO.nwrite fout "Summarize these for me:\n"; flush stdout;
			   let ids = List.of_enum print_changes in 
			   let user_input = read_line () in
				 IO.nwrite fout user_input;
				 IO.flush fout;
			   let split = Str.split space_regexp user_input in 
				 match (List.hd split) with
				   "q"  -> raise (QuitEarly)
				 | _ -> hadd response_ht ids user_input
		   )) 
	with QuitEarly -> ();
	  save_hts ();
	  IO.flush fout;
	  ignore(IO.close_out fout)
