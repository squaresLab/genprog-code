open Batteries
open Random
open Utils
open Diffs

let response_ht = hcreate 10 
let interesting_ht = hcreate 10 
	  
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

let get_user_feedback (max_examples : int) (logfile : string) =
  let save_hts () =
	if logfile <> "" then begin
	  let fout = open_out_bin logfile in
		Marshal.output fout response_ht;
		Marshal.output fout interesting_ht;
		close_out fout
	end
  in
  let max_diff = hlen !big_diff_ht in
  let random_nums = enum_int max_diff in
  let get_new_index () = 
	match Enum.get random_nums with
	  Some(n) -> n
	| None -> failwith "Impossibly empty random number Enum!"
  in
  let last_first = ref (-1) in
  let last_second = ref (-1) in
	try
	  Enum.force
		(Enum.init max_examples
		   (fun iteration -> 
			 let first_change_index = get_new_index () in
			 let second_change_index = get_new_index () in
			 let diff1 = hfind !big_diff_ht first_change_index in
			 let diff2 = hfind !big_diff_ht second_change_index in 
			   
			   pprintf "Diff 1:\n\n";
			   liter
			     (fun change -> pprintf "CHANGE:\n %s\n" change.syntactic) diff1.changes;
			   pprintf "\n\nDiff 2:\n";
			   liter
			     (fun change -> pprintf "CHANGE: %s\n" change.syntactic) diff2.changes;
			   flush stdout;
			   let user_input = read_line () in
			   let split = Str.split space_regexp user_input in 
			   let ranking = int_of_string (List.hd split) in 
				 hadd response_ht (first_change_index,second_change_index) ranking;
				 liter
				   (fun opt ->
					 match (lowercase opt) with
					   "s" -> hadd interesting_ht (first_change_index,second_change_index) ranking
					 | "u" -> 
					   let user_input = read_int () in
						 hrep response_ht (!last_first,!last_second) user_input
					 | "q" -> raise (QuitEarly)) (List.tl split);
				 last_first := first_change_index;
				 last_second := second_change_index
		   )) 
	with QuitEarly -> ();
	  save_hts ()
