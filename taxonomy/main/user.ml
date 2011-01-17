open Batteries
open Random
open Utils
open Diffs

let response_ht = hcreate 10 

let get_user_feedback (collect_diffs_first : bool) (max_examples : int) (logfile : string) =
  if collect_diffs_first then begin
	() (* FIXME *)
  end else begin
	let max_diff = Hashtbl.length !diff_tbl in
	let random_nums = enum_int max_diff in
	let get_new_index () = 
	  match Enum.get random_nums with
		Some(n) -> n
	  | None -> failwith "Impossibly empty random number Enum!"
	in
	let log_file = open_out logfile in
	  Enum.init max_examples
		(fun iteration -> 
		  let first_diff_index = get_new_index () in
		  let second_diff_index = get_new_index () in
		  let diff1 = hfind !diff_tbl first_diff_index in
		  let diff2 = hfind !diff_tbl second_diff_index in 
  end
