(* Minimization
 * Generates the minimized diff script (created by CDiff)
 * Basically an ocaml implementation of delta debugging,
 * specifically for our CDiff-generated script. Used to
 * find the 1-minimal subset for repairs, which will also
 * make applying them to the original file significantly easier
 * and more accurate. *)
open Cil
open Rep
open Cilrep
open Global
open Cdiff
open Printf

let apply_diff_script = ref ""

let _ =
  options := !options @
  [
    "--apply-diff", Arg.Set_string apply_diff_script, " Apply a diff script\n";
  ] 

let whitespace = Str.regexp "[ \t\n]+"

module DiffElement =
  struct
    type t = int * string
    let compare (x,_) (y,_) = x - y
  end
module DiffSet = Set.Make(DiffElement)

(* Read in a diff script to be minimized and store it. *)
let read_in_diff_script filename = begin
  let c = open_in filename in
  let res = ref [] in
  try
    while true do
      let next_line = input_line c in
		res :=  next_line :: !res
    done;
    close_in c; List.rev !res
  with End_of_file -> close_in c;
	List.rev !res
end

(* For repair, split the diff script based on newlines (it's from a string buffer) *)
let diff_script_from_repair the_script = 
  let newline = Str.regexp "\n" in
   Str.split newline the_script 

(* Turn a list of strings into a list of pairs, (string1 * string2), where string1 is unique *)
let split str =
 let whitespace = Str.regexp " " in
 let split_str = Str.split whitespace str in
 match split_str with
 | [a; b; c; d] -> a, a^" "^b^" "^c^" "^d
 | _ -> assert(false)

let script_to_pair_list a =
 let result = List.fold_left (fun (acc : (string * (string list)) list) (ele : string) ->
     let (a : string),(b : string) = split ele in
     match acc with
     | (a',b') :: tl when a=a' -> (a',(b'@[b])) :: tl
     | x -> (a,[b]) :: x
 ) [] a in
 result


exception Test_Failed
(* Run all the tests on the representation. Return true if they all pass. *)

let run_all_tests my_rep = begin
  let result = ref true in
begin
  try
    for i = 1 to !neg_tests do
      let res, _ = my_rep#test_case (Negative i) in
      if not res then raise Test_Failed
    done;
    let sorted_sample, sample_size = 
	  if !sample < 1.0 then begin
		let sample_size = int_of_float (
          max ((float !pos_tests) *. !sample) 1.0) 
      (* always sample at least one test case *) 
		in
		let random_pos = random_order (1 -- !pos_tests) in
          List.sort compare (first_nth random_pos sample_size), sample_size
      end else 
		(1 -- !pos_tests), !pos_tests
	in
	  liter 
		(fun i -> 
		  let res, _ = my_rep#test_case (Positive i) in
			if not res then raise Test_Failed) sorted_sample;
  if sample_size < !pos_tests then begin
      (* If we are sub-sampling and it looks like we have a candidate
       * repair, we must run it on all of the rest of the tests to make
       * sure! *)  
    let rest_tests = List.filter (fun possible_test -> 
      not (List.mem possible_test sorted_sample)) (1 -- !pos_tests)
    in 
	  assert((llen rest_tests) + (llen sorted_sample) = !pos_tests);
	  liter (fun pos_test ->
		let res, _ = my_rep#test_case (Positive pos_test) in
	      if not res then raise Test_Failed) rest_tests
    end;
  with Test_Failed -> result := false;
end;
  !result
end


let debug_diff_script the_script = begin
  List.iter (fun x -> debug "%s\n" x) the_script
end

(* exclude_line
 * utility function. Returns a list containing
 * a copy of the list passed to it, minus the line
 * indicated by the second parameter. (int) *)
let exclude_line the_list to_exclude = begin
  let ret_value = ref [] in
  let count = ref 0 in
  List.iter (fun x ->
	if (!count)!=to_exclude then 
	(
		ret_value := x :: !ret_value;
		incr count
	)
	else incr count;
	) the_list;
  List.rev !ret_value
end

let write_script to_write name = begin
  let outc = open_out name in
  List.iter (fun x -> Printf.fprintf outc "%s\n" x) to_write;
  close_out outc
end

(* Utility function to return a string representation of a given
 * integer in binary. *)
let in_binary num = begin
  let result = ref "" in
  let rec calc n =
    if n<=1 then begin
      result := (string_of_int (n)) ^ (!result);
      n
    end
    else begin
      result := (string_of_int (n mod 2)) ^ (!result);
      calc (n/2)
    end
  in
  ignore (calc num);
  !result
end

(* Print out files to disk. Used to output minimized repaired files. *)
let print_files files_to_cilfiles dirname = begin
  List.iter(fun (fn,cilfile) ->
    let fn = dirname^"/"^fn in
    ensure_directories_exist fn;
    let oc = open_out fn in
    iterGlobals cilfile (fun glob ->
      ignore(Pretty.fprintf oc "%a\n" dn_global glob); ());
    close_out oc
  ) files_to_cilfiles
end

(* process_representation
 * As input, take in the original and repaired variant
 * representations, a diff script to pass to Cdiff,
 * and names for both the temporary "minimized" representation
 * and the temporary "minimized" diff script. Use these
 * to perform the basic process to create a representation,
 * pass it through cdiff, and see if it passes all test cases.
 * If it does pass them all, return true, otherwise return false.
 ****
 * This is a "helper" method for testing; run_all_tests is an internal
 * method for running test cases, while this is the method which should
 * get called by whatever minimization method you're using. *)
let file_ht = hcreate 10
let iteration = ref 0
let process_representation orig node_map diff_script diff_name is_sanity = begin
(* Call structural differencing to reset the data structures in Cdiff. *)
(* Create the directories for the source and diff temporary files, if they don't exist. *)
  (* Arbitrary diffscript (list of strings) to string * (string list) list *)

  let files_and_cilfiles = ref [] in

  List.iter (fun (filename, file_script) ->
    (* Goal: Iterate over each file's diff script. Build it up as final_diff_script.
     * Call usediff on each one of these individually. Append "-filename" to the script
     * and source names to identify them. *)
    (* First pass - remove subdirectories *)
 (* Printf.printf "%s\n" filename; *)

    let filename_without_slashes =
      if (String.contains filename '/') then
	List.hd (List.rev (Str.split (Str.regexp "/") filename))
      else filename
    in

(*  Printf.printf "%s\n" filename_without_slashes; *)
    let diff_name = (diff_name^"-"^(Filename.chop_extension filename_without_slashes)) in
    ensure_directories_exist diff_name;
    write_script file_script diff_name;
    let temp_channel = open_in diff_name in
    let cdiff_data_ht_copy = copy Rep.cdiff_data_ht in
    (* Second pass - include prefix if necessary *)

    let filename_for_rep =
      if (!use_subdirs) then (!prefix^"/"^filename) else filename
    in
	let f1 = if hmem file_ht filename_for_rep then hfind file_ht filename_for_rep else
		begin let f1 = Frontc.parse filename_for_rep () in 
				hadd file_ht filename_for_rep f1; f1
		end
	in
    let the_cilfile = Cdiff.repair_usediff f1 node_map file_script cdiff_data_ht_copy in
    close_in temp_channel;
    files_and_cilfiles := (filename,the_cilfile) :: !files_and_cilfiles;
  ) (script_to_pair_list diff_script);

    let the_rep = orig#copy() in
    the_rep#from_source_min !files_and_cilfiles;
    let res = run_all_tests the_rep in
    the_rep#cleanup (); 
    if (is_sanity) then 
	  begin 
		let old_subdirs = !use_subdirs in
		  use_subdirs := true; 
		let subdir = add_subdir(Some("Minimization_Files/original")) in 
		let filename = Filename.concat subdir ("original."^ !Global.extension^ !Global.suffix_extension ) in
		  orig#output_source filename;
		let subdir = add_subdir(Some("Minimization_Files/minimized")) in 
		let filename = Filename.concat subdir ("minimized."^ !Global.extension^ !Global.suffix_extension ) in
		  the_rep#output_source filename;
		  use_subdirs := old_subdirs
	  end; 
true
end


let delta_set_to_list the_set = begin
  let result = ref [] in
  DiffSet.iter (fun (_,x) ->
    result := x :: !result;
  ) the_set;
  List.rev !result
end

(* Implementation of delta-debugging for efficient diff script minimization.
 * This is all that should ever be used. You don't want 2^N temp files do you?? *)
let delta_count = ref 0

let stmt_id_to_loc = hcreate 10

class statementLocVisitor = object
  inherit nopCilVisitor
  method vstmt stmt = 
    hadd stmt_id_to_loc stmt.sid !currentLoc; DoChildren
end


let res = ref (locUnknown)

class findLocVisitor = object
  inherit nopCilVisitor
  method vstmt stmt = 
    if !currentLoc <> locUnknown then (res := !currentLoc; SkipChildren)
    else DoChildren
end

let loc_visitor = new statementLocVisitor 
let find_loc_visitor = new findLocVisitor
let apply_diff orig node_map diff_file =
  let actions = ref [] in
  let fin = open_in diff_file in
	(try while true do
		actions := (input_line fin) :: !actions
	  done with End_of_file -> close_in fin);
    actions := lrev !actions;
	process_representation orig (copy node_map) !actions "this_diff" true

let delta_debugging orig to_minimize node_map = begin
  (* CLG: hideous hack *)
  if !apply_diff_script <> "" then begin
    (apply_diff orig node_map !apply_diff_script); []
  end else begin
  let counter = ref 0 in
  let c =
	lfoldl
	(fun c ->
	  fun x ->
		let c = 
		  DiffSet.add ((!counter),x) c in
    incr counter; c
  ) (DiffSet.empty) to_minimize in
  
  let rec delta_debug c n =
    incr delta_count;
    debug "Entering delta, pass number %d...\n" !delta_count;
    let count = ref 0 in
    let ci_array = Array.init n (fun _ -> DiffSet.empty) in
	  match n<=(DiffSet.cardinal c) with
	  | true -> begin
		DiffSet.iter (fun (num,x) ->
  		  ci_array.(!count mod n) <- DiffSet.add (num,x) ci_array.(!count mod n);
  		  incr count
		) c;
		let ci_list = Array.to_list ci_array in
		let diff_name = "Minimization_Files/delta_temp_scripts/delta_temp_script_"^(string_of_int !delta_count) in
		  try
			let ci = List.find (fun c_i -> 
			  process_representation orig (copy node_map) (delta_set_to_list c_i) diff_name false) ci_list in
			  delta_debug ci 2
		  with Not_found -> begin
			try
			  let ci = List.find (fun c_i ->
				process_representation orig (copy node_map) (delta_set_to_list (DiffSet.diff c c_i)) diff_name false) ci_list in
				delta_debug (DiffSet.diff c ci) (max (n-1) 2);
			with Not_found -> begin
			  if n < ((DiffSet.cardinal c)) then 
				delta_debug c (min (2*n) (DiffSet.cardinal c))
			  else c
			end
		  end
	  end
	  | false -> c
  in
  let minimized_script = delta_debug c 2 in
  debug "MINIMIZED DIFFSCRIPT: (after %d iterations) \n" !delta_count;
  DiffSet.iter (fun (_,x) -> debug "%s\n" x) minimized_script;
  let output_name = "minimized.diffscript" in
  let minimized = delta_set_to_list minimized_script in
  ensure_directories_exist ("Minimization_Files/full."^output_name);
  write_script minimized ("Minimization_Files/full."^output_name);
  let res = process_representation orig (copy node_map) minimized ("Minimization_Files/"^output_name) true in
  if res then debug "Sanity checking successful!\n" else debug "Sanity checking failed...\n";
	minimized
  end
end

    
  
