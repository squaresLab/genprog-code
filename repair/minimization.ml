(* Minimization
 * Generates the minimized diff script (created by CDiff)
 * Basically an ocaml implementation of delta debugging,
 * specifically for our CDiff-generated script. Used to
 * find the 1-minimal subset for repairs, which will also
 * make applying them to the original file significantly easier
 * and more accurate. *)
open Rep
open Cilrep
open Global
open Cdiff
open Printf

let whitespace = Str.regexp "[ \t\n]+"

module DiffElement =
  struct
    type t = int * string
    let compare (x,_) (y,_) = x - y
  end
module DiffSet = Set.Make(DiffElement)

(* Store the script as a list of strings *)
let my_script = ref []
let my_min_script = ref []

(* Read in a diff script to be minimized and store it. *)
let read_in_diff_script filename = begin
  let c = open_in filename in
  try
    while true do
      let next_line = input_line c in
      my_script := next_line :: !my_script;
    done;
    close_in c;
  with End_of_file -> close_in c;
  my_script := List.rev (!my_script);
  ()
end

(* Alternatively just pass a list of strings as the diff script. *)
let set_diff_script the_script = begin
  my_script := the_script
end

(* For repair, split the diff script based on newlines (it's from a string buffer) *)
let diff_script_from_repair the_script = begin
  let newline = Str.regexp "\n" in
  let split_script = Str.split newline the_script in
  List.iter (fun x -> my_script := x :: !my_script) split_script;
  my_script := List.rev (!my_script)
end

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
    for i = 1 to !pos_tests do
      let res, _ = my_rep#test_case (Positive i) in
      if not res then raise Test_Failed
    done;
    ();
  with Test_Failed -> result := false; ();
end;
  !result
end

let debug_diff_script the_script = begin
  List.iter (fun x -> Printf.printf "%s\n" x) the_script
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

let write_new_script excluded_line_list = begin
  let count = ref 0 in
  let new_script = ref [] in
  List.iter (fun x ->
    if not (List.mem !count excluded_line_list) then
    (
      new_script := x :: !new_script;
      incr count
    )
    else incr count;
  ) !my_script;
  new_script := List.rev !new_script;
  let base, extension = split_ext !program_to_repair in
  let output_name = base^".minimized.diffscript" in
  ensure_directories_exist ("Minimization_Files/"^output_name);
  let oc = open_out ("Minimization_Files/"^output_name) in
  List.iter (fun x ->
    Printf.printf "%s\n" x;
    Printf.fprintf oc "%s\n" x) !new_script;
  close_out oc;
  my_min_script := !new_script
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
let process_representation orig rep diff_script source_name diff_name = begin
(* Call structural differencing to reset the data structures in Cdiff. *)
Printf.printf "Entering process rep...\n";
  Cdiff.reset_data () ;
  let file_list = ref [] in
  let check_list = ref [] in
  let orig_struct = orig#structural_signature in
  let rep_struct = rep#structural_signature in

 Printf.printf "Check\n";
(* This is of type string * (string * (Cdiff.edit_action) list) list) list *)
  let all_files = Rep.structural_difference_edit_script orig_struct rep_struct in
(* Create the directories for the source and diff temporary files, if they don't exist. *)
  ensure_directories_exist source_name;
  ensure_directories_exist diff_name;
  (* Arbitrary diffscript (list of strings) to string * (string list) list *)

  let file_to_script = ref [] in
  file_to_script := script_to_pair_list diff_script;

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
    let source_name = (source_name^"-"^(Filename.chop_extension filename_without_slashes)^".c") in
    file_list := source_name :: !file_list;
    check_list := filename_without_slashes :: !check_list;
    write_script file_script diff_name;
    let temp_channel = open_in diff_name in
    let oc = open_out source_name in
    let cdiff_data_ht_copy = copy Rep.cdiff_data_ht in
    (* Second pass - include prefix if necessary *)
    let filename_for_rep =
      if (!use_subdirs) then (!prefix^"/"^filename) else filename
    in
    Cdiff.repair_usediff temp_channel cdiff_data_ht_copy filename_for_rep oc ;
    close_in temp_channel;
    close_out oc;
  ) !file_to_script;

    List.iter (fun (afile,_) ->
        let afile_without_slashes =
	  if (String.contains afile '/') then
	    List.hd (List.rev (Str.split (Str.regexp "/") afile))
	  else afile
        in
	if not (List.mem afile_without_slashes !check_list) then 
		file_list := (if !multi_file then (Filename.concat !prefix afile) else afile) :: !file_list
    ) all_files;
    List.iter (fun x -> Printf.printf "%s\n" x) !file_list;
    write_script !file_list "minfiles.txt";
    min_flag := true; 
    let the_rep = new Cilrep.cilRep in
    the_rep#from_source "minfiles.txt";
    min_flag := false;
Printf.printf "Leaving process rep to run the tests...\n";
    (run_all_tests the_rep)
end


(* The brute force version of minimization. Try every possible subset
 * of changes, maintaining the order of the original diffscript. Of the
 * ones that work, take the smallest as the minimized diffscript. O(2^n)
 * time, trollface *)
let brute_force_minimize rep orig = begin
  let good_scripts = ref [] in
  let temporary_script = ref [] in
  let count = ref 0 in
  let the_size = List.length (!my_script) in
  let all_subsets = 2.0 ** (float_of_int the_size) in
  let num_of_digits = String.length (in_binary ((int_of_float all_subsets) - 1)) in
  for i=1 to ((int_of_float all_subsets) - 1) do
    let the_value = ref (in_binary i) in 
    let deficiency = num_of_digits - (String.length !the_value) in
    if (deficiency!=0) then
      for i=1 to deficiency do
	the_value := "0" ^ !the_value
      done;
    temporary_script := [];
    count := 0;
    String.iter (fun x -> 
	if (x='1') then temporary_script := (List.nth !my_script !count) :: !temporary_script;
        incr count) !the_value;
    let the_name = "Minimization_Files/bruteforce_temp_scripts/MIN_bruteforce_temp_"^(string_of_int i) in
    let min_temp = "Minimization_Files/bruteforce_temp_files/MIN_bruteforce_minimize_temporary_"^(string_of_int i) in
    if (process_representation orig rep (List.rev !temporary_script) min_temp the_name) then (
      good_scripts := (List.rev !temporary_script) :: !good_scripts
    );
  done;
  let min_length = ref 100 in
  let min_index = ref 0 in
  let counter = ref 0 in
  List.iter (fun x -> if (List.length x)<(!min_length) then begin
	min_length := (List.length x);
	min_index := !counter
      end;
      incr counter
  ) !good_scripts;
  Printf.printf "MINIMIZED DIFFSCRIPT IS:\n";
  List.iter (fun y -> Printf.printf "%s\n" y) (List.nth !good_scripts !min_index);

  let base, extension = split_ext !program_to_repair in
  let output_name = base^".minimized.diffscript" in
  ensure_directories_exist ("Minimization_Files/"^output_name);
  write_script (List.nth !good_scripts !min_index) ("Minimization_Files/"^output_name);
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

let delta_debugging rep orig = begin
  let counter = ref 0 in
  let c = DiffSet.empty in
  let c' = ref DiffSet.empty in
  List.iter (fun x ->
    c' := DiffSet.add ((!counter),x) !c';
    incr counter
  ) !my_script;

  let rec delta_debug c c' n =
    incr delta_count;
    let cprime_minus_c = DiffSet.diff c' c in
    let count = ref 0 in
    let ci_array = Array.init n (fun _ -> DiffSet.empty) in
  
    if n<=(DiffSet.cardinal cprime_minus_c) then
    (
      DiffSet.iter (fun (num,x) ->
  	ci_array.(!count mod n) <- DiffSet.add (num,x) ci_array.(!count mod n);
  	incr count
      ) cprime_minus_c;
    let ci_list = Array.to_list ci_array in
    let diff_name = "Minimization_Files/delta_temp_scripts/delta_temp_script_"^(string_of_int !delta_count) in
    let source_name = "Minimization_Files/delta_temp_files/delta_minimize_temporary_"^(string_of_int !delta_count) in
    try
      let ci = List.find (fun c_i -> 
	process_representation orig rep (delta_set_to_list (DiffSet.union c c_i)) source_name diff_name) ci_list in
      delta_debug c (DiffSet.union c ci) 2;
    with Not_found -> (
      try
	let ci = List.find (fun c_i ->
	  process_representation orig rep (delta_set_to_list (DiffSet.diff c' c_i)) source_name diff_name) ci_list in
	delta_debug c (DiffSet.diff c' ci) (max (n-1) 2);
      with Not_found -> (
	if (n < ((DiffSet.cardinal c') - (DiffSet.cardinal c))) then (
	  delta_debug c c' (min (2*n) ((DiffSet.cardinal c') - (DiffSet.cardinal c)))
	)
	else c, c'
      )
    )
  )
  else c, c'
  in
  let _, minimized_script = delta_debug c (!c') 2 in
  Printf.printf "MINIMIZED DIFFSCRIPT: (after %d iterations) \n" !delta_count;
  DiffSet.iter (fun (_,x) -> Printf.printf "%s\n" x) minimized_script;
  let output_name = "minimized.diffscript" in
  ensure_directories_exist ("Minimization_Files/full."^output_name);
  write_script (delta_set_to_list minimized_script) ("Minimization_Files/full."^output_name);
  my_script := (delta_set_to_list minimized_script);
  let res = process_representation orig rep (delta_set_to_list minimized_script) "Minimization_Files/delta_sanity" ("Minimization_Files/"^output_name) in
  if res then Printf.printf "Sanity checking successful!\n" else Printf.printf "Sanity checking failed...\n"
end

(* Just remove lines that don't change the result.
 * Run through the entire script, mark lines that, 
 * if removed, don't change the tests getting passed.
 * Make the minimized_script := my_script - those lines. *)
let naive_delta_debugger rep orig (rep_struct : structural_signature) (orig_struct : structural_signature) = begin

(*
    write_temp_script !my_script "origscript";
    let temp_channel = open_in "origscript" in
    let oc = open_out "origresult" in

    (* Cdiff.reset_data (); *)
(*
    let orig_s = rep#structural_signature in
    let rep_s = orig#structural_signature in
*)			    
       let _ = Rep.structural_difference_to_string orig_struct rep_struct in 

    (* Cdiff.debug_node_min (); *)
     Printf.printf "sanity checking\n";

    let cdiff_data_ht_copy = copy Rep.cdiff_data_ht in
    Cdiff.repair_usediff temp_channel cdiff_data_ht_copy !program_to_repair oc ;
    close_in temp_channel;
    close_out oc;
    Printf.printf "sanity checking successful\n";
*)


  (* Cil.printCilAsIs := true; *)



  let excluded_lines = ref [] in
  let temp_script = ref !my_script in
  for i=0 to ((List.length !my_script) - 1) do
    Printf.printf "Run number %d...\n" i;
    temp_script := (exclude_line !my_script i);
       Printf.printf "Temp script is now:\n" ;
       debug_diff_script !temp_script;
       print_newline ();
    (* Create the new representation by applying CDiff to the
       * original one, using the temporary script. *)
    let the_name = "Minimization_Files/temp_scripts/MIN_temp_script_"^(string_of_int i) in
    let min_temp = "Minimization_Files/temp_files/MIN_minimize_temporary_"^(string_of_int i)^".c" in
    ensure_directories_exist the_name;
    ensure_directories_exist min_temp;
    write_script !temp_script the_name;
    let temp_channel = open_in the_name in
    let oc = open_out min_temp in


Cdiff.reset_data () ;
    let orig_s = orig#structural_signature in
    let rep_s = rep#structural_signature in
    let _ = Rep.structural_difference_to_string orig_s rep_s in
								     

    (* Cdiff.debug_node_min (); *)

    let cdiff_data_ht_copy = copy Rep.cdiff_data_ht in
    Cdiff.repair_usediff temp_channel cdiff_data_ht_copy !program_to_repair oc ;
    close_in temp_channel;
    close_out oc;
    let the_rep = new Cilrep.cilRep in
    the_rep#from_source min_temp;
    if (run_all_tests the_rep) then (
      excluded_lines := i :: !excluded_lines ;
      Printf.printf "Line %d is excluded\n" i;
    );
    temp_script := !my_script;
    Printf.printf "Finished a run...\n"
  done;
  write_new_script !excluded_lines;
 

(*
  (* Code to ensure that the created diff script actually works *)
  write_temp_script !my_script "origscript";
  let temp_channel = open_in "origscript" in
  let oc = open_out "origresult.c" in
  Cdiff.reset_data () ;
    let orig_s = orig#structural_signature in
    let rep_s = rep#structural_signature in
    let _ = Rep.structural_difference_to_string orig_s rep_s in

    (* Cdiff.debug_node_min (); *)
     Printf.printf "sanity checking\n";

    let cdiff_data_ht_copy = copy Rep.cdiff_data_ht in
    Cdiff.repair_usediff temp_channel cdiff_data_ht_copy !program_to_repair oc ;
    close_in temp_channel;
    close_out oc;

    let final_rep = new Cilrep.cilRep in
    final_rep#from_source "origresult.c";
    if (run_all_tests final_rep) then Printf.printf "sanity checking successful\n"
    else Printf.printf "sanity checking NOT successful. SHITTT!\n";
							*)
       
end;;
