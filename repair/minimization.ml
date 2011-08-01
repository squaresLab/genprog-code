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

let write_temp_script to_write name = begin
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
  let base, extension = split_ext !program_to_repair in
  let output_name = base^".minimized.diffscript" in
  let oc = open_out output_name in
  List.iter (fun x ->
    Printf.printf "%s\n" x;
    Printf.fprintf oc "%s\n" x) !new_script;
  close_out oc;
  my_min_script := !new_script
end

(* Just remove lines that don't change the result.
 * Run through the entire script, mark lines that, 
 * if removed, don't change the tests getting passed.
 * Make the minimized_script := my_script - those lines. *)
let naive_delta_debugger rep orig = begin
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
    let the_name = "MIN_temp_script_"^(string_of_int i) in
    let min_temp = "MIN_minimize_temporary_"^(string_of_int i)^".c" in
    write_temp_script !temp_script the_name;
    let temp_channel = open_in the_name in
    let oc = open_out min_temp in

    Cdiff.reset_data ();

    let orig_struct = orig#structural_signature in
    let rep_struct = rep#structural_signature in
    let _ = Rep.structural_difference_to_string orig_struct rep_struct in


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
  write_new_script !excluded_lines
end;;
