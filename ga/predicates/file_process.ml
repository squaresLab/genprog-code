(* This file does input processing for the predicate project.
 * We can read in several types of input, and a fair amount of
 * input/output can be done of just the resolved runs.
 * See main.ml/command line flags for details
 *)

open Globals
open Printf
open List
open String
open Str
open Hashtbl

let cheap_read_run filename = begin
  let fin = open_in filename in
    try 
      while true do 
	
      done
    with _ -> ()
end

(* liblit stuff deprecated *)
(* simplify takes a resolvedSample file, replaces all the tabs with commas,
 * and cuts out the first column, in place. Saves a backup in 
 * filename.backup *)

(* let simplify filename prefix = begin
  let esc_prefix = Str.global_replace slash_regexp "\/" prefix in
  let cmd = if not (prefix = "") then 
    Printf.sprintf "sed -e 's/%s//g' -e 's/\\t/,/g' %s | \\
      cut -d , --fields 2-20 > %s.simplified" esc_prefix filename filename
  else
    Printf.sprintf "sed -e 's/\\t/,/g' %s | cut -d , --fields 2-20 > %s.simplified" 
      filename filename
  in
    ignore(Unix.system cmd);
    Printf.sprintf "%s.simplified" filename
end *)
   
(* print-run prints a concise version of run information of the
 * variety found in Liblit's datasets. Each line is a run. First number
 * is the run_number. Each value after that is a comma-delimited list.
 * First number is the predicate number.
 * after that, the values associated with the predicates.
 * assumes that you will eventually close the out_channel, so don't
 * forget! *)

let print_run (out : out_channel) (filename : string) =
  let output s = output_string out s in
  let run_num = Hashtbl.find !fname_to_run_num filename in (* FIXME: make run num go to filename, if anything *)
  let inner_tbl = Hashtbl.find !run_and_pred_to_res run_num in
    output (sprintf "%d," run_num);
    Hashtbl.iter 
      (fun pred_num -> 
	 fun res -> 
	   List.iter 
	     (fun x -> output (sprintf "%d:" x))
	     (pred_num::(rev (tl (rev res))));
	   output (sprintf "%d," (hd (rev res)))) inner_tbl;
    output "\n";
    flush out

(* read-run reads those runs back into the hashtables. Assumes that
 * the fname_to_run_num_and_good and pred_to_num hashtables are 
 * populated and consistent with the input passed here (e.g.,
 * that -hin was passed and you weren't stupid)
 *)
let read_run (filename : string) = 
  let rec read_preds run preds_list = 
    let read_pred one_pred = 
      let one_pred_split = to_ints (split colon_regexp one_pred) in
      let inner_tbl = 
	if (Hashtbl.mem !run_and_pred_to_res run) then
	  Hashtbl.find !run_and_pred_to_res run
	else
	  create 10 
      in
	add inner_tbl (hd one_pred_split) (tl one_pred_split);
	replace !run_and_pred_to_res run inner_tbl
    in
      match preds_list with
	  p :: ps -> read_pred p; read_preds run ps
	| [] -> ()
  in
  let fin = open_in filename in
    try
      while true do
	let line = input_line fin in
	let split = split comma_regexp line in 
	  read_preds (to_int (hd split)) (tl split);
      done
    with _ ->
      close_in fin; ()

(* conciseify takes a simplified resolvedSample file for one run
 * and adds the info to several hashtables. Every predicate gets
 * a unique idenitifying number that is consistent across all 
 * runs. Every run gets a unique number. The results for a predicate site
 * (actually contains several predicates, like number of times a branch 
 * was true and the number times it was false)
 * on a run are stored as an integer list (order maintained, obviously) 
 * in run_and_pred_to_res 
 *)

(*exception SchemeFail of string
let run_num = ref 0
let predicate_num = ref 0

let conciseify (filename : string) (gorb : string) = 
  let fin = open_in filename in 
  let good = if (get (capitalize gorb) 0) == 'G' then 0 else 1 in (* 1 signifies anomolous run *)
  let run = 
    if not (Hashtbl.mem !fname_to_run_num filename) then begin
      (add !fname_to_run_num filename !run_num);
      (add !run_num_to_fname_and_good !run_num (filename, good));
      incr run_num
    end;
    Hashtbl.find !fname_to_run_num filename 
  in
    (try 
      while true do
	let line = input_line fin in
	let split = Str.split comma_regexp line in
	let scheme = (nth split 1) in
	let (key, results) = 
	  begin
	    let (pred_vals, num_commas) =
	      match scheme with 
		  "returns" -> (last3 split), 2
		| "scalar-pairs" -> (last3 split), 2
		| "branches" -> (last2 split), 1
		| str -> 
		    raise (SchemeFail("Unexpected conciseify scheme name: "^str))
	    in
	    let length_of_vals = (len_str_list pred_vals) + num_commas in
	      (String.sub line 0 ((String.length line) - length_of_vals)), 
	       (to_ints pred_vals)
	  end
	in
	let pred = 
	  if not (Hashtbl.mem !pred_to_num key) then begin
	    add !pred_to_num key !predicate_num;
	    add !num_to_pred !predicate_num key;
	    incr predicate_num
	  end;
	  Hashtbl.find !pred_to_num key
	in
	  
	let inner_tbl = 
	  if (Hashtbl.mem !run_and_pred_to_res run ) then begin
	    Hashtbl.find !run_and_pred_to_res run 
	  end
	  else begin
	    create 10 
	  end
	in 
	  begin
	    add inner_tbl pred results;
	    replace !run_and_pred_to_res run inner_tbl
	  end
      done 
    with SchemeFail(s) -> raise (SchemeFail(s))
      | _ -> ());
  close_in fin
*)
(* get_pred_text converts predicate numbers to human-readable text
 * for the purposes of sensible output. *)

exception CounterFail of int

let get_pred_text pred_num pred_counter = 
  let raw_text =  Hashtbl.find !num_to_pred pred_num in
  let split = Str.split comma_regexp raw_text in 
  let file_name = (nth split 2) in
  let lineno = (nth split 3) in 
  let cfg_node = (nth split 5) in
  let scheme = (nth split 1) in
  let predicate = 
    match scheme with 
	"returns" ->
	  begin
	    let func = (nth split 6) in
	      match pred_counter with
		  0 -> sprintf "Return value of %s was negative" func
		| 1 -> sprintf "Return value of %s was zero" func
		| 2 -> sprintf "Return value of %s was positive" func
		| n -> 
		    raise (CounterFail(n))
	  end
      | "scalar-pairs" ->
	  begin
	    let assigned_value = (nth split 6) in
	    let comp_value = (nth split 7) in 
	      match pred_counter with
		  0 -> sprintf "%s < %s at assignment" assigned_value comp_value
		| 1 -> sprintf "%s = %s at assignment" assigned_value comp_value
		| 2 -> sprintf "%s > %s at assignment" assigned_value comp_value
		| n -> 
		    raise (CounterFail(n))
	  end
      | "branches" ->
	  begin
	    let cond = (nth split 6) in
	      match pred_counter with
		  0 -> Printf.sprintf "Branch condition %s observed false" cond
		| 1 -> Printf.sprintf "Branch condition %s observed true" cond
		| n -> 
		    raise (CounterFail(n))
	  end
      | str ->
	  raise (SchemeFail("Unexpected get_pred_text scheme name: "^str))
  in
    (predicate, file_name, lineno, cfg_node)
