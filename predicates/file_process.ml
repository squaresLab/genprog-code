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
open Cil

(* print-run prints a concise version of run information of the
 * variety found in Liblit's datasets. Each line is a run. First number
 * is the run_number. Each value after that is a comma-delimited list.
 * First number is the predicate number.
 * after that, the values associated with the predicates.
 * assumes that you will eventually close the out_channel, so don't
 * forget! *)

let print_run (out : out_channel) (filename : string) =
  let output s = output_string out s in
  let run_num = Hashtbl.find !fname_to_run_num filename in 
  let inner_tbl = Hashtbl.find !run_and_pred_to_res run_num in
    output (sprintf "%d," run_num);
    Hashtbl.iter 
      (fun pred_num -> 
		 output (sprintf "%d:" pred_num);
		 fun (num_true, num_false) ->
		   output (sprintf "%d:" num_true);
		   output (sprintf "%d," num_false)) inner_tbl;
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
      let [pred_num;num_true;num_false] = to_ints (split colon_regexp one_pred) in
      let inner_tbl = 
		if (Hashtbl.mem !run_and_pred_to_res run) then
		  Hashtbl.find !run_and_pred_to_res run
		else
		  create 10 
      in
		add inner_tbl pred_num (num_true, num_false); 
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

(* conciseify takes a raw file for one run
 * and adds the info to several hashtables. Every predicate gets
 * a unique idenitifying number that is consistent across all 
 * runs. Every run gets a unique number. The results for a predicate site
 * (actually contains several predicates, like number of times a branch 
 * was true and the number times it was false)
 * on a run are stored as an integer list (order maintained, obviously) 
 * in run_and_pred_to_res 
 *)

exception SchemeFail of string
let run_num = ref 0
let predicate_num = ref 0

let conciseify_compressed_file (filename : string) (gorb : string) = 
  let fin = open_in filename in 
  let good = if (get (capitalize gorb) 0) == 'P' then 0 else 1 in
  let run = 
    if not (Hashtbl.mem !fname_to_run_num filename) then begin
      (add !fname_to_run_num filename !run_num);
      (add !run_num_to_fname_and_good !run_num (filename, good));
      incr run_num
    end;
    Hashtbl.find !fname_to_run_num filename in
  let site_to_res = 
    if Hashtbl.mem !run_and_pred_to_res run then
      Hashtbl.find !run_and_pred_to_res run
    else
      Hashtbl.create 10 
  in
    (try
       while true do
	 let line = input_line fin in
	 let [site_num; num_true; num_false] = 
	   List.map int_of_string (split comma_regexp line) 
	 in
	  Hashtbl.replace site_to_res site_num (num_true,num_false);
	   site_set := IntSet.add site_num !site_set
       done
     with _ -> ());
    Hashtbl.replace !run_and_pred_to_res run site_to_res;
    close_in fin

let compress_and_conciseify (filename : string) (gorb : string) = 
  let fin = open_in filename in 
  let good = if (get (capitalize gorb) 0) == 'p' then 0 else 1 in (* 1 signifies anomolous run *)
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
	 let [site_num; value] = 
	   List.map int_of_string (Str.split comma_regexp line) 
	 in 
	 let site_to_res : (int, int * int) Hashtbl.t =
	   if Hashtbl.mem !run_and_pred_to_res run then
	     Hashtbl.find !run_and_pred_to_res run
	   else
	     Hashtbl.create 10 
	 in
	 let (num_true, num_false) =
	   if Hashtbl.mem site_to_res site_num then
	     Hashtbl.find site_to_res site_num else
	       (0,0)
	 in
	 let res' =
	   if value == 0 then (num_true, num_false + 1) else 
	     (num_true + 1, num_false)
	 in
	   Hashtbl.replace site_to_res site_num res';
	   Hashtbl.replace !run_and_pred_to_res run site_to_res;
	   site_set := IntSet.add site_num !site_set 
       done 
     with _ -> ());
	close_in fin

(* get_pred_text converts predicate numbers to human-readable text
 * for the purposes of sensible output. *)

exception CounterFail of int

let get_pred_text pred_num pred_counter = 
  let loc,scheme,exp = Hashtbl.find !site_ht pred_num in
  let lineno = Printf.sprintf "%d" loc.line in
  let exp_str = Printf.sprintf "Scheme: %s " scheme in
  let exp = if pred_counter == 0 then 
	(Pretty.sprint 80 (d_exp () exp)) else 
	  "not "^(Pretty.sprint 80 (d_exp () exp)) in
  let exp_str' = exp_str^exp in
	(exp_str', loc.file, lineno)
