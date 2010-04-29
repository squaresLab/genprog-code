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

(* this is badly named because it's actually different from the function
 * of the same name in cheap_cbi.ml; it also adds data to the sp_ht to 
 * track which new scalar-pairs predicates we've already seent *)

let get_next_site scheme exp l =
  let exp_str = Pretty.sprint 80 (d_exp () exp) in
  let loc_str = Pretty.sprint 80 (d_loc () l) in
  let str = scheme^exp_str^loc_str in
  let count = !max_site in
	incr max_site;
	Hashtbl.add !site_ht count (l,scheme,exp);
	Hashtbl.add !sp_ht str count;
	count

let conciseify_compressed_file (filename : string) (gorb : string) = 
  let fin = open_in filename in 
	(* CHECK: shouldn't good be 1 if gorb is P? *)
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
(*  Printf.printf "C and C filename: %s\n" filename; flush stdout;*)
  let fin = open_in filename in 
  let good = if (get (capitalize gorb) 0) == 'P' then 0 else 1 in (* 1 signifies anomolous run *)
  let run = 
    if not (Hashtbl.mem !fname_to_run_num filename) then begin
      (add !fname_to_run_num filename !run_num);
      (add !run_num_to_fname_and_good !run_num (filename, good));
      incr run_num
    end;
    Hashtbl.find !fname_to_run_num filename 
  in
  let process_site site_num value =
(*	Printf.printf "Processing site in process_site: %d\n" site_num; flush stdout;*)
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
  in
  let rec process_sp_site fin site lname lval (loc,typ,exp) = 
(*	Printf.printf "processing sp site: %d, lname: %s, lval: %g\n" site lname lval; flush stdout;*)
	let line = input_line fin in
(*	  Printf.printf "Read: %s\n" line; flush stdout;*)
	let split = Str.split comma_regexp line in
	let site_num = int_of_string (List.hd split) in
(*	  Printf.printf "new site num: %d\n" site_num; flush stdout;*)
	  if not (site == site_num) then begin
		let loc,typ,exp = Hashtbl.find !site_ht site_num in
(*		  Printf.printf "Site type: %s\n" typ; flush stdout;*)
		  let rest = List.tl split in
		  if typ = "scalar-pairs" then begin
			let lname = List.hd rest in
			 let lval = try (float_of_string (List.hd (List.tl rest))) with _ -> (float_of_int (int_of_string (List.hd (List.tl rest)))) in
(*			   Printf.printf "did I make it here ever?\n"; flush stdout;*)
			   process_sp_site fin site_num lname lval (loc,typ,exp)
		  end else 
			process_site site_num (int_of_string (List.hd (List.tl split)))
	  end
	  else begin
		let rest = List.tl split in
		let rname = List.hd rest in
(*		  Printf.printf "Rname is: %s, hd of tl is %s\n" rname (List.hd (List.tl rest)); flush stdout;*)
		let rval = try (float_of_string (List.hd (List.tl rest))) with _ -> (float_of_int (int_of_string (List.hd (List.tl rest)))) in
(*		  Printf.printf "comparing to %s, %g\n" rname rval; flush stdout;*)
		let actual_op = if lval > rval then Gt else if lval < rval then Lt else Eq in
		let comp_exps = 
		  List.map 
			(fun op -> BinOp(op, (Const(CStr(lname))), (Const(CStr(rname))), (TInt(IInt,[]))))
			[Gt;Lt;Eq] in
		let exp_str = Pretty.sprint 80 (d_exp () (List.hd comp_exps)) in
		let loc_str = Pretty.sprint 80 (d_loc () loc) in
		let str = "scalar-pairs"^exp_str^loc_str in
(*		  Printf.printf "Str1: %s\n" str; flush stdout;*)
		  if not (Hashtbl.mem !sp_ht str) then 
			List.iter (fun comp_exp -> ignore(get_next_site "scalar-pairs" comp_exp loc)) comp_exps;
		  List.iter
			(fun comp_exp ->
			   let exp_str = Pretty.sprint 80 (d_exp () comp_exp) in
			   let loc_str = Pretty.sprint 80 (d_loc () loc) in
			   let str = "scalar-pairs"^exp_str^loc_str in
(*				 Printf.printf "Looking for: %s\n" str; flush stdout;*)
			   let site_num = Hashtbl.find !sp_ht str in
			   let value = 
				 match comp_exp with BinOp(op,_,_,_) -> 
				   let exp_str = Pretty.sprint 80 (d_exp () comp_exp) in
				   if op == actual_op then 1 else 0
			   in
				 process_site site_num value) comp_exps; 
		  process_sp_site fin site lname lval (loc,typ,exp)
	  end
  in
	(try 
       while true do
		 let line = input_line fin in
(*		   Printf.printf "Read: %s\n" line; flush stdout;*)
		 let split = Str.split comma_regexp line in
		 let site_num, rest = int_of_string (List.hd split), List.tl split in
		 let loc,typ,exp = Hashtbl.find !site_ht site_num in
(*		   Printf.printf "Site type: %s\n" typ; flush stdout;*)
		   if typ = "scalar-pairs" then begin
(*			 Printf.printf "It's a scalar pairs!\n"; flush stdout;*)
			 (* the start of a scalar pairs site. This string contains the name 
			  * value, and a type signifier of the lhs of an assignment. 
			  * Subsequent site entries printed immediately afterwards contain 
			  * the name, value, and a type signifier of all variables that are 
			  * nominally in scope.
			  * This is somewhat complicated because the sites ht does not 
			  * contain information for all the *individual* sites, it only 
			  * contains one entry for the *initial* site. We need to create 
			  * sites for the actual comparisons. 
			  *)
			 let lname = List.hd rest in
			 let lval = try (float_of_string (List.hd (List.tl rest))) with _ -> (float_of_int (int_of_string (List.hd (List.tl rest)))) in
(*			   Printf.printf "did I make it here ever?\n"; flush stdout;*)
			   process_sp_site fin site_num lname lval (loc,typ,exp)
		   end else process_site site_num (int_of_string (List.hd rest))
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
