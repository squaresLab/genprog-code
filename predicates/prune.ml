(* This file is part of the predicates project. It contains functions used to 
 * prune and filter the predicates and counters under consideration. These
 * utilities are used both for proper CBI and also for filtering predicates
 * for the purpose of fitness function utility.
 *)

open Globals
open List
open Hashtbl

type result_pattern = ATAT | ATST | ATNT | STAT | STST | STNT

let filter_unexploded_pairs lsts bit =
  List.filter 
    (fun pred_res ->
       let _, gorb = Hashtbl.find !run_num_to_fname_and_good (hd pred_res)
       in gorb == bit) lsts

let filter_exploded_pairs pairs bit =
  List.filter
    (fun (run_num, counter_value) ->
       let _, gorb = Hashtbl.find !run_num_to_fname_and_good run_num
       in gorb == bit) pairs

let all_run_counter_pairs_where_counter_is_not_zero (lsts : (int * int) list) = 
  List.filter (fun (run, counter) -> counter > 0) lsts

let filter_exploded_pairs_positive pairs bit =
  (all_run_counter_pairs_where_counter_is_not_zero 
     (filter_exploded_pairs pairs bit))


(* these pruning functions are a function between a predicate and a boolean
 * that returns true when the conditions are met *)

let sruns lsts = filter_exploded_pairs lsts sUCC 
let fruns lsts = filter_exploded_pairs lsts fAIL

let alt lsts =   
  fold_left
    (fun so_far ->
       (fun (run_num, counter_value) ->
	  so_far && (counter_value > 0))) true lsts

let st lsts =  
  List.exists (fun (run, counter) -> counter > 0) lsts

let nt lsts =
  fold_left (fun so_far ->
	       fun (run, counter) ->
		 (counter == 0) && so_far) true lsts

let atat lsts = alt lsts
let ats lsts = alt (sruns lsts)
let atn lsts = alt (fruns lsts)
let sts lsts = alt (sruns lsts)
let stn lsts = alt (fruns lsts)
let nts lsts = nt (sruns lsts)
let ntn lsts = nt (fruns lsts)

let atst lsts = (ats lsts) && (stn lsts)
let atnt lsts = (ats lsts) && (ntn lsts) 
let stat lsts = (sts lsts) && (atn lsts) 
let stst lsts = (sts lsts) && (stn lsts) 
let stnt lsts = (sts lsts) && (ntn lsts) 
let ntat lsts = (nts lsts) && (atn lsts)
let ntst lsts = (nts lsts) && (stn lsts)
let ntnt lsts = (nts lsts) && (ntn lsts)

let are_all_counters_in_triples_zero_on_all_runs (lsts : int list list) = 
  fold_left (fun so_far ->
	       fun lst -> 
		 so_far && ((fold_left (fun accum ->
					  fun new_digit -> 
					    accum + new_digit) 0 (tl lst)) == 0))
    true lsts

(* these pruning functions should be read as a function between a predicate and a 
 * boolean (or in the case of lfc, a set of counters and a boolean);
 * the function returns true when the predicate/set of counters should be removed *)

let uf fltr lsts = (fltr 1) && (nt lsts)

let lfc fltr lsts = 
  (fltr 2) && 
    (are_all_counters_in_triples_zero_on_all_runs (filter_unexploded_pairs lsts fAIL))

let lsc fltr lsts = 
  (fltr 2) && 
    (are_all_counters_in_triples_zero_on_all_runs (filter_unexploded_pairs lsts sUCC))

let lfe fltr lsts = 
  (fltr 4) && (nt (fruns lsts))

let prune_on_full_set (pred_tbl : (int, int list list) Hashtbl.t) fltr =
  (* prune on the full counter set for each site *)
  (* lfc, in practice *)
  let pruned_hash = create 10 in
    Hashtbl.iter
      (fun (pred : int) ->
	 (fun result_list_list ->
	    if not (lfc fltr result_list_list) then
	      add pruned_hash pred result_list_list
	 ) 
      ) pred_tbl;
    pruned_hash

(* pruning rules that apply to individual counters *)

let prune_on_individual_counters counter_tbl fltr =
  let final_hash = create 10 in
    Hashtbl.iter
      (fun key -> 
	 (fun (result_list : (int * int) list) ->
	    if (not ((uf fltr result_list) || 
		       (lfe fltr result_list))) then
	      add final_hash key result_list) 
      ) counter_tbl;
    final_hash

let pattern res_list = (* res list = list of (run_num, counter_value) pairs *)
  if atat res_list then ATAT
  else if atst res_list then ATST 
  else if atnt res_list then ATNT
  else if stat res_list then STNT
  else if stst res_list then STST
  else STNT (* I think this is all the options, yes? *) 
