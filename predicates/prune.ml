(* This file is part of the predicates project. It contains functions used to 
 * prune and filter the predicates and counters under consideration. These
 * utilities are used both for proper CBI and also for filtering predicates
 * for the purpose of fitness function utility.
 *)

open Globals
open List
open Hashtbl

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


let are_all_counters_in_triples_zero_on_all_runs (lsts : int list list) = 
  fold_left (fun so_far ->
	       fun lst -> 
		 so_far && ((fold_left (fun accum ->
					 fun new_digit -> 
					   accum + new_digit) 0 (tl lst)) == 0))
    true lsts

let is_this_counter_zero_on_all_runs lsts = 
  fold_left (fun so_far ->
	       fun (run, counter) ->
		 (counter == 0) && so_far) true lsts

let is_this_counter_not_zero_on_all_runs (lsts: (int * int) list) =
  fold_left
    (fun so_far ->
       (fun (run_num, counter_value) ->
	  so_far && (counter_value > 0))) true lsts

let is_this_counter_not_zero_on_some_run (lsts : (int * int) list) = 
  List.exists (fun (run, counter) -> counter > 0) lsts

let is_this_counter_zero_on_some_run lsts = 
  List.exists (fun (run, counter) -> counter == 0) lsts

(* the pruning functions should be read as a function between a predicate and a 
 * boolean (or in the case of lfc, a set of counters and a boolean);
 * the function returns true when the predicate/set of counters should be removed *)

let uf fltr lsts = (fltr 1) && (is_this_counter_zero_on_all_runs lsts) 

let lfc fltr lsts = 
  (fltr 2) && 
    (are_all_counters_in_triples_zero_on_all_runs (filter_unexploded_pairs lsts fAIL))

let lfe fltr lsts = 
  (fltr 4) && 
    (is_this_counter_zero_on_all_runs (filter_exploded_pairs lsts fAIL))

let stnt fltr lsts = (* sometimes true on positive runs, never true on negative runs *)
  (fltr 16) && 
    (is_this_counter_not_zero_on_some_run (filter_exploded_pairs lsts fAIL)) &&
    (is_this_counter_zero_on_all_runs (filter_exploded_pairs lsts sUCC))

let atat fltr lsts = (* always true on positive runs, always true on negative runs *)
  (fltr 32) && (is_this_counter_zero_on_some_run lsts) 

let ntst fltr lsts = (* never true on positive runs, always true on negative runs *)
  (fltr 64) &&
    (is_this_counter_zero_on_some_run (filter_exploded_pairs lsts fAIL)) &&
    (is_this_counter_not_zero_on_some_run (filter_exploded_pairs lsts sUCC))
  

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
		       (lfe fltr result_list) || (stnt fltr result_list) ||
		       (atat fltr result_list) || (ntst fltr result_list))) then
	      add final_hash key result_list) 
      ) counter_tbl;
    final_hash

