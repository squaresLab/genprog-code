(* This file does the main predicate manipulation
 * and ranking for the predicate project.
 * The initial ranking stuff (calulating increase, context,
 * importance, etc) are taken from the CBI research exactly.
 *)

open Hashtbl
open Globals
open Prune
open List

(* make_pred_tbl takes the hashtable that maps
 * runs to lists of predicates and makes a hashtable
 * that maps predicate numbers to a list of 
 * the results for that predicate number on every run.
 * The list looks like:
 * run_num :: counters 
 * good_or_bad is an integer signifying whether that run was good (0) or 
 * anamolous (1)
 * 
 *)

let make_pred_tbl () = 
  Printf.printf "Make pred table!\n"; flush stdout;
  let pred_tbl : (int, int list list) Hashtbl.t = create 10 in
  let (preds : int list) = keys !site_ht in 
    Printf.printf "I have %d preds\n" (List.length preds); flush stdout;
    Hashtbl.iter
      (fun run ->
	 (fun(fname, good) ->
	   let inner_tbl = Hashtbl.find !run_and_pred_to_res run in
	     List.iter 
	       (fun pred -> 
		  let lst = 
		    if Hashtbl.mem pred_tbl pred then
		      Hashtbl.find pred_tbl pred 
		    else 
		      [] 
		  in
		  let res = Hashtbl.find inner_tbl pred in
		    Hashtbl.replace pred_tbl pred ((run :: res) :: lst)
	       )
	       preds))
      !run_num_to_fname_and_good;
    pred_tbl

(* explode_preds explodes the concise predicates into individual predicates.
 * In the concise version, each "site" is associated with some number
 * of counters. When exploding, turn each list corresponding to
 * site_num::res_list into individual predicates for individual counters.)
 *)
let explode_preds pred_tbl =
  (* explode the pred_tbl into a giant set of lists *)

  (* pred_tbl maps predicate to a list of resultitems
   *
   * resultitem = run_num :: counter1 :: counter2 :: counter 3 :: []
   * must become a new table, that maps a signifier for 
   * (site, counter_num) -> (run_num, counter_value) list
   *)
  let counter_tbl : ((int * int), (int * int) list) Hashtbl.t = create 10 in

  let rec process_resultitem resultitem site run_num counter_num = 
    match resultitem with
	r :: rs ->
	  flush stdout;
	  let inner_lst = 
	    if Hashtbl.mem counter_tbl (site, counter_num) then
	      Hashtbl.find counter_tbl (site, counter_num)
	    else [] in
	    Hashtbl.replace counter_tbl 
	      (site, counter_num) ((run_num, r) :: inner_lst);
	    (process_resultitem rs site run_num (counter_num+1))
      | [] -> () in
    Hashtbl.iter
      (fun site ->
	 fun result_list ->
	   List.iter
	     (fun reslist ->
		process_resultitem (tl reslist) site (hd reslist) 0)
	     result_list)
      pred_tbl;
    counter_tbl

let numF () =
  float(Hashtbl.fold
	  (fun run_num ->
	     fun (fname, gorb) -> 
	       fun (accum) ->
		 if(gorb == fAIL) then accum + 1 else accum)
	  !run_num_to_fname_and_good 0)

let num_pred_was_sampled pred_num run_num = 
  let inner_tbl = Hashtbl.find !run_and_pred_to_res run_num in
    if Hashtbl.mem inner_tbl pred_num then begin
      let pred_res = Hashtbl.find inner_tbl pred_num in
	fold_left (fun i -> fun accum -> i + accum) 0 pred_res
    end else 0

(* definitions taken from paper directly *)
let f_of_P lsts = float(List.length (filter_exploded_pairs_positive lsts fAIL))

let s_of_P lsts = float(List.length (filter_exploded_pairs_positive lsts sUCC))

let failure_P lsts = (f_of_P lsts) /. ((s_of_P lsts) +. (f_of_P lsts))

let p_Observed pred_num lsts bit = 
  float(List.length
	  (List.filter 
	     (fun (run, counter) -> (num_pred_was_sampled pred_num run) > 0)
	     (filter_exploded_pairs lsts bit)))

let context_P pred_num lsts = 
  (p_Observed pred_num lsts fAIL) /. 
    ((p_Observed pred_num lsts sUCC) +. (p_Observed pred_num lsts fAIL)) 

let increase_P pred_num lsts = (failure_P lsts) -. (context_P pred_num lsts) 

let importance_P pred_num lsts = 
  let inc = increase_P pred_num lsts in
  let fp = f_of_P lsts in
    if (fp == 0.0) || (inc == 0.0) || ((numF ()) == 0.0) then 0.0 else
      (*      2.0 /. ((1.0 /. inc) +. (1.0 /. ((log fp) /. (log (numF ()))))) in*)
      2.0 /. ((1.0 /. inc) +. (1.0 /. (fp /. (numF ())))) 

(* I HATE that this has to be here and not prune.ml but I can't
 * have circular module dependencies in OCaml and I don't want to define
 * Increase_P over there and I certainly don't want to define it twice.
 * But I am NOT HAPPY. 
 *)

let prune_on_increase counter_hash =
  let pruned_hash = create 10 in
    Hashtbl.iter (fun (pred_num, pred_counter) -> 
		    fun lsts ->
		      let increase = increase_P pred_num lsts in
			if increase >= 0.0 then
			  add pruned_hash (pred_num, pred_counter) lsts
		 ) counter_hash;
    pruned_hash
(* rank_preds assigns "importance" scores to each predicate, as described in
 * Liblit et al's PLDI 2005 paper. This will be good for debugging the system as
 * well as for fault localization. Returns a list of predicates, sorted by
 * importance. 
 *
 *)
let rank_preds counter_hash =
  let ranked_list = ref [] in
  Hashtbl.iter
    (fun (pred_num, pred_counter) ->
       fun lsts ->
	 let fP = f_of_P lsts in
	 let sP = s_of_P lsts in
	 let failureP = failure_P lsts in
	 let fObserved = p_Observed pred_num lsts fAIL in
	 let sObserved = p_Observed pred_num lsts sUCC in
	 let importance = importance_P pred_num lsts in
	 let increase = increase_P pred_num lsts in
	 let context = context_P pred_num lsts in 
	   ranked_list := ((pred_num, pred_counter), importance, increase, context,
			  fP, sP, failureP, fObserved, sObserved,(numF())) :: !ranked_list
    )
    counter_hash;
    List.sort
      (fun ((pn1, pc1), imp1, inc1, c1,_,_,_,_,_,_) ->
	 (fun ((pn2, pc2), imp2, inc2, c2,_,_,_,_,_,_) -> 
	    (Pervasives.compare imp2 imp1)))
      !ranked_list
