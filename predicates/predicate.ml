(* This file does the main predicate manipulation
 * and ranking for the predicate project.
 * The initial ranking stuff (calulating increase, context,
 * importance, etc) are taken from the CBI research exactly.
 *)

open Hashtbl
open Globals
open File_process
open Prune
open List

(* make_pred_tbl takes the hashtable that maps
 * runs to lists of predicates and makes a hashtable
 * that maps site numbers to a list of 
 * the results for that predicate number on every run.
 * The list looks like:
 * (run_num :: num_true :: num_false :: []) :: ...
 * good_or_bad is an integer signifying whether that run was good (0) or 
 * anamolous (1)
 * 
 *)

let make_pred_tbl () = 
  let pred_tbl : (int, int list list) Hashtbl.t = create 10 in
    IntSet.iter
      (fun site -> 
	 Hashtbl.iter
	   (fun run ->
	      (fun(fname, good) ->
		 let lst = if Hashtbl.mem pred_tbl site then 
		   Hashtbl.find pred_tbl site else [] in
		 let inner_tbl = Hashtbl.find !run_and_pred_to_res run in
		 let num_true, num_false =
		   if Hashtbl.mem inner_tbl site then
		     Hashtbl.find inner_tbl site else (0,0)
		 in
		   Hashtbl.replace pred_tbl site (([run;num_true;num_false]) :: lst)
	      )) !run_num_to_fname_and_good) !site_set;
    pred_tbl

(* explode_preds explodes the concise predicates into individual predicates.
 * In the concise version, each "site" is associated with some number
 * of counters. When exploding, turn each list corresponding to
 * site_num::res_list into individual predicates for individual counters.)
 *)
let explode_preds (pred_tbl : (int, int list list) Hashtbl.t) =
  (* explode the pred_tbl into a giant set of lists *)

  (* pred_tbl maps site_num to a list of resultitems
   *
   * resultitem = (run_num, num_true, num_false)
   * must become a new table, that maps a signifier for 
   * (site, counter_num) -> (run_num, counter_value) list
   * where counter_num = 0 is site was true
   * and counter_num = 1 is site was false. This may be stupid.
   *)
  let counter_tbl : ((int * int), (int * int) list) Hashtbl.t = create 10 in

  let process_counters site run_num value counter_num =
	let inner_lst = if Hashtbl.mem counter_tbl (site, counter_num) then
	  Hashtbl.find counter_tbl (site, counter_num) else
		[] in
	  Hashtbl.replace counter_tbl (site, counter_num) ((run_num, value) :: inner_lst)
  in
    Hashtbl.iter
      (fun site ->
		 fun res_list ->
		   List.iter
			 (fun (run_num::num_true::num_false::[]) ->
				process_counters site run_num num_true 0;
				process_counters site run_num num_false 1)
			 res_list)
	  
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
      let (num_true, num_false)  = Hashtbl.find inner_tbl pred_num in
		num_true + num_false 
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
 *)

let rank_preds counter_hash =
  let number num = 
    match (classify_float num) with
	FP_nan -> 0.0
      | _ -> num
  in
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
	   ranked_list := ((pred_num, pred_counter), (number importance), 
			   (number increase), (number context),
			  fP, sP, failureP, fObserved, sObserved,(numF())) :: !ranked_list
    )
    counter_hash;
    List.sort
      (fun ((pn1, pc1), imp1, inc1, c1,_,_,_,_,_,_) ->
	 (fun ((pn2, pc2), imp2, inc2, c2,_,_,_,_,_,_) -> 
	    (Pervasives.compare imp2 imp1)))
      !ranked_list

let sum_observed pred_num lsts = 
  List.fold_left
    (fun accum ->
       fun(run,counter) ->
	 (num_pred_was_sampled pred_num run) + accum)
    0 lsts 

let sum_true lsts = 
  List.fold_left
    (fun accum ->
       fun(run,counter) ->
	 accum+counter) 0 lsts

let summarize_preds ranked_preds exploded_tbl =
  let info_table = Hashtbl.create 10 in
    List.iter
      (fun ((pred_num, pred_counter), 
	    importance, increase, context,
	    fP, sP, failureP, fObserved, sObserved, numF) ->
	 let lsts = Hashtbl.find exploded_tbl (pred_num,pred_counter) in
	   (* lsts is a list of (run, count_true) for this predicate *)
	 let succ_runs = sruns lsts in
	 let fail_runs = fruns lsts in
	   
	 let total_count_observed_s = sum_observed pred_num succ_runs in
	 let total_count_observed_f = sum_observed pred_num fail_runs in

	 let total_count_true_s = sum_true succ_runs in
	 let total_count_true_f = sum_true fail_runs in
	   Hashtbl.add info_table (pred_num,pred_counter)
	     {importance=importance;
	      increase=increase;
	      context=context;
	      f_of_P=fP;
	      s_of_P=sP;
	      failureP=failureP;
	      fObserved=fObserved;
	      sObserved=sObserved;
	      count_obs_s=float_of_int(total_count_observed_s);
	      count_obs_f=float_of_int(total_count_observed_f);
	      count_true_s=float_of_int(total_count_true_s);
	      count_true_f=float_of_int(total_count_true_f)}
      ) ranked_preds; info_table	     

let output_rank ranked_preds = begin
  Printf.printf "%d ranked preds\n" (List.length ranked_preds); flush stdout;
  if not !modify_input then begin
    Printf.printf "Predicate,file name,lineno,F(P),S(P),Failure(P),Context,Increase,F(P Observed),S(P Observed),numF,Importance\n";
  end;
  List.iter (fun ((pred_num, pred_counter), 
		  importance, increase, context,
		  fP, sP, failureP, fObserved, sObserved, numF) ->
	       Printf.printf "Pred_num: %d pred_counter: %d " pred_num pred_counter;
	       let (name, filename, lineno) = get_pred_text pred_num pred_counter in 
		 Printf.printf "%s,%s,%s,%g,%g,%g,%g,%g,%g,%g,%g,%g\n" 
		   name filename lineno fP sP failureP context increase fObserved sObserved numF importance;
		 flush stdout)
    ranked_preds
end

