open Hashtbl
open Prune
open Globals

let get_sets ranked_preds pred_tbl exploded_tbl = begin
  let sliced = 
    List.map 
      (fun ((pred_num,pred_counter), importance,
	    increase, context, _,_,_,_,_,_) ->
	 ((pred_num,pred_counter), importance,increase,context))
      ranked_preds in
  let get_pred_set filter_fun =
    let just_pair ((pred_set,pred_counter),_,_,_) = 
      (pred_set,pred_counter) in
    let rec inner_get lst accum_set =
      match lst with
	  ele :: eles -> 
	    if filter_fun ele then
	      inner_get eles 
		(PredSet.add (just_pair ele) accum_set)
	    else
	      inner_get eles accum_set
	| [] -> accum_set 
    in
      inner_get sliced PredSet.empty
  in

  let imp_preds = 
    get_pred_set (fun ((pred_set, pred_counter), importance, _,_) ->
		    importance > 0.0) in
    
  let inc_preds = 
    get_pred_set (fun ((pred_set,pred_counter), _,increase,_) ->
		    increase > 0.0) in
  let cont_preds = 
    get_pred_set (fun ((pred_set,pred_counter),_,_,context) ->
		    context > 0.0) in
    
  let fltr = fun x -> true in (* make a trivial filter so that 
			       * the pruning functions actually
			       * prune *)
    
  let uf_preds = ref PredSet.empty in
  let lfc_preds = ref PredSet.empty in
  let lfe_preds = ref PredSet.empty in
  let lsc_preds = ref PredSet.empty in (* lack of successful coverage! *)

  let at_at = ref PredSet.empty in
  let at_st = ref PredSet.empty in
  let at_nt = ref PredSet.empty in 
    
  let st_at = ref PredSet.empty in 
  let st_st = ref PredSet.empty in
  let st_nt = ref PredSet.empty in

    (* ntnt is covered by previous cases *)
    Hashtbl.iter 
      (fun key ->
	 fun result_list ->
	   if (uf fltr result_list) then 
	     uf_preds := PredSet.add key !uf_preds
	   else
	     if (lfe fltr result_list) then begin
	       lfe_preds := PredSet.add key !lfe_preds
	     end;
	   let to_add = 
	     match (pattern result_list) with
		 ATAT -> at_at
	       | ATST -> at_st
	       | ATNT -> at_nt
	       | STAT -> st_at
	       | STST -> st_st
	       | STNT -> st_nt
	   in
	     to_add := PredSet.add key !to_add 
      ) exploded_tbl;
    
    Hashtbl.iter
      (fun site ->
	 fun res_list ->
	   if (lfc fltr res_list) then begin
	     lfc_preds := PredSet.add (site,0) !lfc_preds;
	     lfc_preds := PredSet.add (site,1) !lfc_preds
	   end else
	     if (lsc fltr res_list) then begin
	       lsc_preds := PredSet.add (site,0) !lsc_preds;
	       lsc_preds := PredSet.add (site,1) !lsc_preds
	     end
      ) pred_tbl;

    (imp_preds,
     inc_preds,
     cont_preds,
     !uf_preds,
     !lfc_preds,
     !lsc_preds,
     !lfe_preds,
     !at_at,
     !at_st,
     !at_nt,
     !st_at,
     !st_st,
     !st_nt)
end

let output_baseline ranked_preds pred_tbl exploded_tbl = begin
  let (imp_preds,
       inc_preds,
       cont_preds,
       uf_preds,
       lfc_preds,
       lsc_preds,
       lfe_preds,
       at_at,
       at_st,
       at_nt,
       st_at,
       st_st,
       st_nt) = get_sets ranked_preds pred_tbl exploded_tbl in

  let fout = open_out_bin !baseline_out in 

    Marshal.to_channel fout
      ([imp_preds;
	inc_preds;
	cont_preds;
	uf_preds;
	lfc_preds;
	lsc_preds;
	lfe_preds;
	at_at;
	at_st;
	at_nt;
	st_at;
	st_st;
	st_nt], exploded_tbl) [];
    close_out fout
end

let compare_to_baseline b_file v_ranked_list v_pred_tbl v_exploded_tbl = begin
  (* read in baseline sets *)
  let fin = open_in_bin b_file in
  let (baseline_sets, b_exploded_tbl) = Marshal.from_channel fin in 
    close_in fin;
    (* get variant sets *)
   let (imp_preds,
     inc_preds,
     cont_preds,
     uf_preds,
     lfc_preds,
     lsc_preds,
     lfe_preds,
     at_at,
     at_st,
     at_nt,
     st_at,
     st_st,
     st_nt) = get_sets v_ranked_list v_pred_tbl v_exploded_tbl in 
   let variant_sets = [imp_preds;
	inc_preds;
	cont_preds;
	uf_preds;
	lfc_preds;
	lsc_preds;
	lfe_preds;
	at_at;
	at_st;
	at_nt;
	st_at;
	st_st;
	st_nt] in

    
  (* pair the sets *)

   let paired_set_list = List.combine baseline_sets variant_sets in 

List.iter
  (fun (b_set, v_set) ->
     (* what is the difference between the sets? *)
     let diff_set = PredSet.diff b_set v_set in 

       (* which of the predicates in the baseline set are true on the passing runs for this variant? *)
       (* the failing runs for this variant? *)
       (* which of the predicates in the difference are true on the passing runs for this variant? *)
       (* the failing runs for this variant? *)

     let list_of_maybe_interesting_sets = ref [diff_set] in

       (* (this giant list.iter creates a whole bunch of maybe interesting sets of predicates *)

(*       List.iter
	 (fun interesting_predicate_set -> 
	    List.iter
	      (fun truth_value -> 
		 List.iter
		   (fun passing_or_failing_runs -> 
		      let runs_filtered_by_value = filter_runs passing_or_failing_runs truth_value in
		      let predicates_in_this_set_in_this_set_of_runs = 
			PredSet.filter
			  (fun predicate -> 
			     PredSet.mem predicate runs_filtered_by_value
			  ) interesting_predicate_set
		      in
			list_of_maybe_interesting_sets := predicates_in_this_set_in_this_set_of_runs :: !list_of_maybe_interesting_sets
		   ) [passing_runs;failing_runs;all_runs]
	      ) [true;false]
	 ) [b_set;diff_set];*)

       List.iter 
	 (fun interesting_set -> 
	    (* we can quantify these sets by size...*)
	    let num_imp_set_diff = PredSet.cardinal interesting_set in

(*	      List.iter
		(fun weight -> ()) [IMP,INC,CON];*)

	      
	 (* or we can weight by predicate importance, increase, or context... *)
	      
	 (* or we can weight by the absolute difference in the number of times it
	  * was observed to be true ever *)
	 
	 (* or on successful runs... *)
	 (* or on failing runs.. *)
	 
	 (* Same as above, only now we're looking at number of times it was 
	  * observed to be false ever... *)
	 
	 
	 (* successful runs ... *)
	 
	 (* failing runs... *)
	 
	 (* or we can weight by the absolute difference in the number of
	  * successful runs on which the predicate was ever observed to be true*)
	 
	 (* or the number of failing runs...*)
	 
	 (* same as above, only weighting by the number of times the predicate
	  * was ever observed to be false, successful runs...*)
	 
	 (* failing runs...*)
	 
	 (* or the number of total runs on which it was ever observed *)
	 
	 (* dang, that's a lot of options. *)	      
	      ()
	 ) 	 !list_of_maybe_interesting_sets;


	 () ) paired_set_list
end 
