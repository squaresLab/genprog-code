open Hashtbl
open Prune
open Globals

let output_baseline ranked_preds pred_tbl exploded_tbl = begin
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
    
  let fout = open_out_bin !baseline_out in 

  let preds_imp_gt_zero = 
    get_pred_set (fun ((pred_set, pred_counter), importance, _,_) ->
		    importance > 0.0) in
    
  let preds_inc_gt_zero = 
    get_pred_set (fun ((pred_set,pred_counter), _,increase,_) ->
		    increase > 0.0) in
  let preds_cont_gt_zero = 
    get_pred_set (fun ((pred_set,pred_counter),_,_,context) ->
		    context > 0.0) in
    
  let fltr = fun x -> true in (* make a trivial filter so that 
			       * the pruning functions actually
			       * prune *)
    
  let filtered_by_uf = ref PredSet.empty in
  let filtered_by_lfc = ref PredSet.empty in
  let filtered_by_lfe = ref PredSet.empty in
    
    Hashtbl.iter 
      (fun key ->
	 fun result_list ->
	   if (uf fltr result_list) then begin
	     filtered_by_uf := PredSet.add key !filtered_by_uf
	   end;
	   if (lfe fltr result_list) then begin
	     filtered_by_lfe := PredSet.add key !filtered_by_lfe
	   end
      ) exploded_tbl;
    
    Hashtbl.iter
      (fun site ->
	 fun res_list ->
	   if (lfc fltr res_list) then begin
	     filtered_by_lfe := PredSet.add (site,0) !filtered_by_lfe;
	     filtered_by_lfe := PredSet.add (site,1) !filtered_by_lfe
	   end
      ) pred_tbl;
    
    Marshal.to_channel fout
      ([preds_imp_gt_zero;
	preds_inc_gt_zero;
	preds_cont_gt_zero;
	!filtered_by_uf;
	!filtered_by_lfc;
	!filtered_by_lfe], exploded_tbl) [];
    close_out fout
	
(*	let at_pos_at_neg = (atat fltr strip_list) in
	let at_pos_st_neg = (atst fltr strip_list) in 
	let at_pos_nt_neg = (atnt fltr strip_list) in

	let st_pos_at_neg = (stat fltr strip_list) in
	let st_pos_st_neg = (stst fltr strip_list) in
	let st_pos_nt_neg = (stnt fltr strip_list) in

	let nt_pos_at_neg = (ntat fltr strip_list) in
	let nt_pos_st_neg = (ntst fltr strip_list) in*)
(* <--- I don't know if that stuff makes sense right now so
 *      I'll comment it out until I get the other stuff working
 *)

      (* ntnt is covered by previous cases *)

(*	let pred_ranks = [] in do we want to track the actual ranks of
 *      predicates? *)
end
