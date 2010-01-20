open List
open Prune
open Globals

(* we can quantify interesting sets by size...*)
(* or we can weight by the numbers liblit magic gives us *)

(* or by the difference in the weights *)

(* or we can weight by the absolute difference in the number of
 * successful runs on which the predicate was ever observed to be true*)

(* or the number of failing runs...*)

(* same as above, only weighting by the number of times the predicate
 * was ever observed to be false, successful runs...*)

(* failing runs...*)

(* or the number of total runs on which it was ever observed *)

(* dang, that's a lot of options. *)	      

(* anyway. That's all what "strategy" encodes *)


let set_names = 
  ["imp_preds";"inc_preds";"cont_preds";"uf_preds";"lfc_preds";
   "lfe_preds";"at_at";"at_st";"st_at";"st_st";]

let strategies =
  ["IMP";"INC";"CONT";"IMP_D";"INC_D";"CONT_D";"OBS_COUNT_S";
   "OBS_COUNT_F";"OBS_T_COUNT_S";"OBS_T_COUNT_F";"OBS_RUNS_T";
   "OBS_RUNS_F";"OBS_T_RUNS_T";"OBS_T_RUNS_F"]

(* get sets returns a list of sets of predicates, which may or may not 
 * be interesting *)

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
    
  let st_at = ref PredSet.empty in 
  let st_st = ref PredSet.empty in

    (* I think all of the missing patterns are covered by 
     * previous cases *)

    Hashtbl.iter 
      (fun key ->
	 fun result_list ->
	   if (uf fltr result_list) then 
	     uf_preds := PredSet.add key !uf_preds
	   else
	     if (lfe fltr result_list) then begin
	       lfe_preds := PredSet.add key !lfe_preds
	     end;
	   match (pattern result_list) with
	       ATAT -> at_at := PredSet.add key !at_at
	     | ATST -> at_st := PredSet.add key !at_st
	     | STAT -> st_at := PredSet.add key !st_at
	     | STST -> st_st := PredSet.add key !st_st
	     | _ -> ()
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

    [imp_preds;inc_preds;cont_preds;
     !uf_preds;!lfc_preds;!lfe_preds;
     !at_at;!at_st;!st_at;!st_st;]
end

(* write out the baseline information for later use when comparing
 * to a variant *)

let output_baseline pred_summary ranked_preds pred_tbl exploded_tbl = 
  let pred_set_list = get_sets ranked_preds pred_tbl exploded_tbl in
  let fout = open_out_bin !baseline_out in 
    Marshal.to_channel fout
      (pred_set_list, pred_summary, exploded_tbl) [];
    close_out fout

let generate_interesting_sets (b_set : PredSet.t) (diff_set : PredSet.t) (v_exploded_tbl) (set_title : string) : 
    ((PredSet.t * bool) * string) list =
  (* which of the interesting predicates are true on all of the passing 
   * runs this variant? on only some of the passing runs? *)
  (* the failing runs for this variant? *)
  
  (* which of the predicates in the **difference** are true on the 
   * passing runs for this variant? *)
  (* the failing runs for this variant? *)
  
  (* in answering these questions, we generate a number
   * of potentially interesting sets of predicates 

   * The first interesting set includes the predicates that are 
   * different between the baseline set and the variant set *)
  ((diff_set,true),"diff_b_"^set_title^"_v_"^set_title) ::
    (flatten
       (map2
	  (fun (predicate_set,include_size) -> 
	     fun predicate_set_name ->
	       map2
		 (fun filter_function ->
		    fun filter_function_descriptor ->
		      let new_set = 
			PredSet.filter 
			  (fun (site_num,counter) ->
			     if Hashtbl.mem v_exploded_tbl (site_num,counter) then begin
			       filter_function (Hashtbl.find v_exploded_tbl (site_num,counter))
			     end else false) predicate_set
		      in
			((new_set, include_size), predicate_set_name^"b_"^set_title^"_v_"^set_title^"_"^filter_function_descriptor)
		 ) [atat;atst;atnt;stat;stst;stnt;ntat;ntst;ntnt;] 
		 ["atat";"atst";"atnt";"stat";"stst";"stnt";"ntat";"ntst";"ntnt";])
	  [(b_set,false);(diff_set,true)] ["b_";"diff_"]))

let generate_weights interesting_sets b_pred_info v_pred_info =
  flatten
    (map
       (fun ((set,include_size),set_name) -> 
	let tail = 
	    (* and besides that we have so many weighting options it's like ridiculous *)
	    map
	      (fun strategy ->
		 let weight = 
		   PredSet.fold
		     (fun pred ->
			fun accum ->
			  let b = try Hashtbl.find b_pred_info pred with Not_found -> empty_info in
			  let v = try Hashtbl.find v_pred_info pred with Not_found -> empty_info in
			  let weight =
			    match strategy with
				"IMP" -> b.importance
			      | "INC" -> b.increase
			      | "CONT" -> b.context
			      | _ -> begin
				  let b_num, v_num =
				    match strategy with
				      | "IMP_D" -> b.importance, v.importance
				      | "INC_D" -> b.increase, v.increase
				      | "CONT_D" -> b.context, v.context 
				      | "OBS_COUNT_S" -> b.count_obs_s, v.count_obs_s
				      | "OBS_COUNT_F" -> b.count_obs_f, v.count_obs_f 
				      | "OBS_T_COUNT_S" -> b.count_true_s, 
					  v.count_true_s 
				      | "OBS_T_COUNT_F" -> b.count_true_f, 
					  v.count_true_f
				      | "OBS_RUNS_T" -> b.sObserved, v.sObserved 
				      | "OBS_RUNS_F" -> b.fObserved, v.fObserved 
				      | "OBS_T_RUNS_T" -> b.s_of_P, v.s_of_P
				      | "OBS_T_RUNS_F" -> b.f_of_P, v.f_of_P 
				  in
				    (abs_float (b_num -. v_num))
				end
			  in
			    weight +. accum
		     ) set 0.0
		 in
		   (weight, set_name^"_weighted_by_"^strategy))
	      strategies
	in
	  if include_size then 
	    (* we can quantify these sets by size...*)
	    ((float_of_int(PredSet.cardinal set)),"setsize_"^set_name) :: tail 
	  else tail
       ) interesting_sets)

(* compare a variant's predicate information to a baseline set of information *)
let compare_to_baseline v_ranked_preds v_pred_info v_pred_tbl v_exploded_tbl = 
  (* read in baseline sets *)
  let fin = open_in_bin !baseline_in in
  let (baseline_sets, b_pred_info, b_exploded_tbl) = 
    Marshal.from_channel fin in
    close_in fin;
    (* get variant sets *)
    let variant_sets = get_sets v_ranked_preds v_pred_tbl v_exploded_tbl in 
      (* pair the sets *)
    let paired_set_list = List.combine baseline_sets variant_sets in 
      (* compare the sets, and weight the differences *)
      flatten 
	(map2
	   (fun (b_set, v_set) ->
	      fun pair_name ->
(*		Printf.printf "Baseline set:\n";
		PredSet.iter 
		  (fun (pred_num,counter_num) -> 
		     Printf.printf "Pred: %d counter: %d\n" pred_num counter_num; flush stdout)
		  b_set; 
		Printf.printf "Variant set:\n"; 
		PredSet.iter  
		  (fun (pred_num,counter_num) -> 
		     Printf.printf "Pred: %d counter: %d\n" pred_num counter_num; flush stdout) 
		  v_set;*)
	      (* what is the difference between the sets? *)
		(* NOTE: is this union thing OK? I'm also a complete moron. I don't know if th
		 * weighting makes sense for things like the "important" set. Hm. Think about this. *)
		let diff_set = PredSet.union (PredSet.diff v_set b_set) (PredSet.diff b_set v_set) in 
(*		Printf.printf "Diff set:\n";
		PredSet.iter 
		  (fun (pred_num,counter_num) -> 
		     Printf.printf "Pred: %d counter: %d\n" pred_num counter_num; flush stdout) 
		  diff_set;*)
		let interesting_sets = 
		  generate_interesting_sets b_set diff_set v_exploded_tbl pair_name in
		    generate_weights interesting_sets b_pred_info v_pred_info
           ) paired_set_list set_names)
