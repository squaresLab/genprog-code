open List
open Globals
open Invariant
open State
open Graph 

(* FIXME: make sure start and final states have unique ids *)

(* predict operates over a graph to find invariants/states (? locations? Code
   sections?) that are predictive of another predicate *)

module type Predict =
sig
(* these functions return sorted lists. This is also going to change, I'm just
   thinking aloud at this point *)

  type graphT

  val invs_that_predict_inv : graphT -> invariant -> (predicate * int * rank) list 
  val preds_that_predict_inv : graphT -> predicate -> invariant list 
  val invs_that_predict_pred : graphT -> invariant -> predicate list 
  val preds_that_predict_pred : graphT -> predicate -> predicate list 
end


module GraphPredict = 
  functor (G : Graph) -> 
    functor (S : State with type t = G.stateT) -> 
struct 

  type graphT = G.t
  type stateT = S.t

  let invs_that_predict_inv graph predictme_inv = 
    (* fixme: potential opt: get rid of states not on any runs *)
    let [[runs_where_true];[runs_where_false]] = 
      let get_seqs pred =
	let end_states = G.get_end_states graph pred in
	  pprintf "There are %d end states: " (List.length end_states);
	  liter (fun s -> pprintf " %d, " (S.state_id s)) end_states;
	  pprintf "\n";
	  flush stdout;
	  G.get_seqs graph end_states
	    (* wait, this makes no sense, because we're calling get seqs on both
	       the thing we're predicting and its opposite but end_states and
	       get_seqs does splitting based on the predicting and the opposite,
	       so this isn't going to work. FIXME *)
      in
      let oppi =
	match (opposite(Pred(predictme_inv))) with
	  Pred(i) -> i
	| _ -> failwith "Whackadoodle - opposite of predicate is invariant?"
      in
	map get_seqs [predictme_inv;oppi]
    in
      pprintf "false on %d, true on %d\n"  (llength runs_where_false) (llength runs_where_true);
      flush stdout;
    let numF = length runs_where_false in 
    let preds = 
      flatten 
	(map
	   (fun state ->
	      let predicates = S.predicates state in 
		map
		  (fun pred ->
		     let [(f_P,f_P_obs);(s_P,s_P_obs)] =
		       let get_P_and_obs seq_set = 
			 let trues,falses = G.split_seqs graph seq_set state pred in
			 let p = length trues in
			   p, p + (length falses)
		       in
			 map get_P_and_obs [runs_where_false;runs_where_true]
		     in
		     let failure_P = float(f_P) /. (float(f_P) +. float(s_P)) in
		     let context = 
		       float(f_P_obs) /. (float(f_P_obs) +. float(s_P_obs)) in
		     let increase = failure_P -. context in
		     let importance = 
		       2.0 /.  ((1.0 /. increase) +. (float(numF) /. failure_P))
		     in
		       (pred, (S.state_id state),
			{f_P=f_P; s_P=s_P; f_P_obs=f_P_obs; s_P_obs=s_P_obs;
			 numF=numF; failure_P=failure_P; context=context;
			 increase=increase; importance=importance}))
		  predicates
	   ) (G.states graph))
    in
      sort
	(fun (p1,s1,rank1) -> fun (p2,s2,rank2) -> 
	   Pervasives.compare rank1.importance rank2.importance)
	preds
	
    let preds_that_predict_inv graph predictme_inv = failwith "Not implemented"
    let invs_that_predict_pred graph predictme_pred = failwith "Not implemented"
    let preds_that_predict_pred graph predictme_pred = failwith "Not implemented"

end

module DynamicPredict = GraphPredict(DynamicExecGraph)(DynamicState)
