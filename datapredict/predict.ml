open Globals
open Invariant
open Graph 

module type G = Graph
module type S = State

(* a state_sequence is a run number, a start state, and an end state *)
type run = int * State.t * int 

(* FIXME: make sure start and final states have unique ids *)

(* predict operates over a graph to find invariants/states (? locations? Code
   sections?) that are predictive of another predicate *)

module Predict =
sig
(* these functions return sorted lists. This is also going to change, I'm just
   thinking aloud at this point *)

  val invs_that_predict_inv : G -> invariant -> (predicate * int * rank) list 
  val preds_that_predict_inv : G -> predicate -> invariant list 
  val invs_that_predict_pred : G -> invariant -> predicate list 
  val preds_that_predict_pred : G -> predicate -> predicate list 
end

module Predict = 
struct 

  let invs_that_predict_inv graph predictme_inv = 
    (* fixme: potential opt: get rid of states not on any runs *)
    let end_states_pass, end_states_fail = G.get_end_states predictme_inv in
    let runs_pass, runs_fail = G.get_runs relevant_end_states in
    let numF = List.length runs_fail in 
    let preds = 
      List.flatten (
	List.map
	  (fun state ->
	     let state_predicates = S.predicates state in 
	       List.map
		 (fun pred ->
		    let (f_P,f_P_obs),(s_P,s_P_obs) =
		      let get_P_and_obs run_set = 
			let true,false = split_runs run_set state pred in
			let p = List.length true in
			  p, p + (List.length false)
		      in
			List.map get_P_and_obs [runs_pass;runs_fail]
		    in
		    let failure_P = float(f_P) /. (float(f_P) +. float(s_P)) in
		    let context = 
		      float(f_P_obs) /. (float(f_P_obs) +. float(s_P_obs)) in
		    let increase = failure_P -. context in
		    let importance = 
		      2.0 /. ((1.0 /. increase_P) +. (float(numF) / failure_P))
		    in
		      (pred, (S.state_id state),
		       {f_P=f_P; s_P=s_P; f_P_obs=f_P_obs; s_P_obs=s_P_obs;
			numF=numF; failure_P=failure_P; context=context;
			increase=increase; importance=importance}))
		 state_predicates
	  ) (G.states graph))
    in
      List.sort
	(fun (p1,s1,rank1) -> (p2,s2,rank2) -> 
	   Pervasives.compare rank1.importance rank2.importance)
	preds
	
    let preds_that_predict_inv graph predictme_inv = failwith "Not implemented"
    let invs_that_predict_pred graph predictme_pred = failwith "Not implemented"
  let preds_that_predict_pred graph predictme_pred = failwith "Not implemented"

end
