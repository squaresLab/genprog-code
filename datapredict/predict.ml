open List
open Utils
open Globals
open Invariant
open State
open Graph 

(* predict operates over a graph to find invariants/states (? locations? Code
   sections?) that are predictive of another predicate *)

module type Predict =
sig
(* these functions return sorted lists. This is also going to change, I'm just
   thinking aloud at this point *)

  type graphT

  val invs_that_predict_invs : graphT -> predicate -> predicate list 

end


module GraphPredict = 
  functor (G : Graph) -> 
    functor (S : PredictState with type t = G.stateT) -> 
struct 

  type graphT = G.t
  type stateT = S.t

  let invs_that_predict_inv graph predictme_inv = 
	(* this doesn't currently do the fancy stuff it was designed to do, but
	   we'll add it back in as we need it *)
	let print_seq_set (run,start,states) =
	  pprintf "SEQ SET: run: %d, start: %d, states: \n    " run start; 
	  IntSet.iter (fun state_id -> pprintf "%d, " state_id) states;
	  pprintf "\n"; flush stdout
	in
    let get_seqs pred =
      let end_states = G.get_end_states graph pred in
		lflat (lmap (fun state -> 
					   G.get_seqs graph state) end_states)
    in
    let runs_where_true = get_seqs predictme_inv in
	let oppi = opposite predictme_inv in
	let runs_where_false = get_seqs oppi in
	let numF = length runs_where_true in 
	let preds = 
	  flatten 
		(map
		   (fun state ->
			  let predicates = S.predicates state in 
				map
				  (fun (pred,obs_at) ->
					 let [(f_P,f_P_obs);(s_P,s_P_obs)] =
					   let get_P_and_obs seq_set (* either runs_where_true or runs_where_false *) = 
						 let exec_filt = (fun (run,start,sids) ->
											let numT,numF = S.overall_pred_on_run state run pred in
											  numT > 0)
						 in 
						 let reg_filt = (fun (run,start,sids) -> IntSet.mem obs_at sids) in
						 let runs_obs = 
						   match pred with
							 Executed -> lfilt exec_filt seq_set
						   | _ -> lfilt reg_filt seq_set 
						 in
						   let trues,falses = G.split_seqs graph runs_obs state pred in
							 let p = length trues in
							   p, (llen runs_obs)
					   in
						 (* this is a little confusing because I keep
							mixing up "runs where false" with "failing
							runs" when really we're trying to be more
							general in terms of predicting when
							something is true or not, in the original
							case, when the program will fail *)
						 map get_P_and_obs [runs_where_true;runs_where_false]
					 in
					 let state, rank = 
					   S.set_and_compute_rank state pred numF f_P
						 f_P_obs s_P s_P_obs in
					   (pred, (S.state_id state), rank))
				  predicates
		   ) (G.states graph))
	in
	  sort
		(fun (p1,s1,rank1) -> fun (p2,s2,rank2) -> 
		   Pervasives.compare rank2.importance rank1.importance)
		preds
	
end

module DynamicPredict = GraphPredict(DynamicExecGraph)(DynamicState)
