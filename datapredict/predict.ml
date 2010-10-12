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
    pprintf "invs that predict_inv, predicting: %s\n" (d_pred predictme_inv); flush stdout;
    
    let get_seqs pred =
      let end_states = G.get_end_states graph pred in
		pprintf "num end states: %d\n" (llen end_states); flush stdout;
		G.get_seqs graph end_states 
    in
    let runs_where_true = get_seqs predictme_inv in
      pprintf "runs where true length: %d\n" (llen runs_where_true); flush stdout;
      let oppi = opposite predictme_inv in
      let runs_where_false = get_seqs oppi in
		pprintf "runs where false length: %d\n" (llen runs_where_false); flush stdout;
		let numF = length runs_where_true in 
		let preds = 
		  flatten 
			(map
			   (fun state ->
				  let predicates = S.predicates state in 
					pprintf "at state %d\n" (S.state_id state); flush stdout;
					map
					  (fun (pred,obs_at) ->
						 pprintf "pred: %s, obs_at %d\n" (d_pred pred) obs_at; flush stdout;
						 let [(f_P,f_P_obs);(s_P,s_P_obs)] =
						   let get_P_and_obs seq_set (* either runs_where_true or runs_where_false *) = 
							 let runs_obs = 
							   (* can we track "observed" differently? *)
							   (* in which state is it tested? Can we ask that? *)
							   lfilt (fun (run,start,sids) -> 
										pprintf "stateid: %d, run num: %d, start num: %d, seq set: " (S.state_id state) run start;
										IntSet.iter (fun s -> pprintf "%d, " s) sids;
										pprintf "\n"; 
										flush stdout; IntSet.mem obs_at sids) seq_set
							 in
							   pprintf "state: %d length seq_set: %d, runs_obs: %d\n" (S.state_id state) (llen seq_set) (llen runs_obs); flush stdout;
							   let trues,falses = G.split_seqs graph runs_obs state pred in
								 pprintf "length runs_obs: %d length trues: %d, length falses: %d\n" (llen runs_obs) (llen trues) (llen falses); flush stdout;
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
						   pprintf "f_P: %d, f_P_obs: %d, s_P: %d, s_P_obs: %d\n" f_P f_P_obs s_P s_P_obs;
						   flush stdout;

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
