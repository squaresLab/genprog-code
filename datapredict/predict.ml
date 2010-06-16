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
	  G.get_seqs graph end_states
      in
      let oppi =
	match (opposite(Pred(predictme_inv))) with
	  Pred(i) -> i
	| _ -> failwith "Whackadoodle - opposite of predicate is invariant?"
      in
	map get_seqs [predictme_inv;oppi]
    in
    let numF = length runs_where_true in 
      pprintf "there are %d runs where true and %d runs where false\n"
	(llength runs_where_true) (llength runs_where_false); flush stdout;
    let preds = 
      flatten 
	(map
	   (fun state ->
	      pprintf "state is %d\n" (S.state_id state); 
	      pprintf "preds are: \n"; S.print_preds state;
	      let predicates = S.predicates state in 
		(* FIXME problem: using the sequences without checking if
		   this state is *in* that sequence, which is why the
		   numbers are going wonky *)
		map
		  (fun pred ->
		     pprintf "pred is: ";
		     d_pred pred; 
		     let [(f_P,f_P_obs);(s_P,s_P_obs)] =
		       let get_P_and_obs seq_set = 
			 (* (length trues) + (length falses) should =
			    (length seq_set) and I'm pretty sure it
			    doesn't right now. Probably a conceptual
			    thing in terms of what split_seqs is
			    supposed to do! FIXME split seqs is
			    underspecified and is being used incorrectly here. *)
			 let runs_obs = 
			   lfilt (fun (run,start,sids) -> IntSet.mem
				    (S.state_id state) sids) seq_set
			 in
			 let trues,falses =
			   G.split_seqs graph runs_obs state pred in
			   pprintf "trues: %d falses: %d\n" 
			     (llength trues) (llength falses); flush
    stdout;
			 let p = length trues in
			   p, (llength runs_obs)
		       in
			 (* this is a little confusing because I keep
			    mixing up "runs where false" with "failing
			    runs" when really we're trying to be more
			    general in terms of predicting when
			    something is true or not, in the original
			    case, when the program will fail *)
			 map get_P_and_obs [runs_where_true;runs_where_false]
		     in
		       pprintf "numF: %d, f_P: %d, f_P_obs: %d, s_p: %d, s_P_obs: %d, runs: %d\n" numF f_P f_P_obs s_P s_P_obs (llength (S.runs state)); flush stdout;
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
	   Pervasives.compare rank2.importance rank1.importance)
	preds
	
    let preds_that_predict_inv graph predictme_inv = failwith "Not implemented"
    let invs_that_predict_pred graph predictme_pred = failwith "Not implemented"
    let preds_that_predict_pred graph predictme_pred = failwith "Not implemented"

end

module DynamicPredict = GraphPredict(DynamicExecGraph)(DynamicState)
