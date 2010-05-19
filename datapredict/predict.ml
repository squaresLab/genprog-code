open Globals
open Invariant
open Graph 

module type G = Graph
module type S = State

type run = int * S.t * int

(* predict operates over a graph to find invariants/states (? locations? Code
   sections?) that are predictive of another predicate *)

module Predict =
sig
(* these functions return sorted lists. This is also going to change, I'm just
   thinking aloud at this point *)

  val invs_that_predict_inv : G -> invariant -> invariant list 
  val preds_that_predict_inv : G -> predicate -> invariant list 
  val invs_that_predict_pred : G -> invariant -> predicate list 
  val preds_that_predict_pred : G -> predicate -> predicate list 
end

module Predict = 
struct 

  let sum_num_runs states = 
	List.fold_left (fun total_num_runs -> fun state ->
					  total_num_runs + (S.num_runs state)) 0 states
	

  let invs_that_predict_pred graph predictme = begin
    (* F(P) = number of failing runs (runs on which the predicate we're trying
    to predict is not true) on which P (candidate predicate for predictiveness) 
    is observed to be true *) 
    (* S(P) = number of successful runs (runs on which the predicate we're
    trying to predict *is* true) on which P (candidate predicate for
    predictiveness) is observed to be true *)
    (* redefine "run" to be "sequence of states" *)
    let states_where_predictme_is_true = G.states_where_true graph predictme in
    let states_where_predictme_is_false = G.states_where_true graph (opposite predictme) in
	let successful_runs = get_runs_leading_to states_where_predictme_is_true graph
      predictme in
	let failing_runs = get_runs_leading_to states_where_predictme_is_true graph
      predictme in

  end
    

end
