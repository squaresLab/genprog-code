open Globals
open Graph 

module type G = Graph

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

  let invs_that_predict_inv graph predictme = begin


  end
    

end
