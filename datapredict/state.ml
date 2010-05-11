open Cil

module type State =
sig
  type t (* type of state *)
	
end

type invariant = CilExp of exp | ReturnVal of exp | RunFailed
	(* this is definitely tentative *)

type rank = { (* sum or record? *)
  val f_P : float;
  val s_P : float;
  val failure_P : float;
  val context : float;
  val increase : float;
  val f_P_obs : float;
  val s_P_obs : float;
  val numF : int;
  val importance : float;
}

type location = Cil.location

module type Graph =
  functor (S : State ) ->
sig
  type t (* type of graph *)
  val states : t -> S list
  val next_state : t -> S -> int -> S (* takes a state and a run number and
										 returns the next state *)
  val get_states_where_true : t -> invariant -> S list (* return the states
														  where a predicate
														  is true *)
  val is_true_at_state : t -> S -> invariant -> bool

  val get_predictive_invariants : t -> invariant -> (invariant * rank) list 
	(* returns invariants that are predictive of some other invariant *)
	(* thought: do we want to return just invariants or invariants and
	   locations? *)
end

type state = {
  val assumptions : exp list ;  (* conditionals guarding this statement *)
  val sigma : exp StringMap.t ; (* I actually want to map to ints or floats, not
						 		 * exps. I think. Maps strings (variable names)
								 * to values *)
  val loc : Cil.location ; (* location in code *)  
}


module DynamicState
struct
  type t = state

end
