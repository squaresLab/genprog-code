open Globals

type state_run_status = Only_passed | Only_failed | Both

module type State =
  (* make this a functor that takes memory layout type? *)
  (* right now it'll map strings to things of type memV, but maybe we want to
	 separate that out as well *)
sig
  type t    (* type of state *)

  val new_state : int -> location -> t (* the int is the run number *)
  val state_copy : t -> int -> ?loc:location -> t 
	(* make a copy of a state, but it's unique to the run whose signifier is
	   passed in. If no location is passed use the one from the state we're
	   copying. *) 
									
  val is_true : t -> invariant -> bool
  val add_assumption : t -> invariant -> t

  val clear_memory : t -> t
  val add_to_memory : t -> string -> memV -> t
  val change_memory : t -> string -> memV -> t

  (* information about the state *)	
  val run_status : t -> state_run_status 

(* the same state may not be final for all runs. We either want to make them
   different states (split them??) or just be able to tell based on which run it
   is (the approach for now) *)
  val set_final : t -> int -> t 
  val is_final : t -> int -> boolean
  val observed_on_run : int -> t -> boolean

end

module DynamicState
struct
	(* need some kind of decision about whether to do int/float distinction. Do
	   I need the tags? *)

  type t =  {
	val assumptions : invariant list ;  
	(* conditionals guarding this statement *)
	(* somewhere we need to track the "run failed" invariant on states. I think
	   this doesn't go in the state because it's a feature of a run, no? But we
	   want to know if a state corresponds to a passed or a failed run, so maybe
	   we can add that to this struct? *) 
	val sigma : memV StringMap.t ;
	val loc : Cil.location ; (* location in code *)  

	(* runs maps run numbers to the number of times this run visits this
	   state. I think but am not entirely sure that this is a good idea/will
	   work *)
	val runs : int IntMap.t ;
	val predicates : int StringMap.t ; (* I think this is a good idea. Maybe
										  map predicates to ints instead of
										  strings? or keep the strings but hash
										  the predicates so we can find them again. *)
	val final : bool IntMap.t ;
  }

  let empty_state = {
	assumptions = [] ;
	sigma = StringMap.empty ;
	loc = locUnknown ;
	runs = IntMap.empty ;
	predicates = StringMap.empty ;
	final = IntMap.empty
  }

  let new_state run loc = 
	let e = empty_state in
	let runs' = IntMap.add run 1 e.runs in 
	  {e with loc = loc; runs=runs'}

  let state_copy state run (?loc:location) (* -> t *) = failwith "Not implemented" 

  (* note to self: this is too much crap. Removing getters like get_location
	 and get_assumptions because for now, at least, I don't think we need them.  *)

  let is_true state inv (* -> bool *) = failwith "Not implemented"
  let add_assumption state invariant (* -> t *) = failwith "Not implemented"

  let clear_memory state = {state with sigma = StringMap.empty }
  let add_to_memory state key mem (* -> t *) = failwith "Not implemented"
  let change_memory state key mem (* -> t *) = failwith "Not implemented"

  let run_status state (* -> state_run_status *) = failwith "Not implemented"
  let set_final state run (* -> t *) = failwith "Not implemented"
  let is_final state run (* -> boolean *) = failwith "Not implemented"

  let observed_on_run state run = List.mem run state.runs 

end
