open Cil
open Globals

type state_run_status = Only_passed | Only_failed | Both

(* Layout is a stupid name for this and I think this whole setup is dumb and
   unecessary but I can't figure out how else to do it *)
module Layout =
struct
  type +'a t = 'a StringMap.t
  type key = string

  let add map k v = StringMap.add k v map
  let incr map k v =
	let curr = 
	  if StringMap.mem k map then StringMap.find k map else 0 
	in
	  StringMap.add k (v + curr) map

  let mem map k = StringMap.mem k map
  let find map k = StringMap.find k map
  let make () = StringMap.empty 
  let empty = StringMap.empty
end

type memVMap = memV Layout.t
type predMap = int Layout.t

module type State =
sig
  type t    (* type of state *)

  val new_state : int -> location -> t (* the int is the run number *)
  val state_with_mem_preds : int -> location -> memVMap -> predMap -> t

  val is_true : t -> invariant -> bool
  val add_run : t -> int -> t

  val add_assumption : t -> invariant -> t

  val add_to_memory : t -> Layout.key -> memV -> t

  (* information about the state *)	
  val run_status : t -> state_run_status 

(* the same state may not be final for all runs. We either want to make them
   different states (split them??) or just be able to tell based on which run it
   is (the approach for now) *)
  val set_final : t -> int -> t 
  val is_final : t -> int -> bool
  val observed_on_run : int -> t -> bool

  val states_equal : t -> t -> bool
end 

module DynamicState =
struct
	(* need some kind of decision about whether to do int/float distinction. Do
	   I need the tags? *)

  type t =  {
	assumptions : invariant list ;  
	(* conditionals guarding this statement *)
	(* somewhere we need to track the "run failed" invariant on states. I think
	   this doesn't go in the state because it's a feature of a run, no? But we
	   want to know if a state corresponds to a passed or a failed run, so maybe
	   we can add that to this struct? *) 
	memory : memVMap (* StringMap.t ;*) ;
	loc : Cil.location ; (* location in code *)  

	(* runs maps run numbers to the number of times this run visits this
	   state. I think but am not entirely sure that this is a good idea/will
	   work *)
	runs : int IntMap.t ;
	predicates : predMap; (* I think this is a good idea. Maybe
										  map predicates to ints instead of
										  strings? or keep the strings but hash
										  the predicates so we can find them again. *)
	final : bool IntMap.t ;
  }

  let empty_state = {
	assumptions = [] ;
	memory = Layout.empty ;
	loc = locUnknown ;
	runs = IntMap.empty ;
	predicates = Layout.empty ;
	final = IntMap.empty
  }

  let new_state run loc = 
	let e = empty_state in
	let runs' = IntMap.add run 1 e.runs in 
	  {e with loc = loc; runs=runs'}

  let state_with_mem_preds run loc mem preds =
	let state = new_state run loc in
	  {state with memory=mem; predicates=preds}

  (* note to self: this is too much crap. Removing getters like get_location
	 and get_assumptions because for now, at least, I don't think we need them.  *)

  let is_true state inv (* -> bool *) = failwith "Not implemented"
  let add_assumption state invariant (* -> t *) = failwith "Not implemented"

  let add_to_memory state key mem (* -> t *) = failwith "Not implemented"

  let run_status state (* -> state_run_status *) = failwith "Not implemented"
  let set_final state run (* -> t *) = failwith "Not implemented"
  let is_final state run (* -> bool *) = failwith "Not implemented"

  let observed_on_run state run = failwith "Not implemented"

  let states_equal state1 state2 = failwith "Not implemented"
  let add_run state run = failwith "Not implemented" 

end
