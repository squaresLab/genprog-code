open Cil
open Globals
open Invariant

type state_run_status = Only_passed | Only_failed | Both

module Memory =
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

type memVMap = memV Memory.t

module type State =
sig
  type t    (* type of state *)

  val site_num : int

  val new_state : int -> t (* the int is the site number *)
  val final_state : bool -> t

  val is_true : t -> invariant -> bool
  val observed_on_run : int -> t -> bool

  val add_run : t -> int -> t
  val add_assumption : t -> invariant -> t
  val add_predicate : t -> int -> exp -> bool -> t
  val add_to_memory : t -> Memory.key -> memV -> t

  val num_runs : t -> int
  val run_status : t -> state_run_status 
  val compare : t -> t -> int

end 

module DynamicState =
struct
  (* need some kind of decision about whether to do int/float distinction. Do
     I need the tags? *)

  type t =  {
    assumptions : invariant list ;  
    (* conditionals guarding this statement *)
    memory : memVMap (* StringMap.t ;*) ;
    
    (* runs maps run numbers to the number of times this run visits this
       state. I think but am not entirely sure that this is a good idea/will
       work *)
    runs : int IntMap.t ;
    (* predicates: map predicate -> int -> num true, num false *)
	(* left off here; still not sure how to do memory efficiently/to suit my
	   needs *)
    predicates : (predicate, (int, (int * int)) Hashtbl.t) Hashtbl.t;
  }

  let empty_state = {
    assumptions = [] ;
    memory = Memory.empty ;
    runs = IntMap.empty ;
    predicates = Hashtbl.create 100;
  }
    
  let new_state site_num = failwith "Not implemented"
(*    let e = empty_state in
    let runs' = IntMap.add run 1 e.runs in 
      {e with loc = loc; runs=runs'}*)
  let new_state passed = failwith "Not implemented"

  let state_with_mem_preds run mem preds =
    let state = new_state run in
      {state with memory=mem; predicates=preds}
	
  (* note to self: this is too much crap. Removing getters like get_location
     and get_assumptions because for now, at least, I don't think we need them.  *)
	
  let is_true state inv (* -> bool *) = failwith "Not implemented"
  let add_assumption state invariant (* -> t *) = failwith "Not implemented"

  let add_predicate state run e torf = 
    let e_pred = (CilExp(e)) in
    let predT = ht_find state.predicates e_pred (fun x -> Hashtbl.create 100) in
    let (numT, numF) = ht_find predT run (fun x -> (0,0)) in
    let (numT',numF') = if torf then (numT + 1, numF) else (numT, numF + 1) in
      Hashtbl.replace predT run (numT',numF');
      Hashtbl.replace state.predicates e_pred predT;
      state
    
  let add_to_memory state key mem (* -> t *) = failwith "Not implemented"

  let num_runs state = failwith "Not implemented"
  let run_status state (* -> state_run_status *) = failwith "Not implemented"
  let is_final state run (* -> bool *) = failwith "Not implemented"

  let observed_on_run state run = failwith "Not implemented"

  let add_run state run = failwith "Not implemented" 
  let compare state1 state2 = failwith "Not implemented"
  let final_state torb = failwith "Not implemented"
  let site_num = failwith "Not implemented"

end

module StateSet = Set.Make(struct 
			     type t = State.t 
			     let compare s1 s2 = compare s1.site_num
			     s2.site_num)

			       
