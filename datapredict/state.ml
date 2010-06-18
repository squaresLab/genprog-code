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

module type PredictState =
sig
  type t    (* type of state *)


  val state_id : t -> int
  val new_state : int -> t (* the int is the ID number *)
  val final_state : bool -> t

  val overall_pred_on_run : t -> int -> predicate -> int * int
  val observed_on_run : t -> int ->  bool

  val add_run : t -> int -> t
(*  val add_assumption : t -> invariant -> t*)
  val add_predicate : t -> int -> exp -> bool -> t


  val is_true : t -> predicate -> bool
    (* add_to_memory tracks values per run *)
  val add_to_memory : t -> int -> Memory.key -> memV -> t

  val runs : t -> int list



  val set_and_compute_rank : t -> predicate -> int -> int -> int -> int -> int -> (t * rank)

  val predicates : t -> predicate list

  val compare : t -> t -> int

  (* debug *)
  val print_preds : t -> unit 

end 

let id_ref = ref (-1)

module DynamicState =
struct
  (* need some kind of decision about whether to do int/float distinction. Do
     I need the tags? *)

  type final = Not_final | Final of bool

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
    site_num : int ;
    final_status : final ;
    rank : (predicate, rank) Hashtbl.t ;
  }

  let empty_state () = {
    assumptions = [] ;
    memory = Memory.empty ;
    runs = IntMap.empty ;
    predicates = Hashtbl.create 100;
    site_num = (-1) ;
    final_status = Not_final ;
    rank = Hashtbl.create 100 ;
  }
    
  let runs state = 
    IntMap.fold (fun key -> fun count -> fun accum -> key :: accum)
      state.runs []

  let new_state site_num = 
    let news = empty_state () in
      {news with site_num=site_num}

  let predicates state = 
    hfold (fun k -> fun v -> fun accum -> k :: accum) state.predicates []

  let print_preds state = 
    liter 
      (fun pred ->
	 d_pred pred;
	 let innerT = hfind state.predicates pred in 
	   hiter 
	     (fun run -> 
		fun (t,f) -> 
		  pprintf "run %d, t: %d, f: %d\n" run t f) innerT
      ) (predicates state)


  let overall_pred_on_run state run pred = 
    pprintf "state: %d, run: %d, " state.site_num run;
    d_pred pred; flush stdout;
    let predT = 
      ht_find state.predicates pred (fun x -> pprintf "Making hashtbl 1\n"; flush stdout; Hashtbl.create 10) in
      ht_find predT run (fun x -> pprintf "Making hashtbl 2\n"; flush stdout; (0,0))

  let add_assumption state invariant (* -> t *) = failwith "Not implemented"

  let add_predicate state run e torf = 
    let e_pred = (CilExp(e)) in
    let predT = ht_find state.predicates e_pred (fun x -> Hashtbl.create 100) in
    let (numT, numF) = ht_find predT run (fun x -> (0,0)) in
    let (numT',numF') = if torf then (numT + 1, numF) else (numT, numF + 1) in
      Hashtbl.replace predT run (numT',numF');
      Hashtbl.replace state.predicates e_pred predT;
      state
    
  let is_true state = failwith "not implemented"

  let add_to_memory state run key mem = 
    (* fixme: change memory to be run-specific *)
    let new_mem = Memory.add state.memory key mem in
      {state with memory = new_mem}

  let num_runs state = 
    IntMap.fold 
      (fun run_key -> fun num_visited -> fun num_keys -> num_keys + 1)
      state.runs 0

  let observed_on_run state run = IntMap.mem run state.runs

  let add_run state run = 
    (* fixme: make sure we add to this every time a site is visited by a run *)
    let old_val = 
      if IntMap.mem run state.runs then IntMap.find run state.runs else 0 in
    let new_map = IntMap.add run (old_val + 1) state.runs in
      {state with runs=new_map}

  let compare state1 state2 = state1.site_num - state2.site_num

  let final_state torf = 
    decr id_ref;
    let news = empty_state () in
    {news with 
       final_status = Final(torf); 
       site_num = !id_ref}
       
  let state_id state = state.site_num

  let set_and_compute_rank state pred numF f_P f_P_obs s_P s_P_obs = 
    let failure_P = float(f_P) /. (float(f_P) +. float(s_P)) in
    let context = 
      float(f_P_obs) /. (float(f_P_obs) +. float(s_P_obs)) in
    let increase = failure_P -. context in
    let importance = 
      2.0 /.  ((1.0 /. increase) +. (float(numF) /. failure_P))
    in
    let rank = {f_P=f_P; s_P=s_P; f_P_obs=f_P_obs; s_P_obs=s_P_obs;
       numF=numF; failure_P=failure_P; context=context;
       increase=increase; importance=importance} in
      hrep state.rank pred rank;
      (state, rank)
end

type node_type = Uninitialized | Cfg | Ast | Site_node | Abstract
(*
module BPState =
sig
  type t    (* type of state *)
  val state_id : t -> int
  val new_state : int -> t
  val convert : PredictState.t -> t

  val fault_prob : t -> float
  val fix_prob : t -> t -> float
end

module BPState =
  functor (S : PredictState) ->
struct
  type t  =
      {
	(* id is related to the AST/CFG number, right? *)
	id : int ;
	fault_prob : float ;
	fix_prob : (t, float) Hashtbl.t ;
	node_type : node_type ;
	pD : (t, float) Hashtbl.t ;
      }

  let empty_state () = {
    id = -1; 
    fault_prob = 0.0; 
    fix_prob = Hashtbl.create 100;
    node_type = Uninitialized ;
  }

  let state_id state = state.id

  let new_state id = 
    let news = empty_state () in 
      {news with id = id}

  let convert ps = failwith "Not implemented"

  let fault_prob state = state.fault_prob 

  let fix_prob state1 state2 = failwith "Not implemented"

end
*)
