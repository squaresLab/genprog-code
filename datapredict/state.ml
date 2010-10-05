open List
open Cil
open Globals
open Invariant
open Memory

module type PredictState =
sig
  type t    (* type of state *)

  (* create a state *)
  val new_state : int -> t 
	
  (* add info to the state *)
  val add_run : t -> int -> t * int
  val add_predicate : t -> int -> exp -> bool -> t
  val add_layout : t -> int -> int -> t

  (* get basic info about the state *)
  val state_id : t -> int
  val runs : t -> int list
  val predicates : t -> predicate list

  (* more complex info about the state *)

  val fault_localize : t -> strategy -> float
  val observed_on_run : t -> int ->  bool

  (* evaluate/generate new predicates on this state *)

  val is_pred_ever_true : t -> predicate -> bool
  val overall_pred_on_run : t -> int -> predicate -> int * int

  (* rank computation *)
  val set_and_compute_rank : t -> predicate -> int -> int -> int -> int -> int -> (t * rank)

  (* for state sets *)
  val compare : t -> t -> int

  (* debug *)
  val print_vars : t -> unit
  val print_preds : t -> unit 

end 

module DynamicState =
struct
  (* need some kind of decision about whether to do int/float distinction. Do
     I need the tags? *)

  type t =  {
    site_num : int ;
    memory : Memory.t;
	(* memory maps run, count to memory layout id. *)
    (* maps run numbers to the number of times this run visits this
       state. I think but am not entirely sure that this is a good idea/will
       work *)
    runs : int IntMap.t ;
    (* predicates: map predicate -> int -> num true, num false *)
    predicates : (predicate, (int, (int * int)) Hashtbl.t) Hashtbl.t;
    (* FIXME: I actually think we can get rid of predict.ml's business
     * almost entirely, because given a list of "failing" runs, a state
     * should be able to do all that computation on its own, without
     * help, or close to it, if we change this datatype a little bit
     * (since predicate needs to be predicting something, which right
     * now it's not) *)
    rank : (predicate, rank) Hashtbl.t ;
  }

  let empty_state () = {
    site_num = (-1) ;
    memory = Memory.new_state_mem ();
    runs = IntMap.empty ;
    predicates = Hashtbl.create 100;
    rank = Hashtbl.create 100 ;
  }
	
  (******************************************************************)

  let new_state site_num = 
    let news = empty_state () in
      {news with site_num=site_num}

  (******************************************************************)

  let add_run state run = 
    let old_val = 
      if IntMap.mem run state.runs then IntMap.find run state.runs else 0 in
    let new_map = IntMap.add run (old_val + 1) state.runs in
      {state with runs=new_map}, (old_val + 1)

  let add_predicate state run e torf = 
	(* somehow this is having trouble with the predicates made for s-p sites *)
	let exp_str = Pretty.sprint 80 (d_exp () e) in
	let tstr = if torf then "true" else "false" in
	pprintf "run: %d site: %d pred: %s torf: %s\n" run state.site_num exp_str tstr; flush stdout;
    let e_pred = (CilExp(e)) in
	  pprintf "before ht_find 1\n"; flush stdout;
    let predT = ht_find state.predicates e_pred (fun x -> pprintf "not found in state predicates file\n"; flush stdout;
	Hashtbl.create 100) in
	  pprintf "before ht_find 2\n"; flush stdout;

    let (numT, numF) = ht_find predT run (fun x -> pprintf "not found in predT\n"; flush stdout; (0,0)) in
	  pprintf "after ht_find\n"; flush stdout;
    let (numT',numF') = if torf then (numT + 1, numF) else (numT, numF + 1) in
      hrep predT run (numT',numF');
      hrep state.predicates e_pred predT;
      state

  (* add a memory layout to this run for this count, which should be the
   * current number for this run in state.runs *)

  let add_layout state run layout_id = 
	let count = IntMap.find run state.runs in
	  (* do we care about the order? Can we just track total count instead of
	   * actual iterations? *)
	  {state with memory = (Memory.add_layout state.memory run count layout_id)}

  (******************************************************************)

  let state_id state = state.site_num

  let runs state = 
    IntMap.fold (fun key -> fun count -> fun accum -> key :: accum)
      state.runs []

  let predicates state = 
    (* fixme: maybe distinguish between the default, site-specific
     * predicates and the ones we add as we go? At least for the
     * purposes of standard SBI stuff? *)
    hfold (fun k -> fun v -> fun accum -> k :: accum) state.predicates []

  (******************************************************************)

  let observed_on_run state run = IntMap.mem run state.runs

  let fault_localize state strat = 
    let highest_rank compfun valfun =
      let ranks = ht_vals state.rank in
      let sorted_ranks = sort compfun ranks in
	try valfun (hd sorted_ranks) with _ -> 0.0
    in
      match strat with
	  Intersect(w) -> 
	    let on_failed_run,on_passed_run = 
	      hfold 
		(fun run -> 
		   fun (_,f) -> 
		     fun (failed,passed) -> 
		       if observed_on_run state run then 
			 if f == 1 then (true,passed)
			 else failed,true
		       else (failed,passed))
		!run_num_to_fname_and_good (false,false)
	    in
	      if not on_failed_run then 0.0 else 
		if on_passed_run then w else 1.0
	| FailureP ->
	    highest_rank 
	      (fun rank1 -> fun rank2 ->
		 Pervasives.compare rank2.failure_P rank1.failure_P)
	      (fun rank -> rank.failure_P)
	| Increase -> 
	    highest_rank 
	      (fun rank1 -> fun rank2 ->
		 Pervasives.compare rank2.increase rank1.increase)
	      (fun rank -> rank.increase)
	| Context ->
	    highest_rank 
	      (fun rank1 -> fun rank2 ->
		 Pervasives.compare rank2.context rank1.context)
	      (fun rank -> rank.context)
	| Importance -> 
	    highest_rank 
	      (fun rank1 -> fun rank2 ->
		 Pervasives.compare rank2.importance rank1.importance)
	      (fun rank -> rank.importance)
	| Random -> Random.float 1.0
	| Uniform -> 1.0 

  let eval_new_pred state pred = 
	let newPredT = hcreate 10 in
	  (* all_layouts may be empty: state may be a cfg state, 
	   * or this state may not be observed on this run *)
	  liter 
		(fun run ->
		   let (numT,numF) = 
			 Memory.eval_pred_on_run state.memory run pred
		   in
			 hadd newPredT run (numT, numF);
		) (runs state);
	  hrep state.predicates pred newPredT
	
  let is_pred_ever_true state pred = 
	if not (hmem state.predicates pred) then eval_new_pred state pred;
	let predT = hfind state.predicates pred in
	  hfold
		(fun run ->
		   fun (t,f) ->
			 fun accum ->
			   if t > 0 then true else accum) predT false

  let overall_pred_on_run state run pred = 
	if not (hmem state.predicates pred) then eval_new_pred state pred;
	let predT = hfind state.predicates pred in
	  try 
		hfind predT run
	  with Not_found ->
		begin
		  hadd predT run (0,0);
		  hrep state.predicates pred predT;
		  (0,0)
		end

  (******************************************************************)

  let set_and_compute_rank state pred (numF : int) (f_P : int)
    (f_P_obs : int) (s_P : int) (s_P_obs : int) = 
    let failure_P : float = 
	  float(f_P) /. 
		(float(f_P) +. 
		   float(s_P)) in
    let context : float = 
      float(f_P_obs) /. (float(f_P_obs) +. float(s_P_obs)) in
    let increase : float = failure_P -. context in
    let importance : float = 
      2.0 /.  ((1.0 /. increase) +. (float(numF) /. failure_P))
    in
    let rank = 
	  {f_P=f_P; 
	   s_P=s_P; 
	   f_P_obs=f_P_obs; 
	   s_P_obs=s_P_obs;
       numF=numF; 
	   failure_P=failure_P; 
	   context=context;
       increase=increase; 
	   importance=importance} in
      hrep state.rank pred rank;
      (state, rank)

  (******************************************************************)

  let compare state1 state2 = state1.site_num - state2.site_num

  (******************************************************************)

  let print_vars state = 
	Memory.print_in_scope state.memory

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
      
end

(*type node_type = Uninitialized | Cfg | Ast | Site_node | Abstract

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
