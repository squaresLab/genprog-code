open Batteries
open Map
open List
open Cil
open Utils
open Globals
open DPGlobs
open Invariant
open Memory

module type PredictState =
sig
  type t    (* type of state *)

  (* create a state *)
  val new_state : int -> t 
	
  (* add info to the state *)
  val add_run : t -> int -> t
  val add_predicate : t -> int -> (int * predicate) -> bool -> int -> t
  val add_layout : t -> int -> int -> t

  (* get basic info about the state *)
  val state_id : t -> int
  val runs : t -> int list
  val predicates : t -> (predicate * int) list
  val is_ever_executed : t -> bool

  (* more complex info about the state *)

  val fault_localize : t -> strategy -> float
  val fix_localize : t -> strategy -> float
  val observed_on_run : t -> int ->  bool

  (* evaluate/generate new predicates on this state *)

  val is_pred_ever_true : t -> predicate -> bool
  val overall_pred_on_run : t -> int -> predicate -> int * int

  (* rank computation *)
  val set_and_compute_rank : t -> predicate -> predicate -> int -> int -> int -> int -> int -> (t * rank)

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

  (* a dynamic state is actually a statement that was executed at least once *)

  type t =  {
    stmt_id : int ;
    memory : Memory.t;
	(* memory maps run, count to memory layout id. *)
    (* maps run numbers to the number of times this run visits this state. I
       think but am not entirely sure that this is a good idea/will work *)
    runs : int IntMap.t ;
    (* predicates: map predicate -> int -> num true, num false *)
    predicates : (predicate, int * (int, (int * int)) Hashtbl.t) Hashtbl.t;
    rank : (predicate, ((predicate, rank) Hashtbl.t)) Hashtbl.t ;
  }

  let empty_state () = {
    stmt_id = (-1) ;
    memory = Memory.new_state_mem ();
    runs = IntMap.empty ;
    predicates = hcreate 100;
    rank = hcreate 100 ;
  }
	
  (******************************************************************)

  let new_state site_num = 
    let news = empty_state () in
      {news with stmt_id=site_num}

  (******************************************************************)

  let add_run state run = 
    let old_val = 
      if IntMap.mem run state.runs then IntMap.find run state.runs else 0 in
    let new_map = IntMap.add run (old_val + 1) state.runs in
      {state with runs=new_map}

  let add_predicate state run (num,pred) torf count = 
    let (_,predT) = ht_find state.predicates pred 
      (fun x -> 0, (hcreate 100)) in
    let (numT, numF) = ht_find predT run (fun x -> (0,0)) in
    let (numT',numF') = if torf then (numT + count, numF) else (numT, numF + count) in
      hrep predT run (numT',numF');
      hrep state.predicates pred (num,predT);
      state

  (* add a memory layout to this run for this count, which should be the
   * current number for this run in state.runs *)

  let add_layout state run layout_id =
	let count = IntMap.find run state.runs in
	  (* do we care about the order? Can we just track total count instead of
	   * actual iterations? *)
	  {state with memory = (Memory.add_layout state.memory run count layout_id)}

  (******************************************************************)

  let state_id state = state.stmt_id

  let runs state = 
    IntMap.fold (fun key -> fun count -> fun accum -> key :: accum)
      state.runs []

  let predicates state = 
    (* fixme: maybe distinguish between the default, site-specific
     * predicates and the ones we add as we go? At least for the
     * purposes of standard SBI stuff? *)
    hfold (fun k -> fun (n,v) -> fun accum -> (k,n) :: accum) state.predicates []

  let is_ever_executed state = hmem state.predicates (Executed)

  (******************************************************************)

  let eval_new_pred state pred = 
	pprintf "\n\nEVAL NEW PRED: state: %d, pred: %s\n" state.stmt_id (d_pred pred); flush stdout;
	(* first, try to find some variation on this predicate in the predicate
	 * table.  This is just a heuristic/hack, it's not comprehensive.  a > b
	 * could be found if we've done either b <= a, or a <= b *)
	let sid,predT =
	  if hmem state.predicates (flip pred) then hfind state.predicates (flip pred)
	  else if hmem state.predicates (opposite pred) then begin
		let num,oppPredT = hfind state.predicates (opposite pred) in
		  (num, (hfold
				   (fun run ->
					  fun (t,f) ->
						fun newT ->
						  hadd newT run (f,t); newT) 
				   oppPredT (hcreate 10)))
	  end else 
		(* if you don't find it, then actually evaluate it *)
		(state.stmt_id, 		  
		 (lfoldl
			(fun newPredT ->
			   fun run ->
				 let (numT,numF) = 
				   Memory.eval_pred_on_run state.memory run pred
				 in
				   hadd newPredT run (numT, numF); newPredT
			) (hcreate 10) (runs state)))
	in
	  hrep state.predicates pred (sid,predT)

  let overall_pred_on_run state run pred = 
	pprintf "OVERALL_PRED_ON_RUN: state: %d run: %d pred: %s\n" state.stmt_id run (d_pred pred); flush stdout;
    if not (hmem state.predicates pred) then eval_new_pred state pred;
    let (n,predT) = hfind state.predicates pred in
      try 
		hfind predT run
      with Not_found ->
		begin
		  hadd predT run (0,0);
		  hrep state.predicates pred (n,predT);
		  (0,0)
		end

  let observed_on_run state run = IntMap.mem run state.runs

  let localize state strat predicting = 
	let split_runs_by_pred state pred =
	  List.partition 
		(fun run -> 
		   let numT, nmF = overall_pred_on_run state run pred in
			 numT > 0) (runs state) in

	(* assumes rank has been computed for whatever you're trying to localize
	 * on; will fail otherwise *)
	let predictedT = hfind state.rank predicting in
	  (* predictedT maps predicting predicates at this state to ranks *)
	let ranks = List.of_enum (Hashtbl.enum predictedT) in
	let no_executed_ranks = 
	  List.filter (fun(predicting,rank) -> match predicting with Executed -> false | _ -> true) ranks
	in 
    let highest_rank whichranks compfun valfun =
	  let compfun = (fun (pred1,rank1) -> fun (pred2, rank2) -> compfun rank1 rank2) in
	  let valfun = (fun (pred,rank) -> valfun rank) in
	  let sorted = sort ?cmp:(Some(compfun)) whichranks in 
		try valfun (hd sorted) with _ -> 0.0
    in
	  match strat with
		Intersect(w) -> 
		  let (_,execT) = hfind state.predicates (Executed) in
		  let runs_true, runs_false = split_runs_by_pred state predicting in
		  let [on_true_run;on_false_run] = 
			lmap (fun runs -> 
					List.exists 
					  (fun run -> 
						 let num_true,num_false = ht_find execT run (fun x -> (0,0)) in
						   (num_true > 0)) runs) 
			  [runs_true;runs_false] in
			if not on_false_run then 0.0 else 
			  if on_true_run then w else 1.0
	  | FailureP -> highest_rank no_executed_ranks fail_sort failure
	  | Increase -> highest_rank no_executed_ranks inc_sort inc 
	  | Context -> highest_rank ranks con_sort con
	  | Importance -> highest_rank no_executed_ranks imp_sort imp
	  | Random -> Random.float 1.0
	  | Uniform -> 1.0

  let fault_localize state strat = localize state strat (RunFailed)
  let fix_localize state strat = localize state strat (RunSucceeded)


  let is_pred_ever_true state pred = 
    if not (hmem state.predicates pred) then eval_new_pred state pred;
    let (_,predT) = hfind state.predicates pred in
	let res = 
      hfold
		(fun run ->
		   fun (t,f) ->
			 fun accum ->
			   if t > 0 then true else accum) predT false
	in	
	  pprintf "Is pred %s ever true in state %d?\n" (d_pred pred) state.stmt_id; flush stdout;
	  if res then pprintf "Yes!\n" else pprintf "No!\n"; res

  (******************************************************************)

  let set_and_compute_rank state thing_being_predicted pred numT t_P t_P_obs f_P f_P_obs = 
    let failure_P =
      float(t_P) /. 
		(float(t_P) +. 
		   float(f_P)) in
    let context = float(t_P_obs) /. (float(t_P_obs) +. float(f_P_obs)) in
    let increase = let inc = failure_P -. context in if inc > 0.0 then inc else 0.0 in
(*    let importance = 2.0 /.  ((1.0 /. increase) +. (float(numT) /. failure_P))*)
    let importance = 2.0 /.  ((1.0 /. increase) +. (1.0 /. failure_P))

    in
	  pprintf "thing_true_P: %g, context: %g increase: %g, importance: %g\n\n" failure_P context increase importance; flush stdout;
    let rank = 
      {f_P=t_P; 
       s_P=f_P; 
       f_P_obs=t_P_obs; 
       s_P_obs=f_P_obs;
       numF=numT; 
       failure_P=failure_P; 
       context=context;
       increase=increase; 
       importance=importance} in
	let ht = ht_find state.rank thing_being_predicted (fun x -> hcreate 10) in
	  hrep ht pred rank;
      hrep state.rank thing_being_predicted ht;
      (state, rank)

  (******************************************************************)

  let compare state1 state2 = state1.stmt_id - state2.stmt_id

  (******************************************************************)

  let print_vars state = 
	Memory.print_in_scope state.memory

  let print_preds state = 
    liter 
      (fun (pred,_) ->
		 pprintf "%s:\n" (d_pred pred);
		 let (n,innerT) = hfind state.predicates pred in 
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
