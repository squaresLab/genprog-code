open List
open String
open Hashtbl
open Cil
open Globals
open Invariant
open State

module type Graph =
sig

  type transitionsT
  type stateT 
  type t

  val build_graph : (string * string) list -> t
  val new_graph : unit -> t

  val states : t -> stateT list

  val start_state : t -> stateT

  val add_state : t -> stateT -> t
  val add_transition : t -> stateT -> stateT -> int -> t
	
  val next_state : t -> stateT -> int -> stateT
  (* next_state takes a state and a run number and returns the new state that
	 that run moves to from the passed in state *)
  val next_states : t -> stateT -> stateT list
	(* next_states returns a list of all states reachable by all runs
	   from the given state. Good for building *)

  val states_where_true : t -> invariant -> stateT list 

  val get_end_states : t -> invariant -> stateT list
  val get_seqs : t -> stateT list -> stateSeq list
  val split_seqs : t -> stateSeq list -> stateT -> invariant -> stateSeq list * stateSeq list
   
end

module Graph =
  functor (S : State ) ->
struct 

  (* do we need to define these exceptions in the signature? *)
  exception EmptyGraph

  type stateT = S.t
  
  module StateSet = Set.Make(struct 
			       type t = S.t
			       let compare = S.compare
			     end)

  (* state id -> run -> state id *)
  type transitionsT = (int, (int, int) Hashtbl.t) Hashtbl.t 

  type t = {
    states : StateSet.t ;
    forward_transitions : transitionsT ;
    backward_transitions : transitionsT ;
    start_state : stateT;
    pass_final_state : stateT;
    fail_final_state : stateT;
  }
  let site_to_state : (int, S.t) Hashtbl.t = Hashtbl.create 100

  let new_graph () = 
    let start_state = S.new_state (-1) in
    let pass_final_state = S.final_state true in
    let fail_final_state = S.final_state false in 
    let initial_set = 
      let add = StateSet.add in
	(add start_state (add pass_final_state (add fail_final_state
						  StateSet.empty)))
    in
    let states = 
      StateSet.fold
	(fun state ->
	   fun set ->
	     Hashtbl.add site_to_state (S.state_id state) state;
	     StateSet.add state set) StateSet.empty
	initial_set
    in
    { states = states;
      forward_transitions = Hashtbl.create 100;
      backward_transitions = Hashtbl.create 100;
      (* fixme: add start state to hashtable *)
      start_state = start_state;
      pass_final_state = pass_final_state;
      fail_final_state = fail_final_state;
    }

  let states graph = StateSet.elements graph.states
  let start_state graph = graph.start_state

  let final_state graph gorb = 
    if ((get (capitalize gorb) 0) == 'P') 
    then graph.pass_final_state 
    else graph.fail_final_state

(* add_state both adds and replaces states; there will never be duplicates
   because it's a set *)
  let add_state graph state = 
    let states = 
      if StateSet.mem state graph.states then
	StateSet.remove state graph.states
      else graph.states
    in
      {graph with states = StateSet.add state states }

  let add_transition graph (previous : S.t) (next : S.t) run = 
    let inner_trans ht from to2 =
      let innerT = 
	ht_find ht (S.state_id from) (fun x -> Hashtbl.create 100) 
      in
	Hashtbl.add innerT run (S.state_id to2);
	Hashtbl.replace ht (S.state_id from) innerT
    in
      inner_trans graph.forward_transitions previous next;
      inner_trans graph.backward_transitions next previous;
      graph

  let get_trans state trans = 
    let innerT = ht_find trans state (fun x -> Hashtbl.create 100) in
      Hashtbl.fold
	(fun run ->
	   fun state ->
	     fun stateset -> 
	       StateSet.add (Hashtbl.find site_to_state state) stateset)
	innerT StateSet.empty

  let next_states graph state = get_trans state graph.forward_transitions
  let previous_states graph state = get_trans state graph.backward_transitions
		
(* this throws a Not_found if there is no next state for the given state on the
   given run. This is officially Your Problem *)
  let next_state graph state run =
    let nexts = Hashtbl.find graph.forward_transitions state in
      Hashtbl.find nexts run
	
  let states_where_true graph pred = 
    StateSet.elements (StateSet.filter (fun s -> S.is_true s pred) graph.states)
    
  (* between here and "build_graph" are utility functions *)
  let get_and_split_line fin =
    let split = Str.split comma_regexp (input_line fin) in
    let site_num,info = int_of_string (hd split), tl split in
    let (loc,typ,stmt_id,exp) as site_info = Hashtbl.find !site_ht site_num in
      (site_num,info,site_info)
	
  let run_num = ref 0

  let get_run_number fname gorb = begin
    let good = if (get (capitalize gorb) 0) == 'P' then 0 else 1 in 
      if not (Hashtbl.mem !fname_to_run_num fname) then begin
	(add !fname_to_run_num fname !run_num);
	(add !run_num_to_fname_and_good !run_num (fname, good));
	incr run_num
      end;
      Hashtbl.find !fname_to_run_num fname
  end

  let get_name_mval dyn_data = (hd dyn_data), (mval_of_string (hd (tl dyn_data)))
	
  exception EndOfFile of stateT
  exception NewSite of stateT * int* (location * string * int * exp)
    * string list
	
  (* handling this with exceptions is kind of ghetto of me but whatever it
     works *) 
    
  let fold_a_graph graph (fname, gorb) = 
    let fin = open_in fname in 
    let run = get_run_number fname gorb in
      
    let rec add_states (graph : t) (previous : stateT) (site_num : int)
    (site_info : (location * string * int * exp)) (dyn_data : string list ) =

      let rec add_sp_site site_num site_info dyn_data =
	(* name of the variable being assigned to, and its value *)
	let lname,lval = get_name_mval dyn_data in

	(* every site gets its own state, I think *)
	(* thought: how to deal with visits by a run to a site with different values for
	   the stuff tracked at the site? *)
	let state =
	  try Hashtbl.find site_to_state site_num 
	  with Not_found -> begin
	    let new_state = S.add_run (S.new_state site_num) run in
	      Hashtbl.add site_to_state site_num new_state;
	      new_state
	  end
	in

	let rec inner_site (state : S.t) = 
	  (* CHECK: I think "dyn_data" is the same as "rest" in the original code *)
	  let (site_num',dyn_data',site_info') =
	    try get_and_split_line fin 
	    with End_of_file -> raise (EndOfFile(state))
	  in
	    if not (site_num == site_num') then 
	      raise (NewSite(state,site_num',site_info',dyn_data'))
	    else begin (* same site, so continue adding to this state memory *)
	      (* add value of memory to state *)
	      let rname,rval = get_name_mval dyn_data' in
	      let state' = S.add_to_memory state run rname rval in
		
	      (* add predicates to state *)
	      let actual_op = 
		if lval > rval then Gt else if lval < rval then Lt else Eq in
		
	      let comp_exps = 
		List.map 
		  (fun op -> 
		     let value = op == actual_op in
		     let comp_exp =
		       BinOp(op, (Const(CStr(lname))), (Const(CStr(rname))),
			     (TInt(IInt,[]))) in
		       (comp_exp, value)) [Gt;Lt;Eq] 
		  (*		 let exp_str = Pretty.sprint 80 (d_exp () comp_exp) in
				 let loc_str = Pretty.sprint 80 (d_loc () loc) in
				 ("scalar-pairs"^exp_str^loc_str),value)*)
		  (* FIXME: this was once strings, now we want exps; do I want a string producing
		     something somewhere? *)
	      in
	      let state'' = 
		List.fold_left
		  (fun state ->
		     (fun (pred_exp,value) -> 
			S.add_predicate state run pred_exp value))
		       state' comp_exps
		   in 
		    inner_site state''
	    end
	in
	let state', continuation =
	  try 
	    inner_site state 
	  with EndOfFile(state) -> state, (fun (graph,state) -> graph,state)
	  | NewSite(state,site_num',site_info',dyn_data') ->
	      state, (fun (graph',new_state) -> 
			add_states graph' new_state
			  site_num' site_info' dyn_data')
	in
	let graph' = add_state graph state' in
	let graph'' = add_transition graph' previous state' run in 
	  continuation (graph'',state')

      (* this is going to be slightly tricky because we want to guard
	 states internal to an if statement/conditional, which is hard to
	 tell b/c we get the value of the conditional b/f we enter it. *)

      and add_cf_site site_num (loc,typ,stmt_id,exp) dyn_data =
	let value = int_of_string (List.hd dyn_data) in 
	let state =
	  try Hashtbl.find site_to_state site_num 
	  with Not_found -> begin
	    let new_state = S.add_run (S.new_state site_num) run in
	      Hashtbl.add site_to_state site_num new_state;
	      new_state
	  end
	in
	let torf = not (value == 0) in
	let state' = S.add_predicate state run exp torf in 
	let graph' = add_transition graph previous state run in
	let graph'' = add_state graph' state' in
	  graph'', state'

      and get_func (loc,typ,stmt_id,exp) = 
	if typ = "scalar-pairs" 
	then add_sp_site 
	else add_cf_site
      in

      let add_func = get_func site_info in
      let graph',previous' = add_func site_num site_info dyn_data in
	try 
	  let site_num',dyn_data',site_info' = get_and_split_line fin in 
	    add_states graph' previous' site_num' site_info' dyn_data'
	with End_of_file -> 
	  begin
	    close_in fin;
	    let graph'' = 
	      add_transition graph' previous' (final_state graph' gorb) run in
	      graph'', previous'
	  end
      in 
      let site_num,dyn_data,site_info = get_and_split_line fin in 
      let graph',previous' = add_states graph (start_state graph) site_num
	site_info dyn_data in
	graph'

  let build_graph (filenames : (string * string) list) : t = 
    fold_left fold_a_graph (new_graph ()) filenames


  (* the following methods encompass various ways to get subsets of graph
   * states/runs *)

  let get_end_states graph inv = 
    match inv with
      RunFailed -> [graph.pass_final_state], [graph.fail_final_state]
    | RunSucceeded -> [graph.fail_final_state], [graph.pass_final_state]
    | _ -> failwith "Not implemented" 

  (* get seqs returns sequences of states that lead to the end states in the
     passed-in set *)

  let get_seqs graph states = 
    (* OK. Runs contain each set at most once, no matter how many times this run
     * visited it. 
     * And, for now, stateSeqs only start at the start state. The definition is
     * more general than this in case I change my mind later, like for
     * "windows" *)
    let rec one_run s_id run seq = 
      if s_id == (-1) then s_id,run,(IntSet.add s_id seq)
      else begin
	let state_ht = Hashtbl.find graph.backward_transitions s_id in
	let run_prev = Hashtbl.find state_ht run in
	  one_run run_prev run (IntSet.add s_id seq)
      end
    in
    let one_state state = 
      let state_runs = S.runs state in
	map (fun r -> one_run (S.state_id state) r (IntSet.empty)) state_runs
    in
      map one_state states

  (* split_seqs takes a set of sequences and a state and splits them into the
     runs on which the predicate was ever observed to be true and the runs on
     which the predicate was ever observed to be false. These sets can overlap *)
  let split_seqs graph seqs state pred = 
    (* fixme: throw an exception if this state isn't in this sequence? No, that
       makes no sense. Hm. *)
    let eval = 
      map (fun (run,start,set) -> 
	     (run,start,set), 
	     (S.overall_pred_on_run state run pred)) 
	seqs in
      fold_left
	(fun (ever_true,ever_false) ->
	  (fun (seq, (num_true, num_false)) ->
		  if num_true > 0 then
		    if num_false > 0 then
		      (seq :: ever_true, seq :: ever_false)
		    else
		      (seq :: ever_true, ever_false)
		  else
		    if num_false > 0 then
		      (ever_true, seq :: ever_false) 
		    else
		      (ever_true, ever_false)
	       )) ([],[]) eval
      
end

module DynamicExecGraph = Graph(DynamicState)
