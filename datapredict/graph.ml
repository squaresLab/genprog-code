open Printf
open List
open String
open Hashtbl
open Cil
open Utils
open Globals
open Invariant
open Memory
open State

module type Graph =
sig
  type stateT 
  type t

  val build_graph : (string * string) list -> t
  val states : t -> stateT list

  (* print every statement in the graph, with a fault localization value;
   * first bool is "naive baselines", second bool is whether to do
   * CBI-localization (right now print all three values) and the float
   * list is a set of weights to give the good path
   * 
   * Output: each localization strategy gets its own file.  Top line
   * of the file is the weighted fault path.  
   *)
  val print_fault_localization : t -> bool -> bool -> float list -> unit

  (* print every statement in the graph, with a localization value;
   * first bool is random, second bool is whether to do
   * CBI-localization (right now print all three values) and the float
   * list is a set of weights to give the good path *)
(*  val print_fix_localization : t -> bool -> bool -> float list -> unit*)

  (* these are used for traditional Statistical Bug Isolation-style
   * statistics. Designed to be slightly more general, but same
   * general idea.
  * In general, the methods encompass various ways to get subsets of graph
   * states/runs *)

  val get_end_states : t -> predicate -> stateT list

  (* get seqs returns sequences of states that lead to the end states in the
     passed-in set *)
  val get_seqs : t -> stateT list -> stateSeq list

  (* split_seqs takes a set of sequences and a state and splits them
   * into the runs on which the predicate was ever observed to be true
   * and the runs on which the predicate was ever observed to be
   * false. The issue of the predicate not being observed on the
   * sequence is currently handled elsewhere, which is maybe a little
   * awkward? *)
  val split_seqs : t -> stateSeq list -> stateT -> predicate -> stateSeq list * stateSeq list

  (* what if you have a predicate that you care about? Can we
   * evaluate it at all the other states? Of course! But not by
   * default, because that's crazy-talk; you have to ask for it. *)

  val propagate_predicate : t -> predicate -> unit

  (* for debug purposes: *)
  val print_graph : t -> unit
   
end

module PredictGraph =
  functor (S : PredictState ) ->
struct 

  type stateT = S.t
  type transitionsT = (int, (int, IntSet.t) Hashtbl.t) Hashtbl.t 

  module StateSet = 
    Set.Make(struct 
	       type t = S.t 
	       let compare = S.compare
	     end)

  type t = {
    states : (int, S.t) Hashtbl.t; 
	vars : (string, exp) Hashtbl.t;
    forward_transitions : transitionsT ;
    backward_transitions : transitionsT ;
    start_state : int;
    pass_final_state : int;
    fail_final_state : int;
  } 

  let new_graph () = 
    let states = Hashtbl.create 100 in
      liter (fun sid -> 
			   let s = S.new_state sid in
				 hadd states sid s) [-1;-2;-3];
      { states = states;
		vars = Hashtbl.create 100;
		forward_transitions = Hashtbl.create 100;
		backward_transitions = Hashtbl.create 100;
		(* fixme: add start state to hashtable *)
		start_state = -1;
		pass_final_state = -2;
		fail_final_state = -3;
      }

  let states graph = 
    hfold
      (fun num ->
		 fun state ->
		   fun accum ->
			 state :: accum) graph.states []

  let print_fault_localization graph do_baselines do_cbi weights = 
	let strats = 
	  (if do_baselines then [Random;Uniform] else []) @
		(if do_cbi then [FailureP;Increase;Context;Importance] else []) @
		(lmap (fun w -> Intersect(w)) weights)
	in
	let strats_outs =
	  lmap 
		(fun strat -> 
		   let outfile = 
			 sprintf "%s-%s-fault_local.txt" !name (strat_to_string strat) in
		   let fout = open_out outfile in
			 strat, fout)
		strats
	in
	  liter
		(fun state ->
		   if (S.state_id state) >= 0 then 
			 begin
			   liter
				 (fun (strat,fout) ->
					let id = S.state_id state in
					let local_val = S.fault_localize state strat in
					let out_str = sprintf "%d,%g\n" id 
					  (match classify_float local_val with 
					   | FP_nan -> 0.0
					   | _ -> local_val)
					in
					  output_string fout out_str;
				 ) strats_outs
			 end
		) (states graph);
	  liter (fun (strat,fout) -> close_out fout) strats_outs 
		
  let final_state graph gorb = 
    if ((get (capitalize gorb) 0) == 'P') 
    then hfind graph.states graph.pass_final_state 
    else hfind graph.states graph.fail_final_state

(* add_state both adds and replaces states; there will never be duplicates
   because it's a set *)
  let add_state graph state = 
    hrep graph.states (S.state_id state) state;
    graph

  (* only add a transition once *)
  let add_transition graph (previous : S.t) (next : S.t) run = 
    let inner_trans ht from to2 =
      let innerT = 
	ht_find ht (S.state_id from) (fun x -> Hashtbl.create 100) 
      in
      let destset = 
	let d = 
	try Hashtbl.find innerT run 
	with _ -> IntSet.empty
	in IntSet.add (S.state_id to2) d
      in
	Hashtbl.replace innerT run destset;
	Hashtbl.replace ht (S.state_id from) innerT
    in
      inner_trans graph.forward_transitions previous next;
      inner_trans graph.backward_transitions next previous;
      graph

  let get_trans graph state trans = 
    let innerT : (int, Utils.IntSet.t) Hashtbl.t = 
      ht_find trans state (fun x -> Hashtbl.create 100) in
      hfold
	(fun (run : int) ->
	   fun (runs_dests : Utils.IntSet.t) ->
	     fun (all_dests : StateSet.t) ->
	       IntSet.fold
		 (fun (dest : int)  ->
		    fun (all_dests : StateSet.t) ->
		      StateSet.add
			(hfind graph.states dest) all_dests)
		 runs_dests all_dests)
	innerT StateSet.empty

  let next_states graph state = 
    StateSet.elements (get_trans graph state graph.forward_transitions)

  let previous_states graph state = get_trans graph state graph.backward_transitions
		
(* this throws a Not_found if there is no next state for the given state on the
   given run. This is officially Your Problem *)
  let next_state graph state run =
    let nexts = Hashtbl.find graph.forward_transitions state in
      Hashtbl.find nexts run
	
  (* between here and "build_graph" are utility functions *)
  let get_and_split_line fin =
(*	printf "line read: %d\n" !count; flush stdout; incr count;*)
	let in_line = input_line fin in
(*	  printf "in line: %s\n" in_line; flush stdout;*)
    let split = Str.split comma_regexp in_line in
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
	
    
(* OK, the problem is that the states in the state set are not being
   updated when the state is being updated, so we're adding runs to
   the start state, for example, but it's not being reflected in the
   state in the state set *)

  let fold_a_graph graph (fname, gorb) = 
	printf "Debug3: %s\n" fname; flush stdout;
    let fin = open_in fname in 
    let run = get_run_number fname gorb in
	  printf "run number: %d\n" run; flush stdout;
      let rec add_states graph previous =

		let rec add_sp_site graph previous site_num site_info dyn_data = 
		  (* name of the variable being assigned to, and its value *)
		  let lname,lval = get_name_mval dyn_data in
			
		  (* every site gets its own state. "count" is the number of
		   * times this run has visited this state *)

		  let state, count = 
			try (S.add_run (hfind graph.states site_num) run)
			with Not_found -> begin
			  let new_state, count = S.add_run (S.new_state site_num) run in
				hadd graph.states site_num new_state;
				new_state, count
			end
		  in

		  let rec inner_site state layout = 
			let finalize () =
			  let layout_id = Layout.save_layout layout in
			  let state' = S.add_layout state run layout_id in
			  let graph' = add_state graph state' in
			  let graph'' = add_transition graph' previous state' run in
				graph'',state'
			in
			  try
				let (site_num',dyn_data',site_info') = get_and_split_line fin in
				  if not (site_num == site_num') then 
					(* we have reached a different site; finish this
					 * one, start the next *)
					begin
					  let graph',state' = finalize() in
					  let add_func = get_func site_info' in
						add_func graph' state' site_num' site_info' dyn_data'
					end

				  else 
					begin (* same site; continue adding to memory *)
					  let rname,rval = get_name_mval dyn_data' in
					  let memory' = Layout.add_to_layout layout rname rval in
						(* add initial site predicates to state; if we
						 * want more later, we'll evaluate them based on the
						 * memory layouts we're saving *)
					  let actual_op = 
						if lval > rval then Gt else if lval < rval then Lt else Eq in
					  let comp_exps = 
						List.map 
						  (fun op -> 
							 let value = op == actual_op in
							 let [lvar;rvar] = 
							   lmap
								 (fun name ->
									if hmem graph.vars name then
									  hfind graph.vars name else
										let newval = (Lval(Var(makeVarinfo false name
															   (TInt(IInt,[]))),
														 NoOffset)) in
										  hrep graph.vars name newval;
										  newval)
								 [lname;rname] in
							 let comp_exp =
							   BinOp(op, lvar, rvar, (TInt(IInt,[])))
							 in
							   (comp_exp, value)) [Gt;Lt;Eq] 
						  (*
							let exp_str = Pretty.sprint 80 (d_exp () comp_exp) in
							let loc_str = Pretty.sprint 80 (d_loc () loc) in
							("scalar-pairs"^exp_str^loc_str),value)*)
					  in
					  let state' = 
						List.fold_left
						  (fun state ->
							 (fun (pred_exp,value) -> 
								S.add_predicate state run pred_exp value))
						  state comp_exps

					  (* predicates are added; do it again for the
					   * next line in the trace file *)
					  in 
						inner_site state' memory'
					end
			  with End_of_file -> finalize()
		  in
			inner_site state 
			  (Layout.add_to_layout 
				 (Layout.empty_layout ())
				 lname lval)

		(* this is going to be slightly tricky because we want to guard
		 * states internal to an if statement/conditional, which is hard
		 * to tell b/c we get the value of the conditional b/f we enter
		 * it. *)

		and add_cf_site graph previous site_num (loc,typ,stmt_id,exp) dyn_data =
		  let value = int_of_string (List.hd dyn_data) in 
		  let state,count =
			try (S.add_run (hfind graph.states site_num) run)
			with Not_found -> begin
			  let new_state,count = S.add_run (S.new_state site_num) run in
				hadd graph.states site_num new_state;
				new_state,count
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
		  try 
			let site_num,dyn_data,site_info = get_and_split_line fin in 
			let add_func = get_func site_info in
			let graph',previous' = add_func graph previous site_num
			  site_info dyn_data in
			  add_states graph' previous' 
		  with End_of_file -> 
			begin
			  printf "end of file\n"; flush stdout;
			  close_in fin;
			  let graph' = 
				add_transition graph previous (final_state graph gorb) run in
				graph', previous
			end
      in 
      let start,count = S.add_run (hfind graph.states graph.start_state) run in
		(* FIXME: what is going on here with adding runs? *)
		hrep graph.states graph.start_state start;
		let ends,count = S.add_run (final_state graph gorb) run in
		  hrep graph.states (S.state_id ends) ends;
		  let graph',previous' = add_states graph start in
			graph'

  let build_graph (filenames : (string * string) list) : t = 
    fold_left fold_a_graph (new_graph ()) filenames

  (******************************************************************)

  let get_end_states graph pred = 
	(* returns end states where the expression is *ever* true *)
    match pred with
      RunFailed -> [(final_state graph "F")]
    | RunSucceeded -> [(final_state graph "P")]
	| CilExp(exp) -> 
		lfilt (fun state -> S.is_pred_ever_true state pred) (states graph)
    | _ -> failwith "Not implemented one" 


  let get_seqs graph states = 
    (* OK. Runs contain each state at most once, no matter how many times this run
     * visited it. 
     * And, for now, stateSeqs only start at the start state. The definition is
     * more general than this in case I change my mind later, like for
     * "windows" *)
    (* one run returns a list of runs, since loops can make one state come
       from more than one other possible state *)
    let rec one_run s_id run seq : (Utils.IntSet.elt * int * Utils.IntSet.t) list = 
      if s_id == (-1) then [(run,s_id,(IntSet.add s_id seq))]
      else 
	begin
	  let state_ht = Hashtbl.find graph.backward_transitions s_id
	  in
	  let prevs = IntSet.elements (Hashtbl.find state_ht run) in
	  let seq' = IntSet.add s_id seq in 
	    flatten 
	      (map 
		 (fun prev -> 
		    if (IntSet.mem prev seq') then []
		    else one_run prev run seq') prevs)
	end
    in
    let one_state state = 
      let state_runs = S.runs state in
	let s_id = S.state_id state in
	  flatten (map (fun run -> one_run s_id run (IntSet.empty)) state_runs)
    in
      flatten (map one_state states)

  let split_seqs graph seqs state pred = 
    let eval = 
      map (fun (run,start,set) -> 
			 let t,f = S.overall_pred_on_run state run pred in
			   (run,start,set), (t,f))
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

  (******************************************************************)

  let propagate_predicate graph pred = 
    liter (fun state -> 
			 liter 
			   (fun r -> 
				  ignore(S.overall_pred_on_run state r pred))
			   (S.runs state)) 
	  (states graph) 
    
  (******************************************************************)

  let print_graph graph =
    pprintf "Graph has %d states\n" (Hashtbl.length graph.states);
    pprintf "Graph has %d forward transitions.\n" (Hashtbl.length graph.forward_transitions);
    pprintf "Graph has %d backward transitions.\n" (Hashtbl.length graph.backward_transitions);
    pprintf "Start state id: %d\n" graph.start_state;
    pprintf "Pass final state id: %d\n" graph.pass_final_state;
    pprintf "Fail final state id: %d\n" graph.fail_final_state;
    liter
      (fun state -> 
		 pprintf "For state %d:\n" (S.state_id state);
		 pprintf "Runs:\n"; 
		 liter (fun r -> pprintf "%d, " r) (S.runs state);
		 pprintf "\n";
		 pprintf "Predicates:\n";
		 S.print_preds state;
		 pprintf "vars in scope:\n";
		 S.print_vars state
	  ) (states graph);
    liter 
      (fun (prnt,hash) ->
		 prnt();
		 hiter 
		   (fun source ->
			  fun innerT ->
				pprintf "  transitions for state %d:\n" source;
				hiter
				  (fun run ->
					 fun destset ->
					   liter 
						 (fun dest ->
							pprintf "     run %d goes to state %d\n" run dest
						 ) (IntSet.elements destset)
				  ) innerT
		   ) hash)
      [((fun x -> pprintf "FORWARD \n"; flush stdout), graph.forward_transitions);
       ((fun x -> pprintf "BACKWARD \n"; flush stdout), graph.backward_transitions)];
    flush stdout

end

module DynamicExecGraph = PredictGraph(DynamicState)

(* We should be able to do BP on any of the graph structures we
   defined; we just want a way to convert between them, right? So we
   don't want a "BPGraph" unless it's going to be different from
   "Graph", and I think we can hide a lot of the stuff in "Graph",
   since a lot of it is used internally. *) 
(*
module BPGraph =
struct
  type transitionsT
  type stateT 
  type t

  val build_graph : (string * string) list -> t
  val convert_graph : Graph.t -> t
  val states : t -> stateT list

  val start_state : t -> stateT

  val add_state : t -> stateT -> t
  val add_transition : t -> stateT -> stateT -> int -> t
    
  val next_state : t -> int -> int -> Globals.IntSet.t
    (* next_state takes a state and a run number and returns the new state that
       that run moves to from the passed in state *)
  val next_states : t -> int -> stateT list
    (* next_states returns a list of all states reachable by all runs
       from the given state. Good for building *)

  val states_where_true : t -> predicate -> stateT list 

  val get_end_states : t -> predicate -> stateT list
  val get_seqs : t -> stateT list -> stateSeq list list
  val split_seqs : t -> stateSeq list -> stateT -> predicate -> stateSeq list * stateSeq list

  val print_graph : t -> unit
end
*)
(* possibilities:

 module ASTBPGraph =
struct 

end 
 
module CFGBPGraph =
struct 

end

module DynBPGraph =
struct 

end

*)
