open Printf
open List
open String
open Hashtbl
open Cil
open Utils
open DPGlobs
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
		vars = hcreate 100;
		forward_transitions = hcreate 100;
		backward_transitions = hcreate 100;
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
      lflat (
	lmap 
	  (fun strat -> 
	     match strat with
	       Random -> 
		 let fnames = ref [] in
		   for i = 1 to !num_rand do
		     let outfile = sprintf "%s-%s%d-fault_local.txt" !name (strat_to_string strat) i in
		       fnames := outfile :: !fnames 
		   done;
		   lmap (fun fname -> (strat, open_out fname)) !fnames
	     | _ ->
		 let outfile = 
		   sprintf "%s-%s-fault_local.txt" !name (strat_to_string strat) in
		 let fout = open_out outfile in
		   [strat, fout])
	  strats)
    in
      liter
	(fun state ->
	   if (S.state_id state) >= 0 then 
	     begin
	       liter
		 (fun (strat,fout) ->
		    let id = S.state_id state in
		      (* OK: the problem is that this will only print
			 out statement IDs for statements associated
			 with an instrumentation site, when in fact
			 what we want is to print out localization for
			 every statement, more or less. AND: right now
			 this won't work for the set-intersect
			 localization because we're still only looking
			 at instrumentation sites, not individual
			 statements.  Ick. *)
		    let local_val = S.fault_localize state strat in
		    let out_str = sprintf "%d,%g\n" id 
		      (match classify_float local_val with 
		       | FP_nan -> 0.0
		       | _ -> if local_val > 0.0 then local_val else 0.0)
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
    let nexts = hfind graph.forward_transitions state in
      hfind nexts run
	
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
  (* between here and "build_graph" are utility functions *)

  exception Next_section ;;
  exception New_site of int * site_info * string list ;;

  let stmt_from_site site_num =
	(* bad hack: if we can't find it, it's because it's either an
	   artificial site (start or end) or it's a stmt number from the
	   ts, fs branch (so it doesn't necessarily have a site num); in
	   that case, just return it *)
	try
	  let site_info = hfind !site_ht site_num in
		match site_info with
		  Branches((_,n,_,_),_,_) -> n,site_info
		| Returns((_,n,_,_)) -> n,site_info
		| Scalar_pairs((_,n,_,_),_) -> n,site_info
		| Is_visited(_,n) -> n,site_info
	with Not_found -> site_num, (Empty)

  let get_and_split_line fin next_heading =
	let line = input_line fin in
	  if line = next_heading then raise (Next_section)
	  else begin
		let split = Str.split comma_regexp line in
		let frst = hd split in
		  if (String.sub frst 0 1) = "*" then begin
	  		let site_num = int_of_string (Str.string_after frst 1) in
			let stmt_num,site_info = stmt_from_site site_num in
			  raise (New_site(stmt_num, site_info, (tl split)))
		  end
		  else begin
			let site_num,info = int_of_string(frst), tl split in
			let stmt_num,site_info = stmt_from_site site_num in
			  stmt_num,info,site_info
		  end
	  end
  let run_num = ref 0

  let get_run_number fname gorb = begin
    let good = if (get (capitalize gorb) 0) == 'P' then 0 else 1 in 
      if not (hmem !fname_to_run_num fname) then begin
		(add !fname_to_run_num fname !run_num);
		(add !run_num_to_fname_and_good !run_num (fname, good));
		incr run_num
      end;
      hfind !fname_to_run_num fname
  end

  let get_name_mval dyn_data = (hd dyn_data), (mval_of_string (hd (tl dyn_data)))
	
(* what I want to do is this: go back to having a numbered site and a set of
 * predicates at each site.  For each predicate, have a statement associated
 * with it if it's true and one if it's false; those are the statements that get
 * the weights associated with SSIing on that predicate. *)
(* new type of instrumentation: "is visited"? *)

  let fold_a_graph graph (fname, gorb) = 
    pprintf "fold-a-graph: %s\n" fname; flush stdout;

    let get_state stmt_num = 
      try 
		hfind graph.states stmt_num 
      with Not_found -> 
		begin
		  let state = S.new_state stmt_num in
			hadd graph.states stmt_num state; state
		end 
    in
    let fin = open_in fname in 
    let run = get_run_number fname gorb in

    let start = S.add_run (hfind graph.states graph.start_state) run in
    let ends = S.add_run (final_state graph gorb) run in
      hrep graph.states graph.start_state start;
      hrep graph.states (S.state_id ends) ends;
      
      (* first line is "SCALAR PAIRS INFO:"; we can ignore it *)
      ignore(input_line fin);
      pprintf "before add_sp_sites\n"; flush stdout;

      (* do the scalar pairs sites *)
      let rec add_sp_sites (graph) : t =
		try
		  let stmt_num,dyn_data,site_info = get_and_split_line fin "OTHER SITES INFO:" in
		  let state = S.add_run (get_state stmt_num) run in
		  let state = match site_info with
			  Scalar_pairs((loc,stmt_num,e,true),_) -> S.add_predicate state run (stmt_num, Executed) true 1
			| _ -> state 
		  in
		  let lname,lval = get_name_mval dyn_data in

		  let rec inner_site graph state lname lval = 
			try 
			  let stmt_num',dyn_data',site_info' = 
				get_and_split_line fin "OTHER SITES INFO:" in
				(* same site; continue adding to memory *)
			  let rname,rval = get_name_mval dyn_data' in
				
			  let state' = S.add_to_memory state run rname rval in
				(* add initial site predicates to state; if we
				 * want more later, we'll evaluate them based on the
				 * memory layouts we're saving *)
			  let actual_op = 
				if lval > rval then Gt else if lval < rval then Lt else Eq in
			  let comp_exps = 
				lmap 
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
					   (CilExp(BinOp(op, lvar, rvar, (TInt(IInt,[])))))
					 in
					   (comp_exp, value)) [Gt;Lt;Eq] 
			  in
				(* fixme: this doesn't deal with the optional "other
				   statements" that may be provided in the scalar-pairs
				   site info, because it seems unimportant to me at the
				   moment, but the option remains! *)
			  let state'' = 
				lfoldl
				  (fun state ->
					 (fun (pred_exp,value) -> 
						S.add_predicate state run (stmt_num',pred_exp) value 1))
				  state' comp_exps
			  in 
				inner_site graph state'' lname lval
			with End_of_file -> failwith "Other sites not found after scalar-pairs\n"
			| New_site(stmt_num',dyn_data',site_info') -> 
				let graph' = add_state graph state in
				let state = S.add_run (get_state stmt_num) run in
				let lname,lval = get_name_mval dyn_data in
				let state = S.add_to_memory state run lname lval in
				  inner_site graph' state lname lval
			| Next_section -> add_state graph state 
		  in
			inner_site graph state lname lval
		with End_of_file ->  failwith "Other sites not found after scalar-pairs\n"
		| New_site(stmt_num',dyn_data',site_info') -> failwith "New site found where new site not expected\n"
		| Next_section -> graph
      in
      let rec add_other_sites graph =
		try
		  let stmt_num,dyn_data,site_info = get_and_split_line fin "TRANSITION TABLE:" in
		  let count = int_of_string (List.hd (List.rev dyn_data)) in
		  let graph' =
			match site_info with
			  Branches((loc,stmt_num,exp,docov),ts,fs) ->
				let torf = try not ((int_of_string (List.hd dyn_data)) == 0) with _ -> false in
				let state = S.add_run (get_state stmt_num) run in
				let graph = add_state graph 
				  (if docov then S.add_predicate state run (stmt_num, Executed) true count
				   else state)
				in
				  (* this giant fold does the thing with the adding of the
				   * relevant predicates to the statements in the then and else
				   * blocks *)
				  lfoldl
					(fun graph ->
					   (fun (set,torf,pred) -> 
						  lfoldl
							(fun graph ->
							   (fun snum ->
								  let state = S.add_run (get_state snum) run in
								  let state = S.add_predicate state run (stmt_num,pred) torf count in
								  let state = if docov && torf then S.add_predicate state run (snum, Executed) true count else state in
									add_state graph state
							   )) graph set
					   )) graph [(ts,torf,(BranchTrue(exp)));
								 (fs,(not torf),(BranchFalse(exp)))] 
			| Returns((loc,stmt_num,exp,docov)) ->
				let torf = try not ((int_of_string (List.hd dyn_data)) == 0) with _ -> false in
				let state = S.add_run (get_state stmt_num) run in
				let pred = (CilExp(exp)) in
				let state' = S.add_predicate state run (stmt_num, pred) torf count in
				  (* FIXME: this is currently SUPER WRONG: it'll triple-add
					 return executions, so don't currently rely on the execute
					 count to be correct! *)
				let state' = if docov then S.add_predicate state run (stmt_num, Executed) true count else state' in
				  add_state graph state'
			| Is_visited(loc,stmt_num) ->
				let state = S.add_run (get_state stmt_num) run in
				let state' = S.add_predicate state run (stmt_num, Executed) true count in
				  add_state graph state'
			| Scalar_pairs(_) -> failwith "Scalar pairs in other sites info!\n"
		  in
			add_other_sites graph'
		with Next_section -> graph
		| End_of_file -> failwith "Didn't find transition table after other states\n"
      in
      let rec add_transitions graph = 
		try 
		  let line = input_line fin in
		  let split = Str.split comma_regexp line in
		  let site1,site2 = int_of_string(List.hd split), int_of_string(List.hd (List.tl split)) in
		  let stmt1 = match stmt_from_site site1 with n,_ -> n in
		  let stmt2 = match stmt_from_site site2 with n,_ -> n in
		  let frst = get_state stmt1 in
		  let scnd = get_state stmt2 in 
		  let graph = (add_transition graph frst scnd run) in
			add_transitions graph
		with End_of_file -> graph
      in	  
      let graph' = add_sp_sites graph in
      let graph'' = add_other_sites graph' in
		add_transitions graph''

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


  let get_seqs (graph : t) (states : stateT list) : stateSeq list = 
	let rec one_run s_id run seq =
	  if s_id == -3 || s_id == -2 then seq else
		begin
		  let state_ht = hfind graph.forward_transitions s_id in
		  let nexts = hfind state_ht run in
		  let seq = IntSet.add s_id seq in
			if IntSet.subset nexts seq then seq else
			  begin
				let seq = IntSet.union nexts seq in
				  IntSet.fold
					(fun next ->
					   fun accum ->
						 if next == s_id then accum else begin
						   let nexts = one_run next run seq in
							 IntSet.union accum nexts
						 end)
					nexts seq
			  end
		end
	in
	let one_state state =
	  lmap (fun run -> (run, -1, (one_run (-1) run (IntSet.empty)))) (S.runs state)
	in
	  lflat (lmap one_state states)
	  
    (* Runs contain each state at most once, no matter how many times this run
     * visited it. One run returns a list of runs, since loops can make one state
     * come from more than one other possible state *)
	(* this is ultra-broken.  I remember why I did it this way, but it doesn't
	   work for now, so instead I'm going to do the simple thing and come back
	   and reimplement it when I need it to do what I originally wanted it to
	   do, since right now it doesn't need to do that.  *)
(*	let rec one_run s_id run seq =
	  if s_id == (-1) then [(run,s_id,(IntSet.add s_id seq))]
	  else 
		begin
		  let state_ht = hfind graph.backward_transitions s_id in
		  let prevs = IntSet.elements (hfind state_ht run) in
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
	  flatten (map one_state states)*)

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
