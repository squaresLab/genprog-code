open Batteries
open Map
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

  val build_graph : (string * string) list -> string -> t
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

  (* get seqs returns sequences of states that lead to the end state *)
  val get_seqs : t -> stateT -> stateSeq list

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
	  (* (state, run) -> next_states *)
  type transitionsT = ((int * int), IntSet.t) Hashtbl.t 

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
(*	let ens = Enum.seq (-3) ((-) 1) ((<) (-3))) in*)
    let states =  hcreate 10 in
(*	  Hashtbl.of_enum 
		(Enum.map (fun sid -> sid,(S.new_state sid)) ens) in*)
      { states = states;
		vars = hcreate 100;
		forward_transitions = hcreate 100;
		backward_transitions = hcreate 100;
		start_state = -1;
		pass_final_state = -2;
		fail_final_state = -3;
      }

  let states graph = List.of_enum (Hashtbl.values graph.states)

  let print_fix_localization graph = ()
	
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
				 let fnames = RefList.empty () in
				   for i = 1 to !num_rand do
					 let outfile = sprintf "%s-%s%d-fault_local.txt" !name (strat_to_string strat) i in
					   RefList.add fnames outfile
				   done;
				   RefList.map_list (fun fname -> (strat, open_out fname)) fnames
			 | _ ->
				 let outfile = 
				   sprintf "%s-%s-fault_local.txt" !name (strat_to_string strat) in
				 let fout = open_out outfile in
				   [strat, fout])
		  strats)
    in
    let executed_states =
      lfilt
		(fun state -> S.is_ever_executed state)
		(states graph)
    in
      liter
		(fun state ->
		   let id = S.state_id state in
			 liter 
			   (fun (strat,fout) ->
				  if id >= 0 then 
					let local_val = S.fault_localize state strat in
					let out_str = 
					  sprintf "%d,%g\n" id 
						(match classify_float local_val with 
						 | FP_nan -> 0.0
						 | _ -> if local_val > 0.0 then local_val else 0.0)
					in
					  output_string fout out_str
			   ) strats_outs
		) executed_states;
      pprintf "closing out files\n"; flush stdout;
      liter (fun (_,fout) -> close_out fout) strats_outs
	
  let final_state graph gorb = 
    if (String.head (capitalize gorb) 1) == "P"
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
      let set = 
		ht_find ht ((S.state_id from),run) (fun x -> IntSet.empty)
      in
	  let set' = IntSet.add (S.state_id to2) set in
		hrep ht ((S.state_id from), run) set'
    in
      inner_trans graph.forward_transitions previous next;
      inner_trans graph.backward_transitions next previous;
      graph

  let get_trans graph state trans = 
	lfoldl 
	  (fun states ->
		 fun run -> 
		   let set = hfind trans ((S.state_id state),run) in
			 IntSet.union states set
	  ) (IntSet.empty) (S.runs state)

  let next_states graph state = get_trans graph state graph.forward_transitions
  let previous_states graph state = get_trans graph state graph.backward_transitions
		
(* this throws a Not_found if there is no next state for the given state on the
   given run. This is officially Your Problem *)
  let next_state graph state run = hfind graph.forward_transitions (state, run)
	
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
		   (fun (sid,run) ->
			  fun nexts ->
				pprintf "  transitions for state %d on run %d:\n" sid run;
				IntSet.iter
				  (fun next -> pprintf "    -> %d\n" next)
				  nexts) hash)
      [((fun x -> pprintf "FORWARD \n"; flush stdout), graph.forward_transitions);
       ((fun x -> pprintf "BACKWARD \n"; flush stdout), graph.backward_transitions)];
    flush stdout
  (* between here and "build_graph" are utility functions *)

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

  let split_line line =
	let split = Str.split comma_regexp line in
	let frst = hd split in
	let site_num,info = int_of_string(frst), tl split in
	let stmt_num,site_info = stmt_from_site site_num in
	  stmt_num,info,site_info

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

(* what I want to do is this: go back to having a numbered site and a set of
 * predicates at each site.  For each predicate, have a statement associated
 * with it if it's true and one if it's false; those are the statements that get
 * the weights associated with SSIing on that predicate. *)
(* new type of instrumentation: "is visited"? *)

  let fold_a_graph graph ((fname, gorb) : (string * string))= 
	let fin = open_in_bin fname in
	let transitions : (int, int) MultiPMap.t = Marshal.input fin in
	let site_count : (string, int) Hashtbl.t = Marshal.input fin in
	let sp_count : ((string * int), int) Hashtbl.t = Marshal.input fin in

    let run = get_run_number fname gorb in
    let start = S.add_run (hfind graph.states graph.start_state) run in
    let ends = S.add_run (final_state graph gorb) run in
      hrep graph.states graph.start_state start;
      hrep graph.states (S.state_id ends) ends;
	  
      let get_state stmt_num = 
		try 
		  hfind graph.states stmt_num 
		with Not_found -> 
		  begin
			let state = S.new_state stmt_num in
			  hadd graph.states stmt_num state; state
		  end 
	  in
	  let add_sp_sites graph =
		let inner_pred_add stmt_num lname lval rname rval state = 
		  (* add initial site predicates to state; if we
		   * want more later, we'll evaluate them based on the
		   * memory layouts we're saving *)
		  let comp_exps = 
			lmap 
			  (fun (op,val1) -> 
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
				 let comp_exp1 =
				   (CilExp(BinOp(op, lvar, rvar, (TInt(IInt,[])))))
				 in
				   (comp_exp1, val1))
			  [(Gt,(lval > rval));(Lt,(lval < rval));
			   (Eq, (not ((lval > rval) || (lval < rval))))]
		  in
			(* fixme: this doesn't deal with the optional "other
			   statements" that may be provided in the scalar-pairs
			   site info, because it seems unimportant to me at the
			   moment, but the option remains! *)
			lfoldl
			  (fun state ->
				 (fun (pred_exp,value) -> 
					S.add_predicate state run (stmt_num,pred_exp) value 1))
			  state comp_exps 
		in
		let inner_sp_add line mem count graph =
		  let stmt_num,dyn_data,site_info = split_line line in 
		  let memmap = Layout.get_layout mem in
		  let lname,lval =  (hd dyn_data), mval_of_string (hd (tl dyn_data)) in
		  let state = S.add_run (get_state stmt_num) run in
		  let state = match site_info with
			  Scalar_pairs((loc,stmt_num,e,true),_) ->
			    S.add_predicate state run (stmt_num, Executed) true 1
		    | _ -> failwith "site info not a scalar pairs though it should be!\n"
		  in
		  let state : S.t = 
			StringMap.fold
			  (fun rname ->
				 fun rval ->
				   fun state ->
					 if rname <> lname then
					   inner_pred_add stmt_num lname lval rname rval state 
					 else state
			  ) memmap state 
		  in
		  let state = S.add_layout state run mem in
			add_state graph state
		in
		  hfold
			(fun (line, mem) -> 
			   fun count ->
				 fun graph -> inner_sp_add line mem count graph) sp_count graph
	  in

	  let add_other_sites graph =
		let inner_add line count graph =
		  let stmt_num,dyn_data,site_info = split_line line in
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
		  hfold
			(fun line ->
			   fun count ->
				 fun graph -> inner_add line count graph
			) site_count graph
	  in
	  let add_transitions (graph : t) = 
		Enum.fold 
		  (fun (graph : t) ->
			 fun (site1, site2) ->
				 let stmt1 = match stmt_from_site site1 with n,_ -> n in
				 let stmt2 = match stmt_from_site site2 with n,_ -> n in
				 let frst = get_state stmt1 in
				 let scnd = get_state stmt2 in 
				   add_transition 
					 graph 
					 frst 
					 scnd 
					 run
		  ) graph (MultiPMap.enum transitions) 
	  in	  
	  let graph' = add_sp_sites graph in
	  let graph'' = add_other_sites graph' in
		close_in fin; 
		add_transitions graph''

  let build_graph (filenames : (string * string) list) (memfile : string) : t = 
	let fin = open_in_bin memfile in
	  layout_map := Marshal.from_channel fin;
	  rev_map := Marshal.from_channel fin;
	  close_in fin;
    List.fold_left fold_a_graph (new_graph ()) filenames
(*    let not_executed_states = lfilt (fun state -> (S.state_id state) >= 0 && not (S.is_ever_executed state)) (states graph) in
      liter
		(fun state ->
		   let s_id = S.state_id state in
			 hrem graph.states s_id;
			 liter
			   (fun run ->
				  hrem graph.forward_transitions (run, s_id);
				  hrem graph.backward_transitions (run, s_id))
			   (S.runs state)
		) not_executed_states; graph*)

  (******************************************************************)

  let get_end_states graph pred = 
	(* returns end states where the expression is *ever* true *)
	pprintf "Getting end states for %s\n" (d_pred pred); flush stdout;
    match pred with
      RunFailed -> [(final_state graph "F")]
    | RunSucceeded -> [(final_state graph "P")]
	| CilExp(exp) -> 
		lfilt (fun state -> S.is_pred_ever_true state pred) (states graph)
    | _ -> failwith "Not implemented one" 


  let get_seqs (graph : t) (state : stateT) : stateSeq list = 
	(* right now this assumes we're going to the final states! *) 
	(* which means we can just get every state we've ever visited *)
	let one_run run =
	  lfoldl
		(fun (run,start,state_set) ->
		   fun s_id ->
		     let nexts = ht_find graph.forward_transitions (s_id, run) (fun x -> IntSet.empty) in
		       (run,start,(IntSet.union nexts state_set))
		) (run,-1,(IntSet.empty)) (ht_keys graph.states)
	in
	  lmap (fun run -> one_run run) (S.runs state)

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
	  List.map (fun (run,start,set) -> 
			 let t,f = S.overall_pred_on_run state run pred in
			   (run,start,set), (t,f))
		seqs in
	  List.fold_left
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
    liter (fun state -> ignore(S.overall_pred_on_run state 0 pred)) (states graph)
      
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
