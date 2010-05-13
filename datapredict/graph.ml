open List
open String
open Hashtbl
open Cil
open Globals
open State

module type Graph =
sig
  (* states and transitions are up to you *)
  type stateT 
  type transitionsT

  (* graphT is not, in theory. How do I make it so that other structs can use
	 this? *) 
  type graphT

  val build_graph : (string * string) list -> graphT
  val new_graph : unit -> graphT

  val states : graphT -> stateT list

  val start_state : graphT -> stateT

  val add_state : graphT -> stateT -> graphT
  val add_transition : graphT -> stateT -> stateT -> int -> graphT
	
  val next_state : graphT -> stateT -> int -> stateT 
  (* next_state takes a state and a run number and returns the new state that
	 that run moves to from the passed in state *)
  val next_states : graphT -> stateT -> stateT list
	(* next_states returns a list of all states reachable by all runs
	   from the given state. Good for building *)

  val states_where_true : graphT -> invariant -> stateT list 

  (* returns a list of states where a given invariant is true *)

  val get_predictive_invariants : graphT -> invariant -> bool -> (invariant * rank) list
	(* not clear on how to deal with locations yet; sometimes we want just
	   invariants that are predictive and sometimes we want invariants in
	   specific locations that are predictive and I don't know which one we want *)
end

module ExecutionGraph =
  functor (S : State ) ->
struct 

  (* do we need to define these exceptions in the signature? *)
  exception EmptyGraph

  type stateT = S.t
  type transitionsT = (stateT, stateT list) Hashtbl.t 

  type graphT= Empty | Graph of stateT list * transitionsT		  

  let new_graph () = failwith "Not implemented" 

  let states graph =
	match graph with
		Empty -> []
	  | Graph(states, _) -> states

  let start_state graph = failwith "Not implemented"

  let add_state graph state = failwith "Not implemented"
  let add_transition graph previous next run = failwith "Not implemented"

  let next_states graph state = 
	match graph with
		Empty -> raise (EmptyGraph)
	  | Graph(_,transitions) ->
		  try 
			Hashtbl.find transitions state 
		  with Not_found -> []

  let next_state graph state run =
	let nexts = next_states graph state in
	  List.find (S.observed_on_run run) nexts

  let states_where_true graph inv = failwith "Not implemented"

  let get_predictive_invariants graph inv = failwith "Not implemented"		  

  (* I'm not sure if I want this or just a list of "final" states in the
	 graph; I'm putting it in for now to be able to set a state in the
	 graph to "final", because as it stands we won't know if it's final
	 until after we've added it *)
  let replace_state state graph = failwith "Not implemented" 

  (* between here and "build_execution_graph" are utility functions *)
  let get_and_split_line fin =
	let split = Str.split comma_regexp (input_line fin) in
	let site_num,info = int_of_string (hd split), tl split in
	let (loc,typ,exp) as site_info = Hashtbl.find !site_ht site_num in
	  (site_num,info,site_info)

  let run_num = ref 0

  let get_run_number fname gorb = 
	let good = if (get (capitalize gorb) 0) == 'P' then 0 else 1 in 
	  if not (Hashtbl.mem !fname_to_run_num fname) then begin
		(add !fname_to_run_num fname !run_num);
		(add !run_num_to_fname_and_good !run_num (fname, good));
		incr run_num
	  end;
	  Hashtbl.find !fname_to_run_num fname
		
  (*  val add_X_site : graphT -> state -> in_channel -> int -> int -> 
	  (location * string * exp) -> string list -> state * graphT *)

  (* also, do we want to be building up the hashtables from the original
	 "predicates" implementation while we're doing this? Because we can
	 even though that implementation is scary. *)

  let get_name_mval info = (hd info), (mval_of_string (hd (tl info)))
	
  exception EndOfFile of memV StringMap.t * int StringMap.t 
  exception NewSite of memV StringMap.t * int StringMap.t * (int * int * 
													  (location * string * exp) *
													  string list) 
	
  (* handling this with exceptions is kind of ghetto of me but whatever it
	 works *) 

  let rec add_sp_site graph previous fin run site_num (loc,typ,exp) info =
	(* TODO: how do we propagate assumptions between states? *)

	let lname,lval = get_name_mval info in
	let rec inner_site mem preds = 
	  (* CHECK: I think "info" is the same as "rest" in the original code *)
	  let (site_num',info',(loc',typ',exp')) = 
		try
		  get_and_split_line fin 
		with End_of_file -> raise (EndOfFile(mem,preds))
	  in
		if not (site_num == site_num') then 
		  raise (NewSite(mem,preds,(run,site_num',(loc',typ',exp'),info')))
		else begin (* same site, so continue adding to this state memory *)
		  (* add value of memory to state *)
		  let rname,rval = get_name_mval info' in
		  let mem' = StringMap.add rname rval mem in
			
		  (* add predicates to state *)
		  let actual_op = 
			if lval > rval then Gt else if lval < rval then Lt else Eq in

		  let comp_exps = 
			List.map 
			  (fun op -> 
				 let value = if op == actual_op then 1 else 0 in (* this would
																	actually be 
																	less stupid
																	in C *)
				 let comp_exp =
				   BinOp(op, (Const(CStr(lname))), (Const(CStr(rname))),
						 (TInt(IInt,[]))) in
				 let exp_str = Pretty.sprint 80 (d_exp () comp_exp) in
				 let loc_str = Pretty.sprint 80 (d_loc () loc) in
				 ("scalar-pairs"^exp_str^loc_str),value)
			  [Gt;Lt;Eq] in
			
		  let preds' = 
			List.fold_left
			  (fun pred_map ->
				 (fun (pred_str,value) -> incr_map pred_str value pred_map))
			  preds comp_exps
		  in 
			inner_site mem' preds'
		end
	in
	let mem', preds', continuation =
	  try 
		let mem = (StringMap.add lname lval (StringMap.empty)) in
		let preds = StringMap.empty in
		  inner_site mem preds
	  with EndOfFile(mem,preds) -> mem,preds, (fun (graph,state) -> state,graph)
		| NewSite(mem,preds, (run,site_num',(loc',typ',exp'),info')) ->
			let add_func = get_func typ' in
			let cont = 
			  (fun (graph',new_state) -> add_func graph' new_state fin run
				 site_num' (loc',typ',exp') info')
			in
			  mem,preds,cont
	in
	let potential_states = next_states graph previous in 
	  (* if the state we would build already exists, throw away the mem and
		 preds we've built and just increment the existing state for this
		 run. Annoying because we need to process the site to answer the
		 question. The massive use of exceptions here is unlike me. *)
	let modify_graph graph state =
	  let graph' = add_transition previous state run in 
	  let graph'' = replace_state graph' state in
		continuation (graph'',state)
	in
	let new_state = S.state_with_mem_preds run loc mem' preds' in
	let new_state' = 
	  try
		let existing = 
		  List.find
			(fun state -> S.states_equal state new_state) potential_states
		in
		  S.add_run existing run 
	  with Not_found -> new_state 
	in
	  modify_graph graph new_state'		

(* this is going to be slightly tricky because we want to guard
   states internal to an if statement/conditional, which is hard to
   tell b/c we get the value of the conditional b/f we enter it. *)

  and add_cf_site graph previous fin run site_num (loc,typ,exp) info = failwith "Not implemented"

  and get_func typ = 
	if typ = "scalar-pairs" then add_sp_site else add_cf_site

  let build_execution_graph (filenames : (string * string) list) : graphT = 
	fold_left
	  (fun graph ->
		 fun (fname, gorb) -> 
		   let fin = open_in fname in 
			let run = get_run_number fname gorb in
			  
			let rec add_states previous graph =
			  try 
				let site_num,info,(loc,typ,exp) = get_and_split_line fin in
				let add_func = get_func typ in
				let previous', graph' = 
				  add_func graph previous fin run site_num (loc,typ,exp) info
				in
				  add_states previous' graph'
			  with End_of_file -> 
				begin
				  close_in fin;
				  ignore(S.set_final previous run); (* FIXME this won't work because
											   maps are side-effect
											   free. Maybe make it a hashtable
											   instead? *)
				  (* maybe states could use a unique ID/hash? *)
				  graph
				end
			in 
			  add_states (start_state graph) graph 
	  ) (new_graph ()) filenames 
	  
end



  
(*


  let process_site site_num value =
	let site_to_res : (int, int * int) Hashtbl.t =
	  if Hashtbl.mem !run_and_pred_to_res run then
		Hashtbl.find !run_and_pred_to_res run
	  else
		Hashtbl.create 10 
	in
	let (num_true, num_false) =
	  if Hashtbl.mem site_to_res site_num then
		Hashtbl.find site_to_res site_num else
		  (0,0)
	in
	let res' =
	  if value == 0 then (num_true, num_false + 1) else 
		(num_true + 1, num_false)
	in
	  Hashtbl.replace site_to_res site_num res';
	  Hashtbl.replace !run_and_pred_to_res run site_to_res;
	  site_set := IntSet.add site_num !site_set 
  in
  let rec process_sp_site fin site lname lval (loc,typ,exp) = 
	(* ... *)
	  else begin
		(* ... *)
		(* has been copied up to new add_sp_site above, must be changed *)
	  end
  in
	(try 
       while true do
		 let line = input_line fin in
		 let split = Str.split comma_regexp line in
		 let site_num, rest = int_of_string (List.hd split), List.tl split in
		 let loc,typ,exp = Hashtbl.find !site_ht site_num in
		   if typ = "scalar-pairs" then begin
			 (* the start of a scalar pairs site. This string contains the name 
			  * value, and a type signifier of the lhs of an assignment. 
			  * Subsequent site entries printed immediately afterwards contain 
			  * the name, value, and a type signifier of all variables that are 
			  * nominally in scope.
			  * This is somewhat complicated because the sites ht does not 
			  * contain information for all the *individual* sites, it only 
			  * contains one entry for the *initial* site. We need to create 
			  * sites for the actual comparisons. 
			  *)

			   process_sp_site fin site_num lname lval (loc,typ,exp)
		   end else process_site site_num (int_of_string (List.hd rest))
	   done 
     with _ -> ());
	close_in fin) filenames

end*)
