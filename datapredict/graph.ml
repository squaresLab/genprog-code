open List
open String
open Hashtbl
open Cil
open Globals
open Invariant
open State

module type Graph =
sig
  (* states and transitions are up to you *)
  type stateT 
  type transitionsT

  (* graphT is not, in theory. How do I make it so that other structs can use
	 this? *) 
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
end

module ExecutionGraph =
  functor (S : State ) ->
struct 

  (* do we need to define these exceptions in the signature? *)
  exception EmptyGraph

  type stateT = S.t
  
  (* fixme: render this a little more abstract plz *)
  type transitionsT = (int, (int, int) Hashtbl.t) Hashtbl.t 

  type t = {
    states : StateSet.t ;
    transitions : transitionsT ;
    start_state : stateT;
    pass_final_state : stateT;
    fail_final_state : stateT;
  }

  let new_graph () = 
    { states = StateSet.empty;	
      transitions = Hashtbl.create 100;
      (* fixme: add start state to hashtable *)
      start_state = S.new_state (-1);
      pass_final_state = (S.final_state true);
      fail_final_state = (S.final_state false);
    }

  (* I think I want this *)
  let site_to_state : (int, S.t) Hashtbl.t = Hashtbl.create 100

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
      {graph with states = StateSet.add state graph.states }

  let add_transition graph (previous : S.t) (next : S.t) run = 
    let innerT = 
      ht_find graph.transitions previous.site_num 
	(fun x -> Hashtbl.create 100) 
    in
      Hashtbl.add innerT run next.site_num;
      Hashtbl.replace graph.transitions previous.site_num innerT;
      graph

  let next_states graph state =
    let innerT = ht_find graph.transitions state in
      Hashtbl.fold
	(fun run ->
	   fun states ->
	     fun accum -> 
	       List.fold_left
		 (fun stateset ->
		    fun state ->
		      StateSet.add (Hashtbl.find site_to_state state) stateset)
		 accum states) innerT StateSet.empty
		
(* this throws a Not_found if there is no next state for the given state on the
   given run. This is officially Your Problem *)
  let next_state graph state run =
    let nexts = Hashtbl.find graph.transitions state in
      Hashtbl.find nexts run
	
  let states_where_true graph pred = 
    List.filter (fun s -> S.is_true s pred) graph.states 
    
  (* between here and "build_execution_graph" are utility functions *)
  let get_and_split_line fin =
    let split = Str.split comma_regexp (input_line fin) in
    let site_num,stmt_id,info = int_of_string (hd split), tl split in
    let (loc,typ,exp) as site_info = Hashtbl.find !site_ht site_num in
      (site_num,stmt_id,info,site_info)
	
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

  let get_name_mval info = (hd info), (mval_of_string (hd (tl info)))
	
  exception EndOfFile of memV StringMap.t * int StringMap.t 
  exception NewSite of memV StringMap.t * int StringMap.t * (int * int * 
							       (location * string * exp) *
							       string list) 
	
  (* handling this with exceptions is kind of ghetto of me but whatever it
     works *) 
    
  let rec add_sp_site (graph : t) previous fin run site_num stmt_id (loc,typ,exp) info =
    (* name of the variable being assigned to, and its value *)
    let lname,lval = get_name_mval info in

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

    let rec inner_site mem state = 
      (* CHECK: I think "info" is the same as "rest" in the original code *)
      let (site_num',stmt_id',info',(loc',typ',exp')) = 
	try get_and_split_line fin 
	with End_of_file -> raise (EndOfFile(mem,preds))
      in
	if not (site_num == site_num') then 
	  raise (NewSite(mem,preds,(run,site_num',stmt_id',(loc',typ',exp'),info')))
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
	  let state' = 
	    List.fold_left
	      (fun state ->
		 (fun (pred_exp,value) -> 
		    S.add_predicate state run pred_exp value)
		   state comp_exps
	       in 
		inner_site mem' state'
	end
    in
    let state', mem',continuation =
      try 
	let mem = (StringMap.add lname lval (StringMap.empty)) in
	  inner_site mem state
      with EndOfFile(mem,preds) -> mem,preds, (fun (graph,state) -> state,graph)
      | NewSite(mem,preds, (run,site_num',stmt_id',(loc',typ',exp'),info')) ->
	  let add_func = get_func typ' in
	  let cont = 
	    (fun (graph',new_state) -> add_func graph' new_state fin run
	       site_num' stmt_id' (loc',typ',exp') info')
	  in
	    state, mem,cont
    in
    let state'' : S.t = (* FIXME *)  S.add_mem_pred_maps run state' mem' preds' in
      (* Check: I think the following will work because StateSets are ordered by
	 integer, so we should be able to find/remove/replace a state in a stateset
	 without a problem *)
    let graph' = add_state graph state'' in
    let graph'' = add_transition graph previous state' run in 
      continuation (graph'',state'')

  (* this is going to be slightly tricky because we want to guard
     states internal to an if statement/conditional, which is hard to
     tell b/c we get the value of the conditional b/f we enter it. *)

  and add_cf_site (graph : t) (previous : S.t) (fin : in_channel)
      (run : int) (site_num : int) (stmt_id : int) ((loc,typ,exp) : location * string * Cil.exp)
      (info : string list) : S.t * t = 
    let value = int_of_string (List.hd info) in 
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
    let graph'' = add_state graph state' in
      state', graph''

  and get_func typ = 
    if typ = "scalar-pairs" 
    then add_sp_site 
    else add_cf_site

  let build_execution_graph (filenames : (string * string) list) : t = 
    fold_left
      (fun graph ->
	 fun (fname, gorb) -> 
	   let fin = open_in fname in 
	   let run = get_run_number fname gorb in
	     
	   let rec add_states previous graph =
	     try 
	       let site_num,stmt_id,info,(loc,typ,exp) = get_and_split_line fin in
	       let add_func = get_func typ in
	       let previous', graph' = 
		 add_func graph previous fin run site_num stmt_id (loc,typ,exp) info
	       in
		 add_states previous' graph'
	     with End_of_file -> 
	       begin
		 close_in fin;
		 add_transition graph previous (final_state graph gorb) run;
		 graph
	       end
	   in 
	     add_states (start_state graph) graph 
      ) (new_graph ()) filenames 
end
