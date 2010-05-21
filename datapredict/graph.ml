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

  (* returns a list of states where a given invariant is true *)

  val get_predictive_invariants : t -> invariant -> bool -> (invariant * rank) list
    (* not clear on how to deal with locations yet; sometimes we want just
       invariants that are predictive and sometimes we want invariants in
       specific locations that are predictive and I don't know which one we
       want *)
  val replace_state : t -> stateT -> stateT -> t 
end

module ExecutionGraph =
  functor (S : State ) ->
struct 

  (* do we need to define these exceptions in the signature? *)
  exception EmptyGraph

  type stateT = S.t
  type transitionsT = (stateT, (int, stateT) Hashtbl.t) Hashtbl.t 

  type t = {
    states : stateT list;
    transitions : transitionsT;
    start_state : stateT;
    pass_final_state : stateT;
    fail_final_state : stateT;
  }

  let new_graph () = 
    { states = [];	
      transitions = Hashtbl.create 100;
      (* fixme: add start state to hashtable *)
      start_state = S.new_state (-1);
      pass_final_state = (S.final_state true);
      fail_final_state = (S.final_state false);
    }

  (* I think I want this *)
  let site_to_state : (int, S.t) Hashtbl.t = Hashtbl.create 100

  let states graph = graph.states
  let start_state graph = graph.start_state

  let final_state graph gorb = 
    if ((get (capitalize gorb) 0) == 'P') 
    then graph.pass_final_state 
    else graph.fail_final_state

(* NTS: for now we assume that there will be no duplicates added based on logic
   happening elsewhere but we need to double check, maybe; add_state certainly
   doesn't! At least not with the list implementation. *)

  let add_state graph state = {graph with states = state::graph.states}

(* CHECK that this works side-effects in OCaml confuse me *)
  let add_transition graph (previous : S.t) (next : S.t) run = 
    let innerT = 
      try 
	Hashtbl.find graph.transitions previous 
      with Not_found -> Hashtbl.create 100
    in
      Hashtbl.add innerT run next;
      Hashtbl.replace graph.transitions previous innerT

  let replace_state graph state state' = failwith "Not implemented"
  let next_states graph state = failwith "Not implemented"
(*    let innerT = 
      try Hashtbl.find graph.transitions state with Not_found -> Hashtbl.create 100 in *)
      (* LEAVING OFF HERE. This won't work because it'll make double states,
	 need to filter somehow preferably without comparing everything to
	 everything else *)
(*      Hashtbl.fold
	(fun run ->
	   fun states ->
	     fun accum -> states @ accum) innerT []*)
		
  let next_state graph state run =
    let nexts = next_states graph state in
      List.find (S.observed_on_run run) nexts
	
  let states_where_true graph pred = 
    List.filter (fun s -> S.is_true s pred) graph.states 
    
  let get_predictive_invariants graph inv = failwith "Not implemented"		  
    
  (* between here and "build_execution_graph" are utility functions *)
  let get_and_split_line fin =
    let split = Str.split comma_regexp (input_line fin) in
    let site_num,info = int_of_string (hd split), tl split in
    let (loc,typ,exp) as site_info = Hashtbl.find !site_ht site_num in
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

  (*  val add_X_site : t -> state -> in_channel -> int -> int -> 
      (location * string * exp) -> string list -> state * t *)

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
    
  let rec add_sp_site (graph : t) previous fin run site_num (loc,typ,exp) info =
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

    let rec inner_site mem preds = 
      (* CHECK: I think "info" is the same as "rest" in the original code *)
      let (site_num',info',(loc',typ',exp')) = 
	try get_and_split_line fin 
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
	      [Gt;Lt;Eq] 
	  in
	  let preds' = failwith "Not implemented"
(* FIXME *)
(*	    List.fold_left
	      (fun pred_map ->
		 (fun (pred_str,value) -> Layout.incr pred_map pred_str value))
	      preds comp_exps*)
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
    let state' : S.t = state in (* FIXME  S.add_mem_pred_maps run state mem' preds' in*)
      (* FIXME the following *)
    let graph' = add_transition graph previous state' run in 
    let graph'' = replace_state graph' state state' in
      continuation (graph'',state')

  (* this is going to be slightly tricky because we want to guard
     states internal to an if statement/conditional, which is hard to
     tell b/c we get the value of the conditional b/f we enter it. *)

  and add_cf_site (graph : t) (previous : S.t) (fin : in_channel)
      (run : int) (site_num : int) ((loc,typ,exp) : location * string * Cil.exp)
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
    let graph'' = replace_state graph' state state' in
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
	       let site_num,info,(loc,typ,exp) = get_and_split_line fin in
	       let add_func = get_func typ in
	       let previous', graph' = 
		 add_func graph previous fin run site_num (loc,typ,exp) info
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
