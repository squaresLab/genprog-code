open Cil

type predicate = CilExp of exp | ReturnVal of exp
type invariant = General of predicate | Specific of predicate * location | RunFailed
	(* this naming scheme is probably a really crap idea because the two words
	   actually mean the same thing, but I'm using one to be just like "truth-valued
	   statement" and one to be "Slightly more general" *)

module type State =
  (* make this a functor that takes memory layout type? *)
  (* right now it'll map strings to things of type memV, but maybe we want to
	 separate that out as well *)
sig
  type t (* type of state *)
  type run (* run signifier *)
  type memV (* type of value in memory *)

  val new_state : run -> location -> t (* the int is the run number *)
  val state_copy : t -> run -> ?loc:location -> t 
	(* make a copy of a state, but it's unique to the run whose signifier is
	   passed in. If no location is passed use the one from the state we're
	   copying. *) 
									

  val get_location : t -> location 
  val get_assumptions : t -> invariant list 

  val is_true : t -> invariant -> bool
  val add_assumption : t -> invariant -> t

  val clear_memory : t -> t
  val add_to_memory : t -> string -> memV -> t
  val change_memory : t -> string -> memV -> t
	
end

module DynamicState
struct
  type memV = Int of int | Float of float 
  type run = int (* each dynamic run has a unique integer signifier *)

  type t =  {
	val assumptions : invariant list ;  (* conditionals guarding this statement *)
	val sigma : memV StringMap.t ;
	val loc : Cil.location ; (* location in code *)  
	val runs : run list ;
  }

  let empty_state = {
	assumptions = [] ;
	sigma = StringMap.empty ;
	loc = locUnknown ;
	runs = [] ;
  }

  let new_state run loc = {empty_state with loc = loc; runs=[run]}
  let state_copy state run (?loc:location) (* -> t *) = failwith "Not implemented" 

  let get_location state = state.loc

  let get_assumptions = state.assumptions 
  let is_true state inv (* -> bool *) = failwith "Not implemented"
  let add_assumption state invariant (* -> t *) = failwith "Not implemented"

  let clear_memory state = {state with sigma = StringMap.empty }
  let add_to_memory state key mem (* -> t *) = failwith "Not implemented"
  let change_memory state key mem (* -> t *) = failwith "Not implemented"
end

type rank = { (* sum or record? *)
  val f_P : float;
  val s_P : float;
  val failure_P : float;
  val context : float;
  val increase : float;
  val f_P_obs : float;
  val s_P_obs : float;
  val numF : int;
  val importance : float;
}

type location = Cil.location


module type Graph =
sig

  type graphT = Empty | Graph of states * transitions 

  type stateT 
  type transitionsT

  val build_graph : (string * string) list -> graph
  val add_sp_site : graphT -> in_channel -> int -> int -> (location * string * exp) -> string list -> graphT
  val add_cf_site : graphT -> in_channel -> int -> int -> (location * string * exp) -> string list -> graphT

  val new_graph : unit -> graph

  val states : graphT -> state list
  val next_state : graphT -> state -> int -> state 
  (* next_state takes a state and a run number and returns the new state that
	 that run moves to from the passed in state *)

  val states_where_true : graphT -> invariant -> state list 

  (* returns a list of states where a given predicate is true *)

  (*  val is_true_at_state -> this should only be defined on states *)

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

  type stateT = S
  type transitionsT = state -> int -> state 
	(* we can only transition from one state to another, because it's dynamic *)
	(* in practice it should be a hashtable, but I'm not sure how to make that
	   happen *)

  let states graph = 
	match graph with
		Empty -> []
	  | Graph(states, _) -> states
		  
  let next_state graph state run =
	match graph with
		Empty -> raise (EmptyGraph ())
	  | Graph(_,transitions) -> transitions state run

  let states_where_true graph inv = failwith "Not implemented"

  let get_predictive_invariants graph inv = failwith "Not implemented"		  

  let build_execution_graph (filenames : (string * string) list) = 
	let transition_tbl = Hashtbl.create 100 in

	List.fold_left
	  (fun graph ->
		 (fun (fname, gorb) ->
			let fin = open_in fname in 
			let run = get_run_number fname gorb in


			  (try
				 while true do
				   let line = Str.split comma_regexp (input_line fin) in 
				   let site_num,info = int_of_string (hd split), tl split in
				   let (loc,typ,exp) as site_info  = 
					 Hashtbl.find !site_ht site_num in 
					 if typ = "scalar-pairs" then begin
					   add_sp_site graph fin run site_num site_info info
					 end else
					   add_cf_site graph fin run site_num site_info info
				 done
			   with _ -> ()); close_in fin
		 ) filenames) (new_graph()) filenames

  let add_sp_site graph fin run site_num (loc,typ,exp) info =
	let state =
	
	(* get the last allocated state *)
	(* see if there are transitions from it to another state and if that
	   other state is the same as the state we're going to make but for a
	   different run. If so, just use that one and add this run to
		 it. Otherwise you need to make a new one. 
		 The new one can be a copy of the one we came from but with the memory
		 slightly changed? *)
	in
	  ()

  let add_cf_site graph fin run site_num (loc,typ,exp) info =
	
	  
end


module BuildGraph =
  functor (G : ExecutionGraph) ->
struct

let get_run_number fname gorb = 
  let good = if (get (capitalize gorb) 0) == 'P' then 0 else 1 in 
  if not (Hashtbl.mem !fname_to_run_num fname) then begin
    (add !fname_to_run_num fname !run_num);
    (add !run_num_to_fname_and_good !run_num (fname, good));
    incr run_num
  end;
  Hashtbl.find !fname_to_run_num fname
  



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
	let line = input_line fin in
	let split = Str.split comma_regexp line in
	let site_num = int_of_string (List.hd split) in
	  if not (site == site_num) then begin
		let loc,typ,exp = Hashtbl.find !site_ht site_num in
		  let rest = List.tl split in
		  if typ = "scalar-pairs" then begin
			let lname = List.hd rest in
			 let lval = try (float_of_string (List.hd (List.tl rest))) with _ -> (float_of_int (int_of_string (List.hd (List.tl rest)))) in
			   process_sp_site fin site_num lname lval (loc,typ,exp)
		  end else 
			process_site site_num (int_of_string (List.hd (List.tl split)))
	  end
	  else begin
		let rest = List.tl split in
		let rname = List.hd rest in
		let rval = try (float_of_string (List.hd (List.tl rest))) with _ -> (float_of_int (int_of_string (List.hd (List.tl rest)))) in
		let actual_op = if lval > rval then Gt else if lval < rval then Lt else Eq in
		let comp_exps = 
		  List.map 
			(fun op -> BinOp(op, (Const(CStr(lname))), (Const(CStr(rname))), (TInt(IInt,[]))))
			[Gt;Lt;Eq] in
		let exp_str = Pretty.sprint 80 (d_exp () (List.hd comp_exps)) in
		let loc_str = Pretty.sprint 80 (d_loc () loc) in
		let str = "scalar-pairs"^exp_str^loc_str in
		  if not (Hashtbl.mem !sp_ht str) then 
			List.iter (fun comp_exp -> ignore(get_next_site "scalar-pairs" comp_exp loc)) comp_exps;
		  List.iter
			(fun comp_exp ->
			   let exp_str = Pretty.sprint 80 (d_exp () comp_exp) in
			   let loc_str = Pretty.sprint 80 (d_loc () loc) in
			   let str = "scalar-pairs"^exp_str^loc_str in
			   let site_num = Hashtbl.find !sp_ht str in
			   let value = 
				 match comp_exp with BinOp(op,_,_,_) -> 
				   let exp_str = Pretty.sprint 80 (d_exp () comp_exp) in
				   if op == actual_op then 1 else 0
			   in
				 process_site site_num value) comp_exps; 
		  process_sp_site fin site lname lval (loc,typ,exp)
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
			 let lname = List.hd rest in
			 let lval = try (float_of_string (List.hd (List.tl rest))) with _ -> (float_of_int (int_of_string (List.hd (List.tl rest)))) in
			   process_sp_site fin site_num lname lval (loc,typ,exp)
		   end else process_site site_num (int_of_string (List.hd rest))
	   done 
     with _ -> ());
	close_in fin) filenames

end
