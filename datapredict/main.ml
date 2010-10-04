open List
open Cil
open Pretty
open Globals
open Invariant
open State
open Graph
open Predict

let cbi_hash_tables = ref ""
let runs_in = ref ""
let to_eval = ref ""

let usageMsg = "Giant Predicate Processing Program of Doom\n"
let options = ref [
  "-cbi-hin", Arg.Set_string cbi_hash_tables, 
  "\t File containing serialized hash tables from my implementation \
                of CBI." ;
  "-rs", Arg.Set_string runs_in,
  "\t File listing names of files containing runs, followed by a passed \
                or failed on the same line to delineate runs." ;
  "-pred", Arg.Set_string to_eval,
  "\t predicate to evaluate at every state on every run. Debug, mostly.";
] 

(* Utility function to read 'command-line arguments' from a file. 
 * This allows us to avoid the old 'ldflags' file hackery, etc. *) 
let parse_options_in_file (file : string) : unit =
  let args = ref [ Sys.argv.(0) ] in 
    try
      let fin = open_in file in 
	(try while true do
	   let line = input_line fin in
	   let words = Str.bounded_split space_regexp line 2 in 
	     args := !args @ words 
	 done with _ -> close_in fin) ;
	Arg.current := 0 ; 
	Arg.parse_argv (Array.of_list !args) 
	  (Arg.align !options) 
	  (fun str -> debug "%s: unknown option %s\n"  file str) usageMsg 
    with _ -> () 

let main () = begin
  let handleArg str = parse_options_in_file str in
    Arg.parse (Arg.align !options) handleArg usageMsg ;

    (* get relevant hashtables from instrumentation *)
    let max_site = ref 0 in
    let in_channel = open_in !cbi_hash_tables in 
	  site_ht := Marshal.from_channel in_channel;
	  max_site := Marshal.from_channel in_channel;
	  coverage_ht := Marshal.from_channel in_channel;
	  close_in in_channel;
	  (* compile list of files containing output of instrumented program runs *)

	  let fin = open_in !runs_in in
	  let file_list = ref [] in
	    begin
	      try
			while true do
			  let line = input_line fin in
			  let split = Str.split whitespace_regexp line in 
				file_list := ((hd split), (hd (tl split))) :: !file_list
			done
	      with _ -> close_in fin
	    end;
	    let graph = DynamicExecGraph.build_graph !file_list in
	      DynamicExecGraph.print_graph graph;
	      let ranked = DynamicPredict.invs_that_predict_inv graph (RunFailed) in
			liter
			  (fun (p1,s1,rank1) -> 
				 let e = 
				   match p1 with
					 CilExp(e) -> e
				   | _ -> failwith "rank print not implemented"
				 in
				 let exp_str = Pretty.sprint 80 (d_exp () e) in
				   pprintf "pred: %s, state: %d, f_P: %d s_P: %d, f_P_obs: %d s_P_obs: %d, failure_P: %g, context:%g,increase: %g, imp: %g\n" 
	  				 exp_str s1 rank1.f_P rank1.s_P rank1.f_P_obs
					 rank1.s_P_obs rank1.failure_P rank1.context rank1.increase
					 rank1.importance; flush stdout)
			  ranked;
			(* we've ranked; now, if to_eval is set, evaluate that
			   predicate at every state on every run. *)
			(* for now, debug: *)
			let pred = match (List.hd ranked) with (p1,s1,rank1) -> p1 in
			  pprintf "Propagating and predicting the top predictor: ";
			  d_pred pred; flush stdout;
			  ignore(DynamicExecGraph.propagate_predicate graph pred);
			  let ranked = DynamicPredict.invs_that_predict_inv graph (pred) in 
				liter
				  (fun (p1,s1,rank1) -> 
					 let e = 
					   match p1 with
						 CilExp(e) -> e
					   | _ -> failwith "rank print not implemented"
					 in
					 let exp_str = Pretty.sprint 80 (d_exp () e) in
					   pprintf "pred: %s, state: %d, f_P: %d s_P: %d, f_P_obs: %d s_P_obs: %d, failure_P: %g, context:%g,increase: %g, imp: %g\n" 
	  					 exp_str s1 rank1.f_P rank1.s_P rank1.f_P_obs
						 rank1.s_P_obs rank1.failure_P rank1.context rank1.increase
						 rank1.importance; flush stdout)
				  ranked;
				pprintf "\n\n\nPredicting success: \n";
				let ranked = DynamicPredict.invs_that_predict_inv graph
				  (RunSucceeded) in
				  liter
					(fun (p1,s1,rank1) -> 
					   let e = 
						 match p1 with
						   CilExp(e) -> e
						 | _ -> failwith "rank print not implemented"
					   in
					   let exp_str = Pretty.sprint 80 (d_exp () e) in
						 pprintf "pred: %s, state: %d, f_P: %d s_P: %d, f_P_obs: %d s_P_obs: %d, failure_P: %g, context:%g,increase: %g, imp: %g\n" 
	  					   exp_str s1 rank1.f_P rank1.s_P rank1.f_P_obs
						   rank1.s_P_obs rank1.failure_P rank1.context rank1.increase
						   rank1.importance; flush stdout)
					ranked
end ;;

main () ;;
