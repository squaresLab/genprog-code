open List
open Cil
open Pretty
open Utils
open DPGlobs
open Globals
open Invariant
open Memory
open State
open Graph
open Predict

let cbi_hash_tables = ref ""
let runs_in = ref ""
let to_eval = ref ""
let inter_weights = ref []
let do_cbi = ref false
let preprocessed = ref false 
let graph_file = ref ""

(* what we want to do is print out to several files, one for fault and
 * one for fix for each of several strategies, in addition to the
 * debug output. *)

let usageMsg = "Giant Predicate Processing Program of Doom\n"
let options = ref [
  "-cbi-hin", Arg.Set_string cbi_hash_tables, 
  "\t File containing serialized hash tables from my implementation \
                of CBI." ;
  "-rs", Arg.Set_string runs_in,
  "\t File listing names of files containing runs, followed by a passed \
                or failed on the same line to delineate runs." ;
  "-inter", Arg.String (fun str -> inter_weights :=
						  float_of_string(str) :: !inter_weights),
  "\t Do intersection-style localization (think genprog baseline) \
       with X as the weight given to statements on the good path." ;
  "-cbi-fault", Arg.Set do_cbi, "\t Do CBI-style fault localization." ;
  "-pred", Arg.Set_string to_eval,
  "\t predicate to evaluate at every state on every run. Debug, \
      mostly.";
  "-rand", Arg.Set_int num_rand, "\t Number of random path files to generate; Default = 1";
  "-name", Arg.Set_string name, "\t Name to prepend to output files." ;
  "-processed", Arg.Set preprocessed, "\t read hashtables from preprocessed files." ;
  "-graph", Arg.Set_string graph_file, "\t read graph from serialized output.";
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

let file_list () =
  let fin = open_in !runs_in in
  let file_list = ref [] in
	(try
	  while true do
		let line = input_line fin in
		let split = Str.split whitespace_regexp line in 
		  file_list := ((hd split), (hd (tl split))) :: !file_list
	  done
	with _ -> close_in fin); !file_list

let preprocess () = 
  begin
	(* compile list of files containing output of instrumented program runs *)
	let file_list = file_list () in


	(* preprocess the input files *)
	let count = ref 0 in
	let sites_vars = ref StringMap.empty in
	let doing_sp = ref false in
	let sp_line = ref "" in
	let lst =
	  lmap
		(fun (fname,porf) ->
		   let transitions,sp_count,site_count = hcreate 100,hcreate 100,hcreate 100 in
		   let fname' = fname ^".processed" in
		   let fin, fout = open_in fname, open_out_bin fname' in
		   let last_site = ref (-1) in
			 (try 
				while true do
				  let line = input_line fin in
				  let split = Str.split comma_regexp line in 
					if (String.sub (hd split) 0 1) = "*" then 
					  let lval,rval = hd (tl split),hd (tl (tl split)) in 
						sites_vars := StringMap.add lval (mval_of_string rval) !sites_vars
					else
					  (let site_num,info = int_of_string (hd split),(tl split) in
						 hrep transitions (!last_site,site_num) ();
						 if !doing_sp then 
						   (doing_sp := false;
							let memmap = ht_find !layout_map !sites_vars 
							  (fun x -> incr count; 
								 hadd !layout_map !sites_vars !count; 
								 hadd !rev_map !count !sites_vars; !count) in
							  hincr sp_count (!sp_line,memmap));
						 last_site := site_num;
						 match (hfind !site_ht site_num) with
						   Scalar_pairs(_) -> 
							 sp_line := line;
							 sites_vars := Layout.empty_layout();
							 doing_sp := true;
						 | Branches(_,ts,fs) -> 
							 hincr site_count line;
							 let torf = int_of_string (hd (info)) in
							 let sites = if torf == 0 then fs else ts in
							   hrep transitions (!last_site,site_num) ();
							   last_site :=
								 lfoldl
								   (fun last ->
									  fun next ->
										hrep transitions (last,next) (); next) site_num sites
						 | _ -> hincr site_count line)
				done
			  with End_of_file -> ());
			 let final = 
			   if (String.get (String.capitalize porf) 0) == 'P' then -2 else -3 in
			   hrep transitions (!last_site,final) ();
			   Marshal.to_channel fout transitions [];
			   Marshal.to_channel fout site_count [];
			   Marshal.to_channel fout sp_count [];
			   close_in fin; close_out fout; (fname',porf)
		) file_list in
	let fout = open_out_bin (!name^".mem.bin") in
	  Marshal.to_channel fout !layout_map [] ;
	  Marshal.to_channel fout !rev_map [];
	  close_out fout;
	lst

end

let main () = begin
  Random.self_init ();

  let to_parse_later = ref [] in
  let handleArg str = to_parse_later := !to_parse_later @ [str] in
  let aligned = Arg.align !options in
    Arg.parse aligned handleArg usageMsg ;
	liter parse_options_in_file !to_parse_later ;
	Arg.parse aligned handleArg usageMsg;

	liter (fun (name,arg,_) ->
			 pprintf "%s %s\n" name
			   (match arg with
				| Arg.Set br 
				| Arg.Clear br 
				  -> Printf.sprintf "%b" !br 
				| Arg.Set_string sr
				  -> Printf.sprintf "%S" !sr
				| Arg.Set_int ir
				  -> Printf.sprintf "%d" !ir
				| Arg.Set_float fr
				  -> Printf.sprintf "%g" !fr
				| _ -> "?"); flush stdout
		  ) (List.sort (fun (a,_,_) (a',_,_) -> compare a a') (!options)) ; 

    (* get relevant hashtables from instrumentation *)
    let max_site = ref 0 in
    let in_channel = open_in !cbi_hash_tables in 
	  ignore(Marshal.from_channel in_channel); (* first thing is the file and we don't care *)
      coverage_ht := Marshal.from_channel in_channel;
	  ignore(Marshal.from_channel in_channel); (* third thing is the max stmtid and we don't care *)
      site_ht := Marshal.from_channel in_channel;
      max_site := Marshal.from_channel in_channel;
      close_in in_channel;

	  (* build_graph takes processed log files, because unprocessed = hella
		 long.  Preprocess() processes log files, saves the processed versions, and
		 returns a list of processed files for build_graph *)
	  let graph =
		if !graph_file <> "" then
		  let fin = open_in_bin !graph_file in
		  let g = Marshal.from_channel fin in
			close_in fin; g
		else begin
		  let file_list = 
			match !preprocessed with
			  false -> preprocess()
			| true -> lmap (fun (fname,porf) -> fname^".processed", porf) (file_list ())
		  in
			(* FIXME: the name of the mem file is going to turn into a
			   confusing, undocumented mess if I'm not careful *)
		  let g = DynamicExecGraph.build_graph file_list (!name^".mem.bin") in
		  let fout = open_out_bin (!name^"_graph.bin") in
			Marshal.to_channel fout g [];
			g
		end
	  in
		pprintf "SITE HT:\n";
		hiter
		  (fun sitenum ->
			 fun siteinfo ->
			   let typ,stmt,loc,exp_str =
				 match siteinfo with
				   Branches((loc,stmt,exp,b), ts, fs) -> "BRANCHES",stmt,loc,(Pretty.sprint 80 (d_exp () exp))
				 | Returns((loc,stmt,exp,b)) -> "RETURNS",stmt,loc,(Pretty.sprint 80 (d_exp () exp))
				 | Scalar_pairs((loc,stmt,exp,b),_) -> "SCALARS",stmt,loc,""
				 | Is_visited(loc,stmt) -> "IS EXECUTED",stmt,loc,""
				 | Empty -> failwith "Empty statement in siteht print" 
			   in
				 pprintf "%s stmt_num: %d site_num: %d, location: %s, exp: %s\n" typ stmt sitenum (Pretty.sprint 80 (d_loc () loc)) exp_str
		  ) !site_ht; flush stdout;
		let ranked_failure = DynamicPredict.invs_that_predict_inv graph (RunFailed) in
		  liter print_ranked ranked_failure;
		  let ranked_success = DynamicPredict.invs_that_predict_inv graph (RunSucceeded) in
			pprintf "done ranking success\n"; flush stdout;
			liter print_ranked ranked_success;
			let pred = match (List.hd ranked_failure) with (p1,s1,rank1) -> p1 in
			  pprintf "Propagating and predicting the top predictor: %s\n"
				(d_pred pred); flush stdout;
			  DynamicExecGraph.propagate_predicate graph pred;
			  pprintf "Done propagating\n"; flush stdout;
			  let ranked = DynamicPredict.invs_that_predict_inv graph (pred) in 
				liter print_ranked ranked;
				DynamicExecGraph.print_fault_localization graph true true !inter_weights;
				(*		  DynamicExecGraph.print_fix_localization graph true true !inter_weights;*)
				pprintf "Done!\n"; flush stdout;
end ;;

main () ;;
