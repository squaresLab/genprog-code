open Batteries
open Map
open RefList
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
let inter_weights = RefList.empty ()
let do_cbi = ref false
let preprocessed = ref false 
let graph_file = ref ""

(* what we want to do is print out to several files, one for fault and
 * one for fix for each of several strategies, in addition to the
 * debug output. *)

let usageMsg = "Giant Predicate Processing Program of Doom\n"
let options =  [
  "-cbi-hin", Arg.Set_string cbi_hash_tables, 
  "\t File containing serialized hash tables from my implementation \
                of CBI." ;
  "-rs", Arg.Set_string runs_in,
  "\t File listing names of files containing runs, followed by a passed \
                or failed on the same line to delineate runs." ;
  "-inter", Arg.String (fun str -> RefList.add inter_weights (float_of_string (str))),
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

let file_list () =
  let fin = open_in !runs_in in
  let file_list = RefList.empty ()in
	(try
	  while true do
		let line = input_line fin in
		let split = Str.split whitespace_regexp line in 
		  RefList.add file_list ((hd split), (hd (tl split)))
	  done
	with _ -> close_in fin); RefList.to_list file_list

let preprocess () = 
  begin
	(* compile list of files containing output of instrumented program runs *)
	let file_list = file_list () in


	(* preprocess the input files *)
	let count = ref 0 in
	let lst =
	  lmap
		(fun (fname,porf) ->
		   let sp_count,site_count = hcreate 100,hcreate 100 in
		   let fname' = fname ^".processed" in
		   let fout = open_out_bin fname' in
		   let ((transitions : (int, int) MultiPMap.t),(last_site : int),(doing_sp: bool), (sites_vars : memV StringMap.t),(sp_line :string)) = 
			 Enum.fold
			   (fun (transitions,last_site,doing_sp,sites_vars,(sp_line : string))-> 
				  fun line ->
					let split = Str.split comma_regexp line in 
					  if (String.head (hd split) 1)  = "*" then 
						let lval,rval = hd (tl split),hd (tl (tl split)) in 
						  transitions,last_site,doing_sp,(StringMap.add lval (mval_of_string rval) sites_vars),sp_line
					  else begin
						let site_num,info = int_of_string (hd split),(tl split) in
						let transitions = MultiPMap.add last_site site_num transitions in
						  if doing_sp then 
							begin
							  let memmap = ht_find !layout_map sites_vars 
								(fun x -> incr count; 
								   hadd !layout_map sites_vars !count; 
								   hadd !rev_map !count sites_vars; !count) in
								hincr sp_count (sp_line,memmap)
							end;
						  match (hfind !site_ht site_num) with
							Scalar_pairs(_) -> transitions,site_num,true,(Layout.empty_layout ()),line
						  | Branches(_,ts,fs) -> 
							  hincr site_count line;
							  let torf = int_of_string (hd (info)) in
							  let sites = if torf == 0 then fs else ts in
							  let last,transitions =
								lfoldl
								  (fun (last,transitions) ->
									 fun next -> next, (MultiPMap.add last_site site_num transitions))
								  (site_num,transitions) sites
							  in
								transitions,site_num,false,sites_vars,sp_line
						  | _ -> hincr site_count line; transitions,site_num,false,sites_vars,sp_line
					  end)
			   ((MultiPMap.create compare compare),-1,false,(StringMap.empty),"") 
			   (File.lines_of fname) in
			 (* oh hai, we should add the last line otherwise the last site will
				be lost if it's a scalar-pairs site *)
		   let final = 
			 if (String.head (String.capitalize porf) 1) == "P" then -2 else -3 in
		   let transitions : (int, int) MultiPMap.t = MultiPMap.add last_site final transitions in 
			 Marshal.output fout ?closures:(Some(true)) transitions;
			 Marshal.output fout site_count;
			 Marshal.output fout sp_count;
			 close_out fout; (fname',porf)
		) file_list in
	let fout = open_out_bin (!name^".mem.bin") in
	  Marshal.output fout !layout_map;
	  Marshal.output fout !rev_map;
	  close_out fout;
	  lst

  end

let main () = begin
  Random.self_init ();

	Utils.handle_options options usageMsg;

(*  let to_parse_later = RefList.empty () in
  let handleArg str = RefList.add to_parse_later str in
  let aligned = Arg.align options in
    Arg.parse aligned handleArg usageMsg ;
	liter parse_options_in_file (RefList.to_list to_parse_later);
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
		  ) (List.sort ?cmp:(Some(fun (a,_,_) (a',_,_) -> compare a a')) (options)) ; 
*)
    (* get relevant hashtables from instrumentation *)
    let max_site = ref 0 in
    let in_channel = open_in !cbi_hash_tables in 
	  ignore(Marshal.input in_channel); (* first thing is the file and we don't care *)
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
			  false -> pprintf "preprocessing\n"; flush stdout; preprocess()
			| true -> lmap (fun (fname,porf) -> fname^".processed", porf) (file_list ())
		  in
			(* FIXME: the name of the mem file is going to turn into a
			   confusing, undocumented mess if I'm not careful *)
			pprintf "building graph\n"; flush stdout;
		  let g = DynamicExecGraph.build_graph file_list (!name^".mem.bin") in
		  let fout = open_out_bin (!name^"_graph.bin") in
			Marshal.output fout g;
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
				 | Empty -> failwith "Empty statement in site_ht print" 
			   in
				 pprintf "%s stmt_num: %d site_num: %d, location: %s, exp: %s\n" typ stmt sitenum (Pretty.sprint 80 (d_loc () loc)) exp_str
		  ) !site_ht; flush stdout;
		let ranked_failure = DynamicPredict.invs_that_predict_inv graph (RunFailed) in
		  liter print_ranked ranked_failure;
		  pprintf "\n\nAbout to rank success:\n\n"; flush stdout;
		let ranked_success = DynamicPredict.invs_that_predict_inv graph (RunSucceeded) in
		  pprintf "\ndone ranking success\n\n"; flush stdout;
		  liter print_ranked ranked_success;
		  let pred = match (List.hd ranked_failure) with (p1,s1,rank1) -> p1 in
			pprintf "Propagating and predicting the top predictor: %s\n"
			  (d_pred pred); flush stdout;
			DynamicExecGraph.propagate_predicate graph pred;
			pprintf "Done propagating\n"; flush stdout;
			let ranked = DynamicPredict.invs_that_predict_inv graph (pred) in 
			  liter print_ranked ranked;
			  DynamicExecGraph.print_fault_localization graph true true (RefList.to_list inter_weights);
			  DynamicExecGraph.print_fix_localization graph;
				pprintf "Done!\n"; flush stdout;
end ;;

main () ;;
