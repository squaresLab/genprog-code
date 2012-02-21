open Batteries 
open Utils
open Ref
open Map
open Set
open Unix
open IO
open Enum
open Str
open List
open Cil
open Globals
(*open Difftypes
open Cabs
open Treediff*)

(* options *)
let benchmark = ref ""
let svn_log_file_in = ref ""
let svn_log_file_out = ref ""
let read_hts = ref ""
let write_hts = ref ""
let exclude = ref []
let repos = ref ""
let rstart = ref None
let rend = ref None
let templatize = ref ""
let vec_file = ref "vectors.vec"
let print_preloaded = ref false

let devnull = Pervasives.open_out_bin "/dev/null"
let configs = ref []

let fullsave = ref ""
let skip_svn = ref false
let wipe_hts = ref false
let read_temps = ref false

let grouped = ref false 

let _ =
  options := 
    !options @
    [
	  "--configs", Arg.Rest (fun s -> configs := s :: !configs), 
	  "\t input config files for each benchmark. Processed separately in the same way as regular command-line arguments.";
	  "--fullsave", Arg.Set_string fullsave, "\t file to save composed hashtable\n";
	  "--grouped", Arg.Set grouped, "\t input for explore buckets: grouped?\n"
    ]

let diffopts  =
  [
    "--rstart", Arg.Int (fun x -> rstart := Some(x)), "\t Start revision.  Default: 0.";
    "--rend", Arg.Int (fun x -> rend := Some(x)), "\t End revision.  Default: latest.";
    "--bench", Arg.Set_string benchmark, "\t benchmark name, recommended for sanity checking.";
    "--exclude",Arg.String (fun x -> exclude := x :: !exclude), "\t paths/names of files to exclude from diffs";
    "--logfile", Arg.Set_string svn_log_file_in, "\t file containing the svn log\n";
    "--writelog", Arg.Set_string svn_log_file_out, "\t file to which to write the svn log\n";
    "--repos", Arg.Set_string repos, "\t URL of the repository.";
    "--load", Arg.Set_string read_hts, "\t X file from which to read stored svn information\n";
    "--save", Arg.Set_string write_hts, "\t save svn information to file X";
    "--templatize", Arg.Set_string templatize,  "\t Save templates to/read from X\n";
    "--vec-file", Arg.Set_string vec_file, "\t file to output vectors\n";
    "--read-temps", Arg.Set read_temps, "\t Read templates from serialized file passed to templatize";
    "--skip-svn", Arg.Set skip_svn, "\t don't bother getting info from svn";
  ]

let reset_options () =
  benchmark := "";
  svn_log_file_in := "";
  svn_log_file_out := "";
  read_hts := "";
  write_hts := "";
  exclude := [];
  repos := "";
  rstart := None;
  rend := None;
  vec_file := "vectors.vec";
  templatize :=  ""
    
let load_from_saved () = 
  pprintf "Loading from saved: %s\n" !read_hts; flush stdout;
  let in_channel = open_in_bin !read_hts in
  let diff_text_ht = 
    try
      let bench = Marshal.input in_channel in
	if bench <> !benchmark then pprintf "WARNING: bench (%s) and benchmark (%s) do not match\n" bench !benchmark; 
(*	ignore(Marshal.input in_channel);
	ignore(Marshal.input in_channel);*)
	let diff_text_ht = Marshal.input in_channel in
	  diff_text_ht
    with _ -> 
      begin
	pprintf "WARNING: load_from_saved failed.  Resetting everything!\n"; flush stdout;
	hcreate 10
      end
  in
    close_in in_channel; diff_text_ht

(* these refs are mostly here for accounting and debugging purposes *)
let successful = ref 0
let failed = ref 0

(* Claire's version of DeltaDoc, taken from Figure 4 in the ASE paper on the
   subject *)
let delta_doc f1 f2 data_ht f1ht f2ht = 
  let functions_changed = List.of_enum (Hashtbl.keys data_ht) in
	debug "Functions changed: %d, " (llen functions_changed);
	liter (function name -> debug "%s, " name) functions_changed;
	debug "\n";
  let mapping1 = Tigen.path_generation f1 f1ht functions_changed in 
  let mapping2 = Tigen.path_generation f2 f2ht functions_changed in 
	(* FIXME: Statements not guarded by predicates! *)
	StringMap.iter
	  (fun funname stmt_set2 ->
		debug "in delta doc, funname: %s, stmt_set2: %d\n" funname (StringMap.cardinal stmt_set2);
		  StringMap.iter
			(fun k _ -> debug "%s\n" k)
			stmt_set2;
		let stmt_set1 = Map.find funname mapping1 in
		  debug "stmt_set1: %d\n" (StringMap.cardinal stmt_set1);
		let domain_pold = Set.of_enum (Map.keys stmt_set1) in
		let domain_pnew = Set.of_enum (Map.keys stmt_set2) in
		let inserted = StringSet.diff domain_pnew domain_pold in
		let deleted = StringSet.diff domain_pold domain_pnew in
		let intersection = StringSet.inter domain_pnew domain_pold in
		let changed = 
		  StringSet.fold
			(fun stmt changed ->
			  let predicates1,_ = StringMap.find stmt stmt_set1 in
			  let predicates2,_ = StringMap.find stmt stmt_set2 in
				debug "stmt: %s\n predicates1:\n" stmt;
				StringSet.iter (fun pred -> debug "\t%s\n" pred) predicates1;
				debug "predicates2:\n";
				StringSet.iter (fun pred -> debug "\t%s\n" pred) predicates2;
				if predicates1 <> predicates2 then
				  StringSet.add stmt changed
				else changed
			) intersection StringSet.empty
		in
		let mustDoc = StringSet.union inserted (StringSet.union deleted changed) in
		  debug "num mustDoc: %d\n" (StringSet.cardinal mustDoc);
		let pred_count_ht = hcreate 10 in
		let pnew_ht = hcreate 10 in
		let pold_ht = hcreate 10 in
		  StringSet.iter
			(fun stmt ->
			  let predicates1,loc1 = if StringMap.mem stmt stmt_set1 then StringMap.find stmt stmt_set1 else StringSet.empty,builtinLoc in
			  let predicates2,loc2 = if StringMap.mem stmt stmt_set2 then StringMap.find stmt stmt_set2 else StringSet.empty,builtinLoc in
				StringSet.iter (ht_incr pred_count_ht) predicates1;
				let start_lst_old = ht_find pold_ht predicates1 (fun _ -> []) in
				let start_lst_new = ht_find pnew_ht predicates2 (fun _ -> []) in
				  hrep pold_ht predicates1 ((stmt,loc1) :: start_lst_old);
				  hrep pnew_ht predicates2 ((stmt,loc2) :: start_lst_new);
			) mustDoc;
		  let pold_ht = 
			hfold
			  (fun k v pold_ht ->
				let sorted_stmts = 
				  List.sort ~cmp:(fun (_,l1) (_,l2) -> compareLoc l1 l2) v
				in
				  hadd pold_ht k sorted_stmts; pold_ht
			  ) pold_ht (hcreate 10) in
		  let pnew_ht = 
			hfold
			  (fun k v pnew_ht ->
				let sorted_stmts = 
				  List.sort ~cmp:(fun (_,l1) (_,l2) -> compareLoc l1 l2) v
				in
				  hadd pnew_ht k sorted_stmts; pnew_ht
			  ) pnew_ht (hcreate 10) in
		  let pred_count = List.of_enum (Hashtbl.enum pred_count_ht) in 
		  let preds_sorted : (string * int) list = 
			List.sort ~cmp:(fun (p1,c1) (p2,c2) -> Pervasives.compare c1 c2) pred_count
		  in
		  let rec hierarchical_doc (tablevel : string) (mustDoc : StringSet.t) (p : StringSet.t) (predicates : (string * int) list) : StringSet.t =
			if not (StringSet.is_empty mustDoc) then begin
			  let pnew_guarded_by = 
				lfilt (fun (s,_) -> StringSet.mem s mustDoc) 
				  (ht_find pnew_ht p (fun _ -> []))
			  in
			  let mustDoc = 
				lfoldl
				  (fun mustDoc (stmt,l1) -> pprintf "%sDO %s\n" tablevel stmt; StringSet.remove stmt mustDoc)
				  mustDoc pnew_guarded_by
			  in
				
			  let pold_guarded_by =
				lfilt (fun (s,_) -> StringSet.mem s mustDoc) (ht_find pold_ht p (fun _ -> []))
			  in
			  let mustDoc = 
				lfoldl
				  (fun mustDoc (stmt,l1) -> pprintf "%sINSTEAD OF %s\n" tablevel stmt; StringSet.remove stmt mustDoc)
				  mustDoc pold_guarded_by
			  in
				lfoldl
				  (fun mustDoc (pred,c) ->
					if not (StringSet.is_empty mustDoc) then begin
					  pprintf "IF %s\n" pred;
					  let tablevel' = Printf.sprintf "\t%s" tablevel in
					  let predicates = List.remove_assoc pred predicates in
						hierarchical_doc tablevel' (StringSet.add pred p) mustDoc predicates
					end else mustDoc
				  ) mustDoc predicates
			end else mustDoc
		  in
		  let mustDoc = hierarchical_doc "" mustDoc (StringSet.empty) preds_sorted in
			assert(StringSet.is_empty mustDoc)
	  ) mapping2


let parse_files_from_diff input exclude_regexp = 
  let finfos,(lastname,strs) =
	efold
	  (fun (finfos,(fname,strs)) ->
		fun str ->
		  if string_match index_regexp str 0 then 
			begin
			  let split = Str.split space_regexp str in
			  let fname' = hd (tl split) in
			  let matches_exclusions = 
				try 
				  match exclude_regexp with
					Some(exclude_regexp) -> 
					  ignore(Str.search_forward exclude_regexp fname' 0); true 
				  | None -> false with Not_found -> false in
			  let ext = 
				try
				  let base = Filename.chop_extension fname' in
					String.sub fname' ((String.length base)+1)
					  ((String.length fname') - ((String.length base)+1))
				with _ -> "" in
			  let fname' =
				if matches_exclusions then "" else
				  match String.lowercase ext with
				  | "c" | ".h" | ".y" ->  fname'
				  | _ -> "" in
				((fname,strs)::finfos),(fname',[])
			end 
		  else 
			if (String.is_empty fname) ||
			  (string_match junk str 0) then 
			  (finfos,(fname,strs))
			else begin pprintf "%s\n" str; (finfos,(fname,str::strs)) end
	  ) ([],("",[])) input
  in
  let finfos = (lastname,strs)::finfos in
	efilt (fun (str,_) -> not (String.is_empty str)) (List.enum finfos)

(* collect changes is a helper function for get_diffs *)
	
let collect_changes revnum logmsg url exclude_regexp diff_text_ht =
  let svn_gcc fname revnum = 
    let svn_cmd = "svn cat -r"^(String.of_int revnum)^" "^url^"/"^fname in
    let tempfile = Printf.sprintf "temp_%s.c" !benchmark in
    let svn_ret = cmd svn_cmd in 
	  Globals.file_process svn_ret tempfile
  in
	pprintf "collect changes, rev %d, msg: %s\n" revnum logmsg; flush stdout;
	let input : string list = 
	  let svn_cmd = "svn diff -x -uw -r"^(String.of_int (revnum-1))^":"^(String.of_int revnum)^" "^url in
	    ht_find diff_text_ht svn_cmd (fun _ -> List.of_enum (cmd svn_cmd))
	in
	let files = parse_files_from_diff (List.enum input) exclude_regexp in
	let files = efilt (fun (fname,_) -> not (String.is_empty fname)) files in
	  List.of_enum
		(emap
	       (fun (fname,strs) -> 
			 pprintf "FILE NAME: %s, revnum: %d\n" fname revnum;
			 let old_strs = svn_gcc fname (revnum - 1) in 
			 let new_strs = svn_gcc fname revnum in 
			 (* get a list of changed functions between the two files *)
			 let f1, f2, data_ht, f1ht, f2ht = Cdiff.tree_diff_cil old_strs new_strs in
			   delta_doc f1 f2 data_ht f1ht f2ht;
			   pprintf "%d successes so far\n" (pre_incr successful);
			   []
		   ) files)

let rec test_delta_doc files =
  match files with
	one :: two :: rest ->
	  debug "test_delta_doc, file1: %s, file2: %s\n" one two;
	  let file1_strs = File.lines_of one in
	  let file2_strs = File.lines_of two in
	  let f1,f2,data_ht, f1ht, f2ht = Cdiff.tree_diff_cil file1_strs file2_strs in 
		delta_doc f1 f2 data_ht f1ht f2ht;
		test_delta_doc rest
  | _ -> ()

let get_diffs_and_templates  ?donestart:(ds=None) ?doneend:(de=None) diff_text_ht vec_fout =
  let save_hts () =  ()
  (*	diff_ht_counter := 0;
		pprintf "Starting save_hts...\n"; flush stdout;
		if !write_hts <> "" then begin
		let fout = open_out_bin !write_hts in
		Marshal.output fout !benchmark;
		Marshal.output fout diff_text_ht;
		close_out fout
		end;
		if !templatize <> "" then begin
		let fout = open_out_bin !templatize in
		Marshal.output fout Template.template_tbl;
		close_out fout
		end;
		pprintf "Done in save_hts...\n"; flush stdout;*)
   in
  let log = 
	if !svn_log_file_in <> "" then File.lines_of !svn_log_file_in
	else begin
	  let logcmd = 
		match !rstart,!rend with
		  Some(startrev),Some(endrev) ->
			"svn log "^ !repos ^" -r"^(String.of_int startrev)^":"^(String.of_int endrev)
		| _,_ ->  "svn log "^ !repos
	  in
	  let lines = cmd logcmd in
		if !svn_log_file_out <> "" then
		  File.write_lines !svn_log_file_out lines; 
		lines
	end
  in
  let grouped = egroup (fun str -> string_match dashes_regexp str 0) log in
  let filtered =
	efilt
	  (fun enum ->
		(not (eexists
				(fun str -> (string_match dashes_regexp str 0)) enum))
	  ) grouped in
  let all_revs = 
	emap
	  (fun one_enum ->
		let first = Option.get (eget one_enum) in
		  if not (String.is_empty first) then begin
		    let rev_num = int_of_string (string_after (hd (Str.split space_regexp first)) 1) in
			  ejunk one_enum;
			  let logmsg = efold (fun msg -> fun str -> msg^str) "" one_enum in
				(rev_num,logmsg) 
		  end else (-1,"")
	  ) filtered in
  let only_fixes = 
	efilt
	  (fun (revnum,logmsg) ->
		try
		  ignore(search_forward fix_regexp logmsg 0); 
		  let done_yet = 
			match ds,de with
			  Some(r1),Some(r2) -> revnum >= r1 && revnum <= r2
			| _ -> false 
		  in 
			(not done_yet) && 
			  (match !rstart, !rend with
				Some(r1),Some(r2) -> revnum >= r1 && revnum <= r2
			  | _ -> revnum > -1)
		with Not_found -> false) all_revs
  in
  let exclude_regexp = 
	if (llen !exclude) > 0 then begin
	  let exclude_strs = lmap Str.quote !exclude in 
	  let reg_str = 
		if (llen exclude_strs) > 1 then begin
		  lfoldl
			(fun accum ->
			  fun reg_str -> reg_str ^ "\\|" ^ accum) (List.hd exclude_strs) (List.tl exclude_strs) 
		end
		else if (llen exclude_strs) == 1 then (List.hd exclude_strs)
		else ""
	  in
		Some(Str.regexp reg_str)
	end else None
  in 
	(try
	   Enum.iter
		 (fun (revnum,logmsg) ->
		   let changes = lflat (collect_changes revnum logmsg !repos exclude_regexp diff_text_ht) in
		     if (llen changes) > 0 then begin
(*			   let diff = new_diff revnum logmsg changes !benchmark in*)
(*			     try
				   let templates = [] in (*lflat (lmap (fun change -> Template.diff_to_templates diff change change.tree ("",[nd(Globals([change.tree]))])) diff.changes) in*)
(*				   let vectors = 
					 lmap (fun template -> 
					   let vectors = Vectors.template_to_vectors template true true in
						 hadd vector_tbl template.template_id vectors; vectors
					 ) templates in*)
(*				   let print_fun = if !separate_vecs then Vectors.print_vectors_separate vec_fout else Vectors.print_vectors vec_fout in
					 liter print_fun vectors;*)
(*					 if (!diff_ht_counter == 10) then (save_hts (); flush vec_fout)
					 else incr diff_ht_counter*)
			     with e -> pprintf "warning: template failure: %s\n" (Printexc.to_string e)*) ()
		     end) only_fixes
	 with Not_found -> ());
	pprintf "made it after all_diff\n"; flush stdout;
	save_hts();
	pprintf "after save hts\n"; flush stdout;
	pprintf "%d successful change parses, %d failed change parses, %d total changes\n"
	  !successful !failed (!successful + !failed)
	  
let get_many_templates ?vprint:(vprint=true) configs =
  let handleArg _ = 
    failwith "unexpected argument in benchmark config file\n"
  in
    Enum.iter
      (fun config_file -> 
		pprintf "config file: %s\n" config_file; 
		reset_options ();
		let aligned = Arg.align diffopts in
		let max_diff = ref (-1) in
		let min_diff = ref (-1) in
		  parse_options_in_file ~handleArg:handleArg aligned "" config_file;
		  let vec_fout = if vprint then File.open_out !vec_file else File.open_out "/dev/null" in
(*			if !read_temps then begin
			  let fin = open_in_bin !templatize in
			  let res1 = Marshal.input fin in 
				close_in fin; 
				if !debug_bl then begin
				  let templates = List.of_enum (Hashtbl.values res1) in
				  let sorted = List.sort 
					~cmp:(fun temp1 -> fun temp2 -> 
					  temp1.diff.rev_num - temp2.diff.rev_num
					) templates 
				  in
				  let current_rev = ref 0 in
				  let syntax strs = lfoldl
					(fun strs ->
					  fun str ->
						strs^"\n"^str) "" strs 
				  in
					liter
					  (fun template ->
						if template.diff.rev_num <> !current_rev then begin
						  current_rev := template.diff.rev_num;
						  pprintf "Revision: %d; msg: %s; syntax: %s\n" !current_rev template.diff.msg (syntax (hd template.diff.changes).syntax);
						end;
						let stmts = find_stmt_parents template.edits template.def in
						  pprintf "\t template id: %d, parent_stmts:" template.template_id;
						  liter (fun (stmto,_) -> match stmto with Some(stmt) -> pprintf "\t\t %s\n" (stmt_str stmt) | None -> pprintf "\t\t NONE\n") stmts
					  ) sorted
				end else 
				  hiter 
					(fun k ->
					  fun template ->
						if k > !Difftypes.template_id then
						  Difftypes.template_id := k;
						if (!max_diff < 0) || (template.diff.rev_num > !max_diff) then
						  max_diff := template.diff.rev_num;
						if (!min_diff < 0) || (template.diff.rev_num < !min_diff) then
						  min_diff := template.diff.rev_num;
						hadd Template.template_tbl k template;
						let vectors = Vectors.template_to_vectors template true true in 
						let print_fun = if !separate_vecs then Vectors.print_vectors_separate vec_fout else Vectors.print_vectors vec_fout in
						  print_fun vectors
					) res1
			end;*)
			if not !skip_svn then begin
			  let diff_text_ht = 
				if !read_hts <> "" then load_from_saved () 
				else hcreate 10
			  in
				pprintf "max_diff: %d, min_diff: %d\n" !max_diff !min_diff;
				if not (!max_diff < 0) then 
				  get_diffs_and_templates ~donestart:(Some(!min_diff)) ~doneend:(Some(!max_diff)) diff_text_ht vec_fout
				else
				  get_diffs_and_templates diff_text_ht vec_fout
			end;
			close_out vec_fout
	  ) (List.enum configs)

type bucket = int * int list (* bucket is an indicative query point and a list
								of template ids *)
let explore_buckets lsh_output configs = 
  let query_r = if !grouped then Str.regexp_string "Template " else Str.regexp_string "Query point" in
  let neighbor_r = if !grouped then Str.regexp_string "TID:" else Str.regexp "^[0-9][0-9][0-9][0-9][0-9][ \t]+dist:" in
  let query_tid_location = if !grouped then 8 else 6 in
  let query_bench_location = if !grouped then 10 else 8 in
  let neighbor_tid_loc = if !grouped then 0 else 4 in
  let neighbor_bench_loc = 3 in
  let giant_tbl_ht = hcreate 10 in
	Enum.iter
	  (fun config_file ->
		 pprintf "config file: %s\n" config_file; flush stdout;
		 reset_options ();
		 let aligned = Arg.align diffopts in
		   parse_options_in_file ~handleArg:handleArg aligned "" config_file;
		   pprintf "Bench: %s\n" !benchmark;
		   let fin = open_in_bin !templatize in
		   let tbl1 = Marshal.input fin in 
			 hrep giant_tbl_ht !benchmark tbl1
	  ) configs;
	let lsh_data = File.lines_of lsh_output in 
	let bucket_ht : ((string * int), (string * int) list) Hashtbl.t = hcreate 10 in
	let add_to_bucket (bench,query : (string * int)) (neighbor : (string * int)) = 
	  if query < 0 then failwith "adding impossible negative cluster to bucket"
	  else
		let old = ht_find bucket_ht (bench,query) (fun _ -> []) in
		  hrep bucket_ht (bench,query) (neighbor :: old)
	in
	  ignore(efold
			   (fun this_cluster ->
				 fun line ->
				   let split = Str.split space_regexp line in 
					 if Str.string_match query_r line 0 then begin
(*						 pprintf "one: query_tid_loc: %d, line: %s\n" query_tid_location line;*)
						 let query_tid = int_of_string (List.nth (Str.split colon_regexp (List.nth split query_tid_location)) 1) in
						 let query_bench = List.nth split query_bench_location in
(*						   pprintf "Bench: %s, id: %d\n" query_bench query_tid;*)
						 query_bench,query_tid
					 end else begin
					   if Str.string_match neighbor_r line 0 then begin
(*						 pprintf "four. neigh_tid_loc: %d Line: %s\n" neighbor_tid_loc line;*)
						 let neighbor_tid = int_of_string(List.hd (List.tl (Str.split colon_regexp (List.nth split neighbor_tid_loc)))) in
(*						 pprintf "five\n";*)
						 let neigh_bench = List.nth split neighbor_bench_loc in
(*						 pprintf "six\n";*)
						   add_to_bucket this_cluster (neigh_bench,neighbor_tid)
					   end; this_cluster
					   end
			   ) ("",-1) lsh_data);
	  hiter
		(fun (query_bench,query_point) ->
		  fun neighbors ->
			try
			let template_ht = ht_find giant_tbl_ht query_bench (fun _ -> failwith (Printf.sprintf "giant query_bench: %s\n" query_bench)) in 
			let query_t = ht_find template_ht query_point (fun _ -> failwith (Printf.sprintf "giant query_bench: %s query_tid: %d\n" query_bench query_point)) in
			let syntax strs = lfoldl
			  (fun strs ->
				fun str ->
				  strs^"\n"^str) "" strs 
			in
			  ()
(*			  pprintf "\nQuery_point: %d, fname: %s\n" query_t.template_id query_t.change.fname;
			  pprintf "edits: "; liter print_edit query_t.edits; 
			  pprintf "%d Neighbors:\n" (llen neighbors);
			  liter (fun (neigh_bench,neighbor) -> 
					   let template_ht = ht_find giant_tbl_ht neigh_bench (fun _ -> failwith (Printf.sprintf "giant neigh_bench: %s\n" neigh_bench)) in
					   let neighbor = ht_find template_ht neighbor (fun _ -> failwith (Printf.sprintf "neighbor bench: %s tid: %d\n" neigh_bench neighbor)) in
					     pprintf "%d: %s\n" neighbor.template_id neighbor.change.fname;
						 liter print_edit neighbor.edits)
				neighbors*)
			with e -> (pprintf "some kind of fail: %s\n" (Printexc.to_string e))
		) bucket_ht
