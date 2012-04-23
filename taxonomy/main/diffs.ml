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
open Tigen
open Difftypes
(*open Cabs
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
let read_diffs = ref ""
let write_diffs = ref ""
let grouped = ref false 

let _ =
  options := 
    !options @
    [
	  "--configs", Arg.Rest (fun s -> configs := s :: !configs), 
	  "\t input config files for each benchmark. Processed separately in the same way as regular command-line arguments.";
	  "--fullsave", Arg.Set_string fullsave, "\t file to save composed hashtable\n";
	  "--grouped", Arg.Set grouped, "\t input for explore buckets: grouped?\n";
      "--read-diffs", Arg.Set_string read_diffs, "\t don't bother getting info from svn";
      "--write-diffs", Arg.Set_string write_diffs, "\t don't bother getting info from svn";

    ]

let update_script = ref ""
let compile_script = ref ""
let diffopts  =
  [
	"--update-script", Arg.Set_string update_script, "\t svn update script.  Takes a revision number as an argument. Default: BENCHMARK-svn.sh";
	"--compile-script", Arg.Set_string compile_script, "\t compilation script.  Takes no arguments, needs to save temporaries. Default: BENCHMARK-compile.sh";
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
    "--read-diffs", Arg.Set_string read_diffs, "\t don't bother getting info from svn";
    "--write-diffs", Arg.Set_string write_diffs, "\t don't bother getting info from svn";

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
let delta_doc (f1) (f2) (data_ht) (f1ht) (f2ht) : change_node StringMap.t = 
  let functions_changed = List.of_enum (Hashtbl.keys data_ht) in
	debug "Functions changed: %d, " (llen functions_changed);
	liter (function name -> debug "%s, " name) functions_changed;
	debug "\n";
  let mapping1 = Tigen.path_generation f1 f1ht functions_changed in 
  let mapping2 = Tigen.path_generation f2 f2ht functions_changed in 
    (* the mapping from tigen is a map from function name to a stmtmap, where
       the statement map maps statements to assumption sets and locations *)
	StringMap.fold
	  (fun funname pnew concrete_map ->
		let pold = StringMap.find funname mapping1 in
        (* pold and pnew is a map of statements to assumption sets and
           locations *)
		let domain_pold = StmtSet.of_enum (StmtMap.keys pold) in
		let domain_pnew = StmtSet.of_enum (StmtMap.keys pnew) in
		let inserted = StmtSet.diff domain_pnew domain_pold in
		let deleted = StmtSet.diff domain_pold domain_pnew in
		let intersection = StmtSet.inter domain_pnew domain_pold in
		let changed = 
		  StmtSet.fold
			(fun stmt changed ->
			  let predicates1,_ = StmtMap.find stmt pold in
			  let predicates2,_ = StmtMap.find stmt pnew in
				if predicates1 <> predicates2 then
				  StmtSet.add stmt changed
				else changed
			) intersection StmtSet.empty
		in
		let mustDoc = StmtSet.union inserted (StmtSet.union deleted changed) in
		let pred_count_ht = hcreate 10 in
		let pnew_ht = hcreate 10 in
		let pold_ht = hcreate 10 in
		  debug "building predicates lists...\n";
		  StmtSet.iter
			(fun stmt ->
			  let predicates1,loc1 = 
                if StmtMap.mem stmt pold then 
                  StmtMap.find stmt pold else 
                  ExpSet.empty,builtinLoc 
              in
			  let predicates2,loc2 = 
                if StmtMap.mem stmt pnew 
                then StmtMap.find stmt pnew 
                else ExpSet.empty,builtinLoc 
              in
                (* pred_count_ht maps individual expressions to requency counts *)
				ExpSet.iter (ht_incr pred_count_ht) predicates1;
				ExpSet.iter (ht_incr pred_count_ht) predicates2;

				let start_lst_old = ht_find pold_ht predicates1 (fun _ -> []) in
				let start_lst_new = ht_find pnew_ht predicates2 (fun _ -> []) in
                  (* mapping each predicate back to a list of statement,location
                     pairs that this set of predicates have guarded *)
				  hrep pold_ht predicates1 ((stmt,loc1) :: start_lst_old);
				  hrep pnew_ht predicates2 ((stmt,loc2) :: start_lst_new);
			) mustDoc;

		  let pold_ht = 
			hfold (* pold_ht original mapps sets of predicates to a list of
                     stmt,location pairs *)
			  (fun k v pold_ht ->
				let sorted_stmts = 
				  List.sort ~cmp:(fun (_,l1) (_,l2) -> compareLoc l1 l2) v
				in
                  (* so k here is a set of predicates; the stmt list is sorted
                     by location*)
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

            (* pred_count is a list of (exp,count) pairs, exp -> frequency *)
		  let pred_count = List.of_enum (Hashtbl.enum pred_count_ht) in 
            (* list of exp,counts sorted by count *)
		  let preds_sorted = 
			List.sort ~cmp:(fun (p1,c1) (p2,c2) -> Pervasives.compare c1 c2) pred_count
		  in
          let count = ref 0 in
            pprintf "Predicates:\n";
            liter (fun (p1,c1) -> pprintf "\t%s: %d\n" (exp_str p1) c1) preds_sorted;
            
            pprintf "Must doc size: %d\n" (StmtSet.cardinal mustDoc);
            StmtSet.iter (fun stmt -> pprintf "%s\n" (stmt_str stmt)) mustDoc;
		  let rec hierarchical_doc current_node tablevel (mustDoc : StmtSet.t) (p : ExpSet.t) (predicates : (OrderedExp.t * int) list) : change_node * StmtSet.t = begin
            if !count > 5 then exit 1;
            pprintf "hdoc %d\n" (Ref.post_incr count);
            pprintf "predicates: %d\n" (ExpSet.cardinal p);
			if not (StmtSet.is_empty mustDoc) then begin
			  let pnew_guarded_by = 
				lfilt (fun (s,_) -> StmtSet.mem s mustDoc) 
				  (ht_find pnew_ht p (fun _ -> []))
			  in
			  let dolist,mustDoc = 
				lfoldl
				  (fun (dolist,mustdoc) (stmt,_) -> 
                    dolist@[stmt],
                    StmtSet.remove stmt mustDoc)
				  ([],mustDoc) pnew_guarded_by
			  in
				pprintf "mustdoc size after do: %d\n" (StmtSet.cardinal mustDoc);
			  let pold_guarded_by =
				lfilt (fun (s,_) -> StmtSet.mem s mustDoc) (ht_find pold_ht p (fun _ -> []))
			  in
			  let insteadoflist,mustDoc = 
				lfoldl
				  (fun (insteadoflist,mustDoc) (stmt,l1) -> insteadoflist @ [stmt], StmtSet.remove stmt mustDoc)
				  ([],mustDoc) pold_guarded_by
			  in
				pprintf "mustdoc size after insteadof: %d\n" (StmtSet.cardinal mustDoc);
              let this_node = { current_node with change = (dolist,insteadoflist) } in
			    (* The problem with the return 0 example is this: there are two sets
			       of circumstances (TWO DIFFERENT LOCATIONS) in file 2 where return
			       0 can happen. Should treat them differently!  Maybe match the
			       "circumstances", notice that one of them is the same as before,
			       and document the other with a "IF foo DO bar". 
			       In the meantime, however, why isn't this working?  I think part
			       of it is this multiset thing. *)
              let children,mustDoc = (* Hmmm, children... when do I ever make a new node?*)
				lfoldl
				  (fun (children,mustDoc) (pred,c) ->
					if not (StmtSet.is_empty mustDoc) then begin
					  let tablevel' = tablevel + 1 in
                      let new_node = new_node (tablevel + 1) in
					  let predicates = List.remove_assoc pred predicates in
                      let pred' = ExpSet.add pred p in
                      let new_node = {new_node with guards = pred' } in 
                      let new_child,mustDoc = 
                        hierarchical_doc new_node tablevel' mustDoc pred' predicates
                      in
                        new_child :: children, mustDoc
					end else children,mustDoc
				  ) ([],mustDoc) predicates
              in
                {this_node with children = children}, mustDoc
			end else current_node,mustDoc
          end
		  in
		  let tree_node,mustDoc = 
            hierarchical_doc (new_node 0) 0 mustDoc (ExpSet.empty) preds_sorted in
			assert(StmtSet.is_empty mustDoc);
            StringMap.add funname tree_node concrete_map
	  ) mapping2 (StringMap.empty)


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
let update_repository revnum = 
  if !update_script = "" then 
	update_script := Printf.sprintf "%s-svn.sh" !benchmark;
  let cmd = Printf.sprintf "sh %s %d > /dev/null" !update_script revnum in
  ignore(Unix.system cmd)

let compile () = 
  if !compile_script = "" then
	compile_script := Printf.sprintf "%s-compile.sh" !benchmark;
  let cmd = Printf.sprintf "sh %s > /dev/null" !compile_script in
	ignore(Unix.system cmd)

let save_files revnum (fname,_) =
  let saved_dir = Printf.sprintf "%s_saved_files" !benchmark in
	if not (Sys.file_exists saved_dir) then begin
	  let mkdir_cmd = Printf.sprintf "mkdir %s" saved_dir in
		ignore(Unix.system mkdir_cmd)
	end;
	let filename = Filename.basename fname in 
	let filename,ext = split_ext filename in 
	let new_file_name = Printf.sprintf "%s/%s.c-%d" saved_dir filename revnum in
	  if not (Sys.file_exists new_file_name) then begin
		let original_working_dir = Sys.getcwd () in
		let newdir = Printf.sprintf "%s/%s" original_working_dir !benchmark in
		  Sys.chdir newdir;
		  let find_cmd = Printf.sprintf "find . -name \"%s.i\" -type f" filename in
			debug "find cmd: %s\n" find_cmd;
			let intermediate_file = IO.read_all (Unix.open_process_in ~autoclose:true ~cleanup:true find_cmd) in
			let split = Str.split space_nl_regexp intermediate_file in
			let file = List.hd split in
			let cp_cmd = Printf.sprintf "cp %s ../%s/%s.c-%d" file saved_dir filename revnum in
			  debug "cp cmd: %s\n" cp_cmd;
			  ignore(Unix.system cp_cmd);
			  Sys.chdir original_working_dir
	  end

let current_revnum = ref (-1)
let collect_changes (revnum) (logmsg) (url) (exclude_regexp) (diff_text_ht) : (string * change_node StringMap.t) list  =
  (* project is checked out in benchmark/ *)
  (* get diffs *)
	let input : string list = 
	  (try ignore(cmd "rm diff_out.txt") with _ -> ());
	  let svn_cmd = "svn diff -x -uw -r"^(String.of_int (revnum-1))^":"^(String.of_int revnum)^" "^url^" > diff_out.txt" in		
		debug "%s\n" svn_cmd;
	    ht_find diff_text_ht svn_cmd 
		  (fun _ -> ignore(Unix.system svn_cmd); List.of_enum (File.lines_of "diff_out.txt"))
	in
	let files = parse_files_from_diff (List.enum input) exclude_regexp in
	let files = List.of_enum (efilt (fun (fname,_) -> not (String.is_empty fname)) files) in
	let saved_dir = Printf.sprintf "%s_saved_files" !benchmark in
	let need_to_look = 
	  List.exists 
		(fun (fname,_) -> 
		let filename,ext = split_ext (Filename.basename fname) in 
		let old_fname = Printf.sprintf "%s/%s.c-%d" saved_dir filename (revnum-1) in
		let new_fname = Printf.sprintf "%s/%s.c-%d" saved_dir filename revnum in
		  not (Sys.file_exists old_fname) || not (Sys.file_exists new_fname))
		files in 
	  if need_to_look then begin
	  if !current_revnum <> (revnum-1) then begin
		update_repository (revnum-1);
		compile ();
	  end;
	  (* can I automatically apply the svn diff to the relevant files? *)
	  liter
		(fun f ->
		  save_files (revnum-1) f) files;
	  update_repository revnum;
	  compile ();
	  liter
		(fun f -> 
		  save_files revnum f) files;
	  current_revnum := revnum;
	  end;
	  pprintf "collect changes, rev %d, msg: %s\n" revnum logmsg; flush stdout;
      let res : (string * (change_node StringMap.t)) list =
      lmap 
	    (fun (fname,strs)  -> 
		  pprintf "FILE NAME: %s, revnum: %d\n" fname revnum;
		  let filename,ext = split_ext (Filename.basename fname) in 
		  let old_fname = Printf.sprintf "%s/%s.c-%d" saved_dir filename (revnum-1) in
		  let new_fname = Printf.sprintf "%s/%s.c-%d" saved_dir filename revnum in
		  let old_strs = File.lines_of old_fname in 
		  let new_strs = File.lines_of new_fname in 
		(* get a list of changed functions between the two files *)
		  let f1, f2, data_ht, f1ht, f2ht = Cdiff.tree_diff_cil old_strs new_strs in
		  let function_map : change_node StringMap.t = delta_doc f1 f2 data_ht f1ht f2ht in
		    pprintf "%d successes so far\n" (pre_incr successful);
            (fname, function_map)
	    ) files
      in res

let rec test_delta_doc files =
  let rec get_deltas files = 
    match files with
	  one :: two :: rest ->
	    debug "test_delta_doc, file1: %s, file2: %s\n" one two;
	    let file1_strs = File.lines_of one in
	    let file2_strs = File.lines_of two in
	    let f1,f2,data_ht, f1ht, f2ht = Cdiff.tree_diff_cil file1_strs file2_strs in 
	    let function_map : change_node StringMap.t = delta_doc f1 f2 data_ht f1ht f2ht in
		  pprintf "%d successes so far\n" (pre_incr successful);
          (new_diff 0 "" [(one, function_map)] "test_delta_doc") :: (get_deltas rest)
  | _ -> []
  in
  let all_diffs = get_deltas files in

  let just_changes = lmap (fun d -> d.changes) all_diffs in
    let just_changes = lmap snd (lfoldl (fun changes accum -> changes @ accum) [] just_changes) in
    let without_functions = lmap StringMap.values just_changes in 
      lfoldl 
        (fun accum changes -> (List.of_enum changes) @ accum) [] without_functions 
    
let get_diffs  ?donestart:(ds=None) ?doneend:(de=None) diff_text_ht vec_fout =
  if not (Unix.is_directory !benchmark) then begin
  (* check out directory at starting revision *)
  end;
  let save_hts () = 
  	diff_ht_counter := 0;
	pprintf "Starting save_hts...\n"; flush stdout;
	if !write_hts <> "" then begin
	  let fout = open_out_bin !write_hts in
		Marshal.output fout !benchmark;
		Marshal.output fout diff_text_ht;
		close_out fout
	end;
(*	if !templatize <> "" then begin
	  let fout = open_out_bin !templatize in
		Marshal.output fout Template.template_tbl;
		close_out fout
	end;*)
	pprintf "Done in save_hts...\n"; flush stdout;
  in
  let _ =
	debug "svn log file: %s\n" !svn_log_file_in;
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
		if !svn_log_file_out <> "" then begin
		  debug "writing svn log to %s\n" !svn_log_file_out;
		  File.write_lines !svn_log_file_out lines; 
		end;
		lines
	end
  in
  let g,grouped = 
	lfoldl
	  (fun (strgrp,strgrplst) str ->
		if string_match dashes_regexp str 0 then
		  [], (lrev strgrp) :: strgrplst
		else 
		  str::strgrp,strgrplst
	  ) ([],[]) (List.of_enum log)
  in
  let grouped = g :: grouped in
  let filtered =
	lfilt
	  (fun list ->
		(not (List.exists
				(fun str -> (string_match dashes_regexp str 0)) list))
	  ) grouped in
  let filtered = lfilt (fun lst -> (llen lst) > 0) filtered in
  let all_revs = 
	lmap
	  (fun one_enum ->
		let first = List.hd one_enum in (*match (eget one_enum) with Some(x) -> x | None -> "" in*)
		  if not (String.is_empty first) then begin
		    let rev_num = int_of_string (string_after (hd (Str.split space_regexp first)) 1) in
			let one_enum = List.tl one_enum in
			let logmsg = lfoldl (fun msg -> fun str -> msg^str) "" one_enum in
			  (rev_num,logmsg) 
		  end else (-1,"")
	  ) filtered in
	debug "three: %d\n" (llen all_revs);
	let only_fixes = 
	  lfilt
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
			    fun reg_str -> reg_str ^ "\\|" ^ accum) 
              (List.hd exclude_strs) (List.tl exclude_strs) 
		  end
		  else if (llen exclude_strs) = 1 then (List.hd exclude_strs)
		  else ""
	    in
		  Some(Str.regexp reg_str)
	  end else None
    in 
    let all_diffs = 
	  lfoldl
	    (fun diffs (revnum,logmsg) ->
		  debug "revnum: %d, logmsg: %s\n" revnum logmsg;
              (* string is filename, change_node map maps function names to the
                 base node of the change tree *)
		  let changes : (string * change_node StringMap.t) list = 
            collect_changes revnum logmsg !repos exclude_regexp diff_text_ht in
		    if (llen changes) > 0 then 
			  let diff = new_diff revnum logmsg changes !benchmark in
                diff :: diffs
            else diffs
	    ) [] only_fixes
    in
	  pprintf "made it after all_diff\n"; flush stdout;
	  save_hts();
	  pprintf "after save hts\n"; flush stdout;
	  pprintf "%d successful change parses, %d failed change parses, %d total changes\n"
	    !successful !failed (!successful + !failed);
      all_diffs
	  
let get_many_diffs ?vprint:(vprint=true) configs =
  let handleArg _ = 
    failwith "unexpected argument in benchmark config file\n"
  in
  let diff_tbl = 
    if !read_diffs <> "" then 
      let fin = open_in_bin !read_diffs in
      Marshal.input fin 
    else hcreate 10
  in      
    Enum.iter
      (fun config_file -> 
        let _ =
		  pprintf "config file: %s\n" config_file; 
		  reset_options ();
        in
		let aligned = Arg.align diffopts in
		let max_diff = ref (-1) in
		let min_diff = ref (-1) in
        let _ =
		  parse_options_in_file ~handleArg:handleArg aligned "" config_file
        in
        let diffs = 
		  if !read_diffs <> "" then begin
            let fin = open_in_bin !read_diffs in 
            let diffs = Marshal.input fin in
              close_in fin; diffs
          end else begin
			let diff_text_ht = 
			  if !read_hts <> "" then load_from_saved () 
			  else hcreate 10
			in
			  pprintf "max_diff: %d, min_diff: %d\n" !max_diff !min_diff;
			  if not (!max_diff < 0) then 
				get_diffs ~donestart:(Some(!min_diff)) ~doneend:(Some(!max_diff)) diff_text_ht stdout
			  else get_diffs diff_text_ht stdout
		  end 
        in
          hrep diff_tbl !benchmark diffs
	  ) (List.enum configs);
    if !write_diffs <> "" then begin
      let fout = open_out_bin !write_diffs in 
        Marshal.output fout diff_tbl;
        close_out fout
    end;
    let all_diffs = hfold (fun bench diffs accum -> diffs @ accum) diff_tbl [] in
    let just_changes = lmap (fun d -> d.changes) all_diffs in
    let just_changes = lmap snd (lfoldl (fun changes accum -> changes @ accum) [] just_changes) in
    let without_functions = lmap StringMap.values just_changes in 
      lfoldl (fun accum changes -> (List.of_enum changes) @ accum) [] without_functions 
      

(* this was taken from get_many_templates because it was interfering with my ability to mentally process it *)
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
						 let query_tid = int_of_string (List.nth (Str.split colon_regexp (List.nth split query_tid_location)) 1) in
						 let query_bench = List.nth split query_bench_location in
						 query_bench,query_tid
					 end else begin
					   if Str.string_match neighbor_r line 0 then begin
						 let neighbor_tid = int_of_string(List.hd (List.tl (Str.split colon_regexp (List.nth split neighbor_tid_loc)))) in
						 let neigh_bench = List.nth split neighbor_bench_loc in
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
