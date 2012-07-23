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

(* options *)
let benchmark = ref ""
let svn_log_file_in = ref ""
let svn_log_file_out = ref ""
let read_svn_dir = ref ""
let exclude = ref []
let repos = ref ""
let rstart = ref None
let rend = ref None

let devnull = Pervasives.open_out_bin "/dev/null"
let configs = ref []
let read_diffs = ref ""

let diff_out_count = 10

let _ =
  options := 
    !options @
    [
	  "--configs", Arg.Rest (fun s -> configs := s :: !configs), 
	  "\t input config files for each benchmark. Processed separately in the same way as regular command-line arguments.";
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
    "--load-svn", Arg.Set_string read_svn_dir, "\t X directory from which to read stored svn information\n";
    "--load-diffs", Arg.Set_string read_diffs, "\t read diffs from here";
  ]

(* these refs are mostly here for accounting and debugging purposes *)
let successful = ref 0
let failed = ref 0

module ExpHashType = struct
  type t = Cil.exp
  let equal e1 e2 = 
    let e1_str = exp_str e1 in 
    let e2_str = exp_str e2 in 
      (Pervasives.compare e1_str e2_str) = 0

  let hash e1 = 
    let e1_str = exp_str e1 in 
      Hashtbl.hash e1_str
end

module ExpSetHashType = struct
  type t = ExpSet.t
  let equal e1 e2 =
    let e1_str = 
      ExpSet.fold (fun exp accum -> (exp_str exp) ^ accum) e1 "" in
    let e2_str = 
      ExpSet.fold (fun exp accum -> (exp_str exp) ^ accum) e2 "" in
      (Pervasives.compare e1_str e2_str) = 0

  let hash e1 = 
    let e1_str = 
      ExpSet.fold (fun exp accum -> (exp_str exp) ^ accum) e1 "" in
      Hashtbl.hash e1_str
end

module ExpHash = Hashtbl.Make(ExpHashType)
module ExpSetHash = Hashtbl.Make(ExpSetHashType)

let delta_doc name changed_functions =
  let f1s = lmap (fun (a,b,c) -> a,b) changed_functions in
  let f2s = lmap (fun (a,b,c) -> a,c) changed_functions in
  let mapping1 = Tigen.path_generation f1s in
  let mapping2 = Tigen.path_generation f2s in
    (* the mapping from tigen is a map from function name to a stmtmap, where
       the statement map maps statements to assumption sets *)
	StringMap.fold
	  (fun funname pnew acc ->
		let pold = StringMap.find funname mapping1 in
        (* pold and pnew map statements to assumption sets *)
		let domain_pold = StmtSet.of_enum (StmtMap.keys pold) in
		let domain_pnew = StmtSet.of_enum (StmtMap.keys pnew) in
		let inserted = StmtSet.diff domain_pnew domain_pold in
		let deleted = StmtSet.diff domain_pold domain_pnew in
		let intersection = StmtSet.inter domain_pnew domain_pold in
		let changed = 
		  StmtSet.fold
			(fun stmt changed ->
              (* OK, predicates1 and predicates2 are now *lists* of assumptions *)
			  let predicates1,_ = StmtMap.find stmt pold in
			  let predicates2,_ = StmtMap.find stmt pnew in
                if ExpSetSet.subset predicates1 predicates2 &&
                  ExpSetSet.subset predicates2 predicates1 then
                  changed
                else 
                  StmtSet.add stmt changed
			) intersection StmtSet.empty
		in
		let mustDoc = StmtSet.union inserted (StmtSet.union deleted changed) in
		let pred_count_ht = ExpSetHash.create 10 in
		let pnew_ht = ExpSetHash.create 10 in
		let pold_ht = ExpSetHash.create 10 in
		  StmtSet.iter
			(fun stmt ->
              (* we do a lot of things for both old and new, these shorten the code *)
              let count_preds predsetset = 
	            ExpSetSet.iter (fun expset -> 
                  let c = if ExpSetHash.mem pred_count_ht expset then
                      ExpSetHash.find pred_count_ht expset
                    else 0 in
                    ExpSetHash.replace pred_count_ht expset (c + 1))
                  predsetset
              in
              let get_stmt pmap = 
                if StmtMap.mem stmt pmap then
                  StmtMap.find stmt pmap else 
                  ExpSetSet.empty,builtinLoc
              in
              let get_start_lst ht preds = 
                if ExpSetHash.mem ht preds then
                  ExpSetHash.find ht preds else []
              in
			  let predicates1,loc1 = get_stmt pold in
			  let predicates2,loc2 = get_stmt pnew in 
              let _ =
                  (* pred_count_ht maps expression sets to frequency counts *)
                count_preds predicates1; count_preds predicates2
              in
                ExpSetSet.iter
                  (fun predicates1 ->
			        let start_lst_old = get_start_lst pold_ht predicates1 in
				      ExpSetHash.replace pold_ht predicates1 
                        ((stmt,loc1) :: start_lst_old)) predicates1;
          ExpSetSet.iter
            (fun predicates2 ->
			  let start_lst_new = get_start_lst pnew_ht predicates2 in
                  (* mapping each predicate back to a list of statement,location
                     pairs that this set of predicates have guarded *)
				ExpSetHash.replace pnew_ht predicates2 
                  ((stmt,loc2) :: start_lst_new)) predicates2;
	  ) mustDoc;
    let sort_ht ht = 
      ExpSetHash.fold
		(fun k v pold_ht ->
		  let sorted_stmts = 
			(List.sort ~cmp:(fun (_,l1) (_,l2) -> compareLoc l1 l2)) v
		  in
            (* so k here is a set of predicates; the stmt list is sorted
               by location*)
			ExpSetHash.add pold_ht k sorted_stmts; pold_ht
		) ht (ExpSetHash.create 10) in
	let pold_ht = sort_ht pold_ht in 
	let pnew_ht = sort_ht pnew_ht in
    let pold_keys = ExpSetSet.of_enum (ExpSetHash.keys pold_ht) in
    let pnew_keys = ExpSetSet.of_enum (ExpSetHash.keys pnew_ht) in 
    let all_exp_sets = ExpSetSet.union pold_keys pnew_keys in 
    (* pred_count is a list of (exp,count) pairs, exp -> frequency *)
	let pred_count = List.of_enum (ExpSetHash.enum pred_count_ht) in 
    (* list of exp,counts sorted by count *)
	let pred_sets_sorted = 
	  List.sort ~cmp:(fun (p1,c1) (p2,c2) -> Pervasives.compare c2 c1) pred_count
	in
    let mustDoc = ref mustDoc in 
    let nodes = ref [] in
	let rec hierarchical_doc tablevel (p : ExpSet.t) =
      let rec flatten stmt = 
        match stmt.skind with
          Block(b) -> lfoldl (fun accum stmt -> accum @ (flatten stmt)) [] b.bstmts 
        | _ -> [stmt]
      in
      let guarded_by ht = 
        let stmt_lst = 
          if ExpSetHash.mem ht p 
          then ExpSetHash.find ht p else []
        in
		  lfilt (fun (s,_) -> StmtSet.mem s !mustDoc) stmt_lst
      in
      let lst_guarded_by lst = 
		lfoldl
		  (fun dolist (stmt,_) ->
            mustDoc := StmtSet.remove stmt !mustDoc;
            let _,_,f = stmt in
            dolist@(flatten f))
		  [] lst
	  in
		if not (StmtSet.is_empty !mustDoc) then begin
		  let pnew_guarded_by = guarded_by pnew_ht in
		  let dolist = lst_guarded_by pnew_guarded_by in
		  let pold_guarded_by = guarded_by pold_ht in 
          let insteadoflist = lst_guarded_by pold_guarded_by in
            (match dolist,insteadoflist with
              [],[] -> ()
            | _,_ ->
              let this_node = new_node name funname dolist insteadoflist p in
                nodes := this_node :: !nodes);
		end
	in
      liter (fun (predset,_) -> hierarchical_doc "" predset) pred_sets_sorted;
	  assert(StmtSet.is_empty !mustDoc);
      !nodes @ acc
	  ) mapping2 []


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
			else begin (* pprintf "%s\n" str;*) (finfos,(fname,str::strs)) end
	  ) ([],("",[])) input
  in
  let finfos = (lastname,strs)::finfos in
	efilt (fun (str,_) -> not (String.is_empty str)) (List.enum finfos)

let load_saved_diffs file_in = 
  try
    let fin = open_in_bin file_in in 
    let bench = Marshal.input fin in
      assert(!benchmark = bench);
      let min_revision = Marshal.input fin in
      let max_revision = Marshal.input fin in
      let diff_ht = Marshal.input fin in
        close_in fin; min_revision,max_revision,diff_ht
    with _ -> -1,-1,hcreate 10
	 
let write_saved_diffs file_out min max diff_ht =
  let fout = open_out_bin file_out in 
    Marshal.output fout !benchmark;
    Marshal.output fout min;
    Marshal.output fout max;
    Marshal.output fout diff_ht;
    close_out fout

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
			let intermediate_file = IO.read_all (Unix.open_process_in ~autoclose:true ~cleanup:true find_cmd) in
			let split = Str.split space_nl_regexp intermediate_file in
			let file = List.hd split in
			let cp_cmd = Printf.sprintf "cp %s ../%s/%s.c-%d" file saved_dir filename revnum in
			  debug "cp cmd: %s\n" cp_cmd;
			  ignore(Unix.system cp_cmd);
			  Sys.chdir original_working_dir
	  end

let current_revnum = ref (-1)
let collect_changes (revnum) (logmsg) (url) (exclude_regexp) =
  (* project is checked out in benchmark/ *)
  (* get diffs *)
  let input = 
    let diff_fin_name = Printf.sprintf "%s/%s-%d-%d.diff" !read_svn_dir !benchmark (revnum-1) revnum in
	let svn_cmd = "svn diff -x -uw -r"^(String.of_int (revnum-1))^":"^(String.of_int revnum)^" "^url^" > "^diff_fin_name in 
      if not (Sys.file_exists diff_fin_name) then
        (ignore(Unix.system svn_cmd); debug "foo\n");
      File.lines_of diff_fin_name
  in
  let files = parse_files_from_diff input exclude_regexp in
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
      lfoldl 
	    (fun acc (fname,strs) -> 
		  pprintf "FILE NAME: %s, revnum: %d\n" fname revnum;
		  let filename,ext = split_ext (Filename.basename fname) in 
		  let old_fname = Printf.sprintf "%s/%s.c-%d" saved_dir filename (revnum-1) in
		  let new_fname = Printf.sprintf "%s/%s.c-%d" saved_dir filename revnum in
		  (* get a list of changed functions between the two files *)
            try 
              debug "pre-tree-diff, %g live MB\n" (live_mb ());
		      let changed_functions = Cdiff.tree_diff_cil old_fname new_fname in
              debug "post-tree-diff, pre delta_doc, %g live MB\n" (live_mb ());
		      let changes : change_node list = delta_doc fname changed_functions in
              debug "post delta_doc, %g live MB\n" (live_mb ());
		        pprintf "%d successes so far\n" (pre_incr successful);
                changes @ acc
            with e -> (debug "Warning: error in cdiff: %s\n" (Printexc.to_string e); acc)
	    ) [] files
    
let get_diffs ?donestart:(ds=None) ?doneend:(de=None) diff_ht =
(*  if not (Unix.is_directory !benchmark) then begin*)
  (* check out directory at starting revision *)
(*  end;*)
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
	let only_fixes = 
	  lfilt
		(fun (revnum,logmsg) ->
		  try
			ignore(search_forward fix_regexp logmsg 0); 
            match ds,de with
              Some(r1), Some(r2) -> revnum >= r1 && revnum <= r2
            | Some(r1), None -> revnum >= r1
            | None, Some(r2) -> revnum <= r2
            | _,_ -> true
		  with Not_found -> false) all_revs
	in
    let exclude_regexp = 
	  if not (List.is_empty !exclude) then begin
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
    let max_rev = ref (-1) in
    let min_rev = ref (-1) in
    let _ =
	  liter
	    (fun (revnum,logmsg) ->
		  let changes =
            collect_changes revnum logmsg !repos exclude_regexp 
          in
          let _ = 
            if !diff_ht_counter > diff_out_count 
            then (diff_ht_counter := 0; write_saved_diffs (Printf.sprintf "%s.diffs.ht" !benchmark) !min_rev !max_rev diff_ht)
            else incr diff_ht_counter
          in
            debug "revnum: %d, sizeof diff_ht: %g MB, live_mb: %g MB\n" revnum (debug_size_in_mb diff_ht) (live_mb ());
		    if not (List.is_empty changes) then begin
			  let diff = new_diff revnum logmsg changes !benchmark in
                hadd diff_ht diff.fullid diff;
                if revnum < !min_rev || !min_rev == (-1) then min_rev := revnum;
                if revnum > !max_rev || !max_rev == (-1) then max_rev := revnum
            end
	    ) only_fixes
    in
	  pprintf "%d successful change parses, %d failed change parses, %d total changes\n"
	    !successful !failed (!successful + !failed);
      !min_rev,!max_rev,diff_ht


let reset_options () =
  benchmark := "";
  svn_log_file_in := "";
  svn_log_file_out := "";
  read_svn_dir := "";
  exclude := [];
  repos := "";
  rstart := None;
  rend := None

let get_many_diffs configs =
  let handleArg _ = 
    failwith "unexpected argument in benchmark config file\n"
  in
  let all_diffs : Difftypes.full_diff list =
    Enum.fold
      (fun (diffs : Difftypes.full_diff list) config_file -> 
        (* process the config file for this benchmark *)
        let aligned =
		  pprintf "config file: %s\n" config_file; 
		  reset_options ();
          Arg.align diffopts
        in
		let max_diff = ref 0 in
		let min_diff = ref (-1) in
        let _ =
		  parse_options_in_file ~handleArg:handleArg aligned "" config_file
        in
          (match !read_diffs with
            "" -> read_diffs := Printf.sprintf "%s.diffs.ht" !benchmark
          | _ -> ());
          (match !read_svn_dir with
            "" -> read_svn_dir := Printf.sprintf "%s-svn" !benchmark
          | _ -> ());
          (match !rstart with
            Some(x) -> min_diff := x
          | _ -> ());
          (match !rend with
            Some(x) -> max_diff := x
          | _ -> ());
        let min,max,diff_ht = load_saved_diffs !read_diffs in
        let read_more_diffs = 
          Hashtbl.is_empty diff_ht ||
            !min_diff < min ||
            !max_diff == -1 ||
            !max_diff > max
        in
        let min,max,diff_ht = 
          if read_more_diffs then begin
			  pprintf "max_diff: %d, min_diff: %d\n" !max_diff !min_diff;
              if !max_diff > 0 && !min_diff > 0 then
				get_diffs ~donestart:(Some(!min_diff)) ~doneend:(Some(!max_diff)) diff_ht
			  else if !max_diff > 0 then
				get_diffs ~doneend:(Some(!max_diff)) diff_ht
              else if !min_diff > 0 then
				get_diffs ~donestart:(Some(!min_diff)) diff_ht
              else
                get_diffs diff_ht
		  end else !min_diff,!max_diff,diff_ht
        in
          write_saved_diffs (Printf.sprintf "%s.diffs.ht" !benchmark) min max diff_ht;
          hfold (fun diffid diff diffs -> diff :: diffs) diff_ht diffs 
	  ) [] (List.enum configs) 
  in
  let just_changes = lmap (fun d -> d.changes) all_diffs in
    lfoldl (fun changes accum -> changes @ accum) [] just_changes

(* TEST *)
let rec test_delta_doc files =
  let rec get_indiv_deltas files = 
    match files with
	  one :: two :: rest ->
	    debug "test_delta_doc, file1: %s, file2: %s\n" one two;
	    let changed_functions = Cdiff.tree_diff_cil one two in
	    let changes = delta_doc one changed_functions in
          (new_diff 0 "" changes "test_delta_doc") :: (get_indiv_deltas rest)
  | _ -> []
  in
  let get_batch_deltas file =
    let all_pairs = 
      emap (fun str -> 
        let split = Str.split comma_regexp str in
          List.hd split, List.hd (List.tl split)) (File.lines_of file)
    in
      efold
        (fun accum (one,two) ->
	    debug "test_delta_doc, file1: %s, file2: %s\n" one two;
	    let changed_functions = Cdiff.tree_diff_cil one two in
	    let changes = delta_doc one changed_functions in 
		  pprintf "%d successes so far\n" (pre_incr successful);
          (new_diff 0 "" changes "test_delta_doc") :: accum) [] all_pairs
  in
  let all_diffs = 
    if (llen files) > 1 then get_indiv_deltas files 
    else get_batch_deltas (List.hd files)
  in
    debug "eleven\n";
  let just_changes = lmap (fun d -> d.changes) all_diffs in
  lfoldl (fun changes accum -> changes @ accum) [] just_changes
