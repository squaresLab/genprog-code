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
let svn_log_file = ref ""
let read_svn_dir = ref ""
let exclude = ref []
let repos = ref ""
let repos_type = ref "svn"
let num_to_process = ref (-1)

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
    "--num", Arg.Set_int num_to_process, "\t number of changes to process. Default: all";
    "--bench", Arg.Set_string benchmark, "\t benchmark name, recommended for sanity checking.";
    "--exclude",Arg.String (fun x -> exclude := x :: !exclude), "\t paths/names of files to exclude from diffs";
    "--logfile", Arg.Set_string svn_log_file, "\t file containing the svn log\n";
    "--repos", Arg.Set_string repos, "\t URL of the repository.";
    "--load-svn", Arg.Set_string read_svn_dir, "\t X directory from which to read stored svn information\n";
    "--load-diffs", Arg.Set_string read_diffs, "\t read diffs from here";
    "--repos-type", Arg.Set_string repos_type, "\t repos type: svn, git, hg.  Default: svn";
  ]

(* these refs are mostly here for accounting and debugging purposes *)
let successful = ref 0
let failed = ref 0

let delta_doc fname1 fname2 changed_functions =
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
            let _,f = stmt in
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
              let this_node = new_node fname1 fname2 funname dolist insteadoflist p in
                nodes := this_node :: !nodes);
        end
    in
      liter (fun (predset,_) -> hierarchical_doc "" predset) pred_sets_sorted;
      assert(StmtSet.is_empty !mustDoc);
      !nodes @ acc
      ) mapping2 []


let parse_files_from_diff input exclude_regexp (current_rev : string) = 
  let files_changed = 
    let grep_cmd = 
      match !repos_type with
        "svn" ->
          "grep \"Index:\" "^input^" | cut -d ' ' -f2 > "^(!benchmark)^"_scratch/grep_out.txt" 
      | "hg" -> "^diff -r "^input^" | cut -d ' ' -f 6 > "^(!benchmark)^"_scratch/grep_out.txt" 
      | "git" -> failwith "git parse files not implemented"
    in
      ignore(Unix.system grep_cmd);
      File.lines_of (!benchmark^"_scratch/grep_out.txt")
  in
  let files_changed = 
    lfilt (fun fname -> 
      let matches_exclusions = 
        try 
          match exclude_regexp with
            Some(exclude_regexp) -> 
              ignore(Str.search_forward exclude_regexp fname 0); true 
          | None -> false with Not_found -> false in
      let ext = 
        try
          let base = Filename.chop_extension fname in
            String.sub fname ((String.length base)+1)
              ((String.length fname) - ((String.length base)+1))
        with _ -> "" in
        not matches_exclusions &&
          (match String.lowercase ext with
          | "c" | ".h" | ".y" ->  true
          | _ -> false)
    ) (List.of_enum files_changed)
  in
    lmap
      (fun filename ->
        let grep_cmd = 
          match !repos_type with
            "svn" ->
              "grep \"\-\-\- "^filename^"\" "^input^" | cut -d ' ' -f3 | cut -d ')' -f1 > "^(!benchmark)^"_scratch/grep.out" 
          | "hg" ->
            "grep \"^diff \-r.* "^filename^"\"| cut -d ' ' -f 3 > "^(!benchmark)^"_scratch/grep.out"
          | "git" -> "get revs grep not implemented for git"
        in
        let prev_rev = 
          ignore(Unix.system(grep_cmd));
          File.lines_of (!benchmark^"_scratch/grep.out")
        in
        let prev = List.of_enum prev_rev in
          filename,List.hd prev,current_rev)
      files_changed

let load_saved_diffs file_in = 
  try
    debug "loading saved diffs, reading: %s\n" file_in;
    let fin = open_in_bin file_in in 
    let bench = Marshal.input fin in
      debug "read %s, expecting %s\n" bench !benchmark;
      assert(!benchmark = bench);
      let diff_ht = Marshal.input fin in
        debug "hlen: %d\n" (hlen diff_ht);
        close_in fin; diff_ht
    with _ -> hcreate 10
     
let write_saved_diffs file_out  diff_ht =
  debug "writing saved diffs to %s entries: %d\n" file_out (hlen diff_ht);
  let fout = open_out_bin file_out in 
    Marshal.output fout !benchmark;
    Marshal.output fout diff_ht;
    close_out fout

(* collect changes is a helper function for get_diffs *)
let update_repository revnum = 
  if !update_script = "" then 
    update_script := Printf.sprintf "%s-svn.sh" !benchmark;
  let cmd = Printf.sprintf "sh %s %s > /dev/null" !update_script revnum in
  ignore(Unix.system cmd)

let compile () = 
  if !compile_script = "" then
    compile_script := Printf.sprintf "%s-compile.sh" !benchmark;
  let cmd = Printf.sprintf "sh %s >& /dev/null" !compile_script in
    ignore(Unix.system cmd)

let save_files revnum fname =
  let saved_dir = Printf.sprintf "%s_saved_files" !benchmark in
    if not (Sys.file_exists saved_dir) then begin
      let mkdir_cmd = Printf.sprintf "mkdir %s" saved_dir in
        ignore(Unix.system mkdir_cmd)
    end;
    let filename = Filename.basename fname in 
    let filename,ext = split_ext filename in 
    let new_file_name = Printf.sprintf "%s/%s.c-%s" saved_dir filename revnum in
      if not (Sys.file_exists new_file_name) then begin
        let original_working_dir = Sys.getcwd () in
        let newdir = Printf.sprintf "%s/%s" original_working_dir !benchmark in
          Sys.chdir newdir;
          let find_cmd = Printf.sprintf "find . -name \"%s.i\" -type f" filename in
            let intermediate_file = IO.read_all (Unix.open_process_in ~autoclose:true ~cleanup:true find_cmd) in
            let split = Str.split space_nl_regexp intermediate_file in
              if (llen split) > 0 then begin
            let file = List.hd split in
            let cp_cmd = Printf.sprintf "cp %s ../%s/%s.c-%s" file saved_dir filename revnum in
              ignore(Unix.system cp_cmd);
              end;
              Sys.chdir original_working_dir
      end

let current_revnum = ref ""
let collect_changes (this_rev : string) (logmsg) (url) (exclude_regexp) =
  (* project is checked out in benchmark/ *)
  (* get diffs *)
  let saved_dir = Printf.sprintf "%s_saved_files" !benchmark in
  let input = 
    let diff_fin_name = Printf.sprintf "%s/%s-%s.diff" !read_svn_dir !benchmark this_rev in 
    let svn_cmd = 
      match !repos_type with
        "svn" -> "svn diff -x -uw -c "^this_rev^" "^url^" > "^diff_fin_name 
      | "hg" -> "hg diff -w -B -b -c "^this_rev^" "^(!benchmark)^" > "^diff_fin_name
      | "git" -> failwith "diff not implemented on git"
    in
      if not (Sys.file_exists diff_fin_name) then
        ignore(Unix.system svn_cmd); 
      diff_fin_name
  in
  let files : (string * string * string) list  = parse_files_from_diff input exclude_regexp this_rev in
  let need_to_look = 
    List.exists 
      (fun (fname,prev_rev,this_rev) -> 
        let filename,ext = split_ext (Filename.basename fname) in 
        let old_fname = Printf.sprintf "%s/%s.c-%s" saved_dir filename prev_rev in
        let new_fname = Printf.sprintf "%s/%s.c-%s" saved_dir filename this_rev in
          not (Sys.file_exists old_fname) || not (Sys.file_exists new_fname))
      files in 

    if need_to_look then begin
      if !current_revnum <> this_rev then begin
        update_repository this_rev;
        compile ();
      end;
      liter
        (fun (f,_,this_rev) ->
          save_files this_rev f) files;
      let _,prev_rev,_ = List.hd files in 
      update_repository prev_rev;
      compile ();
      liter
        (fun (f,prev_rev,_) -> 
          save_files prev_rev f) files;
      current_revnum := prev_rev;
      end;
      pprintf "collect changes, rev %s, msg: %s\n" this_rev logmsg; flush stdout;
      lfoldl 
        (fun acc (fname,prev_rev,this_rev) -> 
          pprintf "FILE NAME: %s, revnum: %s\n" fname this_rev;
          let filename,ext = split_ext (Filename.basename fname) in 
          let old_fname = Printf.sprintf "%s/%s.c-%s" saved_dir filename prev_rev in
          let new_fname = Printf.sprintf "%s/%s.c-%s" saved_dir filename this_rev in
          (* get a list of changed functions between the two files *)
            try 
              let changed_functions = Cdiff.tree_diff_cil old_fname new_fname in
              let changes : change_node list = delta_doc old_fname new_fname changed_functions in
              let changes' = lfoldl (fun acc change -> summarize_change change :: acc) [] changes in
                debug "changes: %d\n" (llen changes);
              let changes' = 
                lfoldl 
                  (fun acc change -> 
                    let change' = summarize_change change in
                      if (llen change'.add) > 0 ||
                        (llen change'.delete) > 0 then
                        change' :: acc
                      else acc) [] changes' in
		        debug "%d successes so far\n" (pre_incr successful);
                changes' @ acc
            with e -> (debug "Warning: error in cdiff: %s\n" (Printexc.to_string e);
                       acc)
        ) [] files

let git_log_cmd () = failwith "git log cmd not implemented"

let get_log () =
  if (Sys.file_exists !svn_log_file) then
    File.lines_of !svn_log_file
  else begin
    let logcmd = !repos_type^" log "^(!repos) in
    let lines = cmd logcmd in
      if !svn_log_file <> "" then 
        File.write_lines !svn_log_file lines; 
      lines
  end

let get_revs () = 
  let grep_cmd = 
    match !repos_type with
      "svn" ->
        "egrep \"^r[0-9]+ \|\" "^(!svn_log_file)^" | cut -d ' ' -f1 | cut -d 'r' -f2 > "^(!benchmark)^"_scratch/grep_out.txt" 
    | "hg" ->
      "egrep \"^changeset: \" "^(!svn_log_file)^" | cut -d ' ' -f4 | cut -d ':' -f1 >"^(!benchmark)^"_scratch/grep_out.txt"
    | "git" -> failwith "git get revs not implemented"
  in
    debug "grep cmd: %s\n" grep_cmd;
    ignore(Unix.system grep_cmd);
    File.lines_of (!benchmark^"_scratch/grep_out.txt")

let rec group_revs_and_logmsgs revs log = 
  match !repos_type with
    "svn" -> begin
      let rec inner_log_group log =
        match log with
          line :: rev_line  :: lines ->
            if any_match dashes_regexp line then [], lines
            else
              begin 
                let this_log,rest = inner_log_group (rev_line :: lines) in
                  if (String.length line) > 2 then
                    line :: this_log, rest
                  else this_log,rest
              end
        | [line] -> 
          if (String.length line) > 2 then [line], []
          else [],[]
        | [] -> [],[]
      in
      let rec inner_group revs log = 
        match revs with
          r :: rest_revs ->
            let this_log,rest_log = inner_log_group log in
              (r,lfoldl (fun msg -> fun str -> msg^str) "" this_log) :: (inner_group rest_revs rest_log)
        | _ -> []
      in
      let _,rest_log = inner_log_group log in
        inner_group revs rest_log
    end
  | "hg" ->
    let summary_regexp = Str.regexp_string "summary: " in
    let log = 
      lfilt (fun line -> any_match summary_regexp line) log 
    in
    let rec pair_up revs log = 
      match revs,log with
        rev1:: revrest, line1 :: logrest ->
          let summary = String.sub line1 13 ((String.length line1) - 13) in
            (rev1,summary) :: pair_up revrest logrest
      | _,_ -> []
    in
      pair_up revs log
  | "git" -> failwith "git revs and log not implemented" 

let get_diffs (diff_ht : (string, full_diff) Hashtbl.t) =
(*  if not (Unix.is_directory !benchmark) then begin*)
  (* check out directory at starting revision *)
(*  end;*)
  let log = List.of_enum (get_log ()) in
  let revs = List.of_enum (get_revs ()) in
  let revs_and_logs = group_revs_and_logmsgs revs log in
  let only_fixes = 
	lfilt
	  (fun (revnum,logmsg) -> 
        any_match fix_regexp logmsg && 
          not (hmem diff_ht revnum))
      revs_and_logs
  in
    debug "length only fixes: %d, num: %d\n" (llen only_fixes) !num_to_process;
  let only_as_many =
    if !num_to_process < 0 || (llen only_fixes) < !num_to_process then 
      only_fixes
    else first_nth only_fixes !num_to_process
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
  let _ =
	liter
	  (fun (revnum,logmsg) ->
		let changes =
          collect_changes revnum logmsg !repos exclude_regexp 
        in
          let _ = 
            if !diff_ht_counter > diff_out_count 
            then (diff_ht_counter := 0; write_saved_diffs !read_diffs diff_ht)
            else incr diff_ht_counter
          in
            if not (List.is_empty changes) then begin
              let diff = new_diff revnum logmsg changes !benchmark in
                hadd diff_ht revnum diff;
            end;
        ) only_as_many
    in
      write_saved_diffs !read_diffs diff_ht;
      pprintf "%d successful change parses, %d failed change parses, %d total changes\n"
        !successful !failed (!successful + !failed)

let reset_options () =
  benchmark := "";
  svn_log_file := "";
  read_svn_dir := "";
  exclude := [];
  repos := "";
  num_to_process := -1

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
        let _ =
          parse_options_in_file ~handleArg:handleArg aligned "" config_file
        in
          (match !read_diffs with
            "" -> read_diffs := Printf.sprintf "%s.diffs.ht" !benchmark
          | _ -> ());
          (match !read_svn_dir with
            "" -> read_svn_dir := Printf.sprintf "%s-svn" !benchmark
          | _ -> ());
          if not (Sys.file_exists !read_svn_dir) then
            ignore(Unix.system ("mkdir "^(!read_svn_dir)));
        let diff_ht = load_saved_diffs !read_diffs in
          if not (Sys.file_exists (!benchmark^"_scratch")) then
            ignore(Unix.system ("mkdir "^(!benchmark)^"_scratch"));
          get_diffs diff_ht; (* hashtables are stateful *)
          hfold (fun diffid diff diffs -> diff :: diffs) diff_ht diffs 
      ) [] (List.enum configs) 
  in
  let just_changes = lmap (fun d -> lmap (fun c -> (d.rev_num, d.msg, c)) d.changes) all_diffs in
    lfoldl (fun changes accum -> changes @ accum) [] just_changes

(* TEST *)
let rec test_delta_doc files =
  let rec get_indiv_deltas files = 
    match files with
      one :: two :: rest ->
        debug "test_delta_doc, file1: %s, file2: %s\n" one two;
        let changed_functions = Cdiff.tree_diff_cil one two in
        let changes = delta_doc one two changed_functions in
          (new_diff "foo" "" changes "test_delta_doc") :: (get_indiv_deltas rest)
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
        let changes = delta_doc one two changed_functions in 
          pprintf "%d successes so far\n" (pre_incr successful);
          (new_diff "foo" "" changes "test_delta_doc") :: accum) [] all_pairs
  in
  let all_diffs = 
    if (llen files) > 1 then get_indiv_deltas files 
    else get_batch_deltas (List.hd files)
  in
  let just_changes = lmap (fun d -> lmap (fun c -> (d.rev_num, d.msg, c)) d.changes) all_diffs in
  lfoldl (fun changes accum -> changes @ accum) [] just_changes
