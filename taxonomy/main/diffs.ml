open Batteries 
open Utils
open Ref
open Unix
open IO
open Enum
open Str
open List
open Globals
open Difftypes
open Cabs
open Treediff

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

let _ =
  options := 
    !options @
    [
	  "--configs", Arg.Rest (fun s -> configs := s :: !configs), 
	  "\t input config files for each benchmark. Processed separately in the same way as regular command-line arguments.";
	  "--fullsave", Arg.Set_string fullsave, "\t file to save composed hashtable\n";
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
	"--skip-svn", Arg.Set skip_svn, "\t don't bother getting info from svn"
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
		let diff_text_ht = Marshal.input in_channel in
		  diff_text_ht
    with _ -> 
      begin
		pprintf "WARNING: load_from_saved failed.  Resetting everything!\n"; flush stdout;
		hcreate 10
      end
  in
    close_in in_channel; diff_text_ht

let cmd (cmd) : string list = 
  let innerInput = open_process_in ?autoclose:(Some(true)) ?cleanup:(Some(true)) cmd in
  let enum_ret = List.of_enum (IO.lines_of innerInput) in
	(try ignore(close_process_in innerInput) with _ -> begin
	  pprintf "WARNING: diffcmd failed on close process in: %s\n" cmd; flush stdout
	end); enum_ret

(* these refs are mostly here for accounting and debugging purposes *)
let successful = ref 0
let failed = ref 0

let compose strs =
  efold
	(fun strs ->
	  fun str ->
		strs^"\n"^str) "" strs

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
  if revnum == 3095 then [] else begin
	let svn_gcc fname revnum = 
	  let filter strs = efilt (fun str -> not (any_match include_regexp str)) (List.enum strs) in
	  let tempfile = Printf.sprintf "temp_%s.c" !benchmark in
	  let gcc_cmd = "gcc -E "^tempfile in
	  let svn_cmd = "svn cat -r"^(String.of_int revnum)^" "^url^"/"^fname in
	  let enum_ret = filter (ht_find diff_text_ht svn_cmd (fun _ -> cmd svn_cmd)) in
		File.write_lines tempfile enum_ret;
		List.enum (cmd gcc_cmd)
	in
	pprintf "collect changes, rev %d, msg: %s\n" revnum logmsg; flush stdout;
	let input : string list = 
	  let svn_cmd = "svn diff -x -uw -r"^(String.of_int (revnum-1))^":"^(String.of_int revnum)^" "^url in
	    ht_find diff_text_ht svn_cmd (fun _ -> cmd svn_cmd)
	in
	let files = parse_files_from_diff (List.enum input) exclude_regexp in
	let files = efilt (fun (fname,_) -> not (String.is_empty fname)) files in
	  List.of_enum
		(emap
	       (fun (fname,strs) -> 
			 pprintf "FILE NAME: %s, revnum: %d\n" fname revnum;
			 let old_strs = compose (svn_gcc fname (revnum - 1)) in 
			 let new_strs = compose (svn_gcc fname revnum) in 
			   try
				 let diff_res = Treediff.tree_diff_cabs old_strs new_strs in
				   pprintf "%d successes so far\n" (pre_incr successful);
				   let non_empty = lfilt (fun (defo,edits,_) -> match defo with Some(d) -> not (List.is_empty edits) | None -> false) diff_res in
				   let non_opt = lmap (fun (defo,c,t) -> match defo with Some(d) -> d,c,t | None -> failwith "Impossible match") non_empty in
					 lmap (fun (def,edits,info) -> new_change fname def edits info strs) non_opt
			   with e -> begin
				 pprintf "Exception in diff processing: %s\n" (Printexc.to_string e); flush stdout;
				 incr failed;
				 pprintf "%d failures so far\n." !failed; flush stdout;
				 []
			   end
		   ) files)
  end

let get_diffs_and_templates  ?donestart:(ds=None) ?doneend:(de=None) diff_text_ht vec_fout =
  let save_hts () = 
	diff_ht_counter := 0;
	pprintf "Starting save_hts...\n"; flush stdout;
	if !write_hts <> "" then begin
	  let fout = open_out_bin !write_hts in
		Marshal.output fout !benchmark;
		Marshal.output fout diff_text_ht;
		close_out fout
	end;
	if !templatize <> "" then begin
	  let fout = open_out_bin !templatize in
		Marshal.output fout Template.init_template_tbl;
		close_out fout
	end;
	pprintf "Done in save_hts...\n"; flush stdout;
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
	  let lines = List.enum (cmd logcmd) in
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
			   let diff = new_diff revnum logmsg changes !benchmark in
			   let templates = lflat (lmap (fun change -> Template.diff_to_templates diff change change.tree ("",[nd(Globals([change.tree]))])) diff.changes) in
			   let vectors = lmap (fun context -> Vectors.template_to_vectors context) templates in
				 liter (Vectors.print_vectors vec_fout) vectors;
				 if (!diff_ht_counter == 20) then (save_hts (); flush vec_fout)
				 else incr diff_ht_counter
		     end) only_fixes
	 with Not_found -> ());
	pprintf "made it after all_diff\n"; flush stdout;
	save_hts();
	pprintf "after save hts\n"; flush stdout;
	pprintf "%d successful change parses, %d failed change parses, %d total changes\n"
	  !successful !failed (!successful + !failed)
		
let get_many_templates configs =
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
		let vec_fout = File.open_out !vec_file in
		  parse_options_in_file ~handleArg:handleArg aligned "" config_file;
		  if !read_temps then begin
			let fin = open_in_bin !templatize in
			let res1 = Marshal.input fin in 
			  close_in fin; 
			  hiter 
				(fun k ->
				  fun template ->
					if k > !Difftypes.template_id then
					  Difftypes.template_id := k;
					if (!max_diff < 0) || (template.diff.rev_num > !max_diff) then
					  max_diff := template.diff.rev_num;
					if (!min_diff < 0) || (template.diff.rev_num < !min_diff) then
					  min_diff := template.diff.rev_num;
					hadd Template.init_template_tbl k template;
					let vectors = Vectors.template_to_vectors template in 
					  Vectors.print_vectors vec_fout vectors 
				) res1
		  end;
		  if not !skip_svn then begin
			let diff_text_ht = 
			  if !read_hts <> "" then load_from_saved () 
			  else hcreate 10
			in
			if not (!max_diff < 0) then begin
			  pprintf "min_diff: %d, max_diff: %d\n" !min_diff !max_diff;
			  get_diffs_and_templates ~donestart:(Some(!min_diff)) ~doneend:(Some(!max_diff)) diff_text_ht vec_fout
			end else
			  get_diffs_and_templates diff_text_ht vec_fout
		  end;
		  close_out vec_fout
	  ) (List.enum configs)
