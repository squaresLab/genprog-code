open Batteries 
open Utils
open Ref
open Unix
open IO
open Enum
open Str
open List
open Globals
open Treediff

(* options *)
let benchmark = ref ""
let svn_log_file_in = ref ""
let svn_log_file_out = ref ""
let read_hts = ref false
let write_hts = ref false
let ht_file = ref ""
let exclude = ref []
let repos = ref ""
let rstart = ref None
let rend = ref None

let devnull = Pervasives.open_out_bin "/dev/null"
let configs = ref []

let fullsave = ref ""
let skip_svn = ref false
let wipe_hts = ref false

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
  "--load", Arg.Set read_hts, "\t file from which to read stored basic diff information\n";
  "--save", Arg.Set write_hts, "\t save diff information to file";
  "--htfile", Arg.Set_string ht_file, "\t file for reading/storing diff information.";
  "--skip-svn", Arg.Set skip_svn, "\t Just load from saved ht, don't bother with svn\n";
  "--wipe-hts", Arg.Set wipe_hts, "\t load from saved if you can, but wipe diff_hts.  Useful for when you want to reprocess everything but don't want to call svn a billion times if you don't have to."

]

type rev = {
  revnum : int;
  logmsg : string;
  files : (string * string list) Enum.t;
}
	
(* diff type and initialization *)
type change = {
  mutable changeid : int;
  fname : string ;
  oldf : string ;
  newf : string ;
  syntactic : string ;
  tree : Difftypes.standardized_change ;
  alpha : Difftypes.standardized_change ;
  cbench : string
}

type full_diff = {
  mutable fullid : int;
  rev_num : int;
  msg : string;
  mutable changes : change list ;
  dbench : string;
}

let diff_ht_counter = ref 0
let diffid = ref 0
let changeid = ref 0

let diff_text_ht = ref (hcreate 10)
let change_ht = ref (hcreate 10)
let diff_ht = ref (hcreate 10)
  
let new_diff revnum msg changes = 
  {fullid = (post_incr diffid);rev_num=revnum;msg=msg; changes = changes; dbench = !benchmark }

let new_change fname syntactic oldf newf tree alpha = 
  {changeid = (post_incr changeid);fname=fname;oldf=oldf;newf=newf;syntactic=syntactic;tree=tree; alpha=alpha; cbench = !benchmark}

let reset_options () =
  benchmark := "";
  svn_log_file_in := "";
  svn_log_file_out := "";
  read_hts := false;
  write_hts := false;
  ht_file := "";
  exclude := [];
  repos := "";
  rstart := None;
  rend := None


let sanity_check_hts () =
  pprintf "benchmark: %s\n" !benchmark;
  pprintf "max diffid: %d,max changeid: %d\n" !diffid !changeid;
  pprintf "diff_ht: \n";
  hiter
	(fun k ->
	  fun v ->
		pprintf "key: %d, num_changes: %d\n" k (llen v.changes)) !diff_ht; 
  pprintf "change_ht: \n";
  hiter
	(fun k ->
	  fun v ->
		pprintf "key: %d," k) !change_ht; 
(*  pprintf "diff_text_ht: \n";
  hiter
	(fun k ->
	  fun v ->
		pprintf "key: %d,%d" (fst k) (snd k)) !diff_text_ht; *)
  flush stdout
	

let load_from_saved () = 
  let in_channel = open_in_bin !ht_file in
	try
	  let bench = Marshal.input in_channel in
		if bench <> !benchmark then pprintf "WARNING: bench (%s) and benchmark (%s) do not match\n" bench !benchmark; 
		diffid := Marshal.input in_channel;
		changeid := Marshal.input in_channel;
		diff_ht := Marshal.input in_channel;
		change_ht := Marshal.input in_channel;
		diff_text_ht := Marshal.input in_channel;
	with _ -> 
      begin
		pprintf "WARNING: load_from_saved failed.  Resetting everything!\n"; flush stdout;
		diffid := 0; 
		changeid := 0;
		diff_ht := (hcreate 10);
		change_ht := (hcreate 10); 
		diff_text_ht := (hcreate 10);
      end;
      close_in in_channel;
      if !wipe_hts then begin
		change_ht := (hcreate 10);
		diff_ht := (hcreate 10);
		diffid := 0;
		changeid := 0
      end
    

let check_comments strs = 
  lfoldl
	(fun (all_comment, unbalanced_beginnings,unbalanced_ends) ->
	   fun (diffstr : string) ->
		 let matches_comment_line = Str.string_match star_regexp diffstr 0 in
		 let matches_end_comment = try ignore(Str.search_forward end_comment_regexp diffstr 0); true with Not_found -> false in
		 let matches_start_comment = try ignore(Str.search_forward start_comment_regexp diffstr 0); true with Not_found -> false in
		   if matches_end_comment && matches_start_comment then 
			 (all_comment, unbalanced_beginnings, unbalanced_ends)
		   else 
			 begin
			   let unbalanced_beginnings,unbalanced_ends = 
				 if matches_end_comment && unbalanced_beginnings > 0 
				 then (unbalanced_beginnings - 1,unbalanced_ends) 
				 else if matches_end_comment then unbalanced_beginnings, unbalanced_ends + 1 
				 else  unbalanced_beginnings, unbalanced_ends
			   in
			   let unbalanced_beginnings = if matches_start_comment then unbalanced_beginnings + 1 else unbalanced_beginnings in 
				 all_comment && matches_comment_line, unbalanced_beginnings,unbalanced_ends
			 end)
	(true, 0,0) strs


(* these refs are mostly here for accounting and debugging purposes *)
let successful = ref 0
let failed = ref 0
let old_fout = if !debug_bl then open_out "alloldsfs.txt" else stdnull
let new_fout = if !debug_bl then open_out "allnewsfs.txt" else stdnull

let separate_syntactic_diff syntactic_lst = 
  let (frst_syntactic,syntactic_strs),(frst_old,old_file_strs),(frst_new,new_file_strs) = 
	lfoldl
	  (fun ((current_syntactic,syntactic_strs),(current_old,oldfs),(current_new,newfs)) ->
		fun str ->
		  if Str.string_match at_regexp str 0 then ([],current_syntactic::syntactic_strs),([],current_old::oldfs),([],current_new::newfs)
		  else
			begin
			  let syntax_pair = current_syntactic @ [str],syntactic_strs in
				if Str.string_match plus_regexp str 0 then syntax_pair,(current_old,oldfs),(current_new @ [String.lchop str],newfs)
				else if Str.string_match minus_regexp str 0 then syntax_pair,(current_old @ [(String.lchop str)],oldfs),(current_new,newfs)
				else syntax_pair,(current_old @ [str],oldfs),(current_new @ [str],newfs)
			end
	  ) (([],[]),([],[]),([],[])) syntactic_lst
  in
	frst_syntactic::syntactic_strs,frst_old::old_file_strs,frst_new::new_file_strs

let strip_property_changes old_lst new_lst = 
  lfoldl2
	(fun (oldf',newf') -> 
	  fun (oldf : string list) ->
		fun (newf : string list) -> 
		  let prop_regexp = Str.regexp_string "Property changes on:" in 
			if List.exists (fun str -> try ignore(Str.search_forward prop_regexp str 0); true with Not_found -> false) oldf then begin
			  let oldi,newi = 
				fst (List.findi (fun index -> fun str -> try ignore(Str.search_forward prop_regexp str 0); true with Not_found -> false) oldf),
				fst (List.findi (fun index -> fun str -> try ignore(Str.search_forward prop_regexp str 0); true with Not_found -> false) newf)
			  in
				(List.take oldi oldf) :: oldf', (List.take newi newf) :: newf'
			end else oldf :: oldf',newf :: newf'
	) ([],[]) old_lst new_lst

let fix_partial_comments old_lst new_lst = 
  lfoldl2
	(fun (oldfs',newfs') -> 
	  fun (oldf : string list) ->
		fun (newf : string list) -> 
			(* first, see if this change also references property changes *)
			(* next, deal with starting or ending in the middle of a comment *)
		  let all_comment_old,unbalanced_beginnings_old,unbalanced_ends_old = check_comments oldf in
		  let all_comment_new, unbalanced_beginnings_new,unbalanced_ends_new = check_comments newf in

		  let oldf'' = 
			if unbalanced_beginnings_old > 0 || all_comment_old then oldf @ ["*/"] else oldf in
		  let newf'' = 
			if unbalanced_beginnings_new > 0 || all_comment_new then newf @ ["*/"] else newf in
		  let oldf''' = 
			if unbalanced_ends_old > 0 || all_comment_old then "/*" :: oldf'' else oldf'' in
		  let newf''' = 
			if unbalanced_ends_new > 0 || all_comment_new then "/*" :: newf'' else newf'' in
			oldf'''::oldfs',newf'''::newfs'
	) ([],[]) old_lst new_lst

let put_strings_together syntax_lst old_lst new_lst = 
  let foldstrs strs = lfoldl (fun accum -> fun str -> accum^"\n"^str) "" strs in
  let old_and_new = 
	lmap2
	  (fun oldstrs ->
		fun newstrs -> 
		  foldstrs oldstrs, 
		  foldstrs newstrs) 
	  old_lst new_lst 
  in
	lmap2
	  (fun syntax ->
		fun (old,news) -> 
		  foldstrs syntax,old,news) syntax_lst old_and_new

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
					  | "c" | "i" | ".h" | ".y" -> fname'
					  | _ -> "" in
					((fname,strs)::finfos),(fname',[])
			end 
		  else 
			if (String.is_empty fname) ||
			  (string_match junk str 0) then 
			  (finfos,(fname,strs))
			else (finfos,(fname,str::strs))
	  ) ([],("",[])) input
  in
  let finfos = (lastname,strs)::finfos in
	efilt (fun (str,_) -> not (String.is_empty str)) (List.enum finfos) (* FIXME: just use the enum thing in the line above *)

(* collect changes is a helper function for get_diffs *)

	
let collect_changes ?(parse=true) rev url =
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
	pprintf "collect changes, rev %d, msg: %s\n" rev.revnum rev.logmsg; flush stdout;
	let input = 
	  if hmem !diff_text_ht (rev.revnum-1,rev.revnum) then
		hfind !diff_text_ht (rev.revnum-1,rev.revnum)
	  else begin
		let diffcmd = "svn diff -x -uw -r"^(String.of_int (rev.revnum-1))^":"^(String.of_int rev.revnum)^" "^url in
		let innerInput = open_process_in ?autoclose:(Some(true)) ?cleanup:(Some(true)) diffcmd in
		let enum_ret = IO.lines_of innerInput in
		let aslst = List.of_enum enum_ret in 
		  hadd !diff_text_ht (rev.revnum-1,rev.revnum) aslst;
		  (try ignore(close_process_in innerInput) with _ -> begin
		    pprintf "WARNING: diffcmd failed on close process in: %s\n" diffcmd; flush stdout
		   end);
		  aslst
	  end
	in
	let files = parse_files_from_diff (List.enum input) exclude_regexp in
	let files = efilt (fun (fname,changes) -> not (String.is_empty fname)) files in
	  emap 
		(fun (fname,changes) -> 
		  let syntactic = List.rev changes in
			(* Debug output *)
		    if !debug_bl then begin
			pprintf "filename is: %s syntactic: \n" fname;
			liter (fun x -> pprintf "\t%s\n" x) syntactic;
			pprintf "end syntactic\n"; flush stdout
		    end;
			(* process the syntactic diff: separate into before and after files *)
			let syntax_strs,old_strs,new_strs = separate_syntactic_diff syntactic in
			(* strip property change info *)
			let old_strs,new_strs = strip_property_changes old_strs new_strs in
			(* deal with partial comments *)
			let old_strs,new_strs = fix_partial_comments old_strs new_strs in
			(* remove empties *) 

			(* zip up each list of strings corresponding to a change into one long string *)
			let as_strings : (string * string * string) list = put_strings_together syntax_strs old_strs new_strs in
			let without_empties = lfilt (fun (syntax,oldf,newf) -> syntax <> "" && oldf <> "" && newf <> "") as_strings in 
			  (* parse each string, call treediff to construct actual diff *)
			  lmap
				(fun (syntax_str,old_file_str,new_file_str) ->
				  (* Debugging output *)
				   if !debug_bl then begin
(*				  nwrite old_fout old_file_str;
				  nwrite new_fout new_file_str;
				  nwrite old_fout "\nSEPSEPSEPSEP\n";
				  nwrite new_fout "\nSEPSEPSEPSEP\n";
				  flush old_fout;
				  flush new_fout;*)
				  (* end debugging output *)
				  (* debugging output *)
				  
				  pprintf "Syntax_str: %s\n" syntax_str;
				  pprintf "oldf: %s\n" old_file_str;
				  pprintf "newf: %s\n" new_file_str;
				  flush stdout
				   end;
				  let tree,alpha_tree = 
					if parse then begin
					  try
						(* end debugging output *)
						let old_file_tree,new_file_tree =
						  fst (Diffparse.parse_from_string old_file_str),
						  fst (Diffparse.parse_from_string new_file_str)
						in
						let diff,alpha_diff = Treediff.tree_diff_cabs old_file_tree new_file_tree (Printf.sprintf "%d" !diffid) in
						  incr successful; pprintf "%d successes so far\n" !successful; flush stdout;
						  diff,alpha_diff
					  with e -> begin
						pprintf "Exception in diff processing: %s\n" (Printexc.to_string e); flush stdout;
						incr failed;
						pprintf "%d failures so far\n" !failed; flush stdout;
						[],[]
					  end
					end else [],[]
				  in
					new_change fname syntax_str old_file_str new_file_str tree alpha_tree
				) without_empties
		) files

let get_diffs config_file = 
  if !debug_bl then (pprintf "Debug is on!\n"; flush stdout);
  let hts_out = 
	if !write_hts then 
	  Some(Pervasives.open_out_bin !ht_file) 
	else None in
  let save_hts () = 
	match hts_out with
	  Some(hts_out) ->
		seek_out hts_out 0;
		let out_channel = output_channel ~cleanup:false hts_out in
		  Marshal.output out_channel !benchmark;
		  Marshal.output out_channel !diffid;
		  Marshal.output out_channel !changeid;
		  Marshal.output out_channel !diff_ht;
		  Marshal.output out_channel !change_ht;
		  Marshal.output out_channel !diff_text_ht;
		  close_out out_channel;
		  Pervasives.flush hts_out
	| None -> ()
  in
	let log = 
	  if !svn_log_file_in <> "" then
		File.lines_of !svn_log_file_in
	  else begin
		let logcmd = 
		  match !rstart,!rend with
			Some(startrev),Some(endrev) ->
			  "svn log "^ !repos ^" -r"^(String.of_int startrev)^":"^(String.of_int endrev)
		  | _,_ ->  "svn log "^ !repos
		in
		let proc = open_process_in ?autoclose:(Some(true)) ?cleanup:(Some(true)) logcmd in
		let lines = IO.lines_of proc in 
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
				  {revnum=rev_num;logmsg=logmsg;files=(Enum.empty())}
			end
			else {revnum=(-1);logmsg="fix";files=(Enum.empty())} (* hack for now *) (* FIX THIS *)
		) filtered in
	let only_fixes = 
	  efilt
		(fun rev ->
		  try
			ignore(search_forward fix_regexp rev.logmsg 0); 
			match !rstart, !rend with
			  Some(r1),Some(r2) -> rev.revnum >= r1 && rev.revnum <= r2
			| _ -> rev.revnum > -1
		  with Not_found -> false) all_revs
	in
	let all_diffs = 
	  emap (fun rev ->
		let changes = lflat (List.of_enum (collect_changes rev !repos)) in
		  pprintf "For revision %d, %d changes\n" rev.revnum (llen changes); flush stdout;
		  new_diff rev.revnum rev.logmsg changes) only_fixes in
	(* fixme: group changes by file somehow? *)
	let diffs_with_changes =
	  efilt
	    (fun diff -> (llen diff.changes) > 0) all_diffs in
	let made_diffs = 
	  emap
	    (fun diff ->
	       liter (fun c -> hadd !change_ht c.changeid c) diff.changes;
	       hadd !diff_ht diff.fullid diff;
		 if (!diff_ht_counter == 10) then 
		   begin 
		     save_hts (); 
		     diff_ht_counter := 0;
		   end else incr diff_ht_counter;
		 diff) diffs_with_changes in

	let rec convert_to_set enum set =
	  try
		let ele = Option.get (Enum.get enum) in
		let set' = Set.add ele set in
		  convert_to_set enum set'
	  with Not_found -> set 
	in
	let set = convert_to_set made_diffs (Set.empty) in
	  save_hts();
	  (match hts_out with
		Some(hts_out) -> Pervasives.close_out hts_out
	  | None -> ());
	  pprintf "%d successful change parses, %d failed change parses, %d total changes, %d total diffs\n" !successful !failed (!successful + !failed) (Set.cardinal set); flush stdout;
	  set

let big_diff_ht = ref (hcreate 100)
let big_change_ht = ref (hcreate 100)
let big_diff_id = ref 0
let big_change_id = ref 0 



let full_load_from_file filename =
  let filename = 
	if !interactive then begin
	  pprintf "Default BigFile is %s.  Is that OK?" filename;
	  flush stdout;
	  let user_response = Pervasives.read_line() in
		match (String.get (lowercase user_response) 0) with
		  'y' -> filename
		| _ -> 
		  pprintf "OK, what would you prefer?\n";
		  Pervasives.read_line ()
	end
	else filename
  in
	pprintf "OK, trying to load BigFile %s...\n" filename;
  let print_digest (time,benches) = 
	let localtime = Unix.localtime time in 
	  pprintf "Full file saved at %d:%d:%d on %d/%d/%d\n" 
		localtime.tm_hour localtime.tm_min localtime.tm_sec
		localtime.tm_mon localtime.tm_mday localtime.tm_year;
	  pprintf "Includes benches: ";
	  liter (fun bench -> pprintf "%s, " bench) benches;
	  pprintf "\n\n"; flush stdout
  in
  let fin = open_in_bin filename in
	try
	  let digest = Marshal.input fin in
		if !interactive then begin
		  pprintf "Found a BigFile %s with digest " filename;
		  print_digest digest;
		  pprintf "\n\nShould I try to load?\n";
		  let user_response = Pervasives.read_line () in
			match (String.get (lowercase user_response) 0) with
			  'y' -> 
				big_diff_id := Marshal.input fin;
				big_change_id := Marshal.input fin;
				big_diff_ht := Marshal.input fin;
				big_change_ht := Marshal.input fin;
				close_in fin; 
				pprintf "BigFile %s loaded\n" filename; flush stdout;
				Set.of_enum (Hashtbl.keys !big_diff_ht), true
			| _ -> 
			  pprintf "Not loading BigFile %s; you'll have to sit through the combine.\n" filename; 
			  close_in fin; 
			  (Set.empty), false
		end else begin
		  big_diff_id := Marshal.input fin;
		  big_change_id := Marshal.input fin;
		  big_diff_ht := Marshal.input fin;
		  big_change_ht := Marshal.input fin;
		  close_in fin; 		  
		  Set.of_enum (Hashtbl.keys !big_diff_ht),true
		end
	with _ -> 
	  begin
		(if !interactive then 
			pprintf "Failed to load BigFile %s; you'll have to sit through the combine, you poor thing.\n" filename);
		(try close_in fin with _ -> ()); 
		(Set.empty),false
	  end

let get_many_diffs ?(ray="") configs hts_out =
  let full_save bench_list =
	match hts_out with
	  Some(hts_out) ->
		seek_out hts_out 0;
		let out_channel = output_channel ~cleanup:false hts_out in 
		let time = Unix.time () in
		  Marshal.output out_channel (time,bench_list);
		  Marshal.output out_channel !big_diff_id;
		  Marshal.output out_channel !big_change_id;
		  Marshal.output out_channel !big_diff_ht;
		  Marshal.output out_channel !big_change_ht;
		  close_out out_channel;
		  Pervasives.flush hts_out
	| None -> ()
  in
  let handleArg _ = 
	failwith "unexpected argument in benchmark config file\n"
  in
  let renumber_diff diff = 
	let changes' =
	  lmap 
		(fun change -> 
		  change.changeid <- (post_incr big_change_id);
		  hadd !big_change_ht change.changeid change;
		  change
		) (Utils.copy diff.changes)
	in
	let diff' = Utils.copy diff in
	  diff'.fullid <- (post_incr big_diff_id);
	  diff'.changes <- changes';
	  hadd !big_diff_ht diff'.fullid diff';
	  diff'.fullid
  in
	if (llen configs) > 0 then begin
	  lfoldl
		(fun (bigset,benches) ->
		  fun config_file -> 
			reset_options ();
			let aligned = Arg.align diffopts in
			  parse_options_in_file ~handleArg:handleArg aligned "" config_file;
			  if !read_hts then load_from_saved ();
			  let set = 
				if not !skip_svn then 
				  Set.map renumber_diff (get_diffs config_file)
				else 
				  hfold
					(fun diffid ->
					  fun diff ->
						fun set ->
						  Set.add (renumber_diff diff) set
					) !diff_ht (Set.empty)
			  in
				full_save benches;
				Set.union set bigset,!benchmark::benches
		) (Set.empty,[]) configs 
	end else begin
	  pprintf "Assuming Ray mode.\n"; flush stdout;
	  
	  (* try loading the full file\n *)
	  let hts = 
			emap
			  (fun s -> s,"/home/claire/taxonomy/main/test_data/"^s^"_ht.bin")
			  (List.enum ["apache";"fbc";"ffdshow";"gcc";"gnucash";"gnutella";
			   "gs";"handbrake";"lighty";"php";"subversion";"ultradefrag";
			   "warzone2100";"wireshark"] )
	  in
		efold
		  (fun (bigset,benches) ->
			fun (bench,htf) -> 
			  reset_options ();
			  benchmark := bench;
			  ht_file := htf;
			  load_from_saved ();
			  let set = 
				hfold
				  (fun diffid ->
					fun diff ->
					  fun set ->
						Set.add (renumber_diff diff) set
				  ) !diff_ht (Set.empty)
			  in
				full_save benches;
				Set.union set bigset,bench::benches
		  ) (Set.empty,[]) hts
	end		
