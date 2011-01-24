open Batteries 
open Utils
open Ref
open Unix
open IO
open Enum
open Str
open String
open List
open Globals
open Treediff

(* stuff from options *)
let svn_log_file_in = ref ""
let svn_log_file_out = ref ""
let diff_ht_counter = ref 0
let benchmark = ref ""
let read_hts = ref false
let write_hts = ref false
let ht_file = ref ""
let exclude = ref []

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

type rev = {
  revnum : int;
  logmsg : string;
  files : (string * string list) Enum.t;
}
	
(* diff type and initialization *)
type change = {
  changeid : int;
  fname : string ;
  oldf : string ;
  newf : string ;
  syntactic : string ;
  tree : Difftypes.standardized_change ;
  alpha : Difftypes.standardized_change ;
}

type full_diff = {
  fullid : int;
  rev_num : int;
  msg : string;
  changes : change list ;
}

let diffid = ref 0
let changeid = ref 0
  
let new_diff revnum msg changes = 
  {fullid = (post_incr diffid);rev_num=revnum;msg=msg; changes = changes }

let new_change fname syntactic oldf newf tree alpha = 
  {changeid = (post_incr changeid);fname=fname;oldf=oldf;newf=newf;syntactic=syntactic;tree=tree; alpha=alpha}


(* these refs are mostly here for accounting and debugging purposes *)
let successful = ref 0
let failed = ref 0
let old_fout = if !debug_bl then open_out "alloldsfs.txt" else stdnull
let new_fout = if !debug_bl then open_out "allnewsfs.txt" else stdnull

let diff_text_ht = ref (hcreate 10)
let change_ht = ref (hcreate 10)
let diff_ht = ref (hcreate 10)

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
  pprintf "Parsing files from diff\n\n"; flush stdout;
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
	pprintf "collect changes, rev %d\n" rev.revnum; flush stdout;
	let input = 
	  if hmem !diff_text_ht (rev.revnum-1,rev.revnum) then
		hfind !diff_text_ht (rev.revnum-1,rev.revnum)
	  else begin
		let diffcmd = "svn diff -x -uw -r"^(of_int (rev.revnum-1))^":"^(of_int rev.revnum)^" "^url in
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
				  nwrite old_fout old_file_str;
				  nwrite new_fout new_file_str;
				  nwrite old_fout "\nSEPSEPSEPSEP\n";
				  nwrite new_fout "\nSEPSEPSEPSEP\n";
				  flush old_fout;
				  flush new_fout;
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

let get_diffs logfile_in logfile_out url startrev endrev =
  let hts_out = 
	if !write_hts
	then Pervasives.open_out_bin !ht_file 
	else Pervasives.open_out_bin "/dev/null" in
  let save_hts () = 
	seek_out hts_out 0;
	let out_channel = output_channel ~cleanup:false hts_out in(* open_out_bin !ht_file in*)
	  Marshal.output out_channel !benchmark;
	  Marshal.output out_channel !diffid;
	  Marshal.output out_channel !changeid;
	  Marshal.output out_channel !diff_ht;
	  Marshal.output out_channel !change_ht;
	  Marshal.output out_channel !diff_text_ht;
	  close_out out_channel;
	  Pervasives.flush hts_out
  in
  let log = 
	if logfile_in <> "" then
	  File.lines_of logfile_in
	else begin
	  let logcmd = 
		match startrev,endrev with
		  Some(startrev),Some(endrev) ->
			"svn log "^url^" -r"^(of_int startrev)^":"^(of_int endrev)
		| _,_ ->  "svn log "^url
	  in
	  let proc = open_process_in ?autoclose:(Some(true)) ?cleanup:(Some(true)) logcmd in
	  let lines = IO.lines_of proc in 
		if logfile_out <> "" then begin
		  File.write_lines logfile_out lines
		end;
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
		  else {revnum=(-1);logmsg="fix";files=(Enum.empty())} (* hack for now *)
	  ) filtered in
  let only_fixes = 
	efilt
	  (fun rev ->
		try
		  ignore(search_forward fix_regexp rev.logmsg 0); 
		  match startrev,endrev with
			Some(r1),Some(r2) -> rev.revnum >= r1 && rev.revnum <= r2
		  | _ -> true
		with Not_found -> false) all_revs
  in
  let all_changes = 
	emap (fun rev ->
	  let changes = lflat (List.of_enum (collect_changes rev url)) in
		liter (fun c -> hadd !change_ht c.changeid c) changes;
		let diff = new_diff rev.revnum rev.logmsg changes in
		  if (!diff_ht_counter == 10) then 
			begin 
			  save_hts (); 
			  diff_ht_counter := 0;
			end else incr diff_ht_counter;
		  hadd !diff_ht diff.fullid diff; diff
	(* fixme: group changes by file somehow? *)
	) only_fixes in
	(* is this too frequent? *)
	let rec convert_to_set enum set =
	  try
		let ele = Option.get (Enum.get enum) in
		let set' = Set.add ele set in
		  convert_to_set enum set'
	  with Not_found -> set 
	in
	  let set = convert_to_set all_changes (Set.empty) in
	  let set = Set.map
		  (fun diff -> diff.fullid)  set in 
(*	  pprintf "before save hts\n"; flush stdout;*)
	  save_hts();
	  Pervasives.close_out hts_out;
	  pprintf "%d successful, %d failed, %d total\n" !successful !failed (!successful + !failed); flush stdout;
	  set
(*		  pprintf "Change id: %d, rev_num: %d, log_msg: %s.  Changes: \n"
			diff.fullid diff.rev_num diff.msg; flush stdout;
		  liter (fun change -> 
			pprintf "\t Change id: %d fname: %s, tree length: %d;\n " change.changeid change.fname (llen change.tree); flush stdout) diff.changes;
		  diff.fullid) set*)
		
let load_from_saved () = 
  pprintf "loading hts\n"; flush stdout;
  let in_channel = open_in_bin !ht_file in
  let bench = Marshal.input in_channel in
	assert(bench = !benchmark);
	diffid := Marshal.input in_channel;
	changeid := Marshal.input in_channel;
	diff_ht := Marshal.input in_channel;
	change_ht := Marshal.input in_channel;
	diff_text_ht := Marshal.input in_channel;
	close_in in_channel

let sanity_check_hts () =
  pprintf "benchmark: %s\n" !benchmark;
  pprintf "max diffid: %d,max changeid: %d\n" !diffid !changeid;
  pprintf "diff_ht: \n";
  hiter
	(fun k ->
	  fun v ->
		pprintf "key: %d," k) !diff_ht; 
  pprintf "change_ht: \n";
  hiter
	(fun k ->
	  fun v ->
		pprintf "key: %d," k) !change_ht; 
  pprintf "diff_text_ht: \n";
  hiter
	(fun k ->
	  fun v ->
		pprintf "key: %d,%d" (fst k) (snd k)) !diff_text_ht; 
  flush stdout
	
