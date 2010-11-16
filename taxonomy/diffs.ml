open Batteries 
open Utils
open Unix
open IO
open Enum
open Str
open String
open List
open Globals

let rev_num_regexp = Str.regexp "^r[0-9]+"
let space_regexp = Str.regexp_string " "
let dashes_regexp = Str.regexp_string "------------------------------------------------------------------------"
let fix_regexp = Str.regexp_string_case_fold "fix"
let index_regexp = Str.regexp_string "Index: "
let eqs =  Str.regexp_string "==================================================================="
let pluses = Str.regexp_string "+++"
let minuses = Str.regexp_string "---"

module Diffs =
struct

  type rev = {
	revnum : int;
	logmsg : string;
	files : (string * string list) Enum.t;
  }

  type diff = {
	id : int;
	rinfo : int;
	fname : string;
	msg : string;
	syntactic : string list;
  }

  let collect_diffs rev url =
	let diffcmd = "svn diff -r"^(of_int (rev.revnum-1))^":"^(of_int rev.revnum)^" "^url in
	let innerInput = open_process_in ?autoclose:(Some(true)) ?cleanup:(Some(true)) diffcmd in
	let enumInput =  IO.lines_of innerInput in
	let finfos,(lastname,strs) =
	  efold
		(fun (finfos,(fname,strs)) ->
		   fun str ->
			 if (string_match index_regexp str 0) then 
			   begin
				 let split = Str.split space_regexp str in
				 let fname' = hd (tl split) in
				 let ext = 
				   try
					 let base = Filename.chop_extension fname' in
					   String.sub fname' ((String.length base)+1)
						 ((String.length fname') - ((String.length base)+1))
				   with _ -> "" in
				 let fname' =
				   match String.lowercase ext with
				   | "c" | "i" | ".h" | ".y" -> fname'
				   | _ -> "" in
				   ((fname,strs)::finfos),(fname',[])
			   end 
			 else 
			   if (String.is_empty fname) ||
				 (string_match eqs str 0) ||
				 (string_match pluses str 0) ||
				 (string_match minuses str 0) then
				   (finfos,(fname,strs))
			   else (finfos,(fname,str::strs))
		) ([],("",[])) enumInput
	in
	let finfos = List.enum ((lastname,strs)::finfos) in
	let files = efilt (fun (str,_) -> not (String.is_empty str)) finfos in
	  ignore(close_process_in innerInput);
	  emap 
		(fun (str,diff) ->
		   {id=0;rinfo=rev.revnum;fname=str;msg=rev.logmsg;syntactic=(List.rev diff)}
		) files 

  let get_revs url startrev endrev =
	let logcmd = "svn log "^url^" -r"^(of_int startrev)^":"^(of_int endrev) in
	let proc = open_process_in ?autoclose:(Some(true)) ?cleanup:(Some(true)) logcmd in
	let log =  IO.lines_of proc in
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
		   let rev_num = int_of_string (string_after (hd (Str.split space_regexp first)) 1) in
			 ejunk one_enum;
			 let logmsg = efold (fun msg -> fun str -> msg^str) "" one_enum in
			   {revnum=rev_num;logmsg=logmsg;files=(Enum.empty())}
		) filtered in
	let only_fixes = 
	  efilt
		(fun rev ->
		   try
			 ignore(search_forward fix_regexp rev.logmsg 0); true
		   with Not_found -> false) all_revs
	in
	let with_files = 
	  eflat (emap (fun rev -> collect_diffs rev url) only_fixes) 
	in
	let finale = efilt (fun diff -> not (String.is_empty diff.fname)) with_files in
	  pprintf "before final iter\n"; flush stdout;
	  eiter
		(fun diff ->
		   pprintf "Diff id: %d; revnum: %d; fname: %s, msg: %s, syntactic: "
			 diff.id diff.rinfo diff.fname diff.msg;
		   liter (fun str -> pprintf "%s\n" str) diff.syntactic; flush stdout
		) finale

end
