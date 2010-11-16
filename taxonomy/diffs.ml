open Batteries 
open Utils
open Unix
open IO
open Enum
open Str
open String
open List

let rev_num_regexp = Str.regexp "^r[0-9]+"
let space_regexp = Str.regexp_string " "
let dashes_regexp = Str.regexp_string "------------------------------------------------------------------------"
let fix_regexp = Str.regexp_string_case_fold "fix"

module Diffs =
struct

  type rev = {
	revnum : int;
	logmsg : string;
	files : string list;
  }

(*  type diff = {
	id : int;
	rinfo : rev;
	fname : string;
	syntactic : string list;
	tree : 'a list;
  }*)

  let get_revs url startrev endrev =
	let logcmd = "svn log "^url^" -r"^(of_int startrev)^":"^(of_int endrev) in
	let proc = open_process_in ?autoclose:(Some(true)) ?cleanup:(Some(true)) logcmd in
	let log1 =  IO.lines_of proc in
	let grouped =
	  Enum.group
		(fun str -> string_match dashes_regexp str 0) log1
	in
	  pprintf "two\n"; flush stdout;
	  let filtered =
		Enum.filter
		  (fun enum ->
			 (not (Enum.exists
					 (fun str -> (string_match dashes_regexp str 0)) enum))
		  ) grouped in
	  let all_revs = 
		Enum.map
		  (fun one_enum ->
			 let first = Option.get (Enum.get one_enum) in
			 let rev_num = int_of_string (string_after (hd (Str.split space_regexp first)) 1) in
			   Enum.junk one_enum;
			   let logmsg = Enum.fold (fun msg -> fun str -> msg^str) "" one_enum in
				 {revnum=rev_num;logmsg=logmsg;files=[]}
		  ) filtered in
	  let only_fixes = 
		Enum.filter
		  (fun rev ->
			 try
			   ignore(search_forward fix_regexp rev.logmsg 0); true
			 with Not_found -> false) all_revs
	  in
	  let with_files = 
		Enum.map
		  (fun rev ->
			 let diffcmd = "svn diff -r"^(of_int (rev.revnum-1))^":"^(of_int rev.revnum)^" "^url in
			 let grepcmd = "grep Index:" in 
			 let innerInput = open_process_in ?autoclose:(Some(true)) ?cleanup:(Some(true)) (diffcmd^(" | ")^grepcmd) in
			 let enumInput = IO.lines_of innerInput in
			 let files = 
			   List.of_enum
				 (Enum.map
					(fun str ->
					   let split = Str.split space_regexp str in
					   let fname = hd (tl split) in
					   let base,ext = 
						 try
						   let base = Filename.chop_extension fname in
						   let ext = String.sub fname ((String.length base)+1)
							 ((String.length fname) - ((String.length base)+1))
						   in 
							 base,ext 
						 with _ -> fname,"" in
						 match String.lowercase ext with
						 | "c" | "i" | ".h" | ".y" -> fname
						 | _ -> "") enumInput) in
			 let files = List.filter (fun str -> not (String.is_empty str)) files in
			   close_process_in innerInput;
			   {rev with files=files}
		  ) only_fixes in
	  let only_code =
		Enum.filter (fun rev -> not (List.is_empty rev.files)) with_files in
		only_code
(*		  Enum.iter
			(fun rev -> 
			   pprintf "num: %d, msg: %s, files: " rev.revnum rev.logmsg;
			   liter (fun fname -> pprintf "%s, " fname) rev.files;
			   pprintf "\n"; flush stdout
			) only_code*)
end
