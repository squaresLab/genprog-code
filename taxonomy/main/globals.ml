open Batteries
open Enum
open Utils
open Unix
open IO

let debug_bl = ref false

let eiter = Enum.iter
let egroup = Enum.group
let eexists = Enum.exists
let emap = Enum.map
let efilt = Enum.filter
let eget = Enum.get
let ejunk = Enum.junk
let efold = Enum.fold
let eflat = Enum.flatten

let rev_num_regexp = Str.regexp "^r[0-9]+"
let include_regexp = Str.regexp "^#[ \t]*include"
let dashes_regexp = Str.regexp_string "------------------------------------------------------------------------"
let fix_regexp = Str.regexp_case_fold "fixes\\|fix\\|bug\\|bugnum\\|crash\\|failed\\|failure\\|repair\\|\"Bug number\"\\|#"
let index_regexp = Str.regexp_string "Index: "
let junk = Str.regexp "\\(^===================================================================\\)\\|\\(^\\+\\+\\+\\)\\|\\(^---\\)"
let at_regexp = Str.regexp_string "@@"
let plus_regexp = Str.regexp_string "+"
let colon_regexp = Str.regexp_string ":"
let comma_regexp = Str.regexp_string ","
let minus_regexp = Str.regexp_string "-" 
let space_regexp = Str.regexp "[ \t]+" 
let star_regexp = Str.regexp "[ \t]+\\*" 
let start_comment_regexp = Str.regexp "[ \t]*/\\*"
let end_comment_regexp = Str.regexp "[ \t]*\\*/"

let usageMsg = "Fix taxonomy clustering.  Right now assumes svn repository.\n"

let options = ref [
  "--debug", Arg.Set debug_bl, "\t debug output.";
]

let handleArg = (fun str -> Utils.pprintf "unknown option %s\n" str)

let parse_options_in_file ?handleArg:(h=handleArg) options usageMsg (file : string) =
  let args = ref [ Sys.argv.(0) ] in 
    let fin = open_in file in 
    (try while true do
      let line = input_line fin in
      let words = Str.bounded_split space_regexp line 2 in 
      args := !args @ words 
    done with _ -> close_in fin) ;
    Arg.current := 0 ; 
    Arg.parse_argv (Array.of_list !args) 
      (Arg.align options) 
	  h usageMsg 
	
let compose strs = lfoldl (fun strs -> fun str -> strs^"\n"^str) "" strs

let cmd (cmd) : string list = 
  let innerInput = open_process_in ?autoclose:(Some(true)) ?cleanup:(Some(true)) cmd in
  let enum_ret = List.of_enum (IO.lines_of innerInput) in
	(try ignore(close_process_in innerInput) with _ -> begin
	  pprintf "WARNING: diffcmd failed on close process in: %s\n" cmd; flush stdout
	end); enum_ret

let file_process efile tempname = 
  ignore(cmd ("rm "^tempname));
  let filter strs = lfilt (fun str -> not (any_match include_regexp str)) strs in
  let liner = Str.regexp_string "__LINE__" in
  let replace = 
	lmap (fun str -> Str.global_replace liner "_lineno_" str)
  in
  let filtered = replace (filter (List.of_enum efile)) in 
	File.write_lines tempname (List.enum filtered);
	let gcc_cmd = "gcc -E "^tempname in
	  cmd gcc_cmd
