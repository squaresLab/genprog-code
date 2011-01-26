open Batteries
open Enum

let debug_bl = ref false
let interactive = ref false

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
let space_regexp = Str.regexp_string " "
let comma_regexp = Str.regexp_string ","
let dashes_regexp = Str.regexp_string "------------------------------------------------------------------------"
let fix_regexp = Str.regexp_case_fold "fixes\\|fix\\|bug\\|bugnum\\|crash\\|failed\\|failure\\|repair\\|\"Bug number\"\\|#"
let index_regexp = Str.regexp_string "Index: "
let junk = Str.regexp "\\(^===================================================================\\)\\|\\(^\\+\\+\\+\\)\\|\\(^---\\)"
let at_regexp = Str.regexp_string "@@"
let plus_regexp = Str.regexp_string "+"
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
	
