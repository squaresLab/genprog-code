open Batteries
open Enum

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
