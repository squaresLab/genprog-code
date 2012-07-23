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
let space_nl_regexp = Str.regexp "[ \t\n]+" 
let star_regexp = Str.regexp "[ \t]+\\*" 
let start_comment_regexp = Str.regexp "[ \t]*/\\*"
let end_comment_regexp = Str.regexp "[ \t]*\\*/"

let usageMsg = "Fix taxonomy clustering.  Right now assumes svn repository.\n"

let options = ref [
  "--debug", Arg.Set debug_bl, "\t debug output.";
]

let separate_vecs = ref false

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

let copy (x : 'a) = 
  let str = Marshal.to_string x [] in
    (Marshal.from_string str 0 : 'a) 
	
let compose strs = lfoldl (fun strs -> fun str -> strs^"\n"^str) "" strs

let cmd (cmd) = 
  let innerInput = open_process_in ?autoclose:(Some(true)) ?cleanup:(Some(true)) cmd in
  let enum_ret = IO.lines_of innerInput in
	(try ignore(close_process_in innerInput) with _ -> begin
	  pprintf "WARNING: diffcmd failed on close process in: %s\n" cmd; flush stdout
	end); enum_ret

let file_process efile tempname = 
  ignore(cmd ("rm "^tempname));
  let filter strs = lfilt (fun str -> not (any_match include_regexp str)) strs in
  let liner = Str.regexp_string "__LINE__" in
  let dater = Str.regexp_string "__DATE__" in
  let timer = Str.regexp_string "__TIME__" in
  let replace_line = 
	lmap (fun str -> Str.global_replace liner "_lineno_" str) in
  let replace_date = 
	lmap (fun str -> Str.global_replace dater "_date_" str) in
  let replace_time = 
	lmap (fun str -> Str.global_replace timer "_time_" str) in
  let filtered = replace_date (replace_time (replace_line (filter (List.of_enum efile)))) in 
	File.write_lines tempname (List.enum filtered);
	let gcc_cmd = "gcc -E "^tempname^" > temp2.c" in
	  ignore(Unix.system gcc_cmd); File.lines_of "temp2.c"

module OrderedIntPair = struct
  type t = int * int

  let compare (i11,i12) (i21,i22) =
    if i11 = i21 then Pervasives.compare i11 i21
    else Pervasives.compare i12 i22
end

module IntPairSet = Set.Make(OrderedIntPair)


let bytes_per_word = 
  if max_int = 1073741823 then 4 else 8 

let live_bytes () : int = 
  Gc.full_major () ; (* "will collect all currently unreacahble blocks" *) 
  let gc_stat = Gc.stat () in 
    gc_stat.Gc.live_words * bytes_per_word

let live_mb () : float =
  (float_of_int (live_bytes ())) /. (1024.0 *. 1024.0)

let debug_size_in_bytes (x : 'a) : int = 
  let str = Marshal.to_string x [Marshal.Closures] in
    String.length str

let debug_size_in_mb (x : 'a) : float = 
  (float_of_int (debug_size_in_bytes x)) /. (1024.0 *. 1024.0) 
