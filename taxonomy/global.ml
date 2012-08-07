(** Global -- global variables (minimal), debugging, and utility functions.
    AVOID MODULE-SPECIFIC ADDITIONS to this file; stick with utilities and
    *truly* global variables.  This module contains a large number of just
    convenience functions/shorthand to ease OCaml programming. Many of these
    utilities are self-explanatory/short, thus minimal commenting. *)
open Str
open Printf
open Hashtbl
open List
open Unix
open Pervasives

let debug_out = ref stdout 
let debug ?force_gui:(force_gui=false) fmt = 
  let k result = begin
      output_string !debug_out result ; 
(*      output_string stdout result ; *)
(*      flush stdout ; *)
      flush !debug_out;
  end in
    Printf.kprintf k fmt 

(** much like debug, but with ABORT prepending to the message and exits 1 when
    done *)
let abort fmt = 
  let k result = begin
      output_string !debug_out result ; 
      output_string stdout result ; 
      flush stdout ; 
      flush !debug_out;
    exit 1 
  end in
    debug "\nABORT:\n\n" ; 
    Printf.kprintf k fmt 

(** return a copy of 'lst' where each element occurs once *)
let uniq lst = 
  let ht = Hashtbl.create 255 in 
  let lst = List.filter (fun elt ->
    if Hashtbl.mem ht elt then false
    else begin
      Hashtbl.add ht elt () ;
      true 
    end 
  ) lst in
    lst 

let float_array_to_str fa =
  let b = Buffer.create 255 in
  let size = Array.length fa in 
    Array.iteri (fun i v -> 
      Printf.bprintf b "%g" v ;
      if i < pred size then Printf.bprintf b ", " 
    ) fa ;
    Buffer.contents b 

(** split "filename.dat" into ["filename";"dat"] *) 
let split_ext name =
  try 
    let base = Filename.chop_extension name in
    let ext = String.sub name ((String.length base)+1)
      ((String.length name) - ((String.length base)+1))
    in 
      base,ext
  with _ -> name,""

(** split "./src/filename.dat" into ["directories/directories",
   "filename";"data"] *)
let split_base_subdirs_ext name =
  try 
    let base = Filename.basename name in
    let basename,ext = split_ext base in
      Filename.dirname name,basename,ext
  with _ -> "",name,""

let pair_compare (a,_) (b,_) = compare a b

(* Returns the elements of 'lst' in a random order. *) 
let random_order lst = 
  let a = List.map (fun x -> (Random.float 1.0), x) lst in
  let b = List.sort pair_compare a in
    List.map (fun (_,a) -> a) b 


(** given "a/b/c.txt", create "a/" and then "a/b/" if they don't already exist *)
let rec ensure_directories_exist filename = 
  match split_base_subdirs_ext filename with
  | "",_,_ | ".",_,_ | "/",_,_ -> () 
  | dirname,_,_ -> 
    ensure_directories_exist dirname ; 
    (try Unix.mkdir dirname 0o755 with _ -> ())

(** return the size of the given file on the disk *) 
let file_size name = 
  try 
    let stats = Unix.stat name in
      stats.Unix.st_size 
  with _ -> 0 

(** This makes a deep copy of an arbitrary Ocaml data
    structure. Cil.copyFunction does not preserve stmt ids! Don't use it! *)
let copy (x : 'a) = 
  let str = Marshal.to_string x [] in
    (Marshal.from_string str 0 : 'a) 

let copy_closures (x : 'a) = 
  let str = Marshal.to_string x [Marshal.Closures] in
    (Marshal.from_string str 0 : 'a) 

(* a weighted coin toss with probability p *) 
let probability p = 
  if p <= 0.0 then false
  else if p >= 1.0 then true
  else Random.float 1.0 <= p 

(** read an integer from a string with error reporting *) 
let my_int_of_string str =
  try 
    let res = ref 0 in 
      Scanf.sscanf str " %i" (fun i -> res := i) ;
      !res
  with _ -> begin 
    if String.lowercase str = "true" then 1
    else if String.lowercase str = "false" then 0 
    else failwith ("cannot convert to an integer: " ^ str)
  end 

let my_float_of_string str =
  try 
    let res = ref 0.0 in 
      Scanf.sscanf str " %f" (fun i -> res := i) ;
      !res
  with _ -> begin 
    if String.lowercase str = "true" then 1.0
    else if String.lowercase str = "false" then 0.0
    else failwith ("cannot convert to a float: " ^ str)
  end 

let file_to_string (file : string) : string = 
  let b = Buffer.create 255 in 
    try 
      let fin = open_in file in 
        (try while true do
            let line = input_line fin in
              Buffer.add_string b line ; 
              Buffer.add_char b '\n' ; 
          done ; with _ -> begin close_in fin end) ;
        Buffer.contents b 
    with _ -> Buffer.contents b 

(** @return number of lines in a text file as a float *)
let count_lines_in_file (file : string) : float =
  try 
    let fin = open_in file in 
    let count = ref 0 in
      (try while true do
          let line = input_line fin in
            ignore line ;
            incr count 
        done ; 0. with _ -> begin close_in fin ; float_of_int !count end) 
  with _ -> 0.

let get_lines (filename : string) : string list = 
  let fin = open_in filename in
  let res = ref [] in
    (try
       while true do
         res := (input_line fin) :: !res
       done
     with End_of_file -> close_in fin);
    List.rev !res

let iter_lines filename func = 
  let fin = open_in filename in
  let rec dolines () =
    try
      let line = input_line fin in 
        func line; dolines()
    with End_of_file -> close_in fin
  in
    dolines ()

(* returns the first N elements of the given list *) 
let rec first_nth lst n =  
  if n < 1 then [] 
  else match lst with
  | [] -> []
  | hd :: tl -> hd :: (first_nth tl (pred n))

(* return the first N elements of a list and the remainder as well *)
let rec split_nth lst n =  
  if n < 1 then [], lst 
  else match lst with
  | [] -> [], [] 
  | hd :: tl -> 
    let first_part, last_part = split_nth tl (pred n) in
      hd :: first_part, last_part
(**/**)

let space_regexp = Str.regexp "[ \t]+" 
let whitespace_regexp = space_regexp 
let comma_regexp = regexp_string ","
let options = ref []

let usageMsg = "Program Repair Prototype (v2)\n" 
(**/**)
(** Utility function to read 'command-line arguments' from a file.  This allows
    us to avoid the old 'ldflags' file hackery, etc. *)
let parse_options_in_file (file : string) : unit =
  let args = ref [ Sys.argv.(0) ] in 
    ( try
        let fin = open_in file in 
          (try while true do
              let line = input_line fin in
                if line <> "" && line.[0] <> '#' then begin 
        (* allow #comments *) 
        let words = Str.bounded_split space_regexp line 2 in 
        args := !args @ words 
      end 
    done with _ -> close_in fin) ;
  with e -> ()) ; 
  Arg.current := 0 ; 
  Arg.parse_argv (Array.of_list !args) 
    (Arg.align !options) 
    (fun str -> debug "%s: unknown option %s\n"  file str ; exit 1) usageMsg ;
  () 

let replace_in_string base_string list_of_replacements = 
  List.fold_left (fun acc (literal,replacement) ->
    let regexp = Str.regexp (Str.quote literal) in
      Str.global_replace regexp replacement acc 
  ) base_string list_of_replacements 

(**/**)
module OrderedString =
struct
  type t = string
  let compare = compare
end
module StringMap = Map.Make(OrderedString)
module StringSet = Set.Make(OrderedString)
(**/**)

let map_cardinal map = 
(* You know what has this? OCAML 3.12 *)
  StringMap.fold (fun k v count -> count + 1) map 0

let mergemaps map1 map2 = 
  StringMap.fold
    (fun key ->
      fun v ->
        fun newmap ->
          StringMap.add key v newmap)
    map1 map2

(**/**)
module OrderedInt =
struct
  type t = int
  let compare = compare
end
module IntMap = Map.Make(OrderedInt)
module IntSet = Set.Make(OrderedInt)

module OrderedPairs = 
struct
  type t = int * int
  let compare (a1,a2) (b1,b2) =
    if a1 = b1 then compare a2 b2
    else compare a1 b1
end
module PairSet = Set.Make(OrderedPairs)

module OrderedWeights =
struct
  type t = int * float
  let compare (a1,a2) (b1,b2) = 
    if a2 = b2 then compare a1 b1
    else compare a2 b2
end

module WeightSet = Set.Make(OrderedWeights)

module OrderedStringType =
struct
  type t = string * Cil.typ
  let compare = compare
end
module StringTypeMap = Map.Make(OrderedStringType)
(**/**)

let clamp small value big =
  if value < small then small
  else if value > big then big
  else value 

(** Helper function for generating ranges *)
let (--) i j = 
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc)
  in aux j []
    
let any_match regexp s = 
  try ignore (Str.search_forward regexp s 0); true with _   -> false

let get_opt opt = 
  match opt with
    Some(o) -> o | None -> failwith "Get_opt called on non-Some value."

(**/**)
(* not documenting this in the actual API documentation since they're all mostly
   (though admittedly not exclusively) shorthand for existing stdlib
   functions *)

let does_match = any_match 

let pprintf = Printf.printf 
let spprintf = Printf.sprintf
let lfilt = List.filter
let lmap = List.map
let lfoldl = List.fold_left
let liter = List.iter
let llen = List.length
let lmem = List.mem
let lrev = List.rev
let lflat = List.flatten
let lflatmap fnc lst = List.flatten (List.map fnc lst)
let lmap2 = List.map2
let lfoldl2 = List.fold_left2
let lsort = List.sort
let hadd = Hashtbl.add 
let hrem = Hashtbl.remove
let hfind = Hashtbl.find
let hfold = Hashtbl.fold
let hiter = Hashtbl.iter
let hlen = Hashtbl.length
let hclear = Hashtbl.clear
let hmem = Hashtbl.mem
let hrep = Hashtbl.replace
let hcreate = Hashtbl.create

let hincr ht key = 
  let old = try hfind ht key with Not_found -> 0 in
    hrep ht key (old + 1)

let ht_find ht key new_val = 
  try 
    Hashtbl.find ht key 
  with Not_found -> 
    let newval = new_val () in
      hadd ht key newval; newval

let fst3 (a,_,_) = a
let snd3 (_,b,_) = b
let trd3 (_,_,c) = c

(**/**)

(** {6 Memory Management and Debugging Functions} *) 


let bytes_per_word = 
  if max_int = 1073741823 then 4 else 8 

let live_bytes () : int = 
  Gc.full_major () ; (* "will collect all currently unreacahble blocks" *) 
  let gc_stat = Gc.stat () in 
    gc_stat.Gc.live_words * bytes_per_word

let debug_size_in_bytes (x : 'a) : int = 
  let str = Marshal.to_string x [Marshal.Closures] in
    String.length str

let debug_size_in_mb (x : 'a) : float = 
  (float_of_int (debug_size_in_bytes x)) /. (1024.0 *. 1024.0) 

(** Roulette selection from a weighted list *)
let choose_one_weighted (lst : ('a * float) list) : 'a * float =
  assert(lst <> []);
  let total_weight = List.fold_left (fun acc (sid,prob) ->
    acc +. prob) 0.0 lst in
    assert(total_weight > 0.0) ;
    let wanted = Random.float total_weight in
    let rec walk lst sofar =
      match lst with
      | [] -> failwith "choose_one_weighted"
      | (sid,prob) :: rest ->
        let here = sofar +. prob in
          if here >= wanted then (sid,prob)
          else walk rest here
    in
      walk lst 0.0

(* CLG moved these here: potentially-deprecated options that she is proposing to
   remove in the March 2012 refactor.  I didn't want to lose them entirely in
   case they're up for debate, but I will probably remove them at some point
   once I've settled on their elimination. *)
(*let allow_sanity_fail = ref false 
let preprocess = ref false
let preprocess_command = ref "__COMPILER_NAME__ -E __SOURCE_NAME__ __COMPILER_OPTIONS__ > __OUT_NAME__"
let robustness_ops = ref "ads"
let uniq_coverage = ref false
let convert_swaps = ref false
let debug_put = ref false 
let apply_diff_script = ref ""
let output_binrep = ref false 
let delete_existing_subdirs = ref false
let coverage_outname = ref "coverage.path" 
let asm_sample_runs = ref 10
let elf_sample_runs = ref 10
let server = ref false
let prepare_rep = ref false
let use_canonical_source_sids = ref true 
let neutral_walk_pop_size = ref 100
let one_positive_path = ref false
let print_fix_info = ref ""
let print_line_numbers = ref false 
let print_func_lines = ref false 
let use_subatoms = ref false 
let skip_sanity = ref false
let force_sanity = ref false
let multi_file = ref false
let use_full_paths = ref false 
let use_subdirs = ref false
let neutral_walk_steps = ref 100
let suffix_extension = ref ".c"
let mutrb_runs = ref 1000
let neutral_fitness = ref 5.0
let recompute_path_weights = ref false
let mutp = ref 0.05

(* Not committing to this, just trying it out *)
let deprecated_options = [
  "--recompute-weights", Arg.Set recompute_path_weights, " recompute the path weighting scheme; for use with neg-weight and pos-weight";
  (* use number of positive tests, right? *)
  "--neutral", Arg.Set_float neutral_fitness, "X Neutral fitness";
  (* use generations instead *)
  "--mutrb-runs", Arg.Set_int mutrb_runs, "X evaluate neutrality of X runs of each mutation operation";
  "--neutral-walk-steps", Arg.Set_int neutral_walk_steps,
  "X Take X steps through the neutral space.";
  (* default for multi-file *)
  "--use-subdirs", Arg.Set use_subdirs, " use one subdirectory per variant.";
  (* ...just always use the full paths *)
  "--use-full-paths", Arg.Set use_full_paths, " use full pathnames";
  (* intuit this from usage *)
  "--multi-file", Arg.Set multi_file, "X program has multiple source files.  Will use separate subdirs."  ;
  (* just a sanity flag *)
  "--skip-sanity", Arg.Set skip_sanity, " skip sanity checking";
  "--force-sanity", Arg.Set force_sanity, " force sanity checking";
  (* set mutp > 0, no? *)
  "--use-subatoms", Arg.Set use_subatoms, " use subatoms (expression-level mutation)" ;
  "--print-func-lines", Arg.Set print_func_lines, " print start/end line numbers of all functions" ;
  (* I don't know why we ever would *)
  "--print-line-numbers", Arg.Set print_line_numbers, " do print CIL #line numbers" ;
  (* redundant with --coverage-info; remember to fix that, btw *)
  "--print-fix-info", Arg.Set_string print_fix_info, " translate the line file into a list of statements, print to file X.";
  "--one-pos", Arg.Set one_positive_path, " Run only one positive test case, typically for the sake of speed.";
  (* why in the name of the allmighty holy being do you need a separate population size? *)
  "--neutral-walk-pop-size", Arg.Set_int neutral_walk_pop_size,
  "X Walk a population of size X through the neutral space.";
  "--suffix-extension", Arg.Set_string suffix_extension, "X append X to source filename";
  (* I'm 99% confident that literally no one uses this *)
  "--no-canonify-sids", Arg.Clear use_canonical_source_sids, " keep identical source smts separate" ;
  (* separate main for server, like nht *)
  "--server", Arg.Set server, " This is server machine"   ;
  (* there's no really good reason to *not* replace existing subdirs *)
  "--delete-subdirs", Arg.Set delete_existing_subdirs, " recreate subdirectories if they already exist. Default: false";
  (* I actually don't think we need this since it's always renamed to .neg and .pos anyway *)
  "--coverage-out", Arg.Set_string coverage_outname, " where to put the path info when instrumenting source code for coverage.  Default: ./coverage.path";
  (* I think output_binrep will be dealt with via better serialization *)
  "--output-binrep", Arg.Set output_binrep, " output binary representations with source files";
  "--apply-diff", Arg.Set_string apply_diff_script, " Apply a diff script";
  "--debug-put", Arg.Set debug_put, " note each #put in a variant's name" ;
  "--convert-swaps", Arg.Set convert_swaps, " Convert swaps into two deletes and two appends before minimizing.";
  "--uniq-cov", Arg.Set uniq_coverage, " you should use --uniq instead";
  (* settable with the probabilities *)
  "--robustness-ops", Arg.Set_string robustness_ops, "X only test robustness of operations in X, e.g., 'ad' for 'append' and 'delete'" ;
  (* this was for me and I never used it *)
  "--preprocess", Arg.Set preprocess, " preprocess the C code before parsing. Def: false";
  "--preprocessor", Arg.Set_string preprocess_command, " preprocessor command.  Default: __COMPILER__ -E" ;
  (* just one: --sample-runs *)
  "--asm-sample-runs",  Arg.Set_int asm_sample_runs,  "X Execute X runs of the test suite while sampling with oprofile.";
  "--elf-sample-runs",
  Arg.Set_int elf_sample_runs,
  "X Execute X runs of the test suite while sampling with oprofile.";
  
  "--use-line-file", 
  Arg.Unit (fun () -> 
    raise (Arg.Bad " Deprecated.  For the same functionality, do \n \
                         \t\"--fault-scheme line\", \"--fault-file file_with_line_info.ext\"\n")), " --use-line-file is deprecated";
  "--use-path-file", Arg.Unit (fun () -> 
    raise (Arg.Bad " Deprecated; the behavior is default.  You can be explicit \
                     with \"--fault-scheme path\".  --regen-paths forces path regeneration. Overried the default path files with \
                      \"--fault-path/--fix-path path_files.ext\"")),
  " --use-path-file is deprecated.";
  (* CLG is considering deleting this since I can't think of a single case
     where we want it.  We certainly don't need it *and* --skip-sanity, I
     don't think, unless it's critical for the graphics stuff. *)
  "--allow-sanity-fail", Arg.Set allow_sanity_fail, " allow sanity checks to fail";
  (* set num_gens to 0 *)
  "--prepare", Arg.Set prepare_rep, " Prepare representation for repair, but don't actually try to repair it.";
    "--mutp", Arg.Set_float mutp, "X use X as mutation rate";


]
*)
