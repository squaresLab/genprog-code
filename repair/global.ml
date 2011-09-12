(* 
 * Program Repair Prototype (v2) 
 *
 * Global Variables, Debugging and Utility Functions
 *)
open Str
open Printf
open Hashtbl
open List

(* we copy all debugging output to a file and to stdout *)
let debug_out = ref stdout 
let debug fmt = 
  let k result = begin
    output_string !debug_out result ; 
    output_string stdout result ; 
    flush stdout ; 
  end in
  Printf.kprintf k fmt 

let abort fmt = 
  let k result = begin
    output_string !debug_out result ; 
    output_string stdout result ; 
    flush stdout ; 
    exit 1 
  end in
  debug "\nABORT:\n\n" ; 
  Printf.kprintf k fmt 

let uniq lst = (* return a copy of 'lst' where each element occurs once *) 
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

(* split "filename.dat" into ["filename";"dat"] *) 
let split_ext name =
  try 
    let base = Filename.chop_extension name in
    let ext = String.sub name ((String.length base)+1)
      ((String.length name) - ((String.length base)+1))
    in 
    base,ext
  with _ -> name,""

(* split "./src/filename.dat" into ["directories/directories", "filename";"data"] *)
let split_base_subdirs_ext name =
  try 
    let base = Filename.basename name in
	let basename,ext = split_ext base in
	  Filename.dirname name,basename,ext
  with _ -> "",name,""

(* Returns the elements of 'lst' in a random order. *) 
let random_order lst = 
  let a = List.map (fun x -> (Random.float 1.0), x) lst in
  let b = List.sort (fun (a,_) (b,_) -> compare a b) a in 
  List.map (fun (_,a) -> a) b 

(* given "a/b/c.txt", create "a/" and then "a/b/" if they don't already
 * exist *) 
let rec ensure_directories_exist filename = 
  match split_base_subdirs_ext filename with
  | "",_,_ | ".",_,_ | "/",_,_ -> () 
  | dirname,_,_ -> 
    ensure_directories_exist dirname ; 
    (try Unix.mkdir dirname 0o755 with _ -> ())

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

let file_size name = (* return the size of the given file on the disk *) 
  try 
    let stats = Unix.stat name in
    stats.Unix.st_size 
  with _ -> 0 

(* This makes a deep copy of an arbitrary Ocaml data structure *) 
let copy (x : 'a) = 
  let str = Marshal.to_string x [] in
  (Marshal.from_string str 0 : 'a) 
  (* Cil.copyFunction does not preserve stmt ids! Don't use it! *) 

let copy_closures (x : 'a) = 
  let str = Marshal.to_string x [Marshal.Closures] in
  (Marshal.from_string str 0 : 'a) 

(* a weighted coin toss with probability p *) 
let probability p = 
  if p <= 0.0 then false
  else if p >= 1.0 then true
  else Random.float 1.0 <= p 

(* read an integer from a string with error reporting *) 
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

let file_to_lines (file : string) : string list = 
  let b = ref [] in 
  try 
    let fin = open_in file in 
    (try while true do
      let line = input_line fin in
      b := line :: !b 
    done ; with _ -> begin close_in fin end) ;
    List.rev !b 
  with _ -> List.rev !b

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

(* Counts the number of lines in a simple text file.
 * Returns the integer number as a float. *) 
let count_lines_in_file (file : string) 
                        (* returns: *) : float =
  try 
    let fin = open_in file in 
    let count = ref 0 in
    (try while true do
      let line = input_line fin in
      ignore line ;
      incr count 
    done ; 0. with _ -> begin close_in fin ; float_of_int !count end) 
  with _ -> 0.

let random_seed = ref 0 
let program_to_repair = ref "" 
let pos_tests = ref 5 
let neg_tests = ref 1 
let extension = ref "" 
let suffix_extension = ref "" 
let search_strategy = ref "brute"

let usageMsg = "Program Repair Prototype (v2)\n" 
let options = ref [
  "--suffix-extension", Arg.Set_string suffix_extension, "X append X to source filename";
  "--program", Arg.Set_string program_to_repair, "X repair X";
  "--seed", Arg.Set_int random_seed, "X use X as random seed";
  "--pos-tests", Arg.Set_int pos_tests, "X number of positive tests";
  "--neg-tests", Arg.Set_int neg_tests, "X number of negative tests";
  "--search", Arg.Set_string search_strategy, "X use strategy X (brute, ga, neutral, oracle, walk) [comma-separated]";
] 

let space_regexp = Str.regexp "[ \t]+" 
let whitespace_regexp = space_regexp 
let comma_regexp = regexp_string ","

(* Utility function to read 'command-line arguments' from a file. 
 * This allows us to avoid the old 'ldflags' file hackery, etc. *) 
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

(* Used by structural differencing to include the filename *)
(*
module DiffString =
  struct
    type t = string * string
    let compare = compare
  end
module DiffStringMap = Map.Make(DiffString)
*)
module OrderedString =
  struct
    type t = string
    let compare = compare
  end
module StringMap = Map.Make(OrderedString)
module StringSet = Set.Make(OrderedString)

module OrderedInt =
  struct
    type t = int
    let compare = compare
  end
module IntMap = Map.Make(OrderedInt)
module IntSet = Set.Make(OrderedInt)

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

let clamp small value big =
  if value < small then small
  else if value > big then big
  else value 

let iter_lines filename func = 
  let fin = open_in filename in
  let rec dolines () =
	try
	  let line = input_line fin in 
		func line; dolines()
	with End_of_file -> close_in fin
  in
	dolines ()

let get_lines filename = 
  let fin = open_in filename in
  let res = ref [] in
	(try
	  while true do
		res := (input_line fin) :: !res
	  done
	with End_of_file -> close_in fin);
	List.rev !res

(* Helper function for generating ranges *)
let (--) i j = 
    let rec aux n acc =
      if n < i then acc else aux (n-1) (n :: acc)
    in aux j []
		
let any_match regexp s = 
  try ignore (Str.search_forward regexp s 0); true with _	-> false
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

(* This makes multi-line docs wrap prettily *)
let my_align options = try
  let len = String.length in
  let sub = String.sub in 
  let make_space num = String.make num ' ' in
  let max = List.fold_left ( fun prev (a, b, c) ->
    if (len a) > prev then len a else prev ) 0 options in

  let re = Str.regexp "[ ]" in
  List.map ( fun (a, b, c)  ->
    let a, c = 
      if c.[0] == 'X' then
        a ^ " X", (sub c 2 ((len c) - 2))
      else if c.[0] == ' ' then
        a, (sub c 1 ((len c) - 1))
      else
        a, c 
    in
  
    let wordlist = Str.split re c in

    let space = make_space (max - (len a) + 4)  in
    let c = space ^ c in

    let length = (len a) + (len c) in
    
    (* the allowable width minus leading blank *)
    let width = 80 - ((len a + 3) + (len space)) in
    let c = 
      if length >= 78 then begin
        let lines = ref [] in
        let testline = ref "" in  
        let current = ref "" in
  
        (* Make linebreaks if the next word will push us over 80 chars*)
        List.iter ( fun s -> 
          begin
          current := (!testline ^ " " ^  s) ;
          if (len !current) > width then
            begin
              lines := !testline::!lines ;
              testline := s 
            end
          else
            testline := !current
        end) wordlist ;
	  
	      (* add on the final line *)
        lines := !testline::!lines;
        lines := List.rev !lines;
        let firstspace = make_space (len space  - 1) in
        let first_line = firstspace ^ (List.hd !lines) ^ "\n" in
        
        let subsequent_space = make_space ((len a) + (len space) + 3) in
        let rest = List.tl !lines in
        
        let result = List.fold_left (fun sofar next ->
          sofar ^ subsequent_space ^  next ^ "\n"
          ) first_line rest 
        in
        sub result 0 ((len result) - 1)
      end
      else c in (a, b, c)
    ) options
  with _ ->  Arg.align options 

(* Memory Management and Debugging Functions *) 
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
