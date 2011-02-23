(* 
 * Program Repair Prototype (v2) 
 *
 * Global Variables, Debugging and Utility Functions
 *)

(* we copy all debugging output to a file and to stdout *)
let debug_out = ref stdout 
let debug fmt = 
  let k result = begin
    output_string !debug_out result ; 
    output_string stdout result ; 
    flush stdout ; 
  end in
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

(* Returns the elements of 'lst' in a random order. *) 
let random_order lst = 
  let a = List.map (fun x -> (Random.float 1.0), x) lst in
  let b = List.sort (fun (a,_) (b,_) -> compare a b) a in 
  List.map (fun (_,a) -> a) b 

(* returns the first N elements of the given list *) 
let rec first_nth lst n =  
  if n < 1 then [] 
  else match lst with
  | [] -> []
  | hd :: tl -> hd :: (first_nth tl (pred n))

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

let usageMsg = "Program Repair Prototype (v2)\n" 
let options = ref [
  "--program", Arg.Set_string program_to_repair, "X repair X";
  "--seed", Arg.Set_int random_seed, "X use X as random seed";
  "--pos-tests", Arg.Set_int pos_tests, "X number of positive tests";
  "--neg-tests", Arg.Set_int neg_tests, "X number of negative tests";
] 

let space_regexp = Str.regexp "[ \t]+" 

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

(* Added by Ethan *)

let overwrite_file fname str =
	let outf = open_out fname in
	Printf.fprintf outf "%s\n" str;
	close_out outf;
	str 

let append_file fname str = 
	let outf = open_out_gen [Open_append; Open_creat] 0o666 fname in
	Printf.fprintf outf "%s\n" str;
	close_out outf;
	str

let append_file_noline fname str = 
	let outf = open_out_gen [Open_append; Open_creat] 0o666 fname in
	Printf.fprintf outf "%s" str;
	close_out outf;
	str

let file_to_list (file : string)  =
	try 
		let fin = open_in file in
		let lst = ref [] in
		(try while true do
			let line = input_line fin in
			lst := !lst @ [line] ;
		done ; [] with _ -> begin close_in fin ; !lst end)
	with _ -> []

let replace_in_string base_string list_of_replacements = 
  List.fold_left (fun acc (literal,replacement) ->
    let regexp = Str.regexp (Str.quote literal) in
    Str.global_replace regexp replacement acc 
  ) base_string list_of_replacements 

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

