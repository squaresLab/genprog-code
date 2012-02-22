open Str
open Printf
open Hashtbl
open List

let any_match regexp s = try ignore(Str.search_forward regexp s 0); true with _	-> false

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
let lmap2 = List.map2
let lfoldl2 = List.fold_left2
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
let does_match regexp str = 
  try
	ignore(Str.search_forward regexp str 0); true
  with Not_found -> false

let rec first_nth lst n =  
  if n < 1 then [] 
  else match lst with
  | [] -> []
  | hd :: tl -> hd :: (first_nth tl (pred n))

let hincr ht key = 
  let old = try hfind ht key with Not_found -> 0 in
	hrep ht key (old + 1)

(* we copy all debugging output to a file and to stdout *)
let debug_out = ref stdout 
let debug fmt = 
  let k result = begin
(*    output_string !debug_out result ; *)
    output_string stdout result ; 
    flush stdout ; 
  end in
  Printf.kprintf k fmt 

(* split "filename.dat" into ["filename";"dat"] *) 
let split_ext name =
  try 
    let base = Filename.chop_extension name in
    let ext = String.sub name ((String.length base)+1)
      ((String.length name) - ((String.length base)+1))
    in 
    base,ext
  with _ -> name,""

(* This makes a deep copy of an arbitrary Ocaml data structure *) 
let copy (x : 'a) = 
  let str = Marshal.to_string x [] in
  (Marshal.from_string str 0 : 'a) 
  (* Cil.copyFunction does not preserve stmt ids! Don't use it! *) 

let space_regexp = Str.regexp "[ \t]+" 
let comma_regexp = regexp_string ","
let whitespace_regexp = regexp "[ \t]+"

let ht_find ht key new_val = 
  try 
    Hashtbl.find ht key 
  with Not_found -> 
	let newval = new_val () in
	  hadd ht key newval; newval

let ht_incr ht key = 
  let oldval = if hmem ht key then hfind ht key else 0 in
	hrep ht key (oldval + 1)

let handleArg = (fun str -> debug "unknown option %s\n" str)

let parse_options_in_file ?handleArg:(h=handleArg) options usageMsg (file : string) =
  let args = ref [ Sys.argv.(0) ] in 
  try
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
  with _ -> () 
	
let print_opts opts = 
  (* For debugging and reproducibility purposes, print out the values of
   * all command-line argument-settable global variables. *)
  liter (fun (name,arg,_) ->
		   debug "%s %s\n" name 
			 (match arg with
			  | Arg.Set br 
			  | Arg.Clear br 
				-> sprintf "%b" !br 
			  | Arg.Set_string sr
				-> sprintf "%S" !sr
			  | Arg.Set_int ir
				-> sprintf "%d" !ir
			  | Arg.Set_float fr
				-> sprintf "%g" !fr
			  | _ -> "?") 
		) (List.sort (fun (a,_,_) (a',_,_) -> compare a a') (opts))

let handle_options (options : (string * Arg.spec * string) list) usageMsg =
  let to_parse_later = ref [] in 
  let handleArg str = to_parse_later := str :: !to_parse_later in 
  let aligned = Arg.align options in
    Arg.parse aligned handleArg usageMsg ;
	liter (parse_options_in_file options usageMsg) !to_parse_later;
	Arg.parse aligned handleArg usageMsg (*;
	print_opts options*)
