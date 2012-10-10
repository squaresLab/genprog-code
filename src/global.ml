(*
 *
 * Copyright (c) 2012-2013, 
 *  Wes Weimer          <weimer@cs.virginia.edu>
 *  Stephanie Forrest   <forrest@cs.unm.edu>
 *  Claire Le Goues     <legoues@cs.virginia.edu>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)
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

(**/**)

let debug_out = ref stdout 
(**/**)
(** we copy all debugging output to a file and to stdout *)
let debug fmt = 
  let k result = begin
      output_string !debug_out result ; 
      output_string stdout result ; 
      flush stdout ; 
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

let random_seed = ref 0 
let program_to_repair = ref "" 
let pos_tests = ref 5 
let neg_tests = ref 1 
let extension = ref "" 
let search_strategy = ref "brute"
let incoming_pop_file = ref "" 

let usageMsg = "Program Repair Prototype (v2)\n" 
(**/**)

let options = ref [
  "--program", Arg.Set_string program_to_repair, "X repair X";

  "--seed", Arg.Set_int random_seed, "X use X as random seed";

  "--pos-tests", Arg.Set_int pos_tests, "X number of positive tests";

  "--neg-tests", Arg.Set_int neg_tests, "X number of negative tests";

  "--search", Arg.Set_string search_strategy, 
  "X use strategy X (brute, ga, oracle)";

] 

let usage_function aligned usage_msg x = 
  debug "usage: unknown option %s\n" x;
  Arg.usage aligned usage_msg; abort "usage"


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


(** Utility function to read 'command-line arguments' *)
let parse_options () : unit =
  let to_parse_later = ref [] in 
  let handleArg str = to_parse_later := !to_parse_later @ [str] in
    let aligned = Arg.align !options in 
      (* first, parse the arguments, saving config files to parse *)
      try
        Arg.parse aligned handleArg usageMsg ;
          (* now, parse each config file *)
        List.iter
          (fun file ->
            let args = ref [ Sys.argv.(0) ] in 
            let lines = 
            (* allow # comments *)
              List.filter (fun line -> line <> "" && line.[0] <> '#') (get_lines file)
            in
              List.iter
                (fun line ->
                  let words = Str.bounded_split space_regexp line 2 in 
                    args := !args @ words) lines;
              Arg.current := 0 ; 
            (* parse the arguments in this config file *)
              Arg.parse_argv (Array.of_list !args) 
                aligned (usage_function aligned usageMsg) usageMsg
          ) !to_parse_later;
      (* now parse the command-line arguments again, so that they win
       * out over "./configuration" or whatnot *) 
        Arg.current := 0;
        Arg.parse aligned handleArg usageMsg 
      with | Arg.Bad msg -> (eprintf "%s" msg; exit 2)
      | Arg.Help msg -> (printf "%s" msg; exit 0)

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
