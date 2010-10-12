open Str
open Printf
open Hashtbl
open List

module IntMap = Map.Make(struct type t = int let compare = compare
			 end)

module StringMap = Map.Make(struct type t = string let compare = compare end)
module IntSet = Set.Make(struct type t = int let compare = compare end)
module StrSet = Set.Make(struct type t = string let compare = compare end)

let pprintf = Printf.printf 
let spprintf = Printf.sprintf
let lfilt = List.filter
let lmap = List.map
let lfoldl = List.fold_left
let liter = List.iter
let llen = List.length
let lmem = List.mem
let lflat = List.flatten
let hadd = Hashtbl.add 
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

(* we copy all debugging output to a file and to stdout *)
let debug_out = ref stdout 
let debug fmt = 
  let k result = begin
    output_string !debug_out result ; 
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
  with Not_found -> new_val ()

let ht_keys ht = hfold (fun key -> fun _ -> fun keys -> key :: keys) ht []
let ht_vals ht = hfold (fun _ -> fun value -> fun vals -> value :: vals) ht []
let ht_pairs ht = hfold (fun key -> fun value -> fun vals -> (key,value) :: vals) ht []

let ht_incr ht key = 
  let oldval = if hmem ht key then hfind ht key else 0 in
	hrep ht key (oldval + 1)
