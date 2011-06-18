open Str
open Printf
open Hashtbl
open List

(*module IntMap = Map.Make(struct type t = int let compare = compare
			 end)*)

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

let hincr ht key = 
  let old = try hfind ht key with Not_found -> 0 in
	hrep ht key (old + 1)

(* split "filename.dat" into ["filename";"dat"] *) 
let split_ext name =
  try 
    let base = Filename.chop_extension name in
    let ext = String.sub name ((String.length base)+1)
      ((String.length name) - ((String.length base)+1))
    in 
    base,ext
  with _ -> name,""

let space_regexp = Str.regexp "[ \t]+" 
let comma_regexp = regexp_string ","
let whitespace_regexp = regexp "[ \t]+"

let ht_find ht key new_val = 
  try 
    Hashtbl.find ht key 
  with Not_found -> 
	let newval = new_val () in
	  hadd ht key newval; newval

let fst3 (a,_,_) = a
let snd3 (_,b,_) = b
let trd3 (_,_,c) = c
