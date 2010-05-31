open Str
open Hashtbl
open Cil

module IntMap = Map.Make(struct type t = int let compare = compare end)
module StringMap = Map.Make(struct type t = string let compare = compare end)

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

(* actual program-specific global types and variables *)

type rank = { (* sum or record? *)
  f_P : int;
  s_P : int;
  f_P_obs : int;
  s_P_obs : int;
  numF : int;
  failure_P : float;
  context : float;
  increase : float;
  importance : float;
}

let empty_rank = 
{ f_P = 0;
  s_P = 0;
  f_P_obs = 0;
  s_P_obs = 0;
  numF = 0;
  failure_P = 0.0;
  context = 0.0;
  increase = 0.0;
  importance = 0.0;
}

type memV = Int of int | Float of float 
let mval_of_string str = 
  try (Float(float_of_string str)) with _ -> (Int(int_of_string str))

(* these are the giant hashtables from the old implementation, or at least the
   ones we still need *)

let site_ht : (int, (Cil.location * string * int * Cil.exp)) Hashtbl.t ref = 
  ref (create 10)

let coverage_ht : (int, Cil.stmt) Hashtbl.t ref = ref (create 4096)

let fname_to_run_num : (string, int) Hashtbl.t ref = ref (create 10)
let run_num_to_fname_and_good : (int, (string * int)) Hashtbl.t ref = ref (create 10)


