(* Part of predicate project. Contains global variables (THE HASHTABLES)
 * and utility functions/definitions (like print, keys, empty and the regexps).
 *)

open Str
open Hashtbl
open List

(* globals *)
module IntSet = Set.Make (struct 
			    type t = int
			    let compare = compare
			  end)

module PredSet = Set.Make (struct
			     type t = int * int 
			     let compare =
			       fun(x1,y1) ->
				 fun(x2, y2) ->
				   x1 - x2
			   end)

type predicate_info = {
  importance : float ;
  increase : float ;
  context : float ;
  f_of_P : float ;
  s_of_P : float ;
  failureP : float ;
  fObserved : float ;
  sObserved : float ;
  count_obs_s : float ;
  count_obs_f : float ;
  count_true_s : float ;
  count_true_f : float ;
}

let empty_info =   
  { importance = 0.0 ;
  increase = 0.0 ;
  context = 0.0 ;
  f_of_P = 0.0;
  s_of_P = 0.0 ;
  failureP = 0.0; 
  fObserved = 0.0; 
  sObserved = 0.0 ;
  count_obs_s = 0.0 ;
  count_obs_f= 0.0 ;
  count_true_s= 0.0 ;
  count_true_f= 0.0 ;
  }

let debug = ref false 
let comma_regexp = regexp_string ","
let colon_regexp = regexp_string ":"
let whitespace_regexp = regexp "[ \t]+"
let slash_regexp = regexp "/" 

let modify_input = ref false

let concise_runs_out = ref "concise_runs_out.default"
let hashes_out = ref "hashes_out.default"
let baseline_out = ref ""
let baseline_in = ref ""

let fname_to_run_num : (string, int) Hashtbl.t ref = ref (create 10)
let run_and_pred_to_res : (int, (int, (int * int)) Hashtbl.t) Hashtbl.t ref = ref (create 10)
let run_num_to_fname_and_good : (int, (string * int)) Hashtbl.t ref = ref (create 10)

let site_ht : (int, (Cil.location * string * Cil.exp)) Hashtbl.t ref = ref (create 10)

let site_set = ref IntSet.empty

let fAIL = 1
let sUCC = 0

(* utilities *)

let print s = if !debug then Printf.printf s; flush stdout 

let keys tbl = 
  Hashtbl.fold
    (fun k -> fun v -> fun accum -> k :: accum)
    tbl []

let empty li = (length li) == 0

let last2 li = (nth (rev li) 1) :: (nth (rev li) 0) :: []

let last3 li = 
    (nth (rev li) 2) :: ((nth (rev li) 1) :: ((nth (rev li) 0) :: []))

let to_int i = Int32.to_int (Int32.of_string i)

let to_ints (li : string list) : int list =
  map (fun str -> to_int str) li

let len_str_list (li : string list) : int =
  fold_left (fun len -> fun str -> len + (String.length str)) 0 li


