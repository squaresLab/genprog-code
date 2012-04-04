(** Minimization -- implements delta debugging to produce a 1-minimal subset of
	differences between a repaired variant and the original.  Can minimize
	either the edit history list or a list of cdiff changes (provided by the
	cdiff module).  *)
open Cil
open Rep
open Global
open Cdiff
open Rep
open Printf

let minimization = ref false
let minimize_patch = ref false

let _ =
  options := !options @
	[
	  "--minimization", Arg.Set minimization, " Attempt to minimize diff script using delta-debugging";
	  "--edit-script", Arg.Set minimize_patch, " Minimize the edit script, not the tree-based diff. Default: false";
	] 

(** The structural signature of a variant allows us to compute a fine-grained
	diff between individuals using delta-debugging.  This implementation is based on
	our implementation of cdiff, which applies DiffX to C code; this implementation
	could be generalized pretty trivially if necessary.  *)
type structural_signature =  
	{ signature : (Cdiff.node_id StringMap.t) StringMap.t ; 
	  node_map : Cdiff.tree_node IntMap.t }

(** virtual minimizableObject defines the basic interface that a representation
	must support in order to be minimizable.  See cilrep for an example; multiple
	inheritence is a gift *)
class virtual minimizableObject = object(self)

  (* Tree-Structured Comparisons
   *   Mostly for CIL ASTs using the DiffX algorithm. 
   *   Use the "structural_difference" methods to compute the
   *   actual difference. 
   *) 
  val already_signatured = ref None
 
 method virtual internal_structural_signature : unit -> structural_signature
  method structural_signature () = 
	match !already_signatured with
	  Some(s) -> s
	| None -> 
	  let s = self#internal_structural_signature() in
		already_signatured := Some(s); s

  method virtual from_source_min : ((string * string list) list) -> Cdiff.tree_node IntMap.t -> unit (* Build a rep directly from Cil.files, for mininimization *)
end

(* utilities for delta debugging*)

module DiffElement =
struct
  type t = int * string
  let compare (x,_) (y,_) = x - y
end
module DiffSet = Set.Make(DiffElement)

let map_union (map1) (map2) : Cdiff.tree_node IntMap.t = 
  IntMap.fold
	(fun k -> fun v -> fun new_map -> IntMap.add k v new_map)
	map1 map2

(* Turn a list of strings into a list of pairs, (string1 * string2), where string1 is unique *)
let split str =
 let whitespace = Str.regexp " " in
 let split_str = Str.split whitespace str in
 match split_str with
 | [a; b; c; d] -> a, a^" "^b^" "^c^" "^d
 | _ -> assert(false)

let script_to_pair_list a =
  List.fold_left (fun (acc : (string * (string list)) list) (ele : string) ->
    let (a : string),(b : string) = split ele in
	  match acc with
	  | (a',b') :: tl when a=a' -> (a',(b'@[b])) :: tl
	  | x -> (a,[b]) :: x
  ) [] a

let cdiff_data_ht = hcreate 255 

(** structural_difference_edit_script sig1 sig2 returns a list of (file,
	global_diffs list) elements, where a global_diff is a pair of (global_name, edit
	operations).  This list represents the difference between the two signatures
	sig1 and sig2 as reported by Cdiff, our implementation of Xdiff. *)
let structural_difference_edit_script
    (sig1 : structural_signature)
    (sig2 : structural_signature) =
  let node_map = map_union sig1.node_map sig2.node_map in 
  let final_result = ref [] in
	Hashtbl.clear cdiff_data_ht;
	StringMap.iter
	  (fun filename filemap ->
		let file2 = StringMap.find filename sig2.signature in
		let inner_result = ref [] in
		  StringMap.iter
			(fun global_name1 t1->
			  let t2 = StringMap.find global_name1 file2 in
			  let m = Cdiff.mapping node_map t1 t2 in
				Hashtbl.add cdiff_data_ht global_name1 (m,t1,t2);
			  let s = 
				Cdiff.generate_script 
				  node_map
				  (Cdiff.node_of_nid node_map t1) 
				  (Cdiff.node_of_nid node_map t2) m 
			  in
				inner_result := (global_name1,s) :: !inner_result)
			filemap;
		  final_result := (filename, (List.rev !inner_result) ) :: !final_result)
	  sig1.signature;
	List.rev !final_result

(** structural_difference_to_string rep1 rep2 computes the edit script
	describing the changes between rep1 and rep2 and reifies the result as a
	string *)
let structural_difference_to_string rep1 rep2 =
  let script = structural_difference_edit_script rep1 rep2 in
	lfoldl (fun str (file,file_diffs) ->
      lfoldl (fun str (global,global_diffs) ->
		lfoldl (fun str elt ->
          str^(Printf.sprintf "%s %s %s\n" file global (Cdiff.edit_action_to_str elt))
		) str global_diffs
      ) str file_diffs
	) "" script

(** {b process_representation} original_variant node_map diff_script applies
	diff_script (typically subsetted from the initial diff scrupt) to original
	to produce a new variant and calls test_fitness to compute its fitness.  It
	returns true if the variant produced by applying diff_script to
	original_variant passes all test cases and false otherwise.  It is a helper
	functon for delta_debugging *)
(* FIXME: get fitness brute force to sample, call that instead *)
let process_representation orig node_map diff_script =
  let the_rep = orig#copy() in
	if !minimize_patch then 
	  (* thought: maybe make load_from_string minimizableObject instead of representation *)
	  let script = lfoldl (fun acc str -> acc^" "^str) "" diff_script in
		the_rep#load_genome_from_string script
	else
	  the_rep#from_source_min (script_to_pair_list diff_script) node_map;
    Fitness.test_fitness (-1) the_rep

let delta_set_to_list set = lmap snd (DiffSet.elements set) 

let delta_count = ref 0

(** {b delta_debugging} original_variant diff_script node_map returns a
	one-minimal subset of diff_script that, when applied to original_variant,
	produces a variant that still passes all test cases.  delta_debugging
	basically implements Zeller's delta_debugging algorithm. *)
let delta_debugging orig to_minimize node_map = begin
  (* sanity check the diff script *)
  if not (process_representation orig (copy node_map) to_minimize) then
    debug "WARNING: original script doesn't pass!\n"
  else debug "GOOD NEWS: original script passes!\n";

  (* initialize the diffset based on the input *)
  let counter = ref 0 in
  let c =
	lfoldl
	  (fun c x ->
		let c = 
		  DiffSet.add ((!counter),x) c in
		  incr counter; c
	  ) (DiffSet.empty) to_minimize in

  let rec delta_debug c n =
    incr delta_count;
    debug "Entering delta, pass number %d...\n" !delta_count;
    let count = ref 0 in
    let ci_array = Array.init n (fun _ -> DiffSet.empty) in
	  if n<=(DiffSet.cardinal c) then begin
		DiffSet.iter (fun (num,x) ->
  		  ci_array.(!count mod n) <- DiffSet.add (num,x) ci_array.(!count mod n);
  		  incr count
		) c;
		let ci_list = Array.to_list ci_array in
		  try
			let ci = List.find (fun c_i -> 
			  process_representation orig (copy node_map) (delta_set_to_list c_i)) ci_list in
			  delta_debug ci 2
		  with Not_found -> begin
			try
			  let ci = List.find (fun c_i ->
				process_representation orig (copy node_map) (delta_set_to_list (DiffSet.diff c c_i))) ci_list in
				delta_debug (DiffSet.diff c ci) (max (n-1) 2);
			with Not_found -> begin
			  if n < ((DiffSet.cardinal c)) then 
				delta_debug c (min (2*n) (DiffSet.cardinal c))
			  else c
			end
		  end
	  end else c
  in

  (* do minimization...*)
  let minimized_script = delta_debug c 2 in

  (* output minimized script and file *)
  let output_name = "minimized.diffscript" in
  (* FIXME: output the final minimized source file *)
  let minimized = delta_set_to_list minimized_script in
	ensure_directories_exist ("Minimization_Files/full."^output_name);
	let fout = open_out  ("Minimization_Files/full."^output_name) in
	  liter (fun x -> Printf.fprintf fout "%s\n" x) minimized;
	  minimized
end

(** {b do_minimization} original_variant initial_repair_variant computes the
	edit list between the two input variants and calls delta_debugging to
	produce a 1-minimal subset of those edits (if minimization is enabled) *)

let do_minimization orig rep =
  if !minimization then begin
	let to_minimize,node_map = 
	  if !minimize_patch then
		Str.split space_regexp (rep#name()), IntMap.empty
	  else begin
		let orig_sig = orig#structural_signature() in
		let rep_sig = rep#structural_signature() in
		let node_map : Cdiff.tree_node IntMap.t = map_union orig_sig.node_map rep_sig.node_map in 
		let node_id_to_node = hcreate 10 in
		  IntMap.iter (fun node_id -> fun node -> hadd node_id_to_node node_id node) node_map;
		  let diff_script = structural_difference_to_string orig_sig rep_sig in
			Str.split (Str.regexp "\n") diff_script, node_map
	  end
	in
	  ignore(delta_debugging orig to_minimize node_map)
  end
