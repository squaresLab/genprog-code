open Batteries
open Enum
open Ref
open String
open Array
open List
open Utils
open Batteries
open Utils
open Globals
open Treediff
open Datapoint

module type Element =
sig
  type t

  val substitution_cost : t -> t -> int
  val print : t list -> string

end

module StrEle =
struct
  type t = char
  let substitution_cost c1 c2 = 1
  let print strs = String.of_list strs
end

module Distance =
  functor (Ele: Element) ->
struct 

  let levenshtein (str1 : Ele.t list) (str2 : Ele.t list) = 
	let minimum a b c = 
	  if a < b then begin
		if a < c then a 
		else c
	  end else if b < c then b
	  else c
	in

	let iter_js str2_char last_row j =
	  let iter_is str1_char (this_row : int list) (last_row : int Array.t) (i : int) : int list * int =
		let last = last_row.(i - 1) in
		let d_ij = if str2_char == str1_char then last
		else (minimum ((last_row.(i)) + 1) ((hd this_row) + 1) (last + (Ele.substitution_cost str1_char str2_char))) 
		in
		  d_ij::this_row,(i+1)
	  in
	  let last_row = Array.of_list (List.rev last_row) in
	  let next_row,_ = 
		lfoldl
		  (fun (this_row,i) -> 
			 fun str1_char ->
			   iter_is str1_char this_row last_row i)
		  ([j],1) str1
	  in
		next_row,j+1
	in
	let first_row = List.of_backwards (Enum.init ((List.length str1) + 1) (fun n -> n)) in
	let final_row,_=
	  lfoldl
		(fun (last_row,j) ->
		   fun str2_char ->
			 iter_js str2_char last_row j) (first_row,1) str2
	in
	  Printf.printf "levenshtein distance between \"%s\" and \"%s\" is %d\n"
		(Ele.print str1) (Ele.print str2) (hd final_row); flush stdout
end

module StringDistance = Distance(StrEle)

(*function  LCSLength(X[1..m], Y[1..n])
    C = array(0..m, 0..n)
    for i := 0..m
       C[i,0] = 0
    for j := 0..n
       C[0,j] = 0
    for i := 1..m
        for j := 1..n
            if X[i] = Y[j]
                C[i,j] := C[i-1,j-1] + 1
            else:
                C[i,j] := max(C[i,j-1], C[i-1,j])
    return C[m,n]
		*)
module Permutation =
  functor (DP : DataPoint) ->
struct

  type mobility = LEFT | RIGHT 

  type 'a element = 
	{ mutable mobile : mobility;
	  ele : 'a;
	  k : int }

  let k_count = ref 0

  let ele_to_string ele = 
	let mobile = match ele.mobile with LEFT -> "LEFT " | RIGHT -> "RIGHT " in
	  Printf.sprintf "(%s,%s,%d)" mobile (DP.to_string ele.ele) ele.k

  let best_permutation ?compare:(compare=DP.compare) list1 list2 =
	let list1,list2 = if (llen list2) > (llen list1) then list2,list1 else list1,list2 in
	let array1 = Array.of_list list1 in
	let print_permutation perm cost =
	  Array.iteri
		(fun index ->
		  fun ele ->
			pprintf "[%d] Ele %s maps to ele %s\n" index (DP.to_string array1.(index)) (ele_to_string ele)
		) perm;
	  pprintf "total cost: %d\n\n" cost; flush stdout
	in
	let array2 = Array.of_list list2 in
	let init_cost,init_perm = 
	  Array.fold_lefti
		(fun (cost,lst) ->
		  fun index ->
			fun e -> 
			  let as_ele = { mobile = LEFT; ele = e; k = post_incr k_count } in
				(compare e array1.(index)), as_ele :: lst) (0,[]) array2
	in
	let first_permutation = Array.of_list (List.rev init_perm) in
	let array_size = Array.length array1 in
	let rec permutation last_permutation =
	  let is_mobile ele index =
		match ele.mobile with
		  LEFT -> index > 0 && ele.k > last_permutation.(index-1).k, index-1
		| RIGHT -> index < array_size - 1 && ele.k > last_permutation.(index+1).k,index + 1
	  in
	  let largest_mobile,lm_index,swap_ind = 
		Array.fold_lefti
		  (fun (largest_mobile,lm_index,swap_ind) ->
			fun index ->
			fun ele ->
			  let is_mobile,swap_index = is_mobile ele index in
			  match largest_mobile with
				Some(largest_mobile) ->
					if is_mobile && ele.k > largest_mobile.k then 
					  Some(ele),index,swap_index
					else 
					  Some(largest_mobile),lm_index,swap_ind
			  | None -> 
				if is_mobile then
				  Some(ele),index,swap_index
				else 
				  None,lm_index,swap_ind
		  ) (None,0,0) last_permutation
	  in
		match largest_mobile with
		  None -> []
		| Some(largest_mobile) ->
		  begin
		  let swap a b =
			let temp = last_permutation.(b) in
			  Array.set last_permutation b last_permutation.(a);
			  Array.set last_permutation a temp
		  in
		  let reverse_mobility ele = 
			match ele.mobile with
			  LEFT -> ele.mobile <- RIGHT
			| RIGHT -> ele.mobile <- LEFT
		  in
			swap lm_index swap_ind;
			let next_permutation,cost = 
			  Array.fold_lefti
			  (fun (array,cost) ->
				fun index ->
				  fun ele ->
					if ele.k > largest_mobile.k then 
					  reverse_mobility ele;
					let cost' = compare ele.ele array1.(index) in
					  (array,cost')
			  ) (last_permutation,0) last_permutation
			in
			  print_permutation next_permutation cost;
			  (next_permutation,cost) :: (permutation next_permutation)
		end
	in
	let permenum = List.enum (permutation first_permutation) in
	let best,cost = Enum.fold
	  (fun (best_perm,best_cost) -> 
		fun (perm,cost) ->
		  if cost > best_cost then (perm,cost) (* this is confusing because cost is actually measuring information *)
		  else (best_perm,best_cost)) (first_permutation,init_cost) permenum in
	  Array.iteri
		(fun index ->
		  fun ele ->
			pprintf "Ele %s maps to ele %s\n" (DP.to_string array1.(index)) (DP.to_string ele.ele)
			) best;
	  pprintf "total cost: %d\n" cost; flush stdout;
	  Array.to_list
		(Array.mapi 
		(fun index ->
		  fun ele -> 
			array1.(index), ele.ele) best)
		
	  
end

module SimplePerm = Permutation(XYPoint) 

let test_permutation () = 
  let list1 = 
	lmap (fun (x,y) -> XYPoint.create x y) 
	  [(1,2);(12,2);(13,0);(3,3)]
  in
  let list2 = 
	lmap (fun (x,y) -> XYPoint.create x y) 
	  [(0,0);(2,1);(1,1);(2,32)]
  in
	SimplePerm.best_permutation list1 list2
