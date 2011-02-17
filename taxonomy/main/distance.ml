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

let gcs str1 str2 = 
  let m = String.length str1 in
  let n = String.length str2 in 
  let str1 = Array.of_list (' ' :: (String.to_list str1)) in
  let str2 = Array.of_list (' ' :: (String.to_list str2)) in 
  let c = Array.make_matrix (m+1) (n+1) 0 in
  let max a b = if a > b then a else b in
	for i = 1 to m do
	  for j = 1 to n do
		if str1.(i) == str2.(j) then 
		  c.(i).(j) <- c.(i-1).(j-1) + 1
		else 
		 c.(i).(j) <- (max c.(i).(j-1) c.(i-1).(j))
	  done
	done;
	let rec backtrack i j = 
	  if i == 0 || j == 0 then []
	  else if str1.(i) == str2.(j)  then
		 (i,j,str1.(i)) :: backtrack (i-1) (j-1)
	  else
		if c.(i).(j-1) > c.(i-1).(j) then
		  backtrack i (j-1)
		else backtrack (i-1) j
	in
	let str = lrev (backtrack m n) in
	  if (llen str) == 0 then "*" 
	  else if (llen str) == 1 then begin
		let [(i_first,j_first,c_first)] = str in
		  if i_first == 1 && j_first == 1 && ((m > 1) || n > 1) then (String.of_list [c_first])^"*" 
		  else if (i_first > 1 || j_first > 1) && ((i_first < m) || (j_first < n)) then "*"^(String.of_list [c_first])^"*"
		  else "*"^(String.of_list [c_first])
	  end
	  else 
		let i_first,j_first,c_first = (List.hd str) in
		let i_last,j_last,c_last = List.hd (lrev str) in 
		let beginning = 
		  if i_first == 1 && j_first == 1 then ""
		  else "*" in
		let send = if (i_last == m) && (j_last == n) then "" else "*" in
		let ret = String.of_list (lmap (fun (_,_,c) -> c) str) in
		  beginning ^ ret ^ send


  type mobility = LEFT | RIGHT 

  type 'a element = 
	{ mutable mobile : mobility;
	  ele : 'a;
	  k : int }

  let k_count = ref 0

  let best_permutation distance list1 list2 =
	let list1,list2 = if (llen list1) > (llen list2) then list1,list2 else list2,list1 in
	let size1 = llen list1 in
	let size2 = llen list2 in
	let array1 = Array.of_list list1 in
	let array2 = Array.of_list list2 in
	let init_cost,init_perm = 
	  Array.fold_lefti
		(fun (cost,lst) ->
		  fun index ->
			fun e -> 
			  let as_ele = { mobile = LEFT; ele = e; k = post_incr k_count } in
				cost + (if index < size1 then (distance e array1.(index)) else 0), as_ele :: lst) (0,[]) array2
	in

	let first_permutation = Array.of_list (List.rev init_perm) in
	let rec permutation last_permutation =
	  let sizelast = Array.length last_permutation in
	  let is_mobile ele index =
		match ele.mobile with
		  LEFT -> index > 0 && ele.k > last_permutation.(index-1).k, index-1
		| RIGHT -> index < sizelast - 1 && ele.k > last_permutation.(index+1).k,index + 1 
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
			  Array.set last_permutation a temp;
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
					let cost' = if index < size1 then distance ele.ele array1.(index) else 0 in
					  (array,cost' + cost)
			  ) (last_permutation,0) last_permutation
			in
			  (next_permutation,cost) :: (permutation next_permutation)
		end
	in
	let permenum = List.enum (permutation first_permutation) in
	let best,cost = Enum.fold
	  (fun (best_perm,best_cost) -> 
		fun (perm,cost) ->
		  if cost > best_cost then (perm,cost) (* this is confusing because cost is actually measuring information *)
		  else (best_perm,best_cost)) (first_permutation,init_cost) permenum in
(*	  pprintf "total cost: %d\n" cost; flush stdout;*)
	  Array.fold_lefti
	    (fun sofar ->
	       fun index ->
		  fun ele -> 
		    if index < size1 then sofar @ [(array1.(index),ele.ele)] else sofar)
	    [] best
		
	  
let test_permutation () = 
  let list1 = 
	lmap (fun (x,y) -> XYPoint.create x y) 
	  [(1,2);(12,2);(13,0);(3,3)]
  in
  let list2 = 
	lmap (fun (x,y) -> XYPoint.create x y) 
	  [(0,0);(2,1);(1,1);(2,32)]
  in
	best_permutation XYPoint.compare list1 list2

