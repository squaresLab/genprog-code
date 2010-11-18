open Batteries
open String
open Array
open List
open Utils

module Distance =
struct 

  let levenshtein (str1 : string) (str2 : string) = 
	let minimum a b c = 
	  if a < b then begin
		if a < c then a 
		else c
	  end else if b < c then b
	  else c
	in
	let str1',str2' = (String.to_list str1),(String.to_list str2) in

	let iter_js str2_char last_row j =
	  let iter_is (str1_char : char) (this_row : int list) (last_row : int Array.t) (i : int) : int list * int =
		let last = last_row.(i - 1) in
		let d_ij = if str2_char == str1_char then last
		else (minimum ((last_row.(i)) + 1) ((hd this_row) + 1) (last + 1)) 
		in
		  d_ij::this_row,(i+1)
	  in
	  let last_row = Array.of_list (List.rev last_row) in
	  let next_row,_ = 
		lfoldl
		  (fun (this_row,i) -> 
			 fun str1_char ->
			   iter_is str1_char this_row last_row i)
		  ([j],1) str1'
	  in
		next_row,j+1
	in
	let first_row = List.of_backwards (Enum.init ((List.length str1') + 1) (fun n -> n)) in
	let final_row,_=
	  lfoldl
		(fun (last_row,j) ->
		   fun str2_char ->
			 iter_js str2_char last_row j) (first_row,1) str2'
	in
	  Printf.printf "levenshtein distance between \"%s\" and \"%s\" is %d\n"
		str1 str2 (hd final_row); flush stdout
end

