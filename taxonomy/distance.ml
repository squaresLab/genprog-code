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

	let rec iter_is (str1 : char list) (str2_char : char) (last_row : int Array.t) (this_row : int list) (j : int) (i : int): int list = 
	  pprintf "iter_js: str2_char: %c, str1: %s, i: %d, j: %d, last_row: " str2_char (String.of_list str1) i j;
	  (Array.iter (fun d -> pprintf "%d," d) last_row); 
	  pprintf "  this_row: ";
	  (liter (fun d -> pprintf "%d, " d) this_row);
	   pprintf "\n"; flush stdout;

	  if (List.is_empty str1) then this_row 
	  else begin
	   pprintf "head of str1 is %c\n" (hd str1); flush stdout;
		let last = last_row.(i - 1) in
		let d_ij = 
		  if str2_char == (hd str1) then begin pprintf "match!\n"; flush stdout; last end
		  else begin pprintf "options: %d, %d, %d\n" ((last_row.(i)) + 1) ((hd this_row) + 1) (last + 1); flush stdout;  minimum ((last_row.(i)) + 1) ((hd this_row) + 1) (last + 1) end
		in
		  iter_is (tl str1) str2_char last_row (d_ij :: this_row) j (i + 1)
	  end
	in
	let rec iter_js (str1 : char list) (str2 : char list) (last_row : int list) (j : int) : int list =
	  pprintf "str1: %s, str2: %s, j: %d, last_row: " (String.of_list str1) (String.of_list str2) j;
	  (liter (fun d -> pprintf "%d," d) last_row); pprintf "\n"; flush stdout;
	  if (List.is_empty str2) then last_row 
	  else begin
		let next_row = iter_is str1 (hd str2) (Array.of_list (List.rev last_row)) [j] j 1 in
		  iter_js str1 (tl str2) next_row (j + 1)
	  end
	in
	let first_row = List.of_backwards (Enum.init ((List.length str1') + 1) (fun n -> n)) in
	let final_row = (iter_js str1' str2' first_row 1) in
	  Printf.printf "levenshtein distance between \"%s\" and \"%s\" is %d\n"
		str1 str2 (hd final_row); flush stdout
		
end

