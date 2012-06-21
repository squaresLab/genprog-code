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

let measure_info obj = 
  let cmd = "rm temp2.txt.gz" in
	ignore(Unix.system cmd);
  let fout = open_out_bin "temp2.txt" in
	Marshal.output fout ~closures:true obj;
	close_out fout;
	let cmd = "gzip temp2.txt" in
	  ignore(Unix.system cmd); 
	  let stats = Unix.stat "temp2.txt.gz" in
		stats.Unix.st_size

module type Element =
sig
  type t
  type result
  val print : t list -> string
  val blank : t
  val star : t list
  val distance : t -> t -> int
  val star_end : t list -> t list
  val star_beginning : t list -> t list
  val star_both : t list -> t list
  val res : t list -> result
end

module StrEle =
struct
  type t = char
  type result = string
  let print strs = String.of_list strs
  let blank = ' '
  let distance a b = Pervasives.compare a b
  let star = ['*']
  let star_end str = str @ ['*']
  let star_beginning str = '*' :: str
  let star_both str = '*'::str @ ['*']
  let res str = String.of_list str
end

type sedit = DELETION | INSERTION | SUBSTITUTION

let levenshtein ?distance:(distance=Pervasives.compare) ?cost:(cost=(fun x y z -> 1)) str1 str2 =
(*  debug "str1: ";
  liter (fun str -> debug "\t[%s]\n" str) str1;
  debug "str2: ";
  liter (fun str -> debug "\t[%s]\n" str) str2;*)
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
	  let d_ij = if (distance str2_char str1_char) == 0 then last (* no change *)
		else begin
          let deletion = last_row.(i) + (cost DELETION str1_char str2_char) in
          let insertion = (hd this_row) + (cost INSERTION str1_char str2_char) in
          let substitution = last + (cost SUBSTITUTION str1_char str2_char) in
            minimum deletion insertion substitution
        end
	  in
		d_ij::this_row,(i+1)
	in
	let last_row = Array.of_list (lrev last_row) in
	let next_row,_ = 
	  lfoldl
		(fun (this_row,i) -> 
		  fun str1_char ->
			iter_is str1_char this_row last_row i)
		([j],1) str1
	in
	  next_row,j+1
  in
  let first_row = List.of_backwards (Enum.init ((llen str1) + 1) (fun n -> n)) in
  let final_row,_=
	lfoldl
	  (fun (last_row,j) ->
		fun str2_char ->
		  iter_js str2_char last_row j) (first_row,1) str2
  in
  let d = hd final_row in
(*    debug "distance: %d\n" d;*)
    d

let gcs distance blank str1 str2 =
  let m = llen str1 in
  let n = llen str2 in 
  let str1 = Array.of_list (blank :: str1) in
  let str2 = Array.of_list (blank :: str2) in 
  let c = Array.make_matrix (m+1) (n+1) 0 in
  let max a b = if a > b then a else b in
	for i = 1 to m do
	  for j = 1 to n do
		if (distance str1.(i) str2.(j)) == 0 then 
		  c.(i).(j) <- c.(i-1).(j-1) + 1
		else 
		  c.(i).(j) <- (max c.(i).(j-1) c.(i-1).(j))
	  done
	done;
	let rec backtrack i j = 
	  if i == 0 || j == 0 then []
	  else if (distance str1.(i) str2.(j)) == 0 then
		(i,j,str1.(i)) :: backtrack (i-1) (j-1)
	  else
		if c.(i).(j-1) > c.(i-1).(j) then
		  backtrack i (j-1)
		else backtrack (i-1) j
	in
	let str = lrev (backtrack m n) in 
	  if (llen str) == 0 then 0,0,0,0,[]
	  else
		let i_first,j_first,c = hd str in 
		let i_last,j_last,_ = if (llen str) > 1 then hd (tl str) else i_first,j_first,c in
		  i_first,i_last,j_first,j_last, lmap (fun (_,_,c) -> c) str

type mobility = LEFT | RIGHT 

type 'a element = 
	{ mutable mobile : mobility;
	  ele : 'a;
	  k : int }



let str_gcs = gcs (Pervasives.compare) ' '

let unify_string str1 str2 = 
  let list1,list2 = String.to_list str1, String.to_list str2 in
  let m,n = llen list1,llen list2 in
  let i_first,i_last,j_first,j_last,str = str_gcs list1 list2 in
  let str = String.of_list str in
	if (String.length str) == 0 then "*"
	else
	  let star_to_beginning = i_first <> 1 || j_first <> 1 in
	  let star_to_end = (i_first > 1 && i_first < m) || (j_first > 1 && j_first < n) in
		(if star_to_beginning then "*" else "") ^ str ^ (if star_to_end then "*" else "")


let best_mapping ?print:(print=(fun (x: 'a) -> pprintf "")) distance (list1 : 'a list)(list2 : 'a list) : ('a * 'a) list = 
  let permutations (init : 'a element array) = 
	let sizelast = Array.length init in
	let is_mobile ele index array =
	  match ele.mobile with
		LEFT -> index > 0 && ele.k > array.(index-1).k, index-1
	  | RIGHT -> index < sizelast - 1 && ele.k > array.(index+1).k,index + 1 
	in
	let swap a b array =
	  let temp = array.(b) in
		Array.set array b array.(a);
		Array.set array a temp;
	in
	let reverse_mobility ele = 
	  match ele.mobile with
		LEFT -> ele.mobile <- RIGHT
	  | RIGHT -> ele.mobile <- LEFT
	in
	let get_info array = 
	  Array.fold_lefti
		(fun (largest_mobile,lm_index,swap_ind) ->
		  fun index ->
			fun ele ->
			  let is_mobile,swap_index = is_mobile ele index array in
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
	  ) (None,0,0) array
  in
  let largest_mobile,lm_index,swap_index = get_info init in
	Enum.seq
	  (init,largest_mobile,lm_index,swap_index)
	  (fun (array,Some(largest_mobile),lm_index,swap_index) ->
		let array' = Array.copy array in
		swap lm_index swap_index array';
		Array.iter
		  (fun ele -> if ele.k > largest_mobile.k then reverse_mobility ele) array';
		let largest_mobile,lm_index,swap_ind = get_info array' in
		  array',largest_mobile,lm_index,swap_ind)
	  (fun (_,largest_mobile,_,_) ->
		match largest_mobile with
		  None -> false
		| _ -> true) 
	in
  let perm_ht = hcreate 10 in
  let smaller,bigger = if (llen list1) > (llen list2) then list2,list1 else list1,list2 in
	if (llen smaller) == (llen bigger) then begin (* try all permutations of one  compared to the other *)
	  let k = ref 0 in 
	  let init_array = Array.of_list (lmap (fun ele -> { mobile=LEFT;ele=ele;k=post_incr k}) smaller) in
	  let bigger_array = Array.of_list bigger in
	  let eval_cost = 
		Array.fold_lefti
		  (fun cost ->
			fun index -> 
			  fun ele ->
				(distance ele.ele bigger_array.(index)) +. cost) 0.0 
	  in
	  let init_cost = eval_cost init_array in 
		let permuts = permutations init_array in
	  let best,cost = 
		Enum.fold
		  (fun (bestpermut,bestcost) ->
			fun (array,_,_,_) ->
			  let cost = eval_cost array in
				if cost < bestcost then
				  array,cost 
				else bestpermut,bestcost)
		  (init_array,init_cost) permuts
	  in
		let lst = Array.to_list (Array.map (fun ele -> ele.ele) best) in
	  List.combine lst bigger
(*		Array.fold_lefti
		  (fun lst ->
			fun index ->
			  fun ele ->
				lst @ [(ele.ele,bigger_array.(index))]) [] best*)
	end else if (llen smaller) > 5 && (llen bigger) > 5 then begin (* do greedy to avoid stack blowing *)
	  let bigenum : 'a Enum.t = List.enum bigger in
	  let first : 'a = List.hd smaller in 
	  let calcoptions (ele : 'a) (enum : 'a Enum.t) : (float * 'a) Enum.t  = 
		Enum.map (fun opt -> (distance opt ele),opt) enum in
	  let calcbest (enum : (float * 'a) Enum.t) : float * 'a = 
		Enum.reduce 
		  (fun (bestcost,bestopt) ->
			fun (cost,opt) ->
			  if cost < bestcost then cost,opt
			  else bestcost,bestopt) enum in
	  let bestcost,bestopt = calcbest (calcoptions first bigenum) in
	  let options : 'a Set.t = Set.remove bestopt (Set.of_enum (List.enum bigger)) in
	  let _, ret = 
		lfoldl
		  (fun (options,retval) ->
			fun ele ->
			  let eopts : 'a Enum.t = Set.enum options in 
				let opts = calcoptions ele eopts in 
			  let bestcost,bestopt = calcbest opts in
			  let options' : 'a Set.t = Set.remove bestopt options in 
				options',retval @ [(ele,bestopt)]) (options,[(first,bestopt)]) (List.tl smaller)
	  in ret
	end else begin (* do all combos *)
	  liter
		(fun small ->
		  liter
			(fun big ->
			  let small, big = if small < big then small,big else big,small in
				ignore(ht_find perm_ht (small,big) (fun _ -> distance small big))) bigger) 
		smaller;
	  let rec all_mappings (list1 : 'a list) (allmaps : (('a,'a) Map.t * float * 'a Set.t) list) : ('a,'a) Map.t * float * 'a Set.t =
		match list1 with
		  [] -> 
			lfoldl 
			  (fun (bestmap,bestcost,opt) -> 
				fun (map,cost,opt') ->
				  if cost < bestcost then map,cost,opt' else bestmap,bestcost,opt)
			  (List.hd allmaps) (List.tl allmaps)
		| hd :: tl ->
		  all_mappings tl
			(lflat 
			   (lmap 
				  (fun (map,cost,options) ->
					Set.fold
					  (fun option ->
						fun result_list ->
						  let options' = Set.remove option options in
						  let hd,option = if hd < option then hd,option else option,hd in
						  let map' = Map.add hd option map in
							(map',cost +. hfind perm_ht (hd,option), options') :: result_list) 
					  options []) 
				  allmaps))
	  in
	  let init_map = Map.empty,0.0,Set.of_enum (List.enum bigger) in
	  let bestmap,bestcost,_ = all_mappings smaller [init_map] in
		List.of_enum (Map.enum bestmap)
	end
(*
let test_permutation () = 
  let list1 = 
	lmap (fun (x,y) -> XYPoint.create x y) 
	  [(1,2);(12,2);(13,0);(3,3)]
  in
  let list2 = 
	lmap (fun (x,y) -> XYPoint.create x y) 
	  [(0,0);(2,1);(1,1);(2,32)]
  in
	best_mapping XYPoint.distance list1 list2

    *)
