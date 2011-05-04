open Batteries 
open Utils
open Difftypes


module type DataPoint = 
sig

  type t

  val to_string : t -> string
  val distance : t -> t -> float
  val default : t
  val more_info : t -> t -> unit
end

module XYPoint =
struct 
  
  type t =
	  { x : int ;
		y : int ; }
  let to_string p = Printf.sprintf "(%d,%d)" p.x p.y

  let default = {x=(-1);y=(-1)}

  let create x y = { x=x;y=y;}

  let compare p1 p2 = ((p1.x - p2.x) * (p1.x - p2.x)) + ((p1.y - p2.y) * (p1.y - p2.y))
  let distance p1 p2 =  sqrt (float_of_int (compare p1 p2))
  let more_info one two = ()
end

module VectPoint = 
struct

  type t = { vid : int; 
			 template : Difftypes.template ;
			 change : int Array.t ; 
			 mu : int Array.t list;
			 collected: int Array.t list}

  let num_ids = ref 0 
  let new_id () = Ref.post_incr num_ids
  let vcache = hcreate 10

  let to_string p = 
    let print_array array =  "[" ^ (Array.fold_left (fun str -> fun ele -> str ^ (Printf.sprintf "%d," ele)) "" array) ^ "]\n" in
(*      Printf.sprintf "FILE:%s, TEMPLATEID: %d" p.template.Difftypes.change.Difftypes.fname p.template.Difftypes.template_id*) "foo"

  let distance p1 p2 = 
	let euclid a1 a2 = 
	  sqrt
		(Array.fold_lefti
		   (fun total ->
			 fun index ->
			   fun ele1 ->
				 (float_of_int(a2.(index) - ele1)**2.0) +. total)
		   0.0 a1)
	in
	  ht_find vcache (p1.vid,p2.vid) 
	  (fun _ -> 
		let coll1 = Array.of_list p1.collected in
		let coll2 = Array.of_list p2.collected in
		let coll1,coll2 = 
		  if Array.length coll1 > Array.length coll2 then coll2,coll1 else coll1,coll2
		in
		let min = ref (-1.0) in
		  for i = 0 to pred (Array.length coll1) do
			let arr1 = coll1.(i) in
			  for j = 0 to pred (Array.length coll2) do 
				let dist = euclid arr1 coll2.(i) in
				  if !min < 0.0 || dist < !min then min := dist
			done;
		  done; !min
	  )
			
  let default = 
	{vid = -1;
	 template = Difftypes.empty_template;
	 change = Array.make 101 0;
	 mu = [];
	 collected = []} 
  let more_info arr1 arr2 = ()

end
