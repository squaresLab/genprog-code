open Batteries 
open Utils
open Diffs

module type DataPoint = 
sig

  type t

  val to_string : t -> string
  val compare : t -> t -> int
  val cost : t -> t -> float
  val distance : t -> t -> float
end

module XYPoint =
struct 
  
  type t =
	  { x : int ;
		y : int ; }

  let to_string p = Printf.sprintf "%d,%d\n" p.x p.y
	
  let create x y = { x=x;y=y;}

  let compare p1 p2 = ((p1.x - p2.x) * (p1.x - p2.x)) + ((p1.y - p2.y) * (p1.y - p2.y))
  let cost p1 p2 = float_of_int ((abs (p1.x - p2.x)) + (abs (p1.y - p2.y)))
  let distance p1 p2 =  sqrt (float_of_int (compare p1 p2))
end

module type DiffCompare =
  sig
	type t
	val cost : t -> t -> float 
	val distance : t -> t -> float
  end

module DiffPoint =
  functor(Comp : DiffCompare) ->
struct 
  
  type t = Comp.t

  let to_string diff = 
	let real_diff = hfind !diff_ht diff in
	let size = List.length real_diff.changes
	in
	  Printf.sprintf "Diff %d, rev_num: %d, msg: %s, size: %d\n" real_diff.fullid real_diff.rev_num real_diff.msg size

  let compare diff1 diff2 = Pervasives.compare diff1 diff2

  let cost = Comp.cost 
  let distance = Comp.distance

end

module CostFunctionComp =
struct

  type t = int

  let cost_hash = hcreate 10
  let distance_hash = hcreate 10
  let size_hash = hcreate 10

  let cost diff1 diff2 = 
	ht_find cost_hash (diff1,diff2)
	  (fun x ->
		let size1 = 
		  ht_find size_hash diff1 
			(fun y -> 
			  let real_diff1 = hfind !diff_ht diff1 in
				List.length real_diff1.changes)
		in
		let size2 = 
		  ht_find size_hash diff2
			(fun y -> 
			  let real_diff2 = hfind !diff_ht diff2 in
				List.length real_diff2.changes)
		in
		  float_of_int (abs (size1 - size2))
	  )
	  
  let distance diff1 diff2 = 
	ht_find distance_hash (diff1,diff2)
	  (fun x ->
		let size1 = 
		  ht_find size_hash diff1 
			(fun y -> 
			  let real_diff1 = hfind !diff_ht diff1 in
				List.length real_diff1.changes)
		in
		let size2 = 
		  ht_find size_hash diff2
			(fun y -> 
			  let real_diff2 = hfind !diff_ht diff2 in
				List.length real_diff2.changes)
		in
		  float_of_int (abs(size1 - size2))
	  )

end

module UserDefinedComp = 
struct
  type t = int 

  let cost_hash = hcreate 10
  let distance_hash = hcreate 10
  let size_hash = hcreate 10

  let cost diff1 diff2 = 
	ht_find cost_hash (diff1,diff2)
	  (fun x ->
		let size1 = 
		  ht_find size_hash diff1 
			(fun y -> 
			  let real_diff1 = hfind !diff_ht diff1 in
				List.length real_diff1.changes)
		in
		let size2 = 
		  ht_find size_hash diff2
			(fun y -> 
			  let real_diff2 = hfind !diff_ht diff2 in
				List.length real_diff2.changes)
		in
		  float_of_int (abs (size1 - size2))
	  )
	  
  let distance diff1 diff2 = 
	ht_find distance_hash (diff1,diff2)
	  (fun x ->
		let size1 = 
		  ht_find size_hash diff1 
			(fun y -> 
			  let real_diff1 = hfind !diff_ht diff1 in
				List.length real_diff1.changes)
		in
		let size2 = 
		  ht_find size_hash diff2
			(fun y -> 
			  let real_diff2 = hfind !diff_ht diff2 in
				List.length real_diff2.changes)
		in
		  float_of_int (abs(size1 - size2))
	  )
end

module CostFuncDiff = DiffPoint(CostFunctionComp)
module UserFuncDiff = DiffPoint(UserDefinedComp)
