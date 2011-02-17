open Batteries 
open Utils
open Diffs


module type DataPoint = 
sig

  type t

  val to_string : t -> string
  val distance : t -> t -> float
  val default : t
  val is_default : t -> bool
end

module XYPoint =
struct 
  
  type t =
	  { x : int ;
		y : int ; }
  let defaulted = ref true
  let to_string p = Printf.sprintf "(%d,%d)" p.x p.y

  let default = {x=(-1);y=(-1)}

  let is_default _ = if !defaulted then (defaulted := false; true) else false

  let create x y = { x=x;y=y;}

  let compare p1 p2 = ((p1.x - p2.x) * (p1.x - p2.x)) + ((p1.y - p2.y) * (p1.y - p2.y))
  let distance p1 p2 =  sqrt (float_of_int (compare p1 p2))
end
