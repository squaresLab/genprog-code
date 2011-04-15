open Batteries 
open Utils
open Diffs


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

  type t = { vid : int; parent : int Array.t list; change : int Array.t list }

  let to_string p = 
	let print_array array =  "[" ^ (Array.fold_left (fun str -> fun ele -> str ^ (Printf.sprintf "%d," ele)) "" array) ^ "]\n" in
	  Printf.sprintf "ID: %d, CONTEXT: %s CHANGE: %s" 
		p.vid (lfoldl (fun str -> fun array -> str ^ print_array array) "" p.parent)
		(lfoldl (fun str -> fun array -> str ^ print_array array) "" p.change)

  let distance p1 p2 = failwith "Not implemented"
			
  let default = { vid = -1; parent = []; change = [] }

  let more_info arr1 arr2 = ()

end
