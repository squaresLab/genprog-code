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

let alpha = 1.0
let beta = 1.0
let gamma = 1.0
let delta = 1.0

module ChangePoint = 
struct
  type t = change_node
      
  let to_string n1 = 
    Printf.sprintf "%s\n" (change_node_str n1)

  let distance n1 n2 =
    let stmt_sum c1 c2 = 
      let kij c1 c2 set3 = 
        let all_pairs = ref (StmtPairSet.empty) in
          for i = 0 to (llen set3) do
            for j = (i + 1) to (llen set3) do
              all_pairs := 
                StmtPairSet.add ((List.at set3 i), (List.at set3 j)) 
                !all_pairs
            done
          done;
          StmtPairSet.fold
            (fun (s1,s2) dist ->
              let dist' = 
                match (List.index_of s1 c1),(List.index_of s1 c2) with
                  None,Some _ | Some _, None -> 1.0
                | Some(i11), Some(i21) -> 
                  begin
                    match (List.index_of s2 c1), (List.index_of s2 c2) with
                      None,Some _ | Some _, None -> 1.0
                    | Some(i12), Some(i22) ->
                      if (i11 < i12) && (i21 < i22) then 0.0
                      else if (i11 > i12) && (i22 > i22) then 0.0
                      else 1.0
                  end
              in
                dist' +. dist
            ) !all_pairs 0.0
      in
      let set3 = 
        List.of_enum (StmtSet.enum (StmtSet.of_enum (List.enum (c1 @ c2))))
      in
        delta *. (kij c1 c2 set3) (*+.
          epsilon *. (tij c1 c2 set3) *)
    in
    let does c1 c2 = 
      let does1,_ = c1.change in 
      let does2,_ = c2.change in 
      stmt_sum does1 does2 
    in
    let deletes c1 c2 = 
      let _,deletes1 = c1.change in 
      let _, deletes2 = c2.change in 
        stmt_sum deletes1 deletes2 
    in
    let preds c1 c2 = 
      let g1 = c1.guards in
      let g2 = c2.guards in
        float_of_int (ExpSet.cardinal (ExpSet.diff g1 g2))
    in
      alpha *. (preds n1 n2) +. beta *. (does n1 n2) +. gamma *. (deletes n1 n2)

  let default = new_node 0
  let more_info n1 n2 = ()
end

