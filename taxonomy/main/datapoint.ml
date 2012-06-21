open Batteries 
open Globals
open Set
open Utils
open Cil
open Difftypes
open Distance

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

class getExpsVisitor ht cid setref = object
  inherit nopCilVisitor
  method vstmt s = 
    ChangeDoChildrenPost
      (s,fun s ->
        hadd ht (cid, s.sid) !setref ; setref := StringSet.empty; s)

  method vexpr e = setref := StringSet.add (exp_str e) !setref; DoChildren
end

let my_exps = new getExpsVisitor

module ChangePoint = 
struct
  type t = int
      
  let to_string n1 = 
    let n1 = hfind change_ht n1 in
    Printf.sprintf "%d: {%s}" n1.change_id (change_node_str n1)

  let cp_cache = hcreate 10
  let exps_cache = hcreate 10

  (* this does a kendall's tau-like distance on the two changes, similar to what
     Ray does with API usage examples in the paper *)
  (* this is half kt_distance, half something based on string edit distances
     (for preds); copying/saving state so I don't lose everything, which I realize
     is bad practice but wtfever *)
  let kt_distance n1 n2 =
    let n1 = hfind change_ht n1 in
    let n2 = hfind change_ht n2 in
    let n1_str = change_node_str n1 in
    let n2_str = change_node_str n2 in
      ht_find cp_cache (n1_str, n2_str) 
        (fun _ ->
          let distance = 
            if n1_str = n2_str then 0.0 else 
              if hmem cp_cache (n2_str,n1_str) then hfind cp_cache (n2_str,n1_str)
              else begin
                let stmt_lst_compare lst1 lst2 = 
                  let rec make_pairs set lst i =
                    let lst_len = (llen lst) - 1 in
                    let rec inner_pairs set j =
                      if j < lst_len then
                        inner_pairs 
                          (IntPairSet.add ((List.at lst i), List.at lst j) set) 
                          (j + 1)
                      else set
                    in
                      if i < lst_len then
                        let set = inner_pairs set (i + 1) in
                          make_pairs set lst (i + 1) 
                      else set
                  in
                  let kij (s1,s2) c1 c2 = 
                    match (List.index_of s1 c1),(List.index_of s1 c2) with
                    | Some(i11), Some(i21) -> 
                      begin
                        match (List.index_of s2 c1), (List.index_of s2 c2) with
                        | Some(i12), Some(i22) 
                          when ((i11 > i12) && (i21 > i22)
                                || ((i11 < i12) && (i21 < i22))) -> 0.0
                        | _ -> 1.0
                      end
                    | _ ,_ -> 1.0
                  in
                  let tij (s1,s2) (c1 : (int * Cil.stmt) list) (c2 : (int * Cil.stmt) list) = 
                    if s1 = s2 then begin
                      let stmt1 = List.assoc s1 c1 in 
                      let stmt2 = List.assoc s1 c2 in 
                      let exps1 =
                        if not (hmem exps_cache (n1.change_id,stmt1.sid)) then
                          ignore(visitCilStmt (my_exps exps_cache n1.change_id (ref (StringSet.empty))) stmt1);
                        hfind exps_cache (n1.change_id,stmt1.sid)
                      in
                      let exps2 =
                        if not (hmem exps_cache (n2.change_id,stmt2.sid)) then
                          ignore(visitCilStmt (my_exps exps_cache n2.change_id (ref (StringSet.empty))) stmt2);
                        hfind exps_cache (n2.change_id,stmt2.sid)
                      in
                        if (StringSet.cardinal (StringSet.diff exps1 exps2)) = 0 &&
                          (StringSet.cardinal (StringSet.diff exps2 exps1)) = 0 then 0.0
                        else 1.0
                    end else 0.0
                  in
                    match lst1,lst2 with
                      [],_ -> float_of_int (llen lst2)
                    | _, [] -> float_of_int (llen lst1)
                    | [(one1,_)],[(one2,_)] -> if one1 = one2 then 0.0 else 1.0
                    | _,_ -> 
                      let tls1 = lmap fst lst1 in 
                      let tls2 = lmap fst lst2 in 
                      let all_pairs = make_pairs (IntPairSet.empty) (tls1 @ tls2) 0 in
                        IntPairSet.fold
                          (fun (p1,p2) dist ->
                            let k = kij (p1,p2) tls1 tls2 in
                              k +. (tij (p1,p2) lst1 lst2) +. dist) all_pairs 0.0
                in
                let preds c1 c2 = 
                  let g1 = lfoldl (fun acc ele -> acc^" "^(exp_str ele)) "" (List.of_enum (ExpSet.enum c1.guards)) in
                  let g2 = lfoldl (fun acc ele -> acc^" "^(exp_str ele)) "" (List.of_enum (ExpSet.enum c2.guards)) in
                  let g1 = Str.global_replace paren_regexp " " g1 in
                  let g2 = Str.global_replace paren_regexp " " g2 in
                  let g1 = Str.global_replace space_regexp " " g1 in
                  let g2 = Str.global_replace space_regexp " " g2 in
                    float_of_int (levenshtein (Str.split whitespace_regexp g1) (Str.split whitespace_regexp g2))
                (* FIXME: maybe for the remaining see how many operands they have in common? *)
                in
                let alpha = float_of_int (abs ((ExpSet.cardinal n1.guards) - (ExpSet.cardinal n2.guards))) in
                let beta = float_of_int (abs ((llen n1.add) - (llen n2.add))) in
                let gamma = float_of_int (abs ((llen n1.delete) - (llen n2.delete))) in
                let preds1 = preds n1 n2 in 
                let preds_distance = alpha *. (preds1 ** 2.0) in 
(*                  debug "n1 add: %d, n2 add: %d, n1 delete: %d, n2 delete: %d\n"
                    (llen n1.add) (llen n2.add) (llen n1.delete) (llen n2.delete);*)
                let does1 = stmt_lst_compare n1.add n2.add in
(*                  debug "done does\n";*)
                let does_distance = beta *. (does1 ** 2.0) in
                let deletes1 = stmt_lst_compare n1.delete n2.delete in
(*                  debug "done delete\n";*)
                let deletes_distance = gamma *. (deletes1 ** 2.0) in
                let all = stmt_lst_compare (n1.add @ n1.delete) (n2.add @ n2.delete) in
                let all_distance = all ** 2.0 in
                let distance = sqrt preds_distance (* (preds_distance +. does_distance +. deletes_distance +. all_distance)*) in
(*                let distance = preds1 +. does1 +. deletes1 in*)
(*                  debug "change1: %s\n" (to_string n1.change_id);
                  debug "change2: %s\n" (to_string n2.change_id);*)
(*                  debug "preds: %g, does: %g, deletes: %g, all: %g\n" preds1 does1 deletes1 all;*)
(*                  debug "preds: %g\n" preds1;
                  debug "distance: %g\n" distance;*)
                  preds1
              end
          in
            distance
        )

  let distance n1 n2 =
    let simpl g1 =
      let g1 = Str.global_replace semi_regexp "" g1 in
      let g1 = Str.global_replace paren_regexp " " g1 in
      let g1 = Str.global_replace wspace_regexp " " g1 in
        Str.split whitespace_regexp g1
    in
    let n1 = hfind change_ht n1 in
    let n2 = hfind change_ht n2 in
    let n1_str = change_node_str n1 in
    let n2_str = change_node_str n2 in
      ht_find cp_cache (n1_str, n2_str) 
        (fun _ ->
          if n1_str = n2_str then 0.0 else 
            if hmem cp_cache (n2_str,n1_str) then hfind cp_cache (n2_str,n1_str)
            else begin
              let weight1 = float_of_int ((ExpSet.cardinal n1.guards) + (ExpSet.cardinal n2.guards)) /. 2.0 in
              let weight2 = float_of_int (llen (n1.add @ n2.add @ n1.delete @ n2.delete)) /. 2.0 in
              let weight = int_of_float (weight1 +. weight2) in
              let stmt_cost_function edit_type token1 token2 = 
                (* need to account for replacing "NOTHING" with "SOMETHING" *)
                let is_keyword t =
                  match t with
                    "INSERT" | "DELETE" | "NOTHING" | "ALWAYS" | "IF" -> true
                  | _ -> false
                in
                match edit_type with
                  SUBSTITUTION when (is_keyword token1) || (is_keyword token2) -> weight
                | DELETION when (is_keyword token1) || (is_keyword token2) -> weight
                | _ -> 1
              in
                float_of_int (levenshtein ~cost:(stmt_cost_function) (simpl n1_str) (simpl n2_str))
            end
        )

  let default = 
    if hmem change_ht (-1) then -1 else begin
      let n = new_node "" "" [] [] ExpSet.empty in 
        store_change ({n with change_id = -1});
        -1
    end

  let more_info n1 n2 = ()
end

