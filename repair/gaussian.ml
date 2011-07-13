(* 
 * Program Repair Prototype (v2) 
 *
 * This file implements a simple Gaussian blur function for the
 * smoothing of sampled memory addresses and assembly file offsets.
 *
 *)
module Gaussian = struct
  let version = "1"

  let kernel =
    [(-3, 0.006); (-2, 0.061); (-1, 0.242);
     (0, 0.383);
     (1, 0.242);  (2, 0.061); (3, 0.006);]

  let blur (* blur a list of (address,count) pairs by the given kernel *)
      (kernel : (int * float) list)
      (list : (int * int) list) =
    let map = ((Hashtbl.create (List.length list)) : (int, float) Hashtbl.t) in
    let result = ref ([] : (int * int) list) in
      List.iter
        (fun (count, addr) ->
           List.iter
             (fun (offset, mult) ->
                let index = (offset + addr) in
                let current =
                  try Hashtbl.find map index
                  with Not_found -> (float_of_int 0)
                in
                  Hashtbl.add map index (current +. ((float_of_int count) *. mult)))
             kernel)
        list ;
      Hashtbl.iter (fun a b -> result := (a,(int_of_float (ceil b))) :: !result) map ;
      let to_sort = (Array.of_list !result) in
        Array.sort (fun (a,_) (c,_) -> a - c) to_sort;
        Array.to_list to_sort

end
