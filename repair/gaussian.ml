(* 
 * Program Repair Prototype (v2) 
 *
 * This file implements a simple Gaussian blur function for the
 * smoothing of sampled memory addresses and assembly file offsets.
 *
 *)
open Global

let sample_runs = ref 100
let _ = 
  options := !options @
	["--sample-runs", Arg.Set_int sample_runs, "X Execute X runs of the test suite while sampling with oprofile.";]

module Gaussian = struct
  let version = "1"

  let kernel =
    [(-3, 0.006); (-2, 0.061); (-1, 0.242);
     (0, 0.383);
     (1, 0.242);  (2, 0.061); (3, 0.006);]

  let blur (* blur a list of (address, count) pairs by the given kernel *)
      (kernel : (int * float) list)
      (list : (int * int) list) =
    let map = ((Hashtbl.create (List.length list)) : (int, float) Hashtbl.t) in
    let result = ref ([] : (int * float) list) in
      List.iter
        (fun (addr, count) ->
           List.iter
             (fun (offset, mult) ->
                let index = (offset + addr) in
                let current =
                  try Hashtbl.find map index
                  with Not_found -> (float_of_int 0)
                in
                  Hashtbl.replace map index (current +. ((float_of_int count) *. mult)))
             kernel)
        list ;
      Hashtbl.iter (fun a b -> result := (a,b) :: !result) map ;
      List.sort (fun (a,_) (b,_) -> a - b) !result

end
