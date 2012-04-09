(** Stats2 module -- timing and stats information, disjoint timings *)
(* until the March 2012 refactoring, CLG had never actually looked at this
   module despite having included it in possibly every non-trivial OCaml program
   she's ever written in grad school.  She thus assumes it's fine and has not
   attempted to document or understand it *)
type t = {
  name : string; 
  start : float ;
  mutable holes : float ; 
} 

let stack = ref [] 

let totals = Hashtbl.create 255 
let invocations = Hashtbl.create 255 

let load_started = ref (Unix.gettimeofday ())

let clear () = 
  Hashtbl.clear totals ; 
  Hashtbl.clear invocations ; 
  stack := [] ;
  load_started := Unix.gettimeofday () 

let time name f a = 
  let start = Unix.gettimeofday () in 
    Hashtbl.replace invocations name 
      (let x = try Hashtbl.find invocations name with Not_found -> 0 in x+1) ;
    let record = {
      name = name ;
      start = start ;
      holes = 0. ;
    } in
      stack := record :: !stack ;
      let finished () = 
        let myrec = List.hd !stack in
        let now = Unix.gettimeofday () in 
        let my_delta = (now -. myrec.start) in 
        let my_time = my_delta -. (myrec.holes) in 
          stack := List.tl !stack ; 
          begin 
            match !stack with
            | [] -> ()
            | below :: others -> below.holes <- below.holes +. my_delta
          end ;
          let sofar = try Hashtbl.find totals name with Not_found -> 0.0 in 
            Hashtbl.replace totals name (sofar +. my_time) 
      in 
        try
          let res = f a in
            finished () ; 
            res 
        with e -> 
          finished () ;
          raise e 

let hashtbl_to_list ht =
  let lst = ref [] in
    Hashtbl.iter (fun  a b -> lst := (a,b) :: !lst) ht ;
    !lst

let print chn msg = 
  let lst = hashtbl_to_list totals in
  let total = List.fold_left (fun acc (ename,et) -> acc +. et) 0.0 lst in 
  let sorted = List.sort (fun (a,at) (b,bt) -> compare at bt) lst in 
  let _ = 
    Printf.fprintf chn "  %-30s %8s %7s = %s\n"  
      "Activity Name" "Count" "Seconds" "Percent of Total Time" 
  in 
  let _ = 
    List.iter (fun (l,t) ->
      let perc = (100.0 *. t /. total) in 
        if perc > 0.01 then 
          Printf.fprintf chn "  %-30s %8d %7.3f = %g%%\n" l 
            (Hashtbl.find invocations l) t perc 
    ) sorted 
  in
  let now = Unix.gettimeofday () in 
  let delta = now -. (!load_started) in 
    Printf.fprintf chn "  %-30s          %7.3f = %g%% (avg CPU usage)\n" "TOTAL" 
      total (100. *. total /. delta) 
