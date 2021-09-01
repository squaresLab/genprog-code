(*
 *
 * Copyright (c) 2012-2018,
 *  Wes Weimer          <weimerw@umich.edu>
 *  Stephanie Forrest   <steph@asu.edu>
 *  Claire Le Goues     <clegoues@cs.cmu.edu>
 *  Eric Schulte        <eschulte@cs.unm.edu>
 *  Jeremy Lacomis      <jlacomis@cmu.edu>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)
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
    let my_delta = now -. myrec.start in
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
        let perc = 100.0 *. t /. total in
        if perc > 0.01 then
          Printf.fprintf chn "  %-30s %8d %7.3f = %g%%\n" l
            (Hashtbl.find invocations l) t perc
      ) sorted
  in
  let now = Unix.gettimeofday () in
  let delta = now -. (!load_started) in
  Printf.fprintf chn "  %-30s          %7.3f = %g%% (avg CPU usage)\n" "TOTAL"
    total (100. *. total /. delta)
