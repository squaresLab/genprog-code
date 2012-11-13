(*
 *
 * Copyright (c) 2012-2013, 
 *  Wes Weimer          <weimer@cs.virginia.edu>
 *  Stephanie Forrest   <forrest@cs.unm.edu>
 *  Claire Le Goues     <legoues@cs.virginia.edu>
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
(** The "fitness" interface enables the computation of a variant.  Available
    fitness strategies include: 
    -> test until first failure 
    -> standard "test all"
    -> test a subset of all available
    -> test by calling an outside script and reading in a resulting file *)

open Printf
open Global
open Rep

let negative_test_weight = ref 2.0 

let _ = 
  options := !options @ [
    "--negative-test-weight", Arg.Set_float negative_test_weight, 
    "X negative tests fitness factor. Default: 2.0";

  ] 

exception Test_Failed

(* utilities to help test fitness *)

let test_one_rep rep test_maker tests factor = 
  let results = rep#test_cases (lmap test_maker tests) in
    lfoldl 
      (fun fitness (res,_) -> if res then fitness +. factor else fitness)
      0.0 results

let test_fitness_all rep = 
  let fac = 
    (float !pos_tests) *. !negative_test_weight /. (float !neg_tests) in
  let sample = 1 -- !pos_tests in
  let fitness = 
    let neg_fitness = 
      test_one_rep rep (fun x -> Negative x) (1 -- !neg_tests) fac 
    in 
    let pos_fitness = 
      test_one_rep rep (fun x -> Positive x) sample 1.0 
    in
      neg_fitness +. pos_fitness
  in
    fitness,fitness

(** {b test_to_first_failure} variant returns true if the variant passes all
    test cases and false otherwise; unlike other search strategies and as an
    optimization for brute_force search, gives up on a variant as soon as it
    fails a test case.  This makes less sense for single_fitness, but
    single_fitness being true won't break it.  Does do sampling if specified. *)
let test_to_first_failure (rep :('a,'b) Rep.representation) : bool = 
  let count = ref 0.0 in 
    try
      begin 
        (* We start with the negative tests because small changes to the
         * original variant are likely to pass the positive tests but fail the
         * negative one. *)
        for i = 1 to !neg_tests do
          let res, v = rep#test_case (Negative i) in 
            if not res then raise Test_Failed
            else begin 
              assert(Array.length v > 0); 
              count := !count +. v.(0)
            end 
        done ;
        for i = 1 to !pos_tests do 
          let res, v = rep#test_case (Positive i) in 
            if not res then raise Test_Failed
            else begin
              assert(Array.length v > 0); 
              count := !count +. v.(0)
            end 
        done ;
        true
      end 
    with Test_Failed -> false 

(** {b test_fitness} generation variant returns true if the variant passes all
    test cases and false otherwise.  Only tests fitness if the rep has not
    cached it.  Postcondition: records fitness in rep, calls rep#cleanup(). May
    implement sampling strategies if specified by the command line.*)

let test_fitness generation (rep : ('a,'b) Rep.representation) = 
  (* Find the relative weight of positive and negative tests
   * If negative_test_weight is 2 (the default), then the negative tests are
   * worth twice as much, total, as the positive tests. This is the old
   * ICSE'09 behavior, where there were 5 positives tests (worth 1 each) and
   * 1 negative test (worth 10 points). 10:5 == 2:1. *)
  begin
    let fac = 
      (float !pos_tests) *. !negative_test_weight /. (float !neg_tests) in
    let max_fitness = (float !pos_tests) +. ((float !neg_tests) *. fac) in
    let print_info fitness rest =
        debug "\t%3g %s" fitness (rep#name ());
        List.iter (fun name -> debug " %s" name) rep#source_name;
      debug "\n"
    in

  (* rest here is the additional data provided by test_fitness_all_three, when
     applicable *)
    let (sample_fitness, fitness),rest = 
      match (rep#fitness()) with
      | Some(f) -> (f,f),None
      | None -> test_fitness_all rep, None
    in
      print_info fitness rest;
      rep#cleanup();
      rep#set_fitness sample_fitness;
      not (fitness < max_fitness)
  end
