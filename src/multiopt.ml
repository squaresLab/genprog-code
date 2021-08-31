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
(** Multiopt provides multi-objective search strategies.  Currently implements
    NSGA-II, based on:
    http://www.mathworks.com/matlabcentral/fileexchange/10429-nsga-ii-a-multi-objective-optimization-algorithm*)

(* Note: CLG did not write this and has made minimal effort to understand it.  I
   did, however, reformat a lot to get everything closer to 80 characters per
   line; the use of the imperative style in this module combined with OCaml's
   standard indentation practices meant that basically everything was shifted
   really far to the right by the time we got to step 3.3 of the algorithm.  I
   thus do a lot of let _ = imperative computation in, trying to break up the
   computation into the steps suggested by the algorithm.  This is only in the
   interest of readability *)

open Global
open Rep
open Search
open Population

let minimize = ref false
let no_inf = ref false
let num_objectives = ref 2  (* number of objectives *)
let _ =
  options := !options @ [
      "--multiopt-minimize", Arg.Set minimize, " minimize multiopt objective";

      "--multiopt-no-inf", Arg.Set no_inf, " avoid infinite values";

      "--num-objectives", Arg.Set_int num_objectives, "X expect X objective values";
    ]

(* The implementation of NGSA-II below requires O(n^2) fitness lookups. The
   md5sum-based fitness cache from the Rep module is much faster than
   re-evaluating the variant's fitness, but is noticeably slow during replay.
   So we create our own cache here, using the rep name as a key.

   NOTE: this assumes that name1 == name2 implies fitness1 == fitness2.
*)
let yet_another_fitness_cache = Hashtbl.create 255

let calculate_fitness (rep : ('a,'b) GPPopulation.individual) =
  (* FIXME: this function implements Search.calculate_fitness and
     Fitness.test_fitness inline. Unfortunately, those functions are built
     around single-float fitness, which doesn't work for us. *)
  let evals = Rep.num_test_evals_ignore_cache () in
  if !max_evals > 0 && evals > !max_evals then
    raise (Maximum_evals(evals)) ;
  let _, real_values = rep#test_case (Single_Fitness) in
  let values, stddevs = col_mean_stddev real_values in
  let values, stddevs =
    if Array.length values != !num_objectives then
      let v = if !minimize then infinity else neg_infinity in
      Array.make !num_objectives v, Array.make !num_objectives 0.0
    else
      values, stddevs
  in
  let n = sqrt (float_of_int (llen real_values)) in
  let b = Buffer.create 255 in
  Array.iteri (fun i v ->
      Printf.bprintf b "%g" v ;
      if (classify_float stddevs.(i)) != FP_nan && stddevs.(i) > 0.0 then
        Printf.bprintf b " +/- %g" (1.96 *. stddevs.(i) /. n) ;
      Printf.bprintf b " "
    ) values ;
  debug ~force_gui:true "\t%s%s\n" (Buffer.contents b) (rep#name ()) ;
  rep#set_fitness values.(0);
  Hashtbl.replace yet_another_fitness_cache (rep#name()) values ;
  rep#cleanup() ;
  rep

let evaluate (rep : ('a,'b) representation) =
  Hashtbl.find yet_another_fitness_cache (rep#name())

let is_pessimal arr =
  if !minimize then
    arr = Array.make !num_objectives infinity
  else
    arr = Array.make !num_objectives neg_infinity

(* NGSA-II *)

let dominates (p: ('a, 'b) Rep.representation)
    (q: ('a, 'b) Rep.representation) : bool =
  let p_values = evaluate p in
  let q_values = evaluate q in
  assert (Array.length p_values = Array.length q_values) ;
  let better = ref 0 in
  let same = ref 0 in
  let worse = ref 0 in
  for i = 0 to pred (Array.length p_values) do
    if p_values.(i) > q_values.(i) then
      (if !minimize then incr worse else incr better)
    else if p_values.(i) = q_values.(i) then
      incr same
    else
      (if !minimize then incr better else incr worse)
  done ;
  if !worse > 0 then false
  else if !better >0 then true
  else false

let rephash_replace h x y = Hashtbl.replace h (x#name ()) (y)
let rephash_add h x y = Hashtbl.add h (x#name ()) (y)
let rephash_find h x = Hashtbl.find h (x#name ())
let rephash_find_all h x = Hashtbl.find_all h (x#name ())
let rephash_mem h x = Hashtbl.mem h (x#name ())

let rec ngsa_ii (original : ('a,'b) Rep.representation) (incoming_pop) : unit =
  try begin
    let current =
      ref (initialize_ga ~get_fitness:calculate_fitness original incoming_pop)
    in

    debug "multiopt: ngsa_ii begins (%d generations left)\n" !generations;

    try begin
      for gen = 1 to !Search.generations do
        let _ =
          debug "multiopt: ngsa_ii generation %d begins\n" gen
        in
        let is_last_generation = gen = !generations in
        let next_generation =
          ngsa_ii_internal original !current ~is_last_generation in
        let filename = Printf.sprintf "generation-%04d.list" gen in
        let _ =
          debug "multiopt: printing %s\n" filename
        in
        let fout = open_out filename in
        List.iter (fun var ->
            let names = var#source_name in
            let rec handle names =
              match names with
              | [] -> ()
              | [one] -> Printf.fprintf fout "%s\n" one
              | first :: rest ->
                Printf.fprintf fout "%s," first ;
                handle rest
            in
            handle names ;
          ) next_generation ;
        close_out fout ;
        current := next_generation
      done ;
      debug "multiopt: ngsa_ii end\n"
    end with Maximum_evals(evals) ->
      debug "reached maximum evals (%d)\n" evals
  end with Maximum_evals(evals) ->
    debug "reached maximum evals (%d) during population initialization\n" evals

and ngsa_ii_internal
    ?(is_last_generation=false) (original) pop =
  (* Step numbers follow Seshadri's paper *)

  let ngsa_ii_sort pop = begin
    let _ =
      debug "multiopt: beginning sort\n" ;
      Gc.compact()
    in

    let f_max = Hashtbl.create 255 in
    let f_min = Hashtbl.create 255 in
    let adjust_f_max m v =
      let sofar = try Hashtbl.find f_max m with _ -> neg_infinity in
      Hashtbl.replace f_max m (max sofar v)
    in
    let adjust_f_min m v =
      let sofar = try Hashtbl.find f_min m with _ -> infinity in
      Hashtbl.replace f_min m (min sofar v)
    in

    let _ =
      debug "multiopt: computing f_max and f_min %d \n"  (List.length pop)
    in

    let _ =
      List.iter (fun p ->
          let p_values = evaluate p in
          Array.iteri (fun m fval ->
              adjust_f_max m fval ;
              adjust_f_min m fval ;
            ) p_values ;
        ) pop ;
      for m = 0 to pred !num_objectives do
        debug "multiopt: %g <= objective %d <= %g\n"
          (Hashtbl.find f_min m) m (Hashtbl.find f_max m)
      done
    in

    (****** 3.2. Non-Dominated Sort ******)
    let _ =
      debug "multiopt: first non-dominated sort begins\n"
    in
    let dominated_by = hcreate 255 in
    let dominated_by_count = hcreate 255 in
    let rank = hcreate 255 in
    let delta_dominated_by_count (p:('a,'b) Rep.representation) dx =
      let sofar = rephash_find dominated_by_count p in
      rephash_replace dominated_by_count p (sofar + dx)
    in
    let f = Hashtbl.create 255 in

    let _ =
      List.iter (fun (p : ('a,'b) Rep.representation) ->
          rephash_replace dominated_by_count p 0;
          List.iter (fun (q : ('a,'b) Rep.representation)->
              let str =
                if dominates p q then begin
                  rephash_add dominated_by p q ;
                  ">"
                end else if dominates q p then begin
                  delta_dominated_by_count p 1 ;
                  "<"
                end else "="
              in
              ignore str
            ) pop ;
          if rephash_find dominated_by_count p = 0 then begin
            Hashtbl.add f 1 p ;
            rephash_replace rank p 1 ;
          end
        ) pop
    in

    let i = ref 1 in
    let _ =
      while Hashtbl.mem f !i do
        let set_q_names = Hashtbl.create 255 in
        let set_q_reps = ref [] in
        let f_i = Hashtbl.find_all f !i in

        let _ =
          debug "multiopt: front i=%d (%d members)\n" !i (List.length f_i)
        in
        List.iter (fun p ->
            let s_p = rephash_find_all dominated_by p in
            List.iter (fun q ->
                delta_dominated_by_count q (-1) ;
                let n_q = rephash_find dominated_by_count q in
                if n_q = 0 then begin
                  rephash_replace rank q (!i + 1) ;
                  if not (Hashtbl.mem set_q_names (q#name ())) then
                    begin
                      Hashtbl.add set_q_names (q#name ()) true ;
                      set_q_reps := q :: !set_q_reps
                    end
                end
              ) s_p
          ) f_i ;
        incr i ;
        List.iter (fun q ->
            Hashtbl.add f !i q
          ) !set_q_reps
      done
    in

    let i_max = !i in
    let _ =
      List.iter (fun p ->
          if not (rephash_mem rank p) then begin
            rephash_replace rank p i_max ;
            let p_values = evaluate p in
            if not (is_pessimal p_values) then begin
              let n_p = rephash_find dominated_by_count p in
              debug "multiopt: NO RANK for %s %s n_p=%d: setting to %d\n"
                (p#name ()) (float_array_to_str p_values) n_p i_max
            end
          end
        ) pop
    in

    (****** 3.3. Crowding Distance ******)
    let distance = hcreate 255 in
    let add_distance p delta =
      let sofar = rephash_find distance p in
      rephash_replace distance p (sofar +. delta)
    in
    let _ =
      debug "multiopt: crowding distance calculation\n"
    in
    let _ =
      for i = 1 to pred i_max do
        let n = Hashtbl.find_all f i in
        List.iter (fun p ->
            rephash_replace distance p 0.0 ;
          ) n ;
        for m = 0 to pred !num_objectives do
          let i_set = List.sort (fun a b ->
              let a_values = evaluate a in
              let b_values = evaluate b in
              compare a_values.(m) b_values.(m)
            ) n in
          let i_array = Array.of_list i_set in
          let i_size = Array.length i_array in
          assert(i_size > 0);
          rephash_replace distance i_array.(0) infinity ;
          rephash_replace distance i_array.(pred i_size) infinity ;
          for k = 1 to pred (pred i_size) do
            let k_plus_1 = i_array.(k+1) in
            let k_minus_1 = i_array.(k-1) in
            let k_plus_1_values = evaluate k_plus_1 in
            let k_minus_1_values = evaluate k_minus_1 in
            let f_max_m = Hashtbl.find f_max m in
            let f_min_m = Hashtbl.find f_min m in
            add_distance i_array.(k)
              (
                (abs_float (k_plus_1_values.(m) -. k_minus_1_values.(m)))
                /.
                (f_max_m -. f_min_m)
              )
          done
        done
      done
    in

    (****** 3.4. Selection ******)
    let _ =
      debug "multiopt: computing selection operator\n"
    in
    let crowded_compare p q =
      (* "An individual is selected if the rank is lesser than the other or
         if crowding distance is greater than the other" *)
      let rank_order = compare (rephash_find rank q) (rephash_find rank p) in
      if rank_order = 0 then
        compare (rephash_find distance p) (rephash_find distance q)
      else
        rank_order
    in
    crowded_compare, f, distance
  end (* end ngsa_ii_sort *)
  in

  let crowded_compare, f, distance = ngsa_ii_sort pop in

  let _ =
    debug "multiopt: crossover and mutation\n"
  in

  (* crossover, mutate *)
  let children =
    let selected = GPPopulation.selection pop !popsize in
    let crossed = GPPopulation.crossover selected original in
    let mutated = GPPopulation.map crossed (fun one -> mutate one) in
    GPPopulation.map mutated calculate_fitness
  in

  let _ =
    debug "multiopt: adding children, sorting\n"
  in

  let many = pop @ children in
  let crowded_compare, f, distance = ngsa_ii_sort many in

  if is_last_generation then begin
    let f_1 = Hashtbl.find_all f 1 in
    let i = ref 0 in
    let _ =
      debug "\nmultiopt: %d in final generation pareto front:\n(does not include all variants considered)\n\n"
        (List.length f_1)
    in
    let f_1 = List.sort (fun p q ->
        let p_values = evaluate p in
        let q_values = evaluate q in
        compare p_values q_values
      ) f_1 in
    let copy_and_rename_dir rename_fun src dst =
      let ss = Array.to_list (Sys.readdir src) in
      let ds = List.map rename_fun ss in
      List.iter2 (fun s d ->
          Sys.rename (Filename.concat src s) (Filename.concat dst d)
        ) ss ds
    in
    let _ =
      let finaldir = Rep.add_subdir (Some "pareto") in
      List.iter (fun p ->
          let prefix = Printf.sprintf "pareto-%06d" !i in
          let subdir = Rep.add_subdir (Some prefix) in
          let p_values = evaluate p in
          let name = Filename.concat subdir (prefix ^ !Global.extension) in
          let fname = Filename.concat finaldir (prefix ^ ".fitness") in
          incr i;
          p#output_source name ;
          if Sys.is_directory prefix then begin
            copy_and_rename_dir (fun n -> prefix ^ "-" ^ n) prefix finaldir;
            Unix.rmdir prefix
          end;
          let fout = open_out fname in
          output_string fout (float_array_to_str p_values) ;
          output_string fout "\n" ;
          close_out fout ;
          debug "%s %s %s\n" name
            (float_array_to_str p_values)
            (p#name ())
        ) f_1
    in
    f_1
  end else begin
    let next_generation = ref [] in
    let finished = ref false in
    let front_idx = ref 1 in

    let _ =
      while not !finished do
        let indivs_in_front = Hashtbl.find_all f !front_idx in
        let indivs_in_front =
          let do_not_want = if !minimize then infinity else 0.  in
          if !no_inf then
            List.filter (fun p ->
                let p_values = evaluate p in
                List.for_all (fun v ->
                    v <> do_not_want
                  ) (Array.to_list p_values)
              ) indivs_in_front
          else
            indivs_in_front
        in
        let num_indivs = List.length indivs_in_front in
        let _ =
          debug "multiopt: %d individuals in front %d\n" num_indivs !front_idx
        in
        let have_sofar = List.length !next_generation in
        let _ =
          finished := have_sofar + num_indivs >= !Population.popsize
        in
        let to_add =
          if have_sofar + num_indivs <= !Population.popsize then
            (* we can just take them all! *)
            indivs_in_front
          else begin
            (* sort by crowding distance *)
            let sorted = List.sort (fun a b ->
                compare (rephash_find distance a) (rephash_find distance b)
              ) indivs_in_front in
            let num_wanted = !Population.popsize - have_sofar in
            let selected = first_nth sorted num_wanted in
            selected
          end
        in
        let _ = incr front_idx in
        if not !finished && num_indivs = 0 then begin
          let wanted = !Population.popsize - have_sofar in
          let _ =
            debug "multiopt: including %d copies of original\n" wanted
          in
          for i = 1 to wanted do
            next_generation := (original#copy ()) :: !next_generation
          done ;
          finished := true
        end ;
        next_generation := to_add @ !next_generation
      done
    in
    let _ =
      debug "multiopt: next generation has size %d\n" (llen !next_generation)
    in
    !next_generation
  end
