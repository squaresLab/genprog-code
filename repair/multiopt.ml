(* 
 * Program Repair Prototype (v2) 
 *
 * Multi-Objective Search Strategies
 *
 * Currently:
 *   NSGA-II
 *
 * Based on: 
 * http://www.mathworks.com/matlabcentral/fileexchange/10429-nsga-ii-a-multi-objective-optimization-algorithm
 *)
open Printf
open Global
open Fitness
open Rep

(*************************************************************************
 *************************************************************************
                                  NGSA-II
 *************************************************************************
 *************************************************************************)

let num_objectives = ref 0  (* number of objectives *) 

let dominates (p: 'a Rep.representation) 
              (q: 'a Rep.representation) : bool = begin
  let _, p_values = p#test_case (Single_Fitness) in 
  let _, q_values = q#test_case (Single_Fitness) in 
  assert(Array.length p_values = Array.length q_values) ; 
  num_objectives := Array.length p_values ; 
  let p_dominates = ref true in 
  for i = 0 to pred (Array.length p_values) do
    if p_values.(i) <= q_values.(i) then 
      p_dominates := false 
  done ;
  !p_dominates
end 

let ngsa_ii (original : 'a Rep.representation) incoming_pop = begin 
  debug "multiopt: ngsa_ii begins\n" ; 
  if incoming_pop <> [] then begin
    debug "multiopt: incoming population IGNORED\n" ; 
  end ; 

  (* Step numbers follow Seshadri's paper *)

  (******
   * 3.1. Population Initialization
   ******)
  debug "multiopt: generating initial population" ; 
  let pop = ref [original#copy ()] in (* our GP population *) 
  let random () = 
    1 + (Random.int (original#max_atom ()) ) in
  for i = 1 to pred !Search.popsize do
    (* initialize the population to a bunch of random mutants *) 
    pop := (Search.mutate original random) :: !pop 
  done ;

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

  debug "multiopt: computing f_max and f_min" ; 

  List.iter (fun p ->
    let _, p_values = p#test_case (Single_Fitness) in 
    Array.iteri (fun m fval ->
      adjust_f_max m fval ; 
      adjust_f_min m fval ; 
    ) p_values 
  ) !pop ; 
  for m = 0 to pred !num_objectives do
    debug "multiopt: %g <= objective %d <= %g" 
      (Hashtbl.find f_min m) 
      m
      (Hashtbl.find f_max m)
  done ;

  (******
   * 3.2. Non-Dominated Sort
   ******)
  debug "multiopt: first non-dominated sort begins" ; 
  let dominated_by = Hashtbl.create 255 in 
  let dominated_by_count = Hashtbl.create 255 in 
  let rank = Hashtbl.create 255 in 
  let delta_dominated_by_count (p:'a Rep.representation) dx =
    let sofar = Hashtbl.find dominated_by_count p in
    Hashtbl.replace dominated_by_count p (sofar + dx)
  in 
  let f = Hashtbl.create 255 in 

  List.iter (fun (p : 'a Rep.representation) ->
    Hashtbl.replace dominated_by_count p 0;
    List.iter (fun (q : 'a Rep.representation)->
      if dominates p q then 
        Hashtbl.add dominated_by p q
      else if dominates q p then
        delta_dominated_by_count p 1 
    ) !pop ; 
    if Hashtbl.find dominated_by_count p = 0 then begin
      debug "\t%s goes to F_1" (p#name ()) ; 
      Hashtbl.add f 1 p 
    end 
  ) !pop ;

  let i = ref 1 in 
  while Hashtbl.mem f 1 do
    let set_q = Hashtbl.create 255 in 
    let f_i = Hashtbl.find_all f !i in 
    debug "multiopt: i=%d (%d members)" !i (List.length f_i); 
    List.iter (fun p -> 
      let s_p = Hashtbl.find_all dominated_by p in 
      List.iter (fun q ->
        delta_dominated_by_count q (-1) ; 
        let n_q = Hashtbl.find dominated_by_count q in 
        if n_q = 0 then begin
          Hashtbl.replace rank q (!i + 1) ;
          Hashtbl.replace set_q q true ;
        end 
      ) s_p 
    ) f_i ;
    incr i ; 
    Hashtbl.iter (fun q _ ->
      Hashtbl.add f !i q
    ) set_q 
  done ;
  let i_max = !i in 

  (******
   * 3.3. Crowding Distance
   ******)
  let distance = Hashtbl.create 255 in 
  let add_distance p delta = 
    let sofar = Hashtbl.find distance p in
    Hashtbl.replace distance p (sofar +. delta)
  in 
  debug "multiopt: crowding distance calculation" ; 
  for i = 1 to pred i_max do
    let n = Hashtbl.find_all f i in
    List.iter (fun p ->
      Hashtbl.replace distance p 0.0 ;
    ) n ;
    (*
    # List.sort compare [ 3 ; 1 ; 5 ] ;;
    # - : int list = [1; 3; 5]
    *) 
    for m = 0 to pred !num_objectives do

      let i_set = List.sort (fun a b -> 
        let _, a_values = a#test_case (Single_Fitness) in 
        let _, b_values = b#test_case (Single_Fitness) in 
        compare a_values.(m) b_values.(m) 
      ) n in 
      let i_array = Array.of_list i_set in 
      let i_size = Array.length i_array in
      assert(i_size > 0); 
      Hashtbl.replace distance i_array.(0) infinity ; 
      Hashtbl.replace distance i_array.(pred i_size) infinity ; 
      for k = 1 to pred (pred i_size) do
        let k_plus_1 = i_array.(k+1) in 
        let k_minus_1 = i_array.(k+1) in 
        let _, k_plus_1_values = k_plus_1#test_case (Single_Fitness) in 
        let _, k_minus_1_values = k_minus_1#test_case (Single_Fitness) in 
        let f_max_m = Hashtbl.find f_max m in 
        let f_min_m = Hashtbl.find f_min m in 
        add_distance i_array.(k) 
          ((k_plus_1_values.(m) -. k_minus_1_values.(m) ) /.
          (f_max_m -. f_min_m))
      done 
    done 
  done ;

  (******
   * 3.4. Selection
   ******)
  debug "multiopt: selection" ; 
  let crowded_lessthan p q = 
    (* "An individual is selected if the rank is lesser than the other or
    if crowding distance is greater than the other" *)
    let p_rank = Hashtbl.find rank p in 
    let q_rank = Hashtbl.find rank q in 
    if p_rank < q_rank then
      true
    else if p_rank = q_rank then begin
      let p_dist = Hashtbl.find distance p in 
      let q_dist = Hashtbl.find distance q in 
      p_dist > q_dist 
    end else false 
  in 

  (* crossover, mutate *) 
  (* then re-sort the whole thing and select N:

"The new generation is filled by each front subsequently until the
population size exceeds the current population size. If by adding all the
individuals in front Fj the population exceeds N then individuals in front
Fj are selected based on their crowding distance in the descending order
until the population size is N .  And hence the process repeats to generate
the subsequent generations."
*) 



  ()

end 


