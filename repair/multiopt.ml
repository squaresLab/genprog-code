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

(***********************************************************************
 * Tournament Selection
 ***********************************************************************)
let tournament_k = ref 2 
let tournament_p = ref 1.00 

let tournament_selection 
    (population : ('a representation) list) 
    (comparison : ('a representation -> 'a representation -> bool))
    (desired : int) 
    (* returns *) : 'a representation list = 
  let p = !tournament_p in 
  assert ( desired >= 0 ) ; 
  assert ( !tournament_k >= 1 ) ; 
  assert ( p >= 0.0 ) ; 
  assert ( p <= 1.0 ) ; 
  assert ( List.length population > 0 ) ; 
  let my_compare a b =
    if comparison a b then
      -1
    else if comparison b a then
      1
    else 
      0
  in 
  let rec select_one () = 
    (* choose k individuals at random *) 
    let lst = random_order population in 
    (* sort them *) 
    let pool = first_nth lst !tournament_k in 
    let sorted = List.sort my_compare pool in 
    let rec walk lst step = match lst with
    | [] -> select_one () 
    | (indiv) :: rest -> 
        let taken = 
          if p >= 1.0 then true
          else begin 
            let required_prob = p *. ((1.0 -. p)**(step)) in 
            Random.float 1.0 <= required_prob 
          end 
        in
        if taken then (indiv) else walk rest (step +. 1.0)
    in
    walk sorted 0.0
  in 
  let answer = ref [] in 
  for i = 1 to desired do
    answer := (select_one ()) :: !answer
  done ;
  !answer

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

let rec ngsa_ii (original : 'a Rep.representation) incoming_pop = begin 
  debug "multiopt: ngsa_ii begins (%d generations left)\n" 
    !Search.generations; 
  let random () = 1 + (Random.int (original#max_atom ()) ) in

  (* Step numbers follow Seshadri's paper *)

  (******
   * 3.1. Population Initialization
   ******)
  let pop = 
    if incoming_pop = [] then begin 
      debug "multiopt: generating initial population" ; 
      let pop = ref [original#copy ()] in (* our GP population *) 
      for i = 1 to pred !Search.popsize do
        (* initialize the population to a bunch of random mutants *) 
        pop := (Search.mutate original random) :: !pop 
      done ;
      !pop
    end else begin 
      debug "multiopt: using previous population" ; 
      incoming_pop
    end 
  in 

  let ngsa_ii_sort pop = 
      debug "multiopt: beginning sort" ; 

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
      ) pop ; 
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
        ) pop ; 
        if Hashtbl.find dominated_by_count p = 0 then begin
          let _, p_values = p#test_case (Single_Fitness) in 
          debug "\t%s (%s) goes to F_1" (p#name ()) 
            (float_array_to_str p_values) ; 
          Hashtbl.add f 1 p 
        end 
      ) pop ;

      let i = ref 1 in 
      while Hashtbl.mem f 1 do
        let set_q = Hashtbl.create 255 in 
        let f_i = Hashtbl.find_all f !i in 
        debug "multiopt: i=%d (%d members)" !i (List.length f_i); 
        List.iter (fun p -> 
          let _, p_values = p#test_case (Single_Fitness) in 
          let s_p = Hashtbl.find_all dominated_by p in 
          debug "multiopt:\t%s (%s), dominated by %d" 
            (p#name ()) 
            (float_array_to_str p_values) 
            (List.length s_p) 
            ; 
          List.iter (fun q ->
            let _, q_values = q#test_case (Single_Fitness) in 
            debug "multiopt:\t\t%s (%s)"
              (q#name ()) 
              (float_array_to_str q_values) 
              ; 
            delta_dominated_by_count q (-1) ; 
            let n_q = Hashtbl.find dominated_by_count q in 
            if n_q = 0 then begin
              Hashtbl.replace rank q (!i + 1) ;
              debug "multiopt:\t\t%s (%s) n_q=0, rank <- %d" 
                (q#name ()) 
                (float_array_to_str q_values) 
                (!i + 1)
                ; 
              Hashtbl.replace set_q q true ;
            end else begin
              debug "multiopt:\t\t%s (%s) n_q now %d" 
                (q#name ()) 
                (float_array_to_str q_values) 
                n_q 
                ; 
            end 
          ) s_p 
        ) f_i ;
        incr i ; 
        debug "multiopt: adding members to f_%d" !i ; 
        Hashtbl.iter (fun q _ ->
          let _, q_values = q#test_case (Single_Fitness) in 
          debug "multiopt:\t%s (%s) to f_%d"   
            (q#name ()) 
            (float_array_to_str q_values) 
            !i
            ; 
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
        done ;
        List.iter (fun p ->
          let _, p_values = p#test_case (Single_Fitness) in 
          debug "multiopt:\t%s (%s) distance = %g"
            (p#name ())
            (float_array_to_str p_values) 
            (Hashtbl.find distance p) 
        ) n ; 
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
      crowded_lessthan, f, distance

  in

  let crowded_lessthan, f, distance = ngsa_ii_sort pop in 

  debug "multiopt: crossover and mutation" ; 

  (* crossover, mutate *) 
  let children = ref [] in 
  for j = 1 to !Search.popsize/2 do
    let parents = tournament_selection pop crowded_lessthan 2 in 
    match parents with
    | [ one ; two ] ->
      begin
        let p = one in let q = two in 
          let _, p_values = p#test_case (Single_Fitness) in 
          let _, q_values = q#test_case (Single_Fitness) in 
          debug "multiopt: %s (%s) and %s (%s)" 
            (p#name ()) (float_array_to_str p_values) 
            (q#name ()) (float_array_to_str q_values) 
      end ;
      let kids = Search.do_cross one two in 
      let kids = List.map (fun kid -> 
        Search.mutate kid random
      ) kids in 
      children := kids @ !children 
    | _ -> debug "multiopt: wrong number of parents (fatal)" 
  done ; 

  debug "multiopt: adding children, sorting" ; 

  let many = pop @ !children in 
  let crowded_lessthan, f, distance = ngsa_ii_sort many in 
  let next_generation = ref [] in 
  let finished = ref false in 
  let front_idx = ref 1 in 
  while not !finished do
    let indivs_in_front = Hashtbl.find_all f !front_idx in 
    let num_indivs = List.length indivs_in_front in 
    debug "multiopt: %d individuals in front %d" num_indivs !front_idx ;
    let have_sofar = List.length !next_generation in 
    finished := have_sofar + num_indivs >= !Search.popsize ; 
    let to_add = 
      if have_sofar + num_indivs <= !Search.popsize then begin
        (* we can just take them all! *) 
        indivs_in_front 
      end else begin
        (* sort by crowding distance *) 
        let sorted = List.sort (fun a b -> 
          compare (Hashtbl.find distance a) (Hashtbl.find distance b)
        ) indivs_in_front in 
        let num_wanted = !Search.popsize - have_sofar in 
        let selected = first_nth sorted num_wanted in 
        selected 
      end 
    in
    List.iter (fun p ->
      let _, p_values = p#test_case (Single_Fitness) in 
      debug "multiopt:\t%s (%s) to next_generation"
        (p#name ())
        (float_array_to_str p_values) 
    ) to_add ;
    next_generation := to_add @ !next_generation 
  done ;

  debug "multiopt: next generation has size %d" 
    (List.length !next_generation) ;

  decr Search.generations ; 
  if !Search.generations = 0 then 
    !next_generation 
  else
    ngsa_ii original !next_generation 
end 


