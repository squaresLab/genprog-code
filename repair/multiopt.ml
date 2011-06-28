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
 *
 *)
open Printf
open Global
open Fitness
open Rep

let minimize = ref false 
let no_inf = ref false 
let num_objectives = ref 2  (* number of objectives *) 
let _ = 
  options := !options @ [
  "--multiopt-minimize", Arg.Set minimize, " minimize multiopt objective";
  "--multiopt-no-inf", Arg.Set no_inf, " avoid infinite values";
  "--num-objectives", Arg.Set_int num_objectives, "X expect X objective values";
] 

let evaluate (rep : 'a representation) = 
  (*let _ = Gc.compact() in  *)
  let _, values = rep#test_case (Single_Fitness) in 
  if Array.length values < !num_objectives then begin
    (* failed to compile *) 
    if !minimize then 
      Array.make !num_objectives infinity 
    else 
      Array.make !num_objectives neg_infinity 
  end else values 


let is_pessimal arr = 
    if !minimize then 
      arr = Array.make !num_objectives infinity 
    else 
      arr = Array.make !num_objectives neg_infinity 

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


let dominates (p: 'a Rep.representation) 
              (q: 'a Rep.representation) : bool = begin
  let p_values = evaluate p in 
  let q_values = evaluate q in 
  assert(Array.length p_values = Array.length q_values) ; 
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
end 

let rephash_create = Hashtbl.create
let rephash_replace h x y = Hashtbl.replace h (x#name ()) (y) 
let rephash_add h x y = Hashtbl.add h (x#name ()) (y) 
let rephash_find h x = Hashtbl.find h (x#name ())  
let rephash_find_all h x = Hashtbl.find_all h (x#name ())  
let rephash_mem h x = Hashtbl.mem h (x#name ())  

let rec ngsa_ii (original : 'a Rep.representation) incoming_pop = begin 
  debug "multiopt: ngsa_ii begins (%d generations left)\n" 
    !Search.generations; 

  let current = ref incoming_pop in 

  for gen = 1 to !Search.generations do
    debug "multiopt: ngsa_ii generation %d begins\n" gen ; 
    let is_last_generation = gen = !Search.generations in 
    let next_generation = ngsa_ii_internal original !current 
      ~is_last_generation in 
    let filename = Printf.sprintf "generation-%04d.list" gen in 
    debug "multiopt: printing %s\n" filename ; 
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
  debug "multiopt: ngsa_ii end\n" ;
  (Search.calculate_fitness !current)

end 
and ngsa_ii_internal 
    ?(is_last_generation=false) 
    (original : 'a Rep.representation) 
    incoming_pop 
    = begin 
  let random atom_set = 
    let elts = List.map fst (WeightSet.elements atom_set) in 
    let size = List.length elts in 
    List.nth elts (Random.int size) 
  in 

  (* Step numbers follow Seshadri's paper *)

  (******
   * 3.1. Population Initialization
   ******)
  let pop = 
    if incoming_pop = [] then begin 
      debug "multiopt: generating initial population\n" ; 
      let pop = ref [original#copy ()] in (* our GP population *) 
      for i = 1 to pred !Search.popsize do
        (* initialize the population to a bunch of random mutants *) 
        pop := (Search.mutate original random) :: !pop 
      done ;
      !pop
    end else begin 
      debug "multiopt: using previous population\n" ; 
      incoming_pop
    end 
  in 

  let ngsa_ii_sort pop = 
      debug "multiopt: beginning sort\n" ; 
      Gc.compact();

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

      debug "multiopt: computing f_max and f_min %d \n"  (List.length pop); 

      List.iter (fun p ->
        let p_values = evaluate p in 
        Array.iteri (fun m fval ->
          adjust_f_max m fval ; 
          adjust_f_min m fval ; 
        ) p_values ;
      ) pop ; 
      for m = 0 to pred !num_objectives do
        debug "multiopt: %g <= objective %d <= %g\n" 
          (Hashtbl.find f_min m) 
          m
          (Hashtbl.find f_max m)
      done ;

      (******
       * 3.2. Non-Dominated Sort
       ******)
      debug "multiopt: first non-dominated sort begins\n" ; 
      let dominated_by = rephash_create 255 in 
      let dominated_by_count = rephash_create 255 in 
      let rank = rephash_create 255 in 
      let delta_dominated_by_count (p:'a Rep.representation) dx =
        let sofar = rephash_find dominated_by_count p in
        rephash_replace dominated_by_count p (sofar + dx)
      in 
      let f = Hashtbl.create 255 in 

      List.iter (fun (p : 'a Rep.representation) ->
        rephash_replace dominated_by_count p 0;
        List.iter (fun (q : 'a Rep.representation)->
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
          (*
          let _, p_values = p#test_case (Single_Fitness) in 
          let _, q_values = q#test_case (Single_Fitness) in 
          debug "\t\t%s %s %s\n" 
            (float_array_to_str p_values) str
            (float_array_to_str q_values) 
            *)
        ) pop ; 
        if rephash_find dominated_by_count p = 0 then begin
          (*
          let _, p_values = p#test_case (Single_Fitness) in 
          debug "\t%s (%s) goes to F_1\n" (p#name ()) 
            (float_array_to_str p_values) ; 
            *) 
          Hashtbl.add f 1 p ;
          rephash_replace rank p 1 ; 
        end 
      ) pop ;

      let i = ref 1 in 
      while Hashtbl.mem f !i do
        let set_q_names = Hashtbl.create 255 in 
        let set_q_reps = ref [] in 
        let f_i = Hashtbl.find_all f !i in 
        debug "multiopt: front i=%d (%d members)\n" !i (List.length f_i); 
        List.iter (fun p -> 
          let s_p = rephash_find_all dominated_by p in 
          (*
          let _, p_values = p#test_case (Single_Fitness) in 
          debug "multiopt:\t%s (%s), dominates %d\n" 
            (p#name ()) 
            (float_array_to_str p_values) 
            (List.length s_p) 
            ; 
            *) 
          List.iter (fun q ->
            (*
            let _, q_values = q#test_case (Single_Fitness) in 
            debug "multiopt:\t\t%s (%s)\n"
              (q#name ()) 
              (float_array_to_str q_values) 
              ; 
              *) 
            delta_dominated_by_count q (-1) ; 
            let n_q = rephash_find dominated_by_count q in 
            if n_q = 0 then begin
              rephash_replace rank q (!i + 1) ;
              (*
              debug "multiopt:\t\t%s (%s) n_q=0, rank <- %d\n" 
                (q#name ()) 
                (float_array_to_str q_values) 
                (!i + 1)
                ; 
                *) 
              if not (Hashtbl.mem set_q_names (q#name ())) then begin
                Hashtbl.add set_q_names (q#name ()) true ;
                set_q_reps := q :: !set_q_reps 
              end
            end else begin
              (*
              debug "multiopt:\t\t%s (%s) n_q now %d\n" 
                (q#name ()) 
                (float_array_to_str q_values) 
                n_q 
                ; 
                *) 
            end 
          ) s_p 
        ) f_i ;
        incr i ; 
        (*
        debug "multiopt: adding members to f_%d\n" !i ; 
        *) 
        List.iter (fun q ->
        (*
          let _, q_values = q#test_case (Single_Fitness) in 
          debug "multiopt:\t%s (%s) to f_%d\n"   
            (q#name ()) 
            (float_array_to_str q_values) 
            !i
            ; 
            *) 
          Hashtbl.add f !i q
        ) !set_q_reps
      done ;
      let i_max = !i in 
      List.iter (fun p ->
        if not (rephash_mem rank p) then begin
          rephash_replace rank p i_max ;
          let p_values = evaluate p in 
          if not (is_pessimal p_values) then begin 
            let n_p = rephash_find dominated_by_count p in 
            debug "multiopt: NO RANK for %s %s n_p=%d: setting to %d\n" 
              (p#name ()) 
              (float_array_to_str p_values) 
              n_p 
              i_max ;
          end 
        end
      ) pop ;

      (******
       * 3.3. Crowding Distance
       ******)
      let distance = rephash_create 255 in 
      let add_distance p delta = 
        let sofar = rephash_find distance p in
        rephash_replace distance p (sofar +. delta)
      in 
      debug "multiopt: crowding distance calculation\n" ; 
      for i = 1 to pred i_max do
        let n = Hashtbl.find_all f i in
        List.iter (fun p ->
          rephash_replace distance p 0.0 ;
        ) n ;
        (*
        # List.sort compare [ 3 ; 1 ; 5 ] ;;
        # - : int list = [1; 3; 5]
        *) 
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
        done ;
        (*
        List.iter (fun p ->
          let _, p_values = p#test_case (Single_Fitness) in 
          debug "multiopt:\t(%s)  rank = %d  distance = %g\n"
            (* (p#name ()) *)
            (float_array_to_str p_values) 
            (rephash_find rank p) 
            (rephash_find distance p) 
        ) n ; 
        *) 
      done ;

      (******
       * 3.4. Selection
       ******)
      debug "multiopt: computing selection operator\n" ; 
      let crowded_lessthan p q = 
        (* "An individual is selected if the rank is lesser than the other or
        if crowding distance is greater than the other" *)
        let p_rank = rephash_find rank p in 
        let q_rank = rephash_find rank q in 
        if p_rank < q_rank then
          true
        else if p_rank = q_rank then begin
          let p_dist = rephash_find distance p in 
          let q_dist = rephash_find distance q in 
          compare p_dist q_dist = 1 
        end else false 
      in 
      crowded_lessthan, f, distance

  in

  let crowded_lessthan, f, distance = ngsa_ii_sort pop in 

  debug "multiopt: crossover and mutation\n" ; 

  (* crossover, mutate *) 
  let children = ref [] in 
  for j = 1 to !Search.popsize do
    let parents = tournament_selection pop crowded_lessthan 2 in 
    match parents with
    | [ one ; two ] ->
    (*
      begin
        let p = one in let q = two in 
          let _, p_values = p#test_case (Single_Fitness) in 
          let _, q_values = q#test_case (Single_Fitness) in 
          debug "multiopt: %s (%s) and %s (%s)\n" 
            (p#name ()) (float_array_to_str p_values) 
            (q#name ()) (float_array_to_str q_values) 
      end ;
      *) 
      let kids = Search.do_cross one two in 
      let kids = List.map (fun kid -> 
        Search.mutate kid random
      ) kids in 
      children := kids @ !children 
    | _ -> debug "multiopt: wrong number of parents (fatal)\n" 
  done ; 

  debug "multiopt: adding children, sorting\n" ; 

  let many = pop @ !children in 
  let crowded_lessthan, f, distance = ngsa_ii_sort many in 

  if is_last_generation then begin 
    let f_1 = Hashtbl.find_all f 1 in
    let i = ref 0 in 
    debug "\nmultiopt: %d in final generation pareto front:\n(does not include all variants considered)\n\n" (List.length f_1) ;
    let f_1 = List.sort (fun p q ->
      let p_values = evaluate p in 
      let q_values = evaluate q in 
      compare p_values q_values
    ) f_1 in 
    List.iter (fun p ->
      let p_values = evaluate p in 
      let name = Printf.sprintf "pareto-%06d.%s" !i 
        (!Global.extension) in 
      let fname = Printf.sprintf "pareto-%06d.fitness" !i in 
      incr i; 
      p#output_source name ; 
      let fout = open_out fname in 
      output_string fout (float_array_to_str p_values) ;
      output_string fout "\n" ; 
      close_out fout ; 
      debug "%s %s\n %s\n\n" name 
        (float_array_to_str p_values) 
        (p#name ()) 
    ) f_1 ; 
    f_1 
  end else begin 

    let next_generation = ref [] in 
    let finished = ref false in 
    let front_idx = ref 1 in 
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
      debug "multiopt: %d individuals in front %d\n" num_indivs !front_idx ;
      let have_sofar = List.length !next_generation in 
      finished := have_sofar + num_indivs >= !Search.popsize ; 
      let to_add = 
        if have_sofar + num_indivs <= !Search.popsize then begin
          (* we can just take them all! *) 
          indivs_in_front 
        end else begin
          (* sort by crowding distance *) 
          let sorted = List.sort (fun a b -> 
            compare (rephash_find distance a) (rephash_find distance b)
          ) indivs_in_front in 
          let num_wanted = !Search.popsize - have_sofar in 
          let selected = first_nth sorted num_wanted in 
          selected 
        end 
      in
      (*
      List.iter (fun p ->
        let _, p_values = p#test_case (Single_Fitness) in 
        debug "multiopt:\t%s (%s) to next_generation\n"
          (p#name ())
          (float_array_to_str p_values) 
      ) to_add ;
      *) 
      incr front_idx ; 
      if not !finished && num_indivs = 0 then begin
        let wanted = !Search.popsize - have_sofar in 
        debug "multiopt: including %d copies of original\n" wanted ;
        for i = 1 to wanted do
          next_generation := (original#copy ()) :: !next_generation 
        done ;
        finished := true 
      end ; 
      next_generation := to_add @ !next_generation 
    done ;

    debug "multiopt: next generation has size %d\n" 
      (List.length !next_generation) ;
    !next_generation 

  end 
end 


