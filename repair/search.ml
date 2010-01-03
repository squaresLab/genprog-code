(* 
 * Program Repair Prototype (v2) 
 *
 * Search Strategies include: 
 *  -> Brute Force (e.g., all distance-one edits)
 *  -> Genetic Programming (e.g., ICSE'09)
 *     => delete, append and swap based on fault localization
 *     => crossover: none, one point, two point, uniform, ...
 *)
open Printf
open Global
open Rep

let note_success (rep : Rep.representation) =
  let name = rep#name () in 
  debug "\nRepair Found: %s\n" name ;
  rep#output_source "repair.c" ;
  exit 1 

let weight_compare (stmt,prob) (stmt',prob') =
    if prob = prob' then compare stmt stmt' 
    else compare prob' prob 

(*************************************************************************
 *************************************************************************
                     Brute Force: Try All Single Edits
 *************************************************************************
 *************************************************************************)

exception Test_Failed
(* As an optimization, brute force gives up on a variant as soon
 * as that variant fails a test case. *) 
let test_to_first_failure (rep : Rep.representation) = 
  let count = ref 0 in 
  try
    for i = 1 to !neg_tests do
      if not (rep#test_case (Negative i)) then raise Test_Failed
      else incr count 
    done ;
    for i = 1 to !pos_tests do
      if not (rep#test_case (Positive i)) then raise Test_Failed
      else incr count 
    done ;
    note_success rep 

  with Test_Failed -> 
    debug "\t%3d %s\n" !count  (rep#name ()) 

let brute_force_1 (original : Rep.representation) incoming_pop = 
  debug "search: brute_force_1 begins\n" ; 
  if incoming_pop <> [] then begin
    debug "search: incoming population IGNORED\n" ; 
  end ; 
  let localization = original#get_localization () in 
  let localization = List.sort weight_compare localization in 
  let full_localization = original#get_full_localization () in 
  let full_localization = List.sort weight_compare full_localization in 

  let worklist = ref [] in 

  (* first, try all single deletions *) 
  List.iter (fun (atom,weight) ->
    let myfun () = 
      let rep = original#copy () in 
      rep#delete atom; 
      rep
    in 
    worklist := (myfun,weight) :: !worklist ; 
  ) localization ; 

  (* second, try all single appends *) 
  List.iter (fun (dest,w1) ->
    List.iter (fun (src,w2) -> 
      let myfun () = 
        let rep = original#copy () in 
        rep#append dest src; 
        rep 
      in 
      worklist := (myfun, w1 *. w2 *. 0.9) :: !worklist ; 
    ) full_localization 
  ) localization ;  

  (* third, try all single swaps *) 
  List.iter (fun (dest,w1) ->
    List.iter (fun (src,w2) -> 
      if dest <> src then begin (* swap X with X = no-op *) 
        let myfun () = 
          let rep = original#copy () in 
          rep#swap dest src;
          rep
        in 
        worklist := (myfun, w1 *. w2 *. 0.8) :: !worklist ; 
      end 
    ) localization 
  ) localization ;  

  let worklist = List.sort 
    (fun (m,w) (m',w') -> compare w' w) !worklist in 
  let howmany = List.length worklist in 
  let sofar = ref 1 in 
  List.iter (fun (thunk,w) ->
    debug "\tvariant %d/%d (weight %g)\n" !sofar howmany w ;
    let rep = thunk () in 
    incr sofar ;
    test_to_first_failure rep 
  ) worklist ; 

  debug "search: brute_force_1 ends\n" ; 
  [] 

(*************************************************************************
 *************************************************************************
                          Basic Genetic Algorithm
 *************************************************************************
 *************************************************************************)

let generations = ref 10
let negative_test_weight = ref 2.0 
let popsize = ref 40 
let _ = 
  options := !options @ [
  "--generations", Arg.Set_int generations, "X use X genetic algorithm generations";
  "--popsize", Arg.Set_int popsize, "X variant population size";
  "--negative_test_weight", Arg.Set_float negative_test_weight, "X negative tests fitness factor";
] 

let mutate (variant : Rep.representation) weighted random = 
  let result = variant#copy () in 
  (match Random.int 3 with
  | 0 -> result#delete (weighted ())  
  | 1 -> result#append (weighted ()) (random ()) 
  | _ -> result#swap (weighted ()) (random ()) 
  ) ;
  result 

let test_case_fitness (rep : Rep.representation ) = 
  let fac = (float !pos_tests) *. !negative_test_weight /. 
            (float !neg_tests) in 
  let fitness = ref 0.0 in 
  let failed = ref false in
  for i = 1 to !pos_tests do
    if (rep#test_case (Positive i)) then fitness := !fitness +. 1.0 
    else failed := true 
  done ;
  for i = 1 to !neg_tests do
    if (rep#test_case (Negative i)) then fitness := !fitness +. fac
    else failed := true 
  done ;
  debug "\t%3g %s\n" !fitness (rep#name ()) ;
  if not !failed then begin
    note_success rep 
  end ; 
  !fitness 

(***********************************************************************
 * Tournament Selection
 ***********************************************************************)
let tournament_k = ref 2 
let tournament_p = ref 1.00 

let tournament_selection (population : (representation * float) list) 
           (desired : int) 
           (* returns *) : representation list = 
  let p = !tournament_p in 
  assert ( desired >= 0 ) ; 
  assert ( !tournament_k >= 1 ) ; 
  assert ( p >= 0.0 ) ; 
  assert ( p <= 1.0 ) ; 
  assert ( List.length population > 0 ) ; 
  let rec select_one () = 
    (* choose k individuals at random *) 
    let lst = random_order population in 
    (* sort them *) 
    let pool = first_nth lst !tournament_k in 
    let sorted = List.sort (fun (_,f) (_,f') -> compare f' f) pool in 
    let rec walk lst step = match lst with
    | [] -> select_one () 
    | (indiv,fit) :: rest -> 
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

let selection (population : (representation * float) list) 
           (desired : int) 
           (* returns *) : representation list = 
  tournament_selection population desired 

let genetic_algorithm (original : Rep.representation) incoming_pop = 
  debug "search: genetic algorithm begins\n" ; 
  let localization = original#get_localization () in 
  let localization = List.sort weight_compare localization in 
  let full_localization = original#get_full_localization () in 
  let full_localization = List.sort weight_compare full_localization in 
  let localization_total_weight = 
    List.fold_left (fun acc (_,prob) -> acc +. prob) 0. localization 
  in 
  let rec choose_from_weighted_list chosen_index lst = match lst with
  | [] -> failwith "localization error"  
  | (sid,prob) :: tl -> if chosen_index <= prob then sid
                  else choose_from_weighted_list (chosen_index -. prob) tl
  in 
  (* choose a stmt weighted by the localization *) 
  let weighted () = choose_from_weighted_list 
      (Random.float localization_total_weight) full_localization
  in
  (* choose a stmt uniformly at random *) 
  let random () = 
    1 + (Random.int (original#max_atom ()) )
  in
  let fitness_population pop = 
    List.map (fun variant -> (variant, test_case_fitness variant)) pop
  in 

  let pop = ref [] in
  for i = 1 to pred !popsize do
    pop := (mutate original weighted random) :: !pop 
  done ;
  pop := (original#copy ()) :: !pop ;
  for gen = 1 to !generations do
    debug "search: generation %d\n" gen ; 
    let fitpop = fitness_population !pop in 
    let newpop = ref [] in 
    for i = 1 to !popsize do
      match selection fitpop 1 with
      | [one] -> newpop := (mutate one weighted random) :: !newpop
      | _ -> failwith "selection error" 
    done ;
    (* TODO: Should include crossover *) 
    let newpopfit = fitness_population !newpop in 
    pop := selection (fitpop @ newpopfit) !popsize 
  done ;
  debug "search: genetic algorithm ends\n" ;
  !pop 
