open Global
open Rep

let crossp = ref 0.5
let popsize = ref 40
let incoming_pop = ref ""
let tournament_k = ref 2
let crossover = ref "one"

let _ = 
  options := !options @ [
  "--popsize", Arg.Set_int popsize, "X variant population size";
  "--crossover", Arg.Set_string crossover, "X use X as crossover [one,back,subset,flat]";
  "--crossp", Arg.Set_float crossp, "X use X as crossover rate";
  "--tournament-size", Arg.Set_int tournament_k, "X use x as tournament size";
  ]

module type Population =
sig
  type individual 
  type t
  val selection : t -> int -> individual
end

module GPPopulation =
struct

  type ('a,'b) individual = ('a,'b) Rep.representation
  type ('a,'b) t = ('a,'b) individual list

  (***********************************************************************
   * Tournament Selection
   ***********************************************************************)
  let tournament_p = ref 1.00

  let tournament_selection population desired =
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
		let sorted = List.sort 
		  (fun i i'  -> 
			let f = get_opt (i#fitness ()) in
			let f' = get_opt (i'#fitness ()) in
			  compare f' f) pool in
		let rec walk lst step = match lst with
		  | [] -> select_one ()
		  | indiv :: rest ->
			let taken =
			  if p >= 1.0 then true
			  else begin
				let required_prob = p *. ((1.0 -. p)**(step)) in
				  Random.float 1.0 <= required_prob
			  end
			in
			  if taken then indiv else walk rest (step +. 1.0)
		in
		  walk sorted 0.0
	  in
	  let answer = ref [] in
		for i = 1 to desired do
		  answer := (select_one ()) :: !answer
		done ;
		!answer

  (* Selection -- currently we have only tournament selection implemented,
   * but if/when we add others, we choose between them here. *)
  let selection population desired = tournament_selection population desired


  (***********************************************************************
   * Crossover
   *
   * We currently have three approaches to crossover: a standard "one-point"
   * crossover, "patch subset" crossover and "flat" crossover.
   ***********************************************************************)

  (* this implements the AST/WP crossover behavior on the patch
	 representation.  I don't like keeping it around, since the point of
	 refactoring is to decouple the evolutionary behavior from the
	 representation.  I'm still thinking about it *)
  let crossover_patch_old_behavior ?(test = 0)
      (original :('a,'b) Rep.representation)
      (variant1 :('a,'b) Rep.representation)
      (variant2 :('a,'b) Rep.representation)
	  : (('a,'b) representation) list = 
	let h1 = variant1#get_history () in
	let h2 = variant2#get_history () in 
	let wp = lmap fst (variant1#get_faulty_atoms ()) in
	let point = if test=0 then Random.int (llen wp) else test in
	let first_half,second_half = split_nth wp point in
	let c_one = original#copy () in
	let c_two = original#copy () in
	let h11, h12 = 
	  List.partition
		(fun edit ->
		  match edit with
		  | Delete(num)
		  | Append(num, _) 
		  | Swap(num,_) 
		  | Replace(num,_) -> List.mem num first_half
		  (* CLG FIXME: add a tostring here so I can see what the edit is. *)
		  | _ -> abort "unexpected edit in edit history in patch_old_behavior crossover") h1
	in
	let h21, h22 = 
	  List.partition
		(fun edit ->
		  match edit with
		  | Delete(num)
		  | Append(num, _) 
		  | Swap(num,_) 
		  | Replace(num,_)  -> 
			List.mem num first_half
		(* CLG FIXME: add a tostring here so I can see what the edit is. *)
		  | _ -> abort "unexpected edit in edit history in patch_old_behavior crossover") h2
	in
	let new_h1 = h11 @ h22 in
	let new_h2 = h21 @ h12 in
	  c_one#set_history new_h1 ;
	  c_two#set_history new_h2 ;
	  [ c_one ; c_two ]


(* Patch Subset Crossover *)
  let crossover_patch_subset
      (original :('a,'b) Rep.representation)
      (variant1 :('a,'b) Rep.representation)
      (variant2 :('a,'b) Rep.representation)
	  : (('a,'b) representation) list =
	let g1 = variant1#get_genome () in
	let g2 = variant1#get_genome () in
	let new_g1 = List.fold_left (fun acc elt ->
      if probability !crossp then acc @ [elt] else acc
    ) [] (g1 @ g2) in
	let new_g2 = List.fold_left (fun acc elt ->
      if probability !crossp then acc @ [elt] else acc
    ) [] (g2 @ g1) in
	let c_one = original#copy () in
	let c_two = original#copy () in
	  c_one#set_genome new_g1 ;
	  c_two#set_genome new_g2 ;
	  [ c_one ; c_two ]
		
  (* One point crossover *)
  let crossover_one_point ?(test = 0)
      (original :('a,'b) Rep.representation)
      (variant1 :('a,'b) Rep.representation)
      (variant2 :('a,'b) Rep.representation)
	  : (('a,'b) representation) list =
	let child1 = original#copy () in
	let child2 = original#copy () in
	let point1,point2 = 
	  if test = 0 then test,test 
	  else 
		begin
		  let legal1,interfun1 = variant1#available_crossover_points () in
		  let legal2,interfun2 = variant2#available_crossover_points () in
		  (* Gods this is so hideous *)
		  let legal1' = interfun1 legal1 legal2 in
		  let legal2' = interfun2 legal2 legal1 in
			if not variant1#variable_length then 
			  let rand = List.hd (random_order legal1') in
				rand,rand
			else 
			  begin
			  let rand1 = List.hd (random_order legal1') in
			  let rand2 = List.hd (random_order legal2') in
				rand1,rand2
			  end
		end
	in
	let g1a,g1b = split_nth (variant1#get_genome()) point1 in
	let g2a,g2b = split_nth (variant2#get_genome()) point2 in
      child1#add_history (Crossover((Some point1),None)) ;
      child2#add_history (Crossover(None,(Some point2))) ;
	  child1#set_genome (g1a@g2b);
	  (* do we care that the history info is destroyed for patch representation here? *)
	  child2#set_genome (g2a@g1b);
		[child1;child2]

  let do_cross ?(test = 0)
      (original :('a,'b) Rep.representation)
      (variant1 :('a,'b) Rep.representation)
      (variant2 :('a,'b) Rep.representation)
	  : (('a,'b) representation) list =
	match !crossover with
	(* CLG: flat crossover is now implemented by default on elfrep based on
	   available_crossover_points *)
	| "flat" | "flatten"
	| "one" | "patch-one-point" -> crossover_one_point ~test original variant1 variant2

	| "back" -> crossover_one_point ~test original variant1 original
	| "uniform" -> crossover_patch_subset original variant1 variant2 
  (* CLG: I really want to nuke backwards compatibility on this one in terms of
	 the naming scheme but maybe I'll wait till we're all on the same page,
	 sigh.  I also don't love that patch has it's own crossover implementations;
	 I feel like if crossover is going to be representation-specific it should
	 be folded into rep somehow *)
	| "patch"
	| "subset" -> 
	  debug "WARNING: CROSSOVER: use uniform instead.";
	  crossover_patch_subset original variant1 variant2
	| "patch-old" -> crossover_patch_old_behavior ~test original variant1 variant2 
	| x -> abort "unknown --crossover %s\n" x

  let crossover population original =
	let mating_list = random_order population in
  (* should we cross an individual? *)
	let maybe_cross () = if (Random.float 1.0) <= !crossp then true else false in
	let output = ref [] in
	let half = (List.length mating_list) / 2 in
	  for it = 0 to (half - 1) do
		let parent1 = List.nth mating_list it in
		let parent2 = List.nth mating_list (half + it) in
	      if maybe_cross () then
			output := (do_cross original parent1 parent2) @ !output
	      else
			output := parent1 :: parent2 :: !output
	  done ;
	  !output

end
