(* Given a C program and a path through that program, modify the program
 * at random (say, using genetic algorithms). *)
open Printf
open Cil

let file_size name =
  try 
    let stats = Unix.stat name in
    stats.Unix.st_size 
  with _ -> 0 

let copy_file file = 
  Stats2.time "copy_file" (fun () -> 
  { file with globals = 
    List.map (fun g ->
      match g with
      | GFun(fd,loc) -> 
          let newfd = copyFunction fd fd.svar.vname in
          GFun(newfd,loc)
      | x -> x 
    ) file.globals ; 
  } 
  ) () 

(* stochastic universal sampling *) 
let sample population desired = 
  let total = List.fold_left (fun acc (_,fitness) ->  
    acc +. fitness) 0. population in 
  (if total <= 0. then failwith "selection: total <= 0") ; 
  let normalized = List.map (fun (a,fitness) ->
    a, fitness /. total
  ) population in 
  let sofar = ref 0.0 in 
  let accumulated = List.map (fun (a,normalized) ->
    let res = normalized +. !sofar in
    sofar := !sofar +. normalized ;
    (a,res)
  ) normalized in 
  let distance_between_pointers = 1.0 /. (float_of_int desired) in 
  let offset = Random.float distance_between_pointers in 
  let result = ref [] in 
  for i = 0 to pred desired do
    let marker = offset +. ((float_of_int i) *. distance_between_pointers) in 
    let rec walk lst = match lst with
    | [] -> 
      printf "desired = %d\n" desired ; 
      printf "distance_between_pointers = %g\n" distance_between_pointers ;
      printf "offset = %g\n" offset ;
      printf "i = %d\n" i ; 
      printf "marker = %g\n" marker ;
      failwith "selection problem" 
    | (elt, acc) :: rest -> 
      if acc > marker then result := elt :: !result 
      else walk rest 
    in
    walk accumulated
  done ;
  !result 

let copy (x : 'a) = 
  let str = Marshal.to_string x [] in
  (Marshal.from_string str 0 : 'a) 

class swapVisitor file ht to_swap = object
  inherit nopCilVisitor
  method vstmt s = ChangeDoChildrenPost(s, fun s ->
      if Hashtbl.mem to_swap s.sid then begin
        let swap_with = Hashtbl.find to_swap s.sid in 
        let copy = copy swap_with in
        { s with skind = copy } 
      end else s 
    ) 
end 

class appVisitor file ht to_swap = object
  inherit nopCilVisitor
  method vstmt s = ChangeDoChildrenPost(s, fun s ->
      if Hashtbl.mem to_swap s.sid then begin
        let swap_with = Hashtbl.find to_swap s.sid in 
        let copy = copy swap_with in
        let block = {
          battrs = [] ;
          bstmts = [ s ; { s with skind = copy ; } ];
        } in
        { s with skind = Block(block) } 
      end else s 
    ) 
end 

let mutation_chance = ref 0.2 

let mutation (file,ht,count,path) prob = 
  (* each element in "path" has a prob% chance of being replaced by
   * some other statement anywhere from the HT *) 
  Stats2.time "mutation" (fun () -> 
  let any = ref false in 
  let to_swap = Hashtbl.create 255 in 
  List.iter (fun path_step ->
    let coin = Random.float 1.0 in
    if coin < prob then begin
      (* change this path element *) 
      let replace_with_id = Random.int count in 
      if Hashtbl.mem to_swap replace_with_id || 
         Hashtbl.mem to_swap path_step then
        ()
      else begin
        try 
          let ss = Hashtbl.find ht path_step in 
          let rs = Hashtbl.find ht replace_with_id in 
          Hashtbl.add to_swap path_step rs ;
          Hashtbl.add to_swap replace_with_id ss ;
          any := true ; 
        with _ -> ()
      end ;
    end 
  ) path ; 
  if !any then begin
    (* do it! *) 
    let file = copy_file file in 
    let v = 
      if Random.float 1.0 < 0.1 then new swapVisitor else new appVisitor 
    in 
    let my_visitor = v file ht to_swap in 
    visitCilFileSameGlobals my_visitor file ; 
    file, ht, count, path 
  end else begin
    file, ht, count, path
  end 
  ) () 

let crossover (file1,ht1,count1,path1) (file2,ht2,count2,path2) = 
  Stats2.time "crossover" (fun () -> 
  let len1 = List.length path1 in 
  let len2 = List.length path2 in 
  assert(len1 = len2); 
  let cutoff = 1 + (Random.int (pred len1)) in 
  let where = ref 0 in 
  let to_swap1 = Hashtbl.create 255 in 
  let to_swap2 = Hashtbl.create 255 in 
  List.iter2 (fun ps1 ps2 ->
    if !where < cutoff then
      ()
    else begin
      try 
        let s1 = Hashtbl.find ht1 ps1 in 
        let s2 = Hashtbl.find ht2 ps2 in 
        Hashtbl.add to_swap1 ps1 s2 ;
        Hashtbl.add to_swap2 ps2 s1 ;
      with _ -> ()
    end 
  ) path1 path2 ; 
  let file1 = copy_file file1 in 
  let my_visitor1 = new swapVisitor file1 ht1 to_swap1 in 
  visitCilFileSameGlobals my_visitor1 file1 ; 
  let file2 = copy_file file2 in 
  let my_visitor2 = new swapVisitor file2 ht2 to_swap2 in 
  visitCilFileSameGlobals my_visitor2 file2 ; 
  (file1,ht1,count1,path1) ,
  (file2,ht2,count2,path2) 
  ) () 

let gcc_cmd = ref "gcc" 
let good_cmd = ref "./test-good.sh" 
let bad_cmd = ref  "./test-bad.sh" 
let compile_counter = ref 0  

let count_simple_file file =
  try 
    let fin = open_in file in 
    let count = ref 0 in
    (try while true do
      let line = input_line fin in
      ignore line ;
      incr count 
    done ; 0. with _ -> begin close_in fin ; float_of_int !count end) 
  with _ -> 0.

let max_fitness = ref 15 
let most_fit = ref None 
let baseline_size = ref 0 
let first_solution_at = ref 0. 

let fitness_ht = Hashtbl.create 255  

let fitness (file,ht,count,path) = 
  (* Step 0 -> Compile it! *) 
  Stats2.time "fitness" (fun () -> 
  try 


    let c = !compile_counter in
    incr compile_counter ; 
    let source_out = Printf.sprintf "%05d-file.c" c in 
    let fout = open_out source_out in 
    dumpFile defaultCilPrinter fout source_out file ;
    close_out fout ; 
    let digest = Digest.file source_out in 

    if Hashtbl.mem fitness_ht digest then begin
      let fitness = Hashtbl.find fitness_ht digest in 
      printf "\tfitness %g (cached)\n" fitness ; flush stdout ; 
      fitness 
    end else begin 

    let exe_name = Printf.sprintf "./%05d-prog" c in 
    let cmd = Printf.sprintf "%s -o %s %s >& /dev/null" !gcc_cmd exe_name source_out in 
    (match Stats2.time "compile" Unix.system cmd with
    | Unix.WEXITED(0) -> ()
    | _ -> begin 
      (* printf "%s: does not compile\n" source_out ;  *)
      failwith "gcc failed"
    end ) ; 

    let good_name = Printf.sprintf "%05d-good" c in 
    let bad_name  = Printf.sprintf "%05d-bad" c in 
    (try Unix.unlink good_name with _ -> () ) ; 
    (try Unix.unlink bad_name with _ -> () ) ; 

    let cmd = Printf.sprintf "%s %s %s >& /dev/null" !good_cmd exe_name good_name in  
    (match Stats2.time "good test" Unix.system cmd with
    | Unix.WEXITED(0) -> ()
    | _ -> begin 
      printf "FAILED: %s\n" cmd ; failwith "good failed"
    end ) ; 

    let cmd = Printf.sprintf "%s %s %s >& /dev/null" !bad_cmd exe_name bad_name in 
    (match Stats2.time "bad test" Unix.system cmd with
    | Unix.WEXITED(0) -> ()
    | _ -> begin 
      printf "FAILED: %s\n" cmd ; failwith "good failed"
    end ) ; 

    let good = count_simple_file good_name in 
    let bad  = count_simple_file bad_name  in 
    let fitness = good +. (10. *. bad) in 
    let fname = Printf.sprintf "%05d-fitness" c in 
    let fout = open_out fname in 
    Printf.fprintf fout "%g\n" fitness ;
    close_out fout ;
    printf "\tfitness %g\n" fitness ; flush stdout ; 
    if fitness >= (float_of_int !max_fitness) then begin
      let our_size = file_size source_out in 
      let now = Unix.gettimeofday () in 
      let better = 
        match !most_fit with
        | None -> 
          first_solution_at := now ; 
          true
        | Some(best_size,best_fitness,_,_) -> 
          (abs (our_size - !baseline_size)) <= 
          (abs (best_size - !baseline_size)) && 
          (fitness >= best_fitness) 
      in
      if better then begin 
        printf "\t\tbest so far (size delta %d)\n" 
          (abs (our_size - !baseline_size)) ; flush stdout ; 
        most_fit := Some(our_size, fitness, file, now) 
      end 
    end ; 
    Hashtbl.add fitness_ht digest fitness ; 
    fitness 
    end 

  with _ -> 
    printf "\tfitness failure\n" ; flush stdout ; 0.
  ) () 

let initial_population (file,ht,count,path) num = 
  let res = ref [(file,ht,count,path)] in 
  for i = 2 to num do
    let new_pop = mutation (file,ht,count,path) 0.2 in 
    res := new_pop :: !res 
  done ;
  !res 

let random_order lst = 
  let a = List.map (fun x -> (Random.float 1.0), x) lst in
  let b = List.sort (fun (a,_) (b,_) -> compare a b) a in 
  List.map (fun (_,a) -> a) b 

let ga_step original incoming_population desired_number = 
  assert(desired_number mod 2 = 0) ; 
  let pop_with_fitness = List.map (fun member ->
    let f = fitness member in
    (member,f)
  ) incoming_population in 

  let no_zeroes = ref (
    List.filter (fun (member,f) -> f > 0.) pop_with_fitness  
  ) in 

(*
  assert(List.length !no_zeroes > 0) ; 
  while List.length !no_zeroes < desired_number do 
    let needed = desired_number - (List.length !no_zeroes) in
    if needed > 0 then  begin
      let more = initial_population original needed in
      let more = List.map (fun member ->
        let f = fitness member in
        (member,f)
      ) more in 
      let more = List.filter (fun (m,f) -> f > 0.) more in 
      no_zeroes := more @ !no_zeroes ; 
    end ; 
  done ;
  *)

  while List.length !no_zeroes < desired_number do 
    printf "\tViable Size %d; doubling\n" (List.length !no_zeroes ) ; 
    flush stdout ;
    no_zeroes := !no_zeroes @ !no_zeroes 
  done ; 

  let breeding_population = sample !no_zeroes (desired_number/2) in 

  assert(List.length breeding_population = desired_number / 2) ; 

  let order = random_order breeding_population in

  let rec walk lst = match lst with
  | mom :: dad :: rest -> 
      let kid1, kid2 = crossover mom dad in
      [ mom; dad; kid1; kid2] :: (walk rest) 
  | [] -> [] 
  | singleton -> [ singleton ; singleton ] 
  in 
  let result = walk order in
  let result = List.flatten result in
  let result = List.map (fun element -> 
    [element ; mutation element !mutation_chance ]
  ) result in 
  let result = List.flatten result in 
  assert(List.length result = desired_number * 2); 
  result 

let ga (file,ht,count,path) generations num = 
  let initial = (file,ht,count,path) in 
  let population = ref (initial_population (file,ht,count,path) num) in 

  for i = 1 to generations do
    printf "*** Generation %d (size %d)\n" i (List.length !population); 
    flush stdout ; 
    population := ga_step initial !population num 
  done 

let uniq lst = 
  let ht = Hashtbl.create 255 in 
  let lst = List.filter (fun elt ->
    if Hashtbl.mem ht elt then false
    else begin
      Hashtbl.add ht elt () ;
      true 
    end 
  ) lst in
  lst 

let main () = begin
  let generations = ref 10 in 
  let pop = ref 40 in 
  let filename = ref "" in 

  let usageMsg = "Prototype No-Specification Bug-Fixer\n" in 

  let argDescr = [
    "--gcc", Arg.Set_string gcc_cmd, "X use X to compile C files (def: 'gcc')";
    "--good", Arg.Set_string good_cmd, "X use X as good-test command (def: './test-good.sh')"; 
    "--bad", Arg.Set_string bad_cmd, "X use X as bad-test command (def: './test-bad.sh')"; 
    "--gen", Arg.Set_int generations, "X use X genetic algorithm generations (def: 10)";
    "--mut", Arg.Set_float mutation_chance,"X use X mutation chance (def: 0.2)"; 
    "--pop", Arg.Set_int pop,"X use population size of X (def: 40)"; 
    "--max", Arg.Set_int max_fitness,"X best fitness possible is X (def: 15)"; 
  ] in 
  let handleArg str = filename := str in 
  Arg.parse (Arg.align argDescr) handleArg usageMsg ; 
  Cil.initCIL () ; 
  let start = Unix.gettimeofday () in 
  if !filename <> "" then begin
    let path_str = !filename ^ ".path" in 
    let ht_str = !filename ^ ".ht" in 
    let ast_str = !filename ^ ".ast" in 

    let file_fin = open_in_bin ast_str in 
    let file = Marshal.from_channel file_fin in
    close_in file_fin ; 
    let ht_fin = open_in_bin ht_str in 
    let count, ht = Marshal.from_channel ht_fin in
    close_in ht_fin ; 
    let path_fin = open_in path_str in 
    let path = ref [] in 
    (try
      while true do
        let line = input_line path_fin in
        path := (int_of_string line) :: !path 
      done 
     with _ -> close_in path_fin) ; 
    let path = uniq( List.rev !path) in 

    let source_out = (!filename ^ "-baseline.c") in 
    let fout = open_out source_out in 
    dumpFile defaultCilPrinter fout source_out file ;
    close_out fout ; 
    baseline_size := file_size source_out ; 

    ga (file,ht,count,path) !generations !pop;

    match !most_fit with
    | None -> printf "\n\nNo adequate program found.\n" 
    | Some(best_size, best_fitness, best_file, tau) -> begin
      let source_out = (!filename ^ "-best.c") in 
      let fout = open_out source_out in 
      dumpFile defaultCilPrinter fout source_out best_file ;
      close_out fout ; 
      printf "\n\nBest result written to %s\n" source_out ; 
      printf "\tFirst Solution in %g\n" (!first_solution_at -. start) ; 
      printf "\tBest  Solution in %g\n" (tau -. start) ; 
    end 

  end ;
  Stats2.print stdout "Genetic Programming Prototype" ; 
end ;;

main () ;; 
