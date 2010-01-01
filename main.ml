(* 
 * Program Repair Prototype (v2) 
 *
 * This is the main driver: it reads in options, loads the
 * program-to-be-repaired (using the given representation),
 * calls for its fault localization information, and then
 * applies a search technique to the problem. 
 *
 * Still TODO: parallelism (e.g., work queues)
 *)
open Printf
open Cil
open Global

let search_strategy = ref "brute" 
let _ =
  options := !options @
  [
    "--search", Arg.Set_string search_strategy, "X use strategy X (brute, ga) [comma-separated]";
  ] 

(***********************************************************************
 * Parse Command Line Arguments, etc. 
 ***********************************************************************)
let main () = begin
  Random.self_init () ; 
  (* By default we use and note a new random seed each time, but the user
   * can override that if desired for reproducibility. *) 
  random_seed := (Random.bits ()) ;  
  Rep.port := 800 + (Random.int 800) ;  

  at_exit (fun () -> 
    let tc = (Rep.num_test_evals_ignore_cache ()) in 
    debug "\nVariant Test Case Queries: %d\n" tc ;
    debug "\"Test Suite Evaluations\": %g\n\n" 
      ((float tc) /. (float (!pos_tests + !neg_tests))) ;
    debug "Compile Failures: %d\n" !Rep.compile_failures ; 
    Stats2.print !debug_out "Program Repair Prototype (v2)" ; 
    close_out !debug_out ;
    Stats2.print stdout "Program Repair Prototype (v2)" ; 
  ) ; 

  let handleArg str = parse_options_in_file str in 
  Arg.parse (Arg.align !options) handleArg usageMsg ; 
  let debug_str = sprintf "repair.debug.%d" !random_seed in 
  debug_out := open_out debug_str ; 

  Cil.initCIL () ; 
  Random.init !random_seed ; 
  List.iter (fun (name,arg,_) ->
    debug "%s %s\n" name 
    (match arg with
    | Arg.Set br 
    | Arg.Clear br 
    -> sprintf "%b" !br 
    | Arg.Set_string sr
    -> sprintf "%S" !sr
    | Arg.Set_int ir
    -> sprintf "%d" !ir
    | Arg.Set_float fr
    -> sprintf "%g" !fr
    | _ -> "?") 
  ) (!options) ; 


  let base, rep = (match split_ext !program_to_repair with
  | base,"c" 
  | base,"i" 
  -> 
    Rep.test_cache_load () ;
    at_exit Rep.test_cache_save ;
    base, (new Cilrep.cilRep)

  | _,_ -> 
    debug "%s: unknown file type to repair" !program_to_repair ;
    exit 1 
  ) in

  begin
    try 
      rep#load_binary (base^".cache") 
    with _ -> 
      rep#from_source !program_to_repair ; 
      rep#sanity_check () ; 
      rep#compute_fault_localization () ; 
      rep#save_binary (base^".cache") 
  end ;
  rep#debug_info () ; 

  let comma = Str.regexp "," in 

  let what_to_do = Str.split comma !search_strategy in
  ignore (List.fold_left (fun population strategy ->
    match strategy with
    | "brute" | "brute_force" | "bf" -> 
    Search.brute_force_1 rep population
    | "ga" | "gp" | "genetic" -> 
    Search.genetic_algorithm rep population
    | x -> 
    failwith x
  ) [] what_to_do) ; 

  debug "\nNo repair found.\n"  

end ;;

main () ;; 
