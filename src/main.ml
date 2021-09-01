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
(*
 * Program Repair Prototype (v2)
 *
 * This is the main driver: it reads in options, loads the
 * program-to-be-repaired (using the given representation),
 * calls for its fault localization information, and then
 * applies a search technique to the problem.
 *
 *)

(** {b GenProg API Documentation.} *)

open Printf
open Global
open Population

let representation = ref ""
let time_at_start = Unix.gettimeofday ()
let describe_machine = ref false
let oracle_genome = ref ""
let show_version  = ref false

let _ =
  options := !options @
             [
               "--describe-machine", Arg.Set describe_machine,
               " describe the current machine (e.g., for cloud computing)" ;

               "--incoming-pop", Arg.Set_string incoming_pop_file,
               "X file of variants for the first generation.  Either binary or a list of genomes to be read from a string." ;

               "--no-test-cache", Arg.Set Rep.no_test_cache,
               " do not load testing .cache file" ;

               "--nht-server", Arg.Set_string Rep.nht_server,
               "X connect to network test cache server X" ;

               "--nht-port", Arg.Set_int Rep.nht_port,
               "X connect to network test cache server on port X" ;

               "--nht-id", Arg.Set_string Rep.nht_id,
               "X this repair scenario's NHT identifier" ;

               "--rep", Arg.Set_string representation, "X representation X (c,txt,java)" ;

               "--oracle-genome", Arg.Set_string oracle_genome,
               "X genome for oracle search, either string or binary file.";

               "--version", Arg.Set show_version, " print the version and exit";
             ]


(** {b process} base_file_name extension new_representation conducts the repair
    search on a base representation.  It loads the representation in
    new_representation, constructs the incoming population if applicable, and
    applies the search strategies in order until we either find a repair or run
    out.  Process will abort if it receives an unrecognized search
    strategies. *)
let process base ext (rep :('a,'b) Rep.representation) =
  (* load the rep, either from a cache or from source *)
  rep#load base;
  rep#debug_info () ;

  (* load incoming population, if specified.  We no longer have to do this
     before individual loading per Claire's March 2012 refactor *)
  let population = if !incoming_pop_file <> "" then
      let fin = open_in_bin !incoming_pop_file in
      GPPopulation.deserialize ~in_channel:fin !incoming_pop_file rep
    else []
  in
  (* Apply the requested search strategies in order. Typically there
   * is only one, but they can be chained. *)
  try
    (match !search_strategy with
     | "dist" | "distributed" | "dist-net" | "net" | "dn" ->
       Network.distributed_client rep population
     | "brute" | "brute_force" | "bf" ->
       Search.brute_force_1 rep population
     | "geom" | "geometric" ->
       Search.geometric rep population
     | "ww" | "ww_adaptive" | "adaptive" ->
       Search.ww_adaptive_1 rep population
     | "ww_prodiv" | "prodiv" | "pd-exploit" ->
       Search.pd_exploit rep population
     | "mjk_prodiv" | "pd" | "pd-explore" ->
       Search.pd_explore rep population
     | "ga" | "gp" | "genetic" ->
       if not (GPPopulation.sanity (rep#variable_length)) then
         abort "Incompatable representation and crossover types, aborting";
       Search.genetic_algorithm rep population
     | "gasga" ->
       if not (GPPopulation.sanity (rep#variable_length)) then
         abort "Incompatable representation and crossover types, aborting";
       Search.gasga rep population
     | "ssga" | "steady-state" ->
       if not (GPPopulation.sanity (rep#variable_length)) then
         abort "Incompatable representation and crossover types, aborting";
       Search.steady_state_ga rep population
     | "multiopt" | "ngsa_ii" ->
       Multiopt.ngsa_ii rep population
     | "mutrb" | "neut" | "neutral" ->
       Search.neutral_variants rep
     | "oracle" ->
       assert(!oracle_genome <> "");
       Search.oracle_search rep !oracle_genome;
     | "pd-oracle" ->
       assert(!oracle_genome <> "");
       Search.pd_oracle_search rep !oracle_genome;
     | "seq" ->
       assert(!oracle_genome <> "");
       Search.sequence rep !oracle_genome
     | "walk" | "neutral_walk" ->
       Search.neutral_walk rep population
     | x -> abort "unrecognized search strategy: %s\n" x);
    (* If we had found a repair, we could have noted it earlier and
     * thrown an exception. *)
    debug "\nNo repair found.\n"
  with Search.Found_repair(rep) -> ()

(***********************************************************************
 * Main driver; primary argument parsing and some debug output
 ***********************************************************************)

(** main processes the command line arguments, provides some debug output,
    creates a "blank" representation based on the command line arguments and the
    type of program we are trying to repair, and dispatches to the function
    "process."  It can abort if it receives an unrecognized program or
    representation type to repair. *)
let main () = begin
  (* initialize random number generator and port for webserver benchmarks *)
  Random.self_init () ;
  Rep.port := 800 + (Random.int 800) ;

  (* By default we use and note a new random seed each time, but the user can
   * override that if desired for reproducibility. *)
  random_seed := (Random.bits ()) ;

  (* check to see if we're the utility to read a test cache. If so, do that and exit *)
  if (Filename.basename Sys.argv.(0)) = "test-cache-reader" then begin
    Rep.test_cache_load () ;
    let filename = try Sys.argv.(1) with _ -> "repair.cache.human" in
    if filename = "help" || filename = "-help" || filename = "--help" then begin
      Printf.printf "test-cache-reader: a utility to produce a human readable test cache from the test cache of a previous repair run. The only parameter is the filename into which the human readable cache should be printed.";
      exit 0
    end;
    Rep.human_readable_cache_save filename ;
    Printf.printf "Genprog: printing test cache to %s\n" filename;
    exit 0
  end;

  (* parse command-line arguments *)
  parse_options_with_deprecated ();
  if !show_version then begin
    Printf.printf "GenProg Version: %s\n" Version.version;
    exit 0
  end;
  let debug_str = sprintf "repair.debug.%d" !random_seed in
  debug_out := open_out debug_str ;

  debug "GenProg Version: %s\n\n" Version.version;
  (* For debugging and reproducibility purposes, print out the values of
   * all command-line argument-settable global variables. *)
  List.iter (fun (name,arg,_) ->
      if name = "-help" || name = "--help" then ()
      else
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
    ) (List.sort (fun (a,_,_) (a',_,_) -> compare a a') (!options)) ;

  (* Cloud-computing debugging: print out machine information. *)
  if !describe_machine then begin
    List.iter (fun (cmd, args) ->
        let command = String.concat " " (cmd::args) in
        try
          let p = popen ~stdout:NewChannel cmd args in
          let line = input_line (get_opt p.fout) in
          debug "%s: %s\n" command line ;
          ignore (Unix.waitpid [] p.pid);
          close_in (get_opt p.fout)
        with e ->
          debug "%s: %s\n" command (Printexc.to_string e)
      ) [ "uname", ["-a"] ;
          "date", [] ;
          "id", [] ;
          "cat", ["/etc/redhat-release"] ;
          "grep", ["model name"; "/proc/cpuinfo"] ;
          "grep", ["MemTotal"; "/proc/meminfo"] ;
          "grep", ["SwapTotal"; "/proc/meminfo"] ;
        ]
  end ;

  if !program_to_repair = "" then begin
    abort "main: no program to repair (try --help)\n" ;
  end ;

  (* Bookkeeping information to print out whenever we're done ... *)
  Sys.catch_break true ;
  at_exit (fun () ->
      let tc = Rep.num_test_evals_ignore_cache () in
      debug "\nVariant Test Case Queries: %d\n" tc ;
      debug "\"Test Suite Evaluations\": %g\n\n"
        ((float tc) /. (float (!pos_tests + !neg_tests))) ;

      debug "Compile Failures: %d\n" !Rep.compile_failures ;
      debug "Wall-Clock Seconds Elapsed: %g\n"
        ((Unix.gettimeofday ()) -. time_at_start) ;
      if not !gui then
        Stats2.print !debug_out "Program Repair Prototype (v2)" ;
      close_out !debug_out ;
      debug_out := stdout ;
      if not !gui then
        Stats2.print stdout "Program Repair Prototype (v2)" ;
    ) ;

  Random.init !random_seed ;

  if not !Rep.no_test_cache then begin
    Rep.test_cache_load () ;
    at_exit (fun () ->
        debug "Rep: saving test cache\n" ;
        Rep.test_cache_save ()
      )
  end ;

  (* Figure out and initialize the representation *)

  let base, real_ext = split_ext !program_to_repair in
  let filetype =
    if !representation = "" then real_ext else !representation in

    if real_ext = "txt" && real_ext <> filetype || !Rep.prefix <> "./" || !multifile then 
      Rep.use_subdirs := true; 

    match String.lowercase_ascii filetype with 
    | "ll" | "bl" ->
    Global.extension := ".ll" ; 
      process base real_ext ((new Llvmrep.llvmRep) :>('a,'b) Rep.representation)
    | "s" | "asm" ->
    Global.extension := ".s" ; 
      process base real_ext ((new Asmrep.asmRep) :>('i,'j) Rep.representation)
    | "c" | "i" | "cilpatch" | "cil" -> 
      Global.extension := ".c";
      Cil.initCIL ();
      let _ = !Cilrep.fill_va_table () in 
      process base real_ext ((new Cilrep.patchCilRep) :> ('c,'d) Rep.representation)
    | "cilast" -> 
      Global.extension := ".c";
      Cil.initCIL ();
      process base real_ext ((new Cilrep.astCilRep) :> ('e,'f) Rep.representation)
    | "txt" | "string" ->
    Global.extension := ".txt";
    process base real_ext
      ((new Stringrep.stringRep) :>('g,'h) Rep.representation)
  (* | "" | "exe" | "elf" -> *)
  (*   process base real_ext  *)
  (*     ((new Elfrep.elfRep) :>('a,'b) Rep.representation); *)
  | other -> begin
      List.iter (fun (ext,myfun) ->
          if ext = other then myfun ()
        ) !Rep.global_filetypes ;
      abort "%s: unknown file type to repair" !program_to_repair

    end
end ;;

try
  main ()
with
(* as per Mike's request, try to echo system errors to the debug file *)
| Unix.Unix_error(e,s1,s2) as exc -> begin
    let msg = Unix.error_message e in
    debug "%s aborting: Unix error: %S %S %S\n"
      Sys.argv.(0) msg s1 s2 ;
    raise exc
  end
| e -> begin
    debug "%s aborting: %s\n" Sys.argv.(0) (Printexc.to_string e) ;
    raise e
  end
