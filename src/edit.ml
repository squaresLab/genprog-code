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
 * This script accepts standard genprog configuration files.  An
 * additional edit option is accepted which may be used to specify
 * edits to perform to the representation.  The program is loaded, the
 * edits in the edit string are applied, and the fitness of the result
 * is evaluated.
 *
 *)

(** {b GenProg API Documentation.} *)

open Printf
open Cil
open Global
open Fitness
open Elf
open Population

let representation = ref ""
let edits = ref ""
let time_at_start = Unix.gettimeofday ()

let _ =
  options := !options @
             [
               "--rep", Arg.Set_string representation, "X representation X (c,txt,java)" ;
               "--edits", Arg.Set_string edits, "X edits to apply to representation" ;
             ]

let debug fmt =
  let k result = begin
    output_string Pervasives.stdout result ;
    flush Pervasives.stdout ;
  end in
  Printf.kprintf k fmt

(** {b process} base_file_name extension new_representation loads the
    representation, applies the edit operations, and evaluates
    fitness.  *)
let process base ext (rep :('a,'b) Rep.representation) =
  let get_fitness variant =
    if test_fitness (-1) variant then -1.0
    else get_opt (variant#fitness()) in
  let apply_edits (rep :('a,'b) Rep.representation) =
    let parse_indices edit_str =
      List.map int_of_string
        (Str.split (Str.regexp_string " ")
           (Str.global_replace (Str.regexp ",") " "
              (Str.global_replace (Str.regexp "[ads()]") "" edit_str))) in
    List.iter (fun edit ->
        let indices = parse_indices edit in
        try
          match String.lowercase (Str.first_chars edit 1) with
          | "d" -> rep#delete (List.nth indices 0);
          | "a" -> rep#append (List.nth indices 0) (List.nth indices 1);
          | "s" -> rep#swap   (List.nth indices 0) (List.nth indices 1);
          | other -> debug "unknown edit operation -- %s\n" edit;
        with
        | e -> begin
            debug "Malformed edit (ensure enough parameters are supplied for each operation)";
            raise e;
          end
      ) (Str.split (Str.regexp_string " ") !edits) in
  (* load the rep *)
  rep#load base;
  (* Apply the requested edit operations *)
  apply_edits rep;
  (* Evaluate Fitness *)
  debug "%g\n" (get_fitness rep)

(** Edit:
  * - process the command line arguments (accept standard config files)
  * - load the program
  * - apply the edit operations specified on the command line
  * - evaluate fitness
*)
let edit () = begin
  (* parse command-line arguments *)
  parse_options_with_deprecated ();

  if !program_to_repair = "" then begin
    abort "main: no program to repair (try --help)\n" ;
  end ;

  (* inhibit unwanted printing *)
  quiet := true;

  (* Figure out and initialize the representation and then run `process' *)
  let base, real_ext = split_ext !program_to_repair in
  let filetype =
    if !representation = "" then real_ext else !representation in

  if real_ext = "txt" && real_ext <> filetype || !Rep.prefix <> "./" then
    Rep.use_subdirs := true;

  match String.lowercase filetype with
  | "s" | "asm" ->
    Global.extension := ".s" ;
    process base real_ext ((new Asmrep.asmRep) :>('a,'b) Rep.representation)
  | "c" | "i" | "cilpatch" | "cil" ->
    Global.extension := ".c";
    Cil.initCIL ();
    process base real_ext ((new Cilrep.patchCilRep) :> ('c,'d) Rep.representation)
  | "cilast" ->
    Global.extension := ".c";
    Cil.initCIL ();
    process base real_ext ((new Cilrep.astCilRep) :> ('e,'f) Rep.representation)
  | "txt" | "string" ->
    Global.extension := ".txt";
    process base real_ext
      ((new Stringrep.stringRep) :>('a,'b) Rep.representation)
  | "" | "exe" | "elf" ->
    process base real_ext
      ((new Elfrep.elfRep) :>('a,'b) Rep.representation);
  | other -> begin
      List.iter (fun (ext,myfun) ->
          if ext = other then myfun ()
        ) !Rep.global_filetypes ;
      abort "%s: unknown file type to repair" !program_to_repair
    end
end ;;

edit () ;;
