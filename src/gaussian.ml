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
(** The Gaussian module provides support for both [Asmrep.asmRep] and [Elfrep.elfRep]. *)

open Global
open Rep
open Simplerep

(**/**)
let sample_runs = ref 100
let _ =
  options := !options @
             [
               "--sample-runs", Arg.Set_int sample_runs,
               "X Execute X runs of the test suite while sampling with oprofile.";
             ]
(**/**)

(** Implements a simple Gaussian blur function for the smoothing of sampled
    memory addresses and assembly file offsets. *)
module Gaussian = struct
  let version = "1"

  let kernel =
    [(-3, 0.006); (-2, 0.061); (-1, 0.242);
     (0, 0.383);
     (1, 0.242);  (2, 0.061); (3, 0.006);]

  let blur (* blur a list of (address, count) pairs by the given kernel *)
      (kernel : (int * float) list)
      (list : (int * int) list) =
    let map = ((Hashtbl.create (List.length list)) : (int, float) Hashtbl.t) in
    let result = ref ([] : (int * float) list) in
    List.iter
      (fun (addr, count) ->
         List.iter
           (fun (offset, mult) ->
              let index = offset + addr in
              let current = ht_find map index (fun _ -> 0.0) in
              hrep map index (current +. ((float_of_int count) *. mult)))
           kernel)
      list ;
    Hashtbl.iter (fun a b -> result := (a,b) :: !result) map ;
    List.sort pair_compare !result

end

(** [Gaussian.binRep] is a virtual superclass for [Asmrep.asmRep] and
    [Elfrep.elfRep] to reduce the amount of duplicated code between them,
    particularly in coverage info generation *)
class virtual binRep = object (self : 'self_type)
  (** inherits explicitly from both [Rep.faultlocReprepresentation] and
      [Simplerep.simpleRep] to give us access to particular superclass
      implementations as necessary.  note that the order here matters because
      OCaml inheritence is syntactic, not a semantic, relationship *)
  inherit [string list, string list] faultlocRepresentation as faultlocSuper
  inherit simpleRep as super

  method private virtual mem_mapping :
    string -> string -> (int, int) Hashtbl.t

  (** This differs between [Asmrep.asmRep] and [Elfrep.elfRep].

      @param weighted_path statement id, weight list corresponding to a path
      (either positive or negative)
      @param mem_mapping hashtable mapping memory locations to statement IDs (or
      the other way around; Eric?)
      @return weighted_path with appropriate combination based on memory_mapping.
  *)
  method private virtual combine_coverage :
    (int * float) list -> (int, int)  Hashtbl.t  -> (int * float) list

  (** [get_coverage] for both binary representations calls out to oprofile to
      produce samples of visited instructions on the fault and fix paths.  This
      version does not care if the coverage version of the program displays
      unexpected behavior on the positive/negative test cases.  Additionally, it
      does not check the return values of the unix system calls and thus may
      fail silently *)
  method get_coverage coverage_sourcename coverage_exename coverage_outname =
    (* the use of two executable allows oprofile to sample the pos
     * and neg test executions separately.  *)
    let pos_exe = coverage_exename^".pos" in
    let neg_exe = coverage_exename^".neg" in
    ignore(system ("cp "^coverage_exename^" "^coverage_exename^".pos"));
    ignore(system ("cp "^coverage_exename^" "^coverage_exename^".neg"));
    for i = 1 to !sample_runs do (* run the positive tests *)
      for i = 1 to !pos_tests do
        ignore(self#internal_test_case pos_exe
                 coverage_sourcename (Positive i))
      done ;
      for i = 1 to !neg_tests do
        ignore(self#internal_test_case neg_exe coverage_sourcename (Negative i))
      done ;
    done ;
    (* collect the sampled results *)
    let from_opannotate sample_path =
      let regex =
        Str.regexp "^[ \t]*\\([0-9]\\).*:[ \t]*\\([0-9a-zA-Z]*\\):.*"
      in
      let lst = get_lines sample_path in
      let res =
        lfoldl
          (fun acc line ->
             if Str.string_match regex line 0 then
               let count = int_of_string (Str.matched_group 1 line) in
               let addr = int_of_string ("0x"^(Str.matched_group 2 line)) in
               (addr, count) :: acc
             else acc) [] lst
      in
      List.sort pair_compare res in
    let drop_ids_only_to counts file path =
      let fout = open_out path in
      List.iter (fun (line,_) -> Printf.fprintf fout "%d\n" line) counts ;
      close_out fout in
    let pos_samp = pos_exe^".samp" in
    let neg_samp = neg_exe^".samp" in
    let mapping  = self#mem_mapping coverage_sourcename coverage_exename in
    (* collect the samples *)
    if not (Sys.file_exists pos_samp) then
      ignore (system ("opannotate -a "^pos_exe^">"^pos_samp)) ;
    if not (Sys.file_exists neg_samp) then
      ignore (system ("opannotate -a "^neg_exe^">"^neg_samp)) ;
    (* do a Guassian blur on the samples and convert to LOC *)
    let combine_pos =
      Gaussian.blur Gaussian.kernel (from_opannotate pos_samp)
    in
    let combine_neg =
      Gaussian.blur Gaussian.kernel (from_opannotate neg_samp)
    in
    drop_ids_only_to
      (self#combine_coverage combine_pos mapping)
      pos_exe !fix_path ;
    drop_ids_only_to (self#combine_coverage combine_neg mapping)
      neg_exe !fault_path

  (** because fault localization on binary represntations uses oprofile,
      instrumenting for fault localization requires only that we output the
      program to disk.  HOWEVER, it requires as a precondition that oprofile be
      running. *)
  method instrument_fault_localization
      coverage_sourcename coverage_exename coverage_outname =
    debug "binRep: computing fault localization information\n" ;
    debug "binRep: ensure oprofile is running\n" ;
    self#output_source coverage_sourcename ;

    (**/**)
    (* the simpleRep compute_localization throws a fail, so we explicitly dispatch
       to faultLocSuper here *)
  method compute_localization () = faultlocSuper#compute_localization ()
  method get_compiler_command () = faultlocSuper#get_compiler_command ()

  (**/**)

end
