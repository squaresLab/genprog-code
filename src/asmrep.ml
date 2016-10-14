(*
 *
 * Copyright (c) 2012-2016, 
 *  Wes Weimer          <weimer@cs.virginia.edu>
 *  Stephanie Forrest   <forrest@cs.unm.edu>
 *  Claire Le Goues     <legoues@cs.virginia.edu>
 *  Eric Schulte        <eschulte@cs.unm.edu>
 *  Jeremy Lacomis      <lacomis@virginia.edu>
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
(** [asmRep] provides a representation for text .s assembly files as produced
    e.g., by gcc -S.  [asmRep] mostly extends the [Stringrep.stringRep]
    functionality, but provides additional functionality for assembly search
    space reductions. *)

open Global
open Rep
open Stringrep

let asmRep_version = "6"

(**/**)

(* Option from old version of asmRep *)
(* let asm_code_only = ref false *)
(* let _ = *)
(*   options := !options @ *)
(*     [ *)
(*       "--asm-code-only", Arg.Set asm_code_only, *)
(*       " Limit mutation operators to code sections of assembly files"; *)
(*     ] *)

let label_regex = Str.regexp ".*:$"
let dir_regex = Str.regexp "^[ \t]*\\..*[^:]$"
let debug_label_regex = Str.regexp "^\\.L[^0-9].*:$"

(**/**)

(** @version 6 *)
class asmRep = object (self : 'self_type)
  inherit stringRep as super

  (**/**)

  (* Removes directives and debugging labels from the fault space *)
  method reduce_search_space _ _ =
    let not_directive_or_debug_label (i, _) =
      let line = self#get i in
      ((not (Str.string_match dir_regex line 0)) &&
          (not (Str.string_match debug_label_regex line 0)))
    in
    let orig_len = llen !fault_localization in
    fault_localization := lfilt not_directive_or_debug_label !fault_localization;
    let new_len = llen !fault_localization in
    let percentage = (1.0 -. ((float_of_int new_len) /. (float_of_int orig_len))) *. 100.0 in
    debug "asmRep: fault space reduced from %d lines to %d lines (%.2f%%)\n" orig_len new_len percentage;

  (* Removes labels and duplicate lines from the fix space *)
  method reduce_fix_space () =
    let orig_len = List.length !fix_localization in
    super#reduce_fix_space ();
    let not_label (i, _) =
        (not (Str.string_match label_regex (self#get i) 0))
    in
      fix_localization := lfilt not_label !fix_localization;
      let new_len = List.length !fix_localization in
      let percentage = (1.0 -. ((float_of_int new_len) /. (float_of_int orig_len))) *. 100.0 in
      debug "asmRep: fix space reduced from %d lines to %d lines (%.2f%%)\n" orig_len new_len percentage;

  method available_mutations mut_id = 
    lfilt (fun (mutation,prob) ->
      match mutation with
        Delete_mut -> (* Don't delete labels *)
          (not (Str.string_match label_regex (self#get mut_id) 0))
      | _ -> true
    ) (super#available_mutations mut_id)

  (* Make sure that swaps that include labels stay inside one file *)
  method private swap_source_gen x =
    let is_label i = Str.string_match label_regex (self#get i) 0 in
    let file1 = self#base_for_atom_id x in
    let can_swap =
      if is_label x then (* x is a label: can only swap within the same file *)
        fun i -> (x <> i) && ((self#base_for_atom_id i) = file1)
      else (* x is not a label: can swap anywhere except labels in other files *)
        fun i -> (x <> i) && (not (is_label i) || ((self#base_for_atom_id i) = file1))
    in
    let pending = ref !fault_localization in
    let rec gen () =
      match !pending with
      | (i,w)::rest ->
        pending := rest ;
        if (can_swap i) then Some(i,w) else gen ()
      | [] -> None
    in
      gen
end
