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

let asmRep_version = "5"

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

(** @version 5 *)
class asmRep = object (self : 'self_type)
  inherit stringRep as super

  (** List of atom IDs that mark the beginning of files. This is sorted in
      reverse order when it is created by from_source.

      Shared between all clones. *)
  val file_boundaries = ref []

  method serialize ?out_channel ?global_info filename =
    let fout =
      match out_channel with
      | Some(v) -> v
      | None -> open_out_bin filename
    in
      Marshal.to_channel fout (asmRep_version) [] ;
      Marshal.to_channel fout (!global_code) [] ;
      Marshal.to_channel fout (!next_id) [] ;
      Marshal.to_channel fout (!file_boundaries) [] ;
      Marshal.to_channel fout (genome) [] ;
      debug "asmRep: %s: saved\n" filename ;
      super#serialize ~out_channel:fout ?global_info:global_info filename ;
      if out_channel = None then close_out fout

  method deserialize ?in_channel ?global_info filename =
    let fin =
      match in_channel with
      | Some(v) -> v
      | None -> open_in_bin filename
    in
    let version = Marshal.from_channel fin in
      if version <> asmRep_version then begin
        debug "asmRep: %s has old version\n" filename ;
        failwith "version mismatch"
      end ;
      global_code     := Marshal.from_channel fin ;
      next_id         := Marshal.from_channel fin ;
      file_boundaries := Marshal.from_channel fin ;
      genome          <- Marshal.from_channel fin ;
      debug "asmRep: %s: loaded\n" filename ;
      super#deserialize ~in_channel:fin ?global_info:global_info filename ;
      if in_channel = None then close_in fin

  (**/**)

  method from_source filename =
    global_code := StringMap.empty;
    next_id := 1;
    genome <- [];
    iter_lines filename (fun line ->
      let fname =
        if line.[0] = '/' then line else Filename.concat !prefix line
      in
      let lines = Array.of_list (get_lines fname) in
        global_code := StringMap.add line (!next_id, lines) !global_code;
        file_boundaries := !next_id :: !file_boundaries;
        next_id := !next_id + (Array.length lines)
    )

  method debug_info () =
    let nlines =
      StringMap.fold (fun _ (_, lines) sum ->
        sum + (Array.length lines)
      ) !global_code 0
    in
      debug "asmRep: %d lines\n" nlines

  (* Removes directives and debugging labels from the fault space *)
  method reduce_search_space _ _ =
    let not_directive_or_debug_label (i, _) =
      let line = self#get i in
      ((not (Str.string_match dir_regex line 0)) && 
          (not (Str.string_match debug_label_regex line 0)))
    in
    let orig_len = List.length !fault_localization in
    fault_localization := (lfilt not_directive_or_debug_label !fault_localization);
    let new_len = List.length !fault_localization in
    let percentage = (1.0 -. ((float_of_int new_len) /. (float_of_int orig_len))) *. 100.0 in
    debug "asmRep: fault space reduced from %d lines to %d lines (%.2f%%)\n" orig_len new_len percentage;

  (* Removes labels and duplicate lines from the fix space *)
  method reduce_fix_space () =
    let orig_len = List.length !fix_localization in
    super#reduce_fix_space ();
    let not_label (i, _) =
      let line = self#get i in
      (not (Str.string_match label_regex line 0))
    in
    fix_localization := (lfilt not_label !fix_localization);
    let fixes = Hashtbl.create 11 in
      liter (fun (atom_id, weight) ->
        let line = self#get atom_id in
        let oldw = try Hashtbl.find fixes line with Not_found -> 0. in
        let w = max oldw weight in
          Hashtbl.replace fixes line w
      ) !fix_localization ;
      fix_localization := lfilt (fun (atom_id, weight) ->
        let line = self#get atom_id in
          if (Hashtbl.mem fixes line) && (Hashtbl.find fixes line) = weight then
            let _ = Hashtbl.remove fixes line in
              true
          else
            false
      ) !fix_localization;
      let new_len = List.length !fix_localization in
      let percentage = (1.0 -. ((float_of_int new_len) /. (float_of_int orig_len))) *. 100.0 in
      debug "asmRep: fix space reduced from %d lines to %d lines (%.2f%%)\n" orig_len new_len percentage;

  method available_mutations mut_id = 
    let compute_available _ =
      lfilt
        (fun (mutation,prob) ->
          let line = self#get mut_id in
          match mutation with
            (* Don't delete labels *)
            Delete_mut -> 
              (not (Str.string_match label_regex line 0))
          | Append_mut -> 
             (WeightSet.cardinal (self#append_sources mut_id)) > 0
          | Swap_mut ->
             (WeightSet.cardinal (self#swap_sources mut_id)) > 0
          | Replace_mut ->
            (WeightSet.cardinal (self#replace_sources mut_id)) > 0
          | Lase_Template_mut -> true
          | Template_mut(s) -> (llen (self#template_available_mutations s mut_id)) > 0
        ) !mutations
    in
      (* JL: Copied from rep.ml, not sure if needed *)
      (* Cannot cache available mutations if nested mutations are enabled; the
         set of applicable sources may change based on previous mutations. *)
      if !do_nested then compute_available ()
      else ht_find mutation_cache mut_id compute_available

  (* Make sure that swaps that include labels stay inside one file *)
  method swap_sources x = 
    lfoldl
      (fun weightset ->
        fun (i,w) ->
          WeightSet.add (i,w) weightset)
      (WeightSet.empty) (lfilt (fun (i,w) -> 
          let in_same_file id1 id2 =
            let file1 = List.find (fun x -> id1 >= x) !file_boundaries in
            let file2 = List.find (fun x -> id2 >= x) !file_boundaries in
            file1 == file2
          in
          let l1 = self#get x in
          let l2 = self#get i in
          if ((Str.string_match label_regex l1 0) || (Str.string_match label_regex l2 0)) then
            ((i <> x) && (in_same_file i x))
          else
            i <> x
       ) !fault_localization)

end
