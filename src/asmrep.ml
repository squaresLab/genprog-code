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
(** [asmRep] provides a representation for text .s assembly files as produced
    e.g., by gcc -S.  [asmRep] mostly extends the [Stringrep.stringRep]
    functionality, but provides additional functionality for assembly search
    space reductions. *)

open Global
open Rep
open Stringrep

let asmRep_version = "7"

(**/**)

let label_regex = Str.regexp ".*:$"
let dir_regex = Str.regexp "^[ \t]*\\..*[^:]$"
let debug_label_regex = Str.regexp "^\\.L[^0-9].*:$"

module StringTrie = Trie.Make(
                        struct
                          type t = char
                          let compare = compare
                        end
                      )

(**/**)

(** @version 7 *)
class asmRep = object (self : 'self_type)
  inherit stringRep as super

  (**/**)

  (** Mapping from file base index to lists of labels *)
  val labels = ref IntMap.empty

  (** Set of all labels in the code *)
  val all_labels = ref StringSet.empty

  (** Map from file base id to the trie for the labels in that file *)
  val label_tries = ref IntMap.empty

  (** Trie containing all labels in the code *)
  val all_labels_trie = ref StringTrie.empty

  method from_source filename =
    super#from_source filename;
    let strip = function
      | "" -> ""
      | str -> String.sub str 0 ((String.length str) - 1)
    in
    let get_lines lines i =
      let found_string = Str.string_match label_regex (!atoms.(i)) 0 in
      if found_string then
        (strip (Str.matched_string (!atoms.(i)))) :: lines
      else
        lines
    in
    let collect_labels fname (start, stop) labels =
      let lines = List.fold_left get_lines [] (start -- (stop - 1)) in
      IntMap.add start lines labels
    in
    labels := StringMap.fold collect_labels !ranges IntMap.empty;
    let add_flabels fnum flabels =
      let add_label label = all_labels := StringSet.add label !all_labels in
      List.iter add_label flabels
    in
    IntMap.iter add_flabels !labels;
    let add_to_trie trie elem =
      StringTrie.add (chars_of_string elem) true trie in
    let make_tries fnum flabels label_tries =
      let make_trie = List.fold_left add_to_trie StringTrie.empty flabels in
      IntMap.add fnum make_trie label_tries
    in
    label_tries := IntMap.fold make_tries !labels IntMap.empty;
    all_labels_trie := StringSet.fold (fun elem trie -> add_to_trie trie elem)
                         !all_labels StringTrie.empty;

  (* Removes directives and debugging labels from the fault space *)
  method reduce_search_space _ _ =
    let not_directive_or_debug_label (i, _) =
      let line = self#get i in
      ((not (Str.string_match dir_regex line 0)) &&
         (not (Str.string_match debug_label_regex line 0)))
    in
    let orig_len = List.length !fault_localization in
    fault_localization :=
      List.filter not_directive_or_debug_label !fault_localization;
    let new_len = llen !fault_localization in
    let percentage =
      (1.0 -. ((float_of_int new_len) /. (float_of_int orig_len))) *. 100.0 in
    let debug_string = format_of_string
      "asmRep: fault space reduced from %d lines to %d lines (%.2f%%)\n" in
    debug debug_string orig_len new_len percentage;

  (* Removes labels and duplicate lines from the fix space *)
  method reduce_fix_space () =
    let orig_len = List.length !fix_localization in
    super#reduce_fix_space ();
    let not_label (i, _) =
      (not (Str.string_match label_regex (self#get i) 0))
    in
    fix_localization := lfilt not_label !fix_localization;
    let new_len = List.length !fix_localization in
    let percentage =
      (1.0 -. ((float_of_int new_len) /. (float_of_int orig_len))) *. 100.0 in
    let debug_string = format_of_string
      "asmRep: fix space reduced from %d lines to %d lines (%.2f%%)\n" in
    debug debug_string orig_len new_len percentage;

  method available_mutations mut_id =
    let delete_label_filter (mutation, _) =
      match mutation with
      (* Don't delete labels *)
      | Delete_mut -> (not (Str.string_match label_regex (self#get mut_id) 0))
      | _ -> true
    in
    lfilt delete_label_filter (super#available_mutations mut_id)

  (* Do not allow appends if an instuction references a label that does not
     exist in this file *)
  method private append_source_gen x =
    let contains_label line trie =
      let rec helper x =
        match x with
        | i :: rest ->
           let prefix = (StringTrie.contains_prefix_of (i :: rest) trie) in
           if (prefix == []) then
             helper rest
           else
             begin
               let prefix_len = llen prefix in
               let next_char =
                 try
                   List.nth x prefix_len
                 with _ -> '\n'
               in
               match next_char with
               | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '.' | '_' -> false
               | _ -> true
             end
        | _ -> false
      in
      helper (chars_of_string line)
    in
    let label_in_file file line =
      let trie = IntMap.find file !label_tries in
      contains_label (self#get line) trie
    in
    let file = self#base_for_atom_id x in
    let can_append line =
      if contains_label (self#get line) !all_labels_trie then
        (x <> line) && (label_in_file file line)
      else
        (x <> line)
    in
    let pending = ref !fix_localization in
    let rec gen () =
      match !pending with
      | (i, w) :: rest ->
         pending := rest;
         if (can_append i) then Some(i, w) else gen ()
      | [] -> None
    in
    gen

  (* Make sure that swaps that include labels stay inside one file *)
  method private swap_source_gen x =
    let is_label i = Str.string_match label_regex (self#get i) 0 in
    let file1 = self#base_for_atom_id x in
    let can_swap =
      (* x is a label: can only swap within the same file *)
      if is_label x then
        fun i -> (x <> i) && ((self#base_for_atom_id i) = file1)
      (* x is not a label: can swap anywhere except labels in other files *)
      else
        fun i -> (x <> i) &&
                   (not (is_label i) || ((self#base_for_atom_id i) = file1))
    in
    let pending = ref !fault_localization in
    let rec gen () =
      match !pending with
      | (i, w) :: rest ->
        pending := rest;
        if (can_swap i) then Some (i, w) else gen ()
      | [] -> None
    in
    gen
end
