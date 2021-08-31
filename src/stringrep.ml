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

(** stringRep represents a program as a list of line edits to an original
    program. This is very similar to the information contained in the output
    of the diff tool and used by patch. *)

open Global
open Rep

let stringRep_version = "3"

class stringRep = object (self : 'self_type)
  inherit [string edit_history, string] faultlocRepresentation
    as super

  (**/**)

  (** Mapping from filenames to the half-open range of atom IDs in the file.
      Shared between all copies. *)
  val ranges = ref StringMap.empty

  (** The atoms for all files. Shared between all copies. *)
  val atoms = ref (Array.make 0 "")

  (** Sorted array of the first atom ID in each file for quickly finding the source
      file, given an atom ID. Shared among all copies. *)
  val file_boundaries = ref (Array.make 0 0)

  (** The genome: a list of edits and the ID of the first inserted line for
      each. Unique to each instance. *)
  val mutable genome = []

  (* only need copy if we have an object-specific value with a mutable type,
     as opposed to a mutable value of an immutable type *)
  (*
  method copy () : 'self_type =
    let super_copy : 'self_type = super#copy () in
      (* do not copy genome because lists are immutable in OCaml *)
      super_copy
  *)

  method serialize ?out_channel ?global_info filename =
    let fout =
      match out_channel with
      | Some(v) -> v
      | None -> open_out_bin filename
    in
    Marshal.to_channel fout (stringRep_version) [] ;
    Marshal.to_channel fout (!ranges) [] ;
    Marshal.to_channel fout (!atoms) [] ;
    Marshal.to_channel fout (!file_boundaries) [] ;
    Marshal.to_channel fout (genome) [] ;
    debug "stringRep: %s: saved\n" filename ;
    super#serialize ~out_channel:fout ?global_info:global_info filename ;
    if out_channel = None then close_out fout

  method deserialize ?in_channel ?global_info filename =
    let fin =
      match in_channel with
      | Some(v) -> v
      | None -> open_in_bin filename
    in
    let version = Marshal.from_channel fin in
    if version <> stringRep_version then begin
      debug "stringRep: %s has old version\n" filename ;
      failwith "version mismatch"
    end ;
    ranges          := Marshal.from_channel fin ;
    atoms           := Marshal.from_channel fin ;
    file_boundaries := Marshal.from_channel fin ;
    genome          <- Marshal.from_channel fin ;
    debug "stringRep: %s: loaded\n" filename ;
    super#deserialize ~in_channel:fin ?global_info:global_info filename ;
    if in_channel = None then close_in fin

  (**/**)

  method variable_length = true

  method get_genome () = genome

  method set_genome g =
    self#updated ();
    history := g;
    genome <- g

  method add_history h =
    self#updated () ;
    super#add_history h ;
    genome <- genome @ [h]

  method genome_length () = llen genome

  method atom_to_str line = line

  method name () =
    if genome = [] then "original"
    else
      let strs = lmap self#history_element_to_str genome in
      String.concat " " strs

  method load_genome_from_string str =
    let scan_history_element b =
      Scanf.bscanf b "%c" (fun action -> match action with
          | 'd' -> Scanf.bscanf b "(%d)"    (fun id -> Delete(id))
          | 'a' -> Scanf.bscanf b "(%d,%d)" (fun dst src -> Append(dst, src))
          | 's' -> Scanf.bscanf b "(%d,%d)" (fun id1 id2 -> Swap(id1, id2))
          | 'r' -> Scanf.bscanf b "(%d,%d)" (fun dst src -> Replace(dst, src))
          | x ->
            failwith ((String.make 1 x)^" mutations are not supported by stringRep")
        )
    in
    let split_genome = Str.split (Str.regexp " ") str in
    let g =
      if split_genome = ["original"] then []
      else
        List.map (fun x ->
            try
              Scanf.sscanf x "%r" scan_history_element (fun g -> g)
            with End_of_file ->
              failwith (Printf.sprintf "incomplete gene '%s'" x)
          ) split_genome
    in
    self#set_genome g

  method from_source filename =
    ranges := StringMap.empty;
    genome <- [];

    (* include a placeholder "line" at index 0 so we don't have to remap atom
       IDs into indexes *)
    let all_lines = ref [ "" ] in
    let starts = ref [] in
    let next_id = ref 1 in
    iter_lines filename (fun line ->
        let fname =
          if line.[0] = '/' then line else Filename.concat !prefix line
        in
        let lines = get_lines fname in
        let last_id = !next_id in
        next_id := !next_id + (llen lines) ;
        ranges := StringMap.add line (last_id, !next_id) !ranges ;
        starts := last_id :: !starts ;
        all_lines := List.rev_append lines !all_lines ;
      ) ;
    atoms := Array.of_list (lrev !all_lines) ;
    file_boundaries := Array.of_list !starts ;
    Array.sort compare !file_boundaries

  method private base_for_atom_id x =
    let rec bin_search lo hi i =
      if (lo + 1) = hi then
        !file_boundaries.(lo)
      else
        let mid = (lo + hi) lsr 1 in
        if !file_boundaries.(mid) <= i then
          bin_search mid hi i
        else
          bin_search lo mid i
    in
    bin_search 0 (Array.length !file_boundaries) x

  method get_atoms () =
    let rec add_from i accum =
      if (Array.length !atoms) <= i then
        accum
      else
        add_from (i + 1) (AtomSet.add i accum)
    in
    add_from 1 AtomSet.empty

  method atom_id_of_source_line source_file source_line =
    let check_one_file start stop =
      if source_line <= 0 || (stop - start) < source_line then []
      else [source_line + start]
    in
    if source_file = "" then
      StringMap.fold (fun _ (start, stop) ids ->
          (check_one_file start stop) @ ids
        ) !ranges []
    else
      try
        let start, stop = StringMap.find source_file !ranges in
        check_one_file start stop
      with Not_found -> []

  method instrument_fault_localization _ _ _ =
    failwith "stringRep: no fault localization"

  method debug_info () =
    let nlines = Array.length !atoms - 1 in
    debug "stringRep: %d lines\n" nlines

  method get i =
    if (1 <= i) && (i < (Array.length !atoms)) then
      (!atoms).(i)
    else
      failwith ("invalid atom ID: "^(string_of_int i))

  method reduce_fix_space () =
    super#reduce_fix_space ();
    let fixes = Hashtbl.create ((llen !fix_localization) lsr 1) in
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
      ) !fix_localization

  method available_mutations x =
    let compute_available _ =
      lfilt
        (fun (mutation,prob) ->
           match mutation with
             Delete_mut -> true
           | Append_mut -> (self#append_source_gen x ()) <> None
           | Swap_mut -> (self#swap_source_gen x ()) <> None
           | Replace_mut -> (self#replace_source_gen x ()) <> None
           | Template_mut(s) -> (llen (self#template_available_mutations s x)) > 0
           | _ -> false
        ) !mutations
    in
    (* Cannot cache available mutations if nested mutations are enabled; the
       set of applicable sources may change based on previous mutations. *)
    if !do_nested then compute_available ()
    else ht_find mutation_cache x compute_available

  method private weight_set_of_generator gen =
    let rec collect xs =
      match gen () with
      | Some(x) -> collect (WeightSet.add x xs)
      | None -> xs
    in
    collect WeightSet.empty

  method append_sources x =
    self#weight_set_of_generator (self#append_source_gen x)

  method private append_source_gen x =
    let pending = ref !fix_localization in
    let gen () =
      match !pending with
      | (i,w)::rest -> pending := rest ; Some(i,w)
      | [] -> None
    in
    gen

  method swap_sources x =
    self#weight_set_of_generator (self#swap_source_gen x)

  method private swap_source_gen x =
    let pending = ref !fault_localization in
    let rec gen () =
      match !pending with
      | (i,w)::rest ->
        pending := rest ;
        if i <> x then Some(i,w) else gen ()
      | [] -> None
    in
    gen

  method replace_sources x =
    self#weight_set_of_generator (self#replace_source_gen x)

  method private replace_source_gen x =
    let pending = ref !fix_localization in
    let rec gen () =
      match !pending with
      | (i,w)::rest ->
        pending := rest ;
        if i <> x then Some(i,w) else gen ()
      | [] -> None
    in
    gen

  method internal_compute_source_buffers () =
    (* The following algorithm is based on the following assumptions:
       1) Each line in the program has a unique ID. When new lines are inserted
          they are assigned the ID 0.
       2) Edits may remove the current line or insert lines before or after it.
       3) Edits do not change the order of lines.
    *)

    (* Create a hashtable of all the edits to apply to each line. Insert the
       edits in reverse order so that the earliest edit will be at the top of
       the stack associated with each id. *)
    let edits = Hashtbl.create (llen genome) in
    let files = Hashtbl.create (Array.length !file_boundaries) in
    liter (fun h ->
        let ts, es =
          match h with
          | Delete(id) ->
            [id],  [fun _ -> []]
          | Append(dst, src) ->
            [dst], [fun line -> [ dst, line; 0, self#get src ]]
          | Swap(id1, id2) ->
            (* This must be kept in sync with add_gene so that the strings are
               assigned their IDs consistently. *)
            [id1; id2], [
              (fun line -> [ 0, self#get id2 ]);
              (fun line -> [ 0, self#get id1 ])
            ]
          | Replace(dst, src) ->
            [dst], [fun line -> [ 0, self#get src ]]
          | _ -> [], []
        in
        List.iter (fun t ->
            if t <> 0 then
              Hashtbl.replace files (self#base_for_atom_id t) ()
          ) ts ;
        List.iter2 (fun t e -> if t <> 0 then Hashtbl.add edits t e) ts es
      ) (lrev genome) ;

    (* While any lines remain, pop the first one and apply the first relevant
       edit, if any, or else print the line. *)
    let rec add_line buf = function
      | (i, line) :: lines ->
        let lines =
          try
            let f = Hashtbl.find edits i in
            Hashtbl.remove edits i;
            (f line) @ lines
          with Not_found ->
            Buffer.add_string buf line ;
            Buffer.add_char buf '\n' ;
            lines
        in
        add_line buf lines
      | [] -> buf
    in

    Stats2.time "build files" (fun () ->
        StringMap.fold (fun fname (start, stop) output_list ->
            if Hashtbl.mem files start then begin
              (* Create a stack of identified lines from the original program. *)
              let rec get_lines i lines =
                if i < start then
                  lines
                else
                  get_lines (i-1) ((i, !atoms.(i))::lines)
              in
              let lines = get_lines (stop-1) [] in

              let body = add_line (Buffer.create 1024) lines in
              (Some(fname), Some(Buffer.contents body)) :: output_list
            end else
              (Some(fname), None) :: output_list
          ) !ranges []
      )()
end
