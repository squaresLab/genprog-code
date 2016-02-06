(*
 *
 * Copyright (c) 2012-2015, 
 *  Wes Weimer          <weimer@cs.virginia.edu>
 *  Stephanie Forrest   <forrest@cs.unm.edu>
 *  Claire Le Goues     <legoues@cs.virginia.edu>
 *  Jonathan Dorn       <dorn@cs.virginia.edu>
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

let stringRep_version = "2"

class stringRep = object (self : 'self_type)
  inherit [string edit_history, string] faultlocRepresentation
    as super

  (**/**)

  (** The collection of strings representing the original program. Maps
      filenames to an array of strings for that file and the base atom ID of
      the first string in the file. Thus, atom ID x refers to the string at
      index (x - base).

      Shared between all clones. *)
  val global_code = ref StringMap.empty

  (** The ID to assign to the next line inserted into the program. Shared
      between all clones so that crossover remains moderately meaningful. *)
  val next_id = ref 1

  (** The genome: a list of edits and the ID of the first inserted line for
      each. Unique to each instance. *)
  val mutable genome = []

  (* only need copy if we have an object-specific value with a mutable type,
     as opposed to a mutable value of an immutable type *)
  (*
  method copy () : 'self_type =
    let super_copy : 'self_type = super#copy () in
      super_copy
  *)

  method serialize ?out_channel ?global_info filename =
    let fout =
      match out_channel with
      | Some(v) -> v
      | None -> open_out_bin filename
    in
      Marshal.to_channel fout (stringRep_version) [] ;
      Marshal.to_channel fout (!global_code) [] ;
      Marshal.to_channel fout (!next_id) [] ;
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
      global_code := Marshal.from_channel fin ;
      next_id     := Marshal.from_channel fin ;
      genome      <- Marshal.from_channel fin ;
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
    global_code := StringMap.empty;
    next_id := 1;
    genome <- [];
    iter_lines filename (fun line ->
      let fname =
        if line.[0] = '/' then line else Filename.concat !prefix line
      in
      let lines = Array.of_list (get_lines fname) in
        global_code := StringMap.add line (!next_id, lines) !global_code;
        next_id := !next_id + (Array.length lines)
    )

  method get_atoms () =
    StringMap.fold (fun _ (base, lines) atoms ->
      fst (Array.fold_left (fun (atoms, i) _ ->
        AtomSet.add i atoms, i+1
      ) (atoms, base) lines)
    ) !global_code AtomSet.empty

  method atom_id_of_source_line source_file source_line =
    let check_one_file base lines =
      if source_line <= 0 || (Array.length lines) < source_line then []
      else [source_line + base]
    in
    if source_file = "" then
      StringMap.fold (fun _ (base, lines) ids ->
        (check_one_file base lines) @ ids
      ) !global_code []
    else
      try
        let base, lines = StringMap.find source_file !global_code in
          check_one_file base lines
      with Not_found -> []

  method instrument_fault_localization _ _ _ =
    failwith "stringRep: no fault localization"

  method debug_info () =
    let nlines =
      StringMap.fold (fun _ (_, lines) sum ->
        sum + (Array.length lines)
      ) !global_code 0
    in
      debug "stringRep: %d lines\n" nlines

  method get i =
    let line =
      StringMap.fold (fun _ (base, lines) line ->
        if base <= i && i < base + (Array.length lines) then
          Some( lines.( i - base ) )
        else
          line
      ) !global_code None
    in
      match line with
      | Some(line) -> line
      | None -> failwith ("invalid atom ID: "^(string_of_int i))

  method reduce_fix_space () =
    super#reduce_fix_space ();
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
      ) !fix_localization

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
    let edits = Hashtbl.create 11 in
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
      StringMap.fold (fun fname (base, lines) output_list ->
        (* Create a stack of identified lines from the original program. *)
        let lines = Array.to_list (Array.mapi (fun i l -> (i+base,l)) lines) in

        let body = add_line (Buffer.create 1024) lines in
          (Some(fname), Buffer.contents body) :: output_list
      ) !global_code []
      )()
end

