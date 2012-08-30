(*
 *
 * Copyright (c) 2012-2013, 
 *  Wes Weimer          <weimer@cs.virginia.edu>
 *  Stephanie Forrest   <forrest@cs.unm.edu>
 *  Claire Le Goues     <legoues@cs.virginia.edu>
 *  Eric Schulte        <eschulte@cs.unm.edu>
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
    functionality, with the notable exception of the use of oprofile sampling
    for localization. *)

open Printf
open Global
open Gaussian
open Rep
open Stringrep

(**/**)
let asm_code_only = ref false
let _ =
  options := !options @
    [
      "--asm-code-only", Arg.Set asm_code_only,
      " Limit mutation operators to code sections of assembly files";
    ]

let asmRep_version = "3"
(**/**)

(** @version 3 *)
class asmRep = object (self : 'self_type)
  (** to avoid duplicating coverage generation code between [Elfrep.elfRep] and
      [Asmrep.asmRep] *)
  inherit binRep as super

  (** stores the beginning and ends of actual code sections in the assembly
      file(s) *)
  val range = ref [ ]

  (**/**)
  method copy () : 'self_type = 
    let super_copy = super#copy () in 
      super_copy#internal_copy()

  method internal_copy () : 'self_type =
    {<
      genome  = ref (Global.copy !genome)  ;
      range = ref (Global.copy !range) ;
    >}

  method from_source (filename : string) =
    super#from_source filename;
    if !asm_code_only then begin
      let beg_points = ref [] in
      let end_points = ref [] in
      (* beg/end start and stop code sections respectively *)
      let beg_regx = Str.regexp "^[0-9a-zA-Z_]+:$" in
      let end_regx = Str.regexp "^[ \t]+\\.size.*" in
      let in_code_p = ref false in
        Array.iteri (fun i line ->
          if  i > 0 then 
            if !in_code_p then begin
              if Str.string_match end_regx (List.hd line) 0 then begin
                in_code_p := false ;
                end_points := i :: !end_points ;
              end
            end else if Str.string_match beg_regx (List.hd line) 0 then begin
              in_code_p := true ;
              beg_points := i :: !beg_points ;
            end
        ) !genome ;
        if !in_code_p then
          end_points := (Array.length !genome) :: !end_points ;
        range := List.rev (List.combine !beg_points !end_points) ;
    end

  method serialize ?out_channel ?global_info (filename : string) =
    let fout =
      match out_channel with
      | Some(v) -> v
      | None -> open_out_bin filename
    in
      Marshal.to_channel fout (asmRep_version) [] ;
      Marshal.to_channel fout (!range) [] ;
      Marshal.to_channel fout (!genome) [] ;
      debug "asm: %s: saved\n" filename ;
      super#serialize ~out_channel:fout ?global_info:global_info filename ;
      if out_channel = None then close_out fout
  (**/**)

  (** @raise Fail("version mismatch") if the version specified in the binary file is
      different from the current [asmRep_version] *)
  method deserialize ?in_channel ?global_info (filename : string) =
    let fin =
      match in_channel with
      | Some(v) -> v
      | None -> open_in_bin filename
    in
    let version = Marshal.from_channel fin in
      if version <> asmRep_version then begin
        debug "asm: %s has old version\n" filename ;
        failwith "version mismatch"
      end ;
      range := Marshal.from_channel fin ;
      genome := Marshal.from_channel fin ;
      debug "asm: %s: loaded\n" filename ;
      super#deserialize ~in_channel:fin ?global_info:global_info filename ;
      if in_channel = None then close_in fin

  (**/**)
  method max_atom () =
    if !asm_code_only then
      List.fold_left (+) 0 (List.map (fun (a,b) -> (b - a)) !range)
    else
      Array.length !genome

  method atom_id_of_source_line source_file source_line =
    (* return the in-code offset from the global offset *)
    if !asm_code_only then
      List.fold_left (+) 0 (List.map (fun (a,b) ->
        if a > source_line then
          if b > source_line then b - a
          else source_line - a
        else 0) !range)
    else
      source_line

  method source_line_of_atom_id atom_id =
    (* return global offset from in-code offset *)
    if !asm_code_only then
      let i,j = 
        lfoldl (fun (i,j) (b,e) ->
          if j = 0 then 
            let chunk_size = e - b in
              if i > chunk_size then
                i - chunk_size, j
              else i, b + i
          else i, j
        ) (atom_id, 0) !range in
        j
    else atom_id
  (**/**)

  (** can fail if the [Unix.system] call to gdb fails; it does not currently
      check the return value of the call to [Unix.system] that dispatches to
      gdb. *)
  method mem_mapping asm_name bin_name =
    let asm_lines = get_lines asm_name in
    let lose_by_regexp_ind reg_str indexes =
      let lst = List.map (fun i -> (i, List.nth asm_lines i)) indexes in
      let it = ref [] in
      let regexp = Str.regexp reg_str in
        List.iter (fun (i, line) ->
          if not (Str.string_match regexp line 0) then
            it := i :: !it) lst ;
        (List.rev !it) in
    let gdb_disassemble func =
      let tmp = Filename.temp_file func ".gdb-output" in
      let gdb_command = 
        "gdb --batch --eval-command=\"disassemble "^func^"\" "^bin_name^">"^tmp
      in
        ignore (Unix.system gdb_command); get_lines tmp 
    in
    let addrs func =
      let regex = 
        Str.regexp "[ \t]*0x\\([a-zA-Z0-9]+\\)[ \t]*<\\([^ \t]\\)*>:.*" 
      in
      let it = ref [] in
        List.iter (fun line ->
          if (Str.string_match regex line 0) then
            it := (Str.matched_group 1 line) :: !it)
          (gdb_disassemble func) ;
        List.rev !it in
    let lines func =
      let on = ref false in
      let collector = ref [] in
      let regex = Str.regexp "^\\([^\\.][^ \t]+\\):" in
        Array.iteri (fun i line ->
          if !on then
            collector := i :: !collector;
          if Str.string_match regex line 0 then
            if (String.compare func (Str.matched_group 1 line)) = 0 then
              on := true
            else on := false)
          (Array.of_list asm_lines) ;
        List.rev !collector in
    let asm_lines = 
      lfilt 
        (fun line -> 
          Str.string_match (Str.regexp "^[^\\.][a-zA-Z0-9]*:") line 0)
        asm_lines
    in
    let asm_lines = 
      lmap (fun line -> String.sub line 0 (String.length line - 1)) asm_lines
    in
    let asm_lines = 
      lflatmap
        (fun func ->
          let f_lines = lose_by_regexp_ind "^[ \t]*\\." (lines func) in
          let f_addrs = 
            lmap (fun str -> int_of_string ("0x"^str)) (addrs func) 
          in
          let len = min (llen f_lines) (llen f_addrs) in
          let sub lst n = 
            Array.to_list (Array.sub (Array.of_list lst) 0 n) 
          in
            List.combine (sub f_addrs len) (sub f_lines len))
        asm_lines
    in   
    let map = List.sort pair_compare asm_lines in
    let hash = Hashtbl.create (List.length map) in
      List.iter (fun (addr, count) -> Hashtbl.add hash addr count) map ;
      hash

  (**/**)
  method private combine_coverage samples map =
    let results = Hashtbl.create (List.length samples) in
      List.iter
        (fun (addr, count) ->
          if Hashtbl.mem map addr then begin
            let line_num = Hashtbl.find map addr in
            let current = ht_find results line_num (fun _ -> 0.0) in
              Hashtbl.replace results line_num (current +. count)
          end) samples ;
      List.sort pair_compare
        (Hashtbl.fold (fun a b accum -> (a,b) :: accum) results [])
  (**/**)

  (** will print a warning, but not abort, if given invalid atom ids *)
  method swap i_off j_off =
    try
      let i = self#source_line_of_atom_id i_off in
      let j = self#source_line_of_atom_id j_off in
        super#swap i j ;
        let temp = !genome.(i) in
          !genome.(i) <- !genome.(j) ;
          !genome.(j) <- temp
    with Invalid_argument(arg) -> 
      debug "swap invalid argument %s\n" arg;

  (** will print a warning, but not abort, if given an invalid atom id *)
  method delete i_off =
    try
      let i = self#source_line_of_atom_id i_off in
        super#delete i ;
        !genome.(i) <- []
    with Invalid_argument(arg) -> 
      debug "delete invalid argument %s\n" arg;

  (** will print a warning, but not abort, if given invalid atom ids *)
  method append i_off j_off =
    try
      let i = self#source_line_of_atom_id i_off in
      let j = self#source_line_of_atom_id j_off in
        super#append i j ;
        !genome.(i) <- !genome.(i) @ !genome.(j)
    with Invalid_argument(arg) -> 
      debug "append invalid argument %s\n" arg;

  (** will print a warning, but not abort, if given invalid atom ids *)
  method replace i_off j_off =
    try
      let i = self#source_line_of_atom_id i_off in
      let j = self#source_line_of_atom_id j_off in
        super#replace i j ;
        !genome.(i) <- !genome.(j) ;
    with Invalid_argument(arg) -> 
      debug "replace invalid argument %s\n" arg;

end
